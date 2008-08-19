#include "sera3d.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_view_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the widgets for viewing controls
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_view_form(main_gui_t *gui)
{
  int i;  
  char cam_name[6][10];
  Pixmap pixmap;
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered build_view_form\n");
  
  gui->view_panel.form = (Widget)XmCreateForm(gui->inner_control_form, "Viewform", NULL, 0);
  XtVaSetValues(gui->view_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);
  XtManageChild(gui->view_panel.form);

  gui->view_panel.view_style_label = XtVaCreateManagedWidget("Style", xmLabelWidgetClass,
							     gui->view_panel.form,
							     XmNleftAttachment,XmATTACH_FORM,
							     XmNrightAttachment, XmATTACH_NONE,
							     XmNbottomAttachment, XmATTACH_NONE,
							     XmNtopAttachment, XmATTACH_FORM,
							     XmNtopOffset, 8,
							     NULL); 

   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)points_bits, 
					points_width, points_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->view_panel.view_s[0] = XtVaCreateManagedWidget("vs0",
						       xmDrawnButtonWidgetClass,gui->view_panel.form,
						       XmNleftAttachment, XmATTACH_WIDGET,
						       XmNleftWidget, gui->view_panel.view_style_label,
						       XmNtopAttachment, XmATTACH_FORM,
						       XmNtopWidget, gui->view_panel.view_style_label,
						       XmNleftOffset, 0,
						       XmNlabelType,        XmPIXMAP,
						       XmNlabelPixmap,      pixmap,
						       XmNbottomAttachment, XmATTACH_NONE,
						       XmNrightAttachment, XmATTACH_NONE,
						       XmNshadowType, XmSHADOW_IN,
						       NULL); 
   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)outline_bits, 
					outline_width, outline_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->view_panel.view_s[1] = XtVaCreateManagedWidget("vs1",
						       xmDrawnButtonWidgetClass, gui->view_panel.form,
						       XmNleftAttachment, XmATTACH_WIDGET,
						       XmNleftWidget, gui->view_panel.view_s[0],
						       XmNtopAttachment, XmATTACH_FORM,
						       XmNtopWidget, gui->view_panel.view_style_label,
						       XmNleftOffset, 0,
						       XmNlabelType,        XmPIXMAP,
						       XmNlabelPixmap,      pixmap,
						       XmNbottomAttachment, XmATTACH_NONE,
						       XmNrightAttachment, XmATTACH_NONE,
						       XmNshadowType, XmSHADOW_OUT,
						       NULL); 
   
   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)solid_bits, 
					solid_width, solid_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->view_panel.view_s[2] = XtVaCreateManagedWidget("vs2",
						       xmDrawnButtonWidgetClass,gui->view_panel.form,
						       XmNleftAttachment, XmATTACH_WIDGET,
						       XmNleftWidget, gui->view_panel.view_s[1],
						       XmNtopAttachment, XmATTACH_FORM,
						       XmNtopWidget, gui->view_panel.view_style_label,
						       XmNleftOffset, 0,
						       XmNlabelType,        XmPIXMAP,
						       XmNlabelPixmap,      pixmap,
						       XmNbottomAttachment, XmATTACH_NONE,
						       XmNrightAttachment, XmATTACH_NONE,
						       XmNshadowType, XmSHADOW_OUT,
						       NULL); 
      pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)polys_bits, 
					polys_width, polys_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->view_panel.view_s[3] = XtVaCreateManagedWidget("vs3",
						       xmDrawnButtonWidgetClass,gui->view_panel.form,
						       XmNleftAttachment, XmATTACH_WIDGET,
						       XmNleftWidget, gui->view_panel.view_s[2],
						       XmNtopAttachment, XmATTACH_FORM,
						       XmNtopWidget, gui->view_panel.view_style_label,
						       XmNleftOffset, 0,
						       XmNlabelType,        XmPIXMAP,
						       XmNlabelPixmap,      pixmap,
						       XmNbottomAttachment, XmATTACH_NONE,
						       XmNrightAttachment, XmATTACH_NONE,
						       XmNshadowType, XmSHADOW_OUT,
						       NULL);    

   for (i=0;i<4;i++) XtAddCallback(gui->view_panel.view_s[i], XmNactivateCallback, View_Style_changedCB, (XtPointer)gui);
   
   gui->view_panel.divider_view_style = XtVaCreateManagedWidget("divider",
						xmSeparatorWidgetClass, gui->view_panel.form,
						XmNorientation, XmHORIZONTAL,
						XmNseparatorType, XmSHADOW_ETCHED_OUT,
						XmNrightAttachment, XmATTACH_FORM,
						XmNleftAttachment, XmATTACH_FORM,
						XmNbottomAttachment, XmATTACH_NONE,
						XmNtopAttachment, XmATTACH_WIDGET,
						XmNtopWidget, gui->view_panel.view_s[0],
						XmNtopOffset, 0,
						XmNrightOffset, 5,
						XmNleftOffset, 5,
						NULL); 


   xmstr = XmStringCreateLocalized("Global Scaling (%)");
   gui->view_panel.scaling_slider = (Widget)XtVaCreateManagedWidget("Scalingslider",
								    xmScaleWidgetClass,
								    gui->view_panel.form,
								    XmNrightAttachment, XmATTACH_FORM,
								    XmNrightOffset, 5,
								    XmNleftAttachment, XmATTACH_FORM,
								    XmNleftOffset,10,
								    XmNbottomAttachment, XmATTACH_NONE,
								    XmNtopAttachment, XmATTACH_WIDGET,
								    XmNtopWidget, gui->view_panel.divider_view_style,
								    XmNtopOffset, 0,
								    XmNvalue, (int)(gui->scaling*100),
								    XmNminimum, 50,
								    XmNmaximum, 1000,
								    XmNshowValue, True,
								    XmNscaleWidth, 175,
								    XmNscaleHeight, 15,
                                                                    XmNscaleMultiple, 10,
								    XmNorientation, XmHORIZONTAL,
								    XmNtitleString, xmstr,
								    NULL);
   XmStringFree(xmstr);
   
   XtAddCallback(gui->view_panel.scaling_slider,XmNdragCallback, Scaling_changedCB, (XtPointer)gui);
   XtAddCallback(gui->view_panel.scaling_slider,XmNvalueChangedCallback, Scaling_changedCB, (XtPointer)gui);
/*   XtAddCallback(gui->view_panel.scaling_slider, XmNvalueChangedCallback, (XtCallbackProc)Unset_High_Lists, (XtPointer)gui);*/


  gui->view_panel.divider_scaling = XtVaCreateManagedWidget("divider",
					    xmSeparatorWidgetClass, gui->view_panel.form,
					    XmNorientation, XmHORIZONTAL,
					    XmNseparatorType, XmSHADOW_ETCHED_OUT,
					    XmNrightAttachment, XmATTACH_FORM,
					    XmNleftAttachment, XmATTACH_FORM,
					    XmNbottomAttachment, XmATTACH_NONE,
					    XmNtopAttachment, XmATTACH_WIDGET,
					    XmNtopWidget, gui->view_panel.scaling_slider,
					    XmNtopOffset, 3,
					    XmNrightOffset, 5,
					    XmNleftOffset, 5,
					    NULL); 

  gui->view_panel.cam_label = (Widget)XtVaCreateManagedWidget("Camera", 
							      xmLabelWidgetClass,
							      gui->view_panel.form,
							      XmNleftAttachment,XmATTACH_FORM,
							      XmNleftOffset, 10,
							      XmNrightAttachment, XmATTACH_NONE,
							      XmNtopAttachment, XmATTACH_WIDGET,
							      XmNtopWidget, gui->view_panel.divider_scaling,
							      XmNtopOffset, 3,
							      NULL);

  strcpy(cam_name[1],"Anterior");
  strcpy(cam_name[2],"Right");
  strcpy(cam_name[3],"Left");
  strcpy(cam_name[4],"Posterior");
  strcpy(cam_name[5],"Top");

  for (i = 1;i<6;i++){
    if (i==1)
      gui->view_panel.camera[i] = (Widget)XtVaCreateManagedWidget(cam_name[i],
								  xmToggleButtonWidgetClass, gui->view_panel.form,
								  XmNleftAttachment, XmATTACH_FORM,
								  XmNrightAttachment, XmATTACH_NONE,
								  XmNtopAttachment, XmATTACH_WIDGET,
								  XmNtopWidget, gui->view_panel.cam_label,
								  XmNleftOffset, 5,
								  XmNtopOffset,  5,
								  NULL);
    else
      gui->view_panel.camera[i] = (Widget)XtVaCreateManagedWidget(cam_name[i],
								  xmToggleButtonWidgetClass, gui->view_panel.form,
								  XmNleftAttachment, XmATTACH_FORM,
								  XmNrightAttachment, XmATTACH_NONE,
								  XmNtopAttachment, XmATTACH_WIDGET,
								  XmNtopWidget, gui->view_panel.camera[i-1],
								  XmNleftOffset, 5,
								  NULL);

    XtAddCallback(gui->view_panel.camera[i], XmNvalueChangedCallback, Camera_ChangedCB, (XtPointer)gui);
  }

  /* Reset the labels */
  /*changeLabels( gui, VIEW_PANEL );*/

  gui->view_panel.vert_divider = XtVaCreateManagedWidget("vert_divider",
							 xmSeparatorWidgetClass, gui->view_panel.form,
							 XmNorientation, XmVERTICAL,
							 XmNseparatorType, XmSHADOW_ETCHED_OUT,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNleftAttachment, XmATTACH_WIDGET,
							 XmNleftWidget, gui->view_panel.camera[4],
							 XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
							 XmNbottomWidget, gui->view_panel.camera[5],
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->view_panel.divider_scaling,
							 XmNbottomOffset, 5,
							 XmNtopOffset, 5,
							 XmNleftOffset, 0,
							 NULL);

  gui->view_panel.rot_label = (Widget)XtVaCreateManagedWidget("Auto \nRotate", 
							      xmLabelWidgetClass,gui->view_panel.form,
							      XmNleftAttachment,XmATTACH_WIDGET,
							      XmNleftWidget, gui->view_panel.vert_divider,
							      XmNleftOffset, 10,
							      XmNrightAttachment, XmATTACH_NONE,
							      XmNtopAttachment, XmATTACH_WIDGET,
							      XmNtopWidget, gui->view_panel.divider_scaling,
							      XmNtopOffset, 5,
							      NULL); 

  gui->view_panel.auto_rotate_left = XtVaCreateManagedWidget("auto_rotate_left",
							     xmArrowButtonWidgetClass, gui->view_panel.form,
							     XmNarrowDirection, XmARROW_LEFT,
							     XmNleftAttachment, XmATTACH_WIDGET,
							     XmNleftWidget, gui->view_panel.vert_divider,
							     XmNleftOffset, 15,
							     XmNtopAttachment, XmATTACH_WIDGET,
							     XmNtopWidget, gui->view_panel.divider_scaling,
							     XmNtopOffset, 75,
							     NULL);
  gui->view_panel.auto_rotate_up = XtVaCreateManagedWidget("auto_rotate_up",
							   xmArrowButtonWidgetClass, gui->view_panel.form,
							   XmNarrowDirection, XmARROW_UP,
							   XmNleftAttachment, XmATTACH_WIDGET,
							   XmNleftWidget, gui->view_panel.auto_rotate_left,
							   XmNleftOffset, 0,
							   XmNbottomAttachment, XmATTACH_WIDGET,
							   XmNbottomWidget, gui->view_panel.auto_rotate_left,
							   XmNbottomOffset, 0,
							   NULL);
  gui->view_panel.auto_rotate_down = XtVaCreateManagedWidget("auto_rotate_down",
							     xmArrowButtonWidgetClass, gui->view_panel.form,
							     XmNarrowDirection, XmARROW_DOWN,
							     XmNleftAttachment, XmATTACH_WIDGET,
							     XmNleftWidget, gui->view_panel.auto_rotate_left,
							     XmNleftOffset, 0,
							     XmNtopAttachment, XmATTACH_WIDGET,
							     XmNtopWidget, gui->view_panel.auto_rotate_left,
							     XmNtopOffset, 0,
							     NULL);
  gui->view_panel.auto_rotate_right = XtVaCreateManagedWidget("auto_rotate_right",
							      xmArrowButtonWidgetClass, gui->view_panel.form,
							      XmNarrowDirection, XmARROW_RIGHT,
							      XmNleftAttachment, XmATTACH_WIDGET,
							      XmNleftWidget, gui->view_panel.auto_rotate_up,
							      XmNleftOffset, 0,
							      XmNtopAttachment, XmATTACH_WIDGET,
							      XmNtopWidget, gui->view_panel.auto_rotate_up,
							      XmNtopOffset, 0,
							      NULL);
  
  pixmap = XCreatePixmapFromBitmapData(gui->display,
				       RootWindowOfScreen(XtScreen(gui->form)),
				       (char *)stop_bits, stop_width, stop_height,
				       gui->hl,gui->fg,
				       DefaultDepthOfScreen(XtScreen(gui->form)));
  gui->view_panel.auto_rotate_stop = XtVaCreateManagedWidget("stop_rotate",
							     xmDrawnButtonWidgetClass,gui->view_panel.form,
							     XmNleftAttachment, XmATTACH_WIDGET,
							     XmNleftWidget, gui->view_panel.auto_rotate_left,
							     XmNtopAttachment, XmATTACH_WIDGET,
							     XmNtopWidget, gui->view_panel.auto_rotate_up,
							     XmNleftOffset, 0,
							     XmNtopOffset, 0,
							     XmNlabelType,        XmPIXMAP,
							     XmNlabelPixmap,      pixmap,
							     XmNshadowType, XmSHADOW_OUT,
							     XmNshadowThickness, 2,
							     NULL); 
  
  XtAddCallback(gui->view_panel.auto_rotate_left, XmNactivateCallback,	Auto_RotateCB, (XtPointer)gui);
  XtAddCallback(gui->view_panel.auto_rotate_right, XmNactivateCallback,	Auto_RotateCB, (XtPointer)gui);
  XtAddCallback(gui->view_panel.auto_rotate_up, XmNactivateCallback,	Auto_RotateCB, (XtPointer)gui);
  XtAddCallback(gui->view_panel.auto_rotate_down, XmNactivateCallback,	Auto_RotateCB, (XtPointer)gui);
  XtAddCallback(gui->view_panel.auto_rotate_stop, XmNactivateCallback,	Auto_RotateCB, (XtPointer)gui);


  
  gui->view_panel.deg_up = XtVaCreateManagedWidget("deg_up",
						   xmArrowButtonWidgetClass, gui->view_panel.form,
						   XmNarrowDirection, XmARROW_UP,
						   XmNleftAttachment, XmATTACH_WIDGET,
						   XmNleftWidget, gui->view_panel.vert_divider,
						   XmNleftOffset, 25,
						   XmNtopAttachment, XmATTACH_WIDGET,
						   XmNtopWidget, gui->view_panel.auto_rotate_down,
						   XmNtopOffset, 10,
						   NULL);
  gui->view_panel.deg_down = XtVaCreateManagedWidget("deg_down",
						     xmArrowButtonWidgetClass, gui->view_panel.form,
						     XmNarrowDirection, XmARROW_DOWN,
						     XmNleftAttachment, XmATTACH_WIDGET,
						     XmNleftWidget, gui->view_panel.deg_up,
						     XmNleftOffset, 0,
						     XmNtopAttachment, XmATTACH_WIDGET,
						     XmNtopWidget, gui->view_panel.auto_rotate_down,
						     XmNtopOffset, 10,
						     NULL);
  gui->view_panel.deg_label = XtVaCreateManagedWidget("10 Degs",
						      xmLabelWidgetClass, gui->view_panel.form,
						      XmNarrowDirection, XmARROW_UP,
						      XmNleftAttachment, XmATTACH_WIDGET,
						      XmNleftWidget, gui->view_panel.vert_divider,
						      XmNleftOffset, 15,
						      XmNtopAttachment, XmATTACH_WIDGET,
						      XmNtopWidget, gui->view_panel.deg_up,
						      XmNtopOffset, 5,
						      NULL);
  XtAddCallback(gui->view_panel.deg_up, XmNactivateCallback, Auto_Rotate_Degs_ChangedCB, (XtPointer)gui);
  XtAddCallback(gui->view_panel.deg_down, XmNactivateCallback, Auto_Rotate_Degs_ChangedCB, (XtPointer)gui);




  gui->view_panel.divider = XtVaCreateManagedWidget("divider",
						    xmSeparatorWidgetClass, gui->view_panel.form,
						    XmNorientation, XmHORIZONTAL,
						    XmNseparatorType, XmSHADOW_ETCHED_OUT,
						    XmNrightAttachment, XmATTACH_FORM,
						    XmNleftAttachment, XmATTACH_FORM,
						    XmNtopAttachment, XmATTACH_WIDGET,
						    /*XmNtopWidget, gui->view_panel.camera[5],*/
						    XmNtopWidget, gui->view_panel.deg_label,
						    XmNtopOffset, 5,
						    XmNrightOffset, 5,
						    XmNleftOffset, 5,
						    NULL);

 /*************************************************/
 /** Reset Button - resets the view back to the
 /**   original place, all object rotations are
 /**   undone.
 /*************************************************/
  gui->view_panel.reset_button = (Widget)XtVaCreateManagedWidget("RESET VIEWING",
								 xmPushButtonWidgetClass, gui->view_panel.form,
								 XmNleftAttachment, XmATTACH_FORM,
								 XmNrightAttachment, XmATTACH_FORM,
								 XmNbottomAttachment, XmATTACH_FORM,
								 XmNtopAttachment, XmATTACH_WIDGET,
								 XmNtopWidget, gui->view_panel.divider,
								 XmNleftOffset, 5,
								 XmNtopOffset, 5,
								 XmNbottomOffset,  5,
								 XmNrightOffset, 5,
								 NULL);
  
  XtAddCallback(gui->view_panel.reset_button, XmNactivateCallback, ResetButtonCB, (XtPointer)gui);

  DEBUG_TRACE_OUT printf("Done with build_view_form\n");
}
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_color_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the widgets for controlling the color
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_color_form(main_gui_t *gui)
{
  int i;
  XColor r,g,b;
  XColor swatch;
  XmString xmstr;
  
  DEBUG_TRACE_IN printf("Entered build_color_form\n");

  
  init_color(gui,65535, 0,0, &r);
  init_color(gui,0,65535, 0, &g);
  init_color(gui,0,0,65535, &b);
    
  gui->color_panel.form = XmCreateForm(gui->inner_control_form, "Colorform", NULL, 0);
  
  XtVaSetValues(gui->color_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);
  
  gui->color_panel.body_label = (Widget)XtVaCreateManagedWidget("Body :", 
								xmLabelWidgetClass, gui->color_panel.form,
								XmNleftAttachment,XmATTACH_FORM,
								XmNrightAttachment, XmATTACH_NONE,
								XmNbottomAttachment, XmATTACH_NONE,
								XmNtopAttachment, XmATTACH_FORM,
								XmNleftOffset, 5,
								XmNrightOffset, 5,
								XmNtopOffset, 8,
								XmNhighlightOnEnter, TRUE,
								NULL);

  gui->color_panel.body_pane = (Widget)XmCreatePulldownMenu(gui->color_panel.form,"body_pane",NULL, 0);
  gui->color_panel.body_menu = (Widget)XtVaCreateManagedWidget("Body_menu",xmRowColumnWidgetClass,gui->color_panel.form,
                XmNmarginHeight,       0,
                XmNmarginWidth,        0,
                XmNpacking,            XmPACK_TIGHT,
                XmNpopupEnabled,       TRUE,
                XmNrowColumnType,      XmMENU_OPTION,
                XmNspacing,            0,
		XmNsubMenuId, gui->color_panel.body_pane, 
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, gui->color_panel.body_label,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftOffset, 2,
		XmNtopOffset, 5,
		XmNrightOffset, 5,
		NULL);

  gui->color_panel.bodies[0] = (Widget)XtCreateManagedWidget("Empty",xmPushButtonWidgetClass,
							     gui->color_panel.body_pane, NULL, 0);

  gui->color_panel.color_presets_frame = XtVaCreateManagedWidget("Color Presets",
								 xmFrameWidgetClass, gui->color_panel.form,
								 XmNshadowType, XmSHADOW_IN,
								 XmNleftAttachment, XmATTACH_FORM,
								 XmNrightAttachment, XmATTACH_FORM,
								 XmNtopAttachment, XmATTACH_WIDGET,
								 XmNtopWidget, gui->color_panel.body_menu,
								 XmNleftOffset, 5,
								 XmNrightOffset, 5,
								 XmNtopOffset, 0,
								 NULL);
  

  gui->color_panel.color_presets_label = XtVaCreateManagedWidget ("Color Presets",xmLabelWidgetClass, 
								  gui->color_panel.color_presets_frame,
								  XmNchildType,XmFRAME_TITLE_CHILD,
								  NULL);  


  gui->color_panel.color_presets_list = (Widget)XmCreateScrolledList(gui->color_panel.color_presets_frame,
								     "ColorPresetsList", NULL,0);
  XtManageChild(gui->color_panel.color_presets_list);
  XtVaSetValues(gui->color_panel.color_presets_list,
		XmNselectionPolicy, XmBROWSE_SELECT,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftOffset , 5,		 
		XmNrightOffset , 5,		 
		XmNbottomOffset , 5,
		XmNtopOffset , 5,	   
		XmNvisibleItemCount, 3,
		NULL);

  XtAddCallback (gui->color_panel.color_presets_list, XmNbrowseSelectionCallback, ColorPresetCB, (XtPointer)gui);

  xmstr = XmStringCreateLocalized("Scalp1");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,1); XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Scalp2");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,2); XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Skull1");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,3); XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Skull2");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,4); XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Brain1");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,5); XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Brain2");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,6); XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Target1");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,7); XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Target2");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,8); XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Tumor1");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,9); XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Tumor2");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,10);XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Red");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,11);XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Green");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,12);XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Blue");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,13);XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Cyan");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,14);XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Magenta");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,15);XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("Yellow");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,16);XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("White");
  XmListAddItem(gui->color_panel.color_presets_list,xmstr,17);XmStringFree(xmstr);


  gui->color_panel.divider_swatch = (Widget)XtVaCreateManagedWidget("divider_swatch",
								    xmSeparatorWidgetClass, gui->color_panel.form,
								    XmNorientation, XmHORIZONTAL,
								    XmNseparatorType, XmSHADOW_ETCHED_OUT,
								    XmNrightAttachment, XmATTACH_FORM,
								    XmNleftAttachment, XmATTACH_FORM,
								    XmNtopAttachment, XmATTACH_WIDGET,
								    XmNtopWidget, gui->color_panel.color_presets_frame,
								    XmNtopOffset, 5,
								    XmNrightOffset, 5,
								    XmNleftOffset, 5,
								    NULL);

  gui->color_panel.main_color_swatch = XtVaCreateManagedWidget("color_swatch",
							       xmDrawnButtonWidgetClass, gui->color_panel.form,
							       XmNtopAttachment, XmATTACH_WIDGET,
							       XmNtopWidget, gui->color_panel.divider_swatch,
							       XmNrightAttachment, XmATTACH_FORM,
							       XmNleftAttachment, XmATTACH_FORM,
							       XmNbottomAttachment, XmATTACH_NONE,
							       XmNrightOffset, 15,
							       XmNleftOffset, 15,
							       XmNtopOffset, 5,
							       XmNheight, 30,
							       NULL);
  gui->color_panel.divider = (Widget)XtVaCreateManagedWidget("divider",
							     xmSeparatorWidgetClass, gui->color_panel.form,
							     XmNorientation, XmHORIZONTAL,
							     XmNseparatorType, XmSHADOW_ETCHED_OUT,
							     XmNrightAttachment, XmATTACH_FORM,
							     XmNleftAttachment, XmATTACH_FORM,
							     XmNtopAttachment, XmATTACH_WIDGET,
							     XmNtopWidget, gui->color_panel.main_color_swatch,
							     XmNtopOffset, 5,
							     XmNrightOffset, 5,
							     XmNleftOffset, 5,
							     NULL);

  gui->color_panel.r_slider_label = (Widget)XtVaCreateManagedWidget(" R ", xmLabelWidgetClass,
								    gui->color_panel.form,
								    XmNleftAttachment,XmATTACH_FORM,
								    XmNleftOffset, 5,
								    XmNrightAttachment, XmATTACH_NONE,
								    XmNtopAttachment, XmATTACH_WIDGET,
								    XmNtopWidget, gui->color_panel.divider,
								    XmNtopOffset, 5,
								    XmNbottomAttachment, XmATTACH_NONE,
								    NULL);

  gui->color_panel.r_slider = (Widget)XtVaCreateManagedWidget("Redslider",xmScaleWidgetClass,
							      gui->color_panel.form,
							      XmNrightAttachment, XmATTACH_NONE,
							      XmNleftAttachment, XmATTACH_WIDGET,
							      XmNleftWidget, gui->color_panel.r_slider_label,
							      XmNleftOffset, 10,
							      XmNtopAttachment, XmATTACH_WIDGET,
							      XmNtopWidget, gui->color_panel.divider,
							      XmNtopOffset, 10,
							      XmNvalue, 0,
							      XmNminimum, 0,
							      XmNmaximum, 65535,
							      XmNscaleWidth, 150,
							      XmNscaleHeight, 15,
							      XmNorientation, XmHORIZONTAL,
							      XmNtopShadowColor, r.pixel,
							      XmNhighlightColor, r.pixel,
							      NULL);
  
  gui->color_panel.g_slider_label = (Widget)XtVaCreateManagedWidget(" G ", xmLabelWidgetClass,
								    gui->color_panel.form,
								    XmNleftAttachment,XmATTACH_FORM,
								    XmNleftOffset, 5,
								    XmNrightAttachment, XmATTACH_NONE,
								    XmNtopAttachment, XmATTACH_WIDGET,
								    XmNtopWidget, gui->color_panel.r_slider_label,
								    XmNtopOffset, 5,
								    XmNbottomAttachment, XmATTACH_NONE,
								    NULL);
  gui->color_panel.g_slider = (Widget)XtVaCreateManagedWidget("Greenslider",xmScaleWidgetClass,
							      gui->color_panel.form,
							      XmNrightAttachment, XmATTACH_NONE,
							      XmNleftAttachment, XmATTACH_WIDGET,
							      XmNleftWidget, gui->color_panel.g_slider_label,
							      XmNleftOffset, 10,
							      XmNbottomAttachment, XmATTACH_NONE, 
							      XmNtopAttachment, XmATTACH_WIDGET,
							      XmNtopWidget, gui->color_panel.r_slider_label,
							      XmNtopOffset, 10,
							      XmNvalue, 0,
							      XmNminimum, 0,
							      XmNmaximum, 65535,
							      XmNscaleWidth, 150,
							      XmNscaleHeight, 15,
							      XmNorientation, XmHORIZONTAL,
							      XmNtopShadowColor, g.pixel,
							      XmNhighlightColor, g.pixel,
							      NULL);

  gui->color_panel.b_slider_label = (Widget)XtVaCreateManagedWidget(" B ", xmLabelWidgetClass,
			   gui->color_panel.form,
			   XmNleftAttachment,XmATTACH_FORM,
			   XmNleftOffset, 5,
			   XmNrightAttachment, XmATTACH_NONE,
			   XmNtopAttachment, XmATTACH_WIDGET,
			   XmNtopWidget, gui->color_panel.g_slider_label,
			   XmNtopOffset, 5,
			   XmNbottomAttachment, XmATTACH_NONE,
			   NULL);
  gui->color_panel.b_slider = (Widget)XtVaCreateManagedWidget("Blueslider",xmScaleWidgetClass,
							      gui->color_panel.form,
							      XmNrightAttachment, XmATTACH_NONE,
							      XmNleftAttachment, XmATTACH_WIDGET,
							      XmNleftWidget, gui->color_panel.b_slider_label,
							      XmNleftOffset, 10,
							      XmNbottomAttachment, XmNONE, 
							      XmNtopAttachment, XmATTACH_WIDGET,
							      XmNtopWidget, gui->color_panel.g_slider_label,
							      XmNtopOffset, 10,
							      XmNvalue, 0,
							      XmNminimum, 0,
							      XmNmaximum, 65535,
							      XmNscaleWidth, 150,
							      XmNscaleHeight, 15,
							      XmNorientation, XmHORIZONTAL,
							      XmNtopShadowColor, b.pixel,
							      XmNhighlightColor, b.pixel,
							      NULL);

  XtAddCallback(gui->color_panel.r_slider, XmNdragCallback, (XtCallbackProc)body_color_changedCB,(XtPointer)gui);
  XtAddCallback(gui->color_panel.g_slider, XmNdragCallback, (XtCallbackProc)body_color_changedCB,(XtPointer)gui);
  XtAddCallback(gui->color_panel.b_slider, XmNdragCallback, (XtCallbackProc)body_color_changedCB,(XtPointer)gui);
  XtAddCallback(gui->color_panel.r_slider, XmNvalueChangedCallback, (XtCallbackProc)body_color_changedCB,(XtPointer)gui);
  XtAddCallback(gui->color_panel.g_slider, XmNvalueChangedCallback, (XtCallbackProc)body_color_changedCB,(XtPointer)gui);
  XtAddCallback(gui->color_panel.b_slider, XmNvalueChangedCallback, (XtCallbackProc)body_color_changedCB,(XtPointer)gui);

  gui->color_panel.apply_divider = XtVaCreateManagedWidget("apply",
							   xmSeparatorWidgetClass, gui->color_panel.form,
							   XmNorientation, XmHORIZONTAL,
							   XmNseparatorType, XmSHADOW_ETCHED_OUT,
							   XmNbottomAttachment, XmATTACH_NONE,
							   XmNtopAttachment, XmATTACH_WIDGET,
							   XmNtopWidget, gui->color_panel.b_slider_label,
							   XmNleftAttachment, XmATTACH_FORM,
							   XmNrightAttachment, XmATTACH_FORM,
							   XmNrightOffset, 5,
							   XmNtopOffset, 5,
							   XmNleftOffset, 5,
							   NULL);  
  
  gui->color_panel.apply_button = XtVaCreateManagedWidget("Apply to Body",
							  xmPushButtonWidgetClass, gui->color_panel.form,
							  XmNtopAttachment, XmATTACH_WIDGET,
							  XmNtopWidget, gui->color_panel.apply_divider,
							  XmNrightAttachment, XmATTACH_FORM, 
							  XmNleftAttachment, XmATTACH_FORM,
							  XmNbottomAttachment, XmATTACH_FORM,
							  XmNrightOffset, 5,
							  XmNleftOffset, 5,
							  XmNbottomOffset, 5,
							  XmNtopOffset, 5,
							  NULL);
  XtAddCallback(gui->color_panel.apply_button, XmNactivateCallback, ApplyColorToBodyCB, (XtPointer)gui);
  

  DEBUG_TRACE_OUT printf("Done with build_color_form\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_transparency_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the widgets for controlling the transparency
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_transparency_form(main_gui_t *gui)
{
  int i;

  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered build_transparency_form\n");
  
  gui->transparency_panel.form = (Widget)XmCreateForm(gui->inner_control_form, 
						      "Transparencyform", NULL, 0);
  XtVaSetValues(gui->transparency_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);
    
  gui->transparency_panel.transparency_label = (Widget)XtVaCreateManagedWidget("Body :", 
									       xmLabelWidgetClass, gui->transparency_panel.form,
									       XmNleftAttachment,XmATTACH_FORM,
									       XmNrightAttachment, XmATTACH_NONE,
									       XmNbottomAttachment, XmATTACH_NONE,
									       XmNtopAttachment, XmATTACH_FORM,
									       XmNleftOffset, 5,
									       XmNtopOffset, 10,
									       NULL);

  gui->transparency_panel.transparency_pane = (Widget)XmCreatePulldownMenu(gui->transparency_panel.form, "transparency_pane", NULL, 0);
  gui->transparency_panel.transparency_menu = (Widget)XtVaCreateManagedWidget("T_menu",xmRowColumnWidgetClass,gui->transparency_panel.form,
                XmNmarginHeight,       0,
                XmNmarginWidth,        0,
                XmNpacking,            XmPACK_TIGHT,
                XmNpopupEnabled,       TRUE,
                XmNrowColumnType,      XmMENU_OPTION,
                XmNspacing,            0,
		XmNsubMenuId, gui->transparency_panel.transparency_pane, 
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, gui->transparency_panel.transparency_label,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopOffset, 5,
		XmNtopAttachment, XmATTACH_FORM,
		NULL);

  gui->transparency_panel.transparencies[0] = (Widget)XtCreateManagedWidget("Empty",
									    xmPushButtonWidgetClass, gui->transparency_panel.transparency_pane, 
									    NULL, 0);
  
  gui->transparency_panel.t_presets_frame = XtVaCreateManagedWidget("Opacity Presets",
								    xmFrameWidgetClass, gui->transparency_panel.form,
								    XmNshadowType, XmSHADOW_IN,
								    XmNleftAttachment, XmATTACH_FORM,
								    XmNrightAttachment, XmATTACH_FORM,
								    XmNtopAttachment, XmATTACH_WIDGET,
								    XmNtopWidget, gui->transparency_panel.transparency_menu,
								    XmNleftOffset, 5,
								    XmNrightOffset, 5,
								    XmNtopOffset, 0,
								    NULL);
  
  gui->transparency_panel.t_presets_label = XtVaCreateManagedWidget ("Opacity Presets",xmLabelWidgetClass, 
								     gui->transparency_panel.t_presets_frame,
								     XmNchildType,XmFRAME_TITLE_CHILD,
								     NULL);  
  
   gui->transparency_panel.t_presets_list = (Widget)XmCreateScrolledList(gui->transparency_panel.t_presets_frame,
									 "TransPresetsList", NULL,0);
   XtManageChild(gui->transparency_panel.t_presets_list);
   XtVaSetValues(gui->transparency_panel.t_presets_list,
		 XmNselectionPolicy, XmBROWSE_SELECT,
                 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment, XmATTACH_FORM,
		 XmNtopAttachment, XmATTACH_FORM,
		 XmNbottomAttachment, XmATTACH_FORM,
		 XmNleftOffset , 5,		 
		 XmNrightOffset , 5,		 
		 XmNbottomOffset , 5,
		 XmNtopOffset , 5,	   
		 XmNvisibleItemCount, 3,
		 NULL);
   
   XtAddCallback (gui->transparency_panel.t_presets_list, XmNbrowseSelectionCallback,
		  TransparencyPresetCB, (XtPointer)gui);
		  
   xmstr = XmStringCreateLocalized("100%");
   XmListAddItem(gui->transparency_panel.t_presets_list,xmstr,1); XmStringFree(xmstr);
   xmstr = XmStringCreateLocalized("90%");
   XmListAddItem(gui->transparency_panel.t_presets_list,xmstr,2); XmStringFree(xmstr);
   xmstr = XmStringCreateLocalized("80%");
   XmListAddItem(gui->transparency_panel.t_presets_list,xmstr,3); XmStringFree(xmstr);
   xmstr = XmStringCreateLocalized("70%");
   XmListAddItem(gui->transparency_panel.t_presets_list,xmstr,4); XmStringFree(xmstr);
   xmstr = XmStringCreateLocalized("60%");
   XmListAddItem(gui->transparency_panel.t_presets_list,xmstr,5); XmStringFree(xmstr);
   xmstr = XmStringCreateLocalized("50%");
   XmListAddItem(gui->transparency_panel.t_presets_list,xmstr,6); XmStringFree(xmstr);
   xmstr = XmStringCreateLocalized("40%");
   XmListAddItem(gui->transparency_panel.t_presets_list,xmstr,7); XmStringFree(xmstr);
   xmstr = XmStringCreateLocalized("30%");
   XmListAddItem(gui->transparency_panel.t_presets_list,xmstr,8); XmStringFree(xmstr);
   xmstr = XmStringCreateLocalized("20%");
   XmListAddItem(gui->transparency_panel.t_presets_list,xmstr,9); XmStringFree(xmstr);
   xmstr = XmStringCreateLocalized("10%");
   XmListAddItem(gui->transparency_panel.t_presets_list,xmstr,10);XmStringFree(xmstr);


   gui->transparency_panel.divider = (Widget)XtVaCreateManagedWidget("divider",
								     xmSeparatorWidgetClass, gui->transparency_panel.form,
								     XmNorientation, XmHORIZONTAL,
								     XmNseparatorType, XmSHADOW_ETCHED_OUT,
								     XmNrightAttachment, XmATTACH_FORM,
								     XmNleftAttachment, XmATTACH_FORM,
								     XmNtopAttachment, XmATTACH_WIDGET,
								     XmNtopWidget, gui->transparency_panel.t_presets_frame,
								     XmNtopOffset, 5,
								     XmNrightOffset, 5,
								     XmNleftOffset, 5,
								     NULL);

   xmstr = XmStringCreateLocalized("Opacity (%)");
   gui->transparency_panel.t_slider = (Widget)XtVaCreateManagedWidget("t_slider",
								      xmScaleWidgetClass, gui->transparency_panel.form,
								      XmNrightAttachment, XmATTACH_NONE,
								      XmNleftAttachment, XmATTACH_FORM, 
								      XmNleftOffset, 15,
								      XmNshowValue, TRUE,
								      XmNtopAttachment, XmATTACH_WIDGET,
								      XmNtopWidget, gui->transparency_panel.divider,
								      XmNtopOffset, 5,
								      XmNvalue, 0,
								      XmNminimum, 0,
								      XmNmaximum, 100,
								      XmNscaleWidth, 175,
								      XmNscaleHeight, 15,
								      XmNorientation, XmHORIZONTAL,
								      XmNtitleString, xmstr,
								      NULL);
   XmStringFree(xmstr);
   
   gui->transparency_panel.apply_divider = (Widget)XtVaCreateManagedWidget("divider",
									   xmSeparatorWidgetClass, gui->transparency_panel.form,
									   XmNorientation, XmHORIZONTAL,
									   XmNseparatorType, XmSHADOW_ETCHED_OUT,
									   XmNrightAttachment, XmATTACH_FORM,
									   XmNleftAttachment, XmATTACH_FORM,
									   XmNtopAttachment, XmATTACH_WIDGET,
									   XmNtopWidget, gui->transparency_panel.t_slider,
									   XmNtopOffset, 5,
									   XmNrightOffset, 5,
									   XmNleftOffset, 5,
									   NULL);
   
   gui->transparency_panel.apply_button = XtVaCreateManagedWidget("Apply to Body",
								  xmPushButtonWidgetClass, gui->transparency_panel.form,
								  XmNbottomAttachment, XmATTACH_NONE,
								  XmNrightAttachment, XmATTACH_FORM,
								  XmNleftAttachment, XmATTACH_FORM,
								  XmNtopAttachment, XmATTACH_WIDGET,
								  XmNtopWidget, gui->transparency_panel.apply_divider,
								  XmNrightOffset, 5,
								  XmNleftOffset, 5,
								  XmNtopOffset, 5,
								  NULL);

   XtAddCallback(gui->transparency_panel.apply_button, XmNactivateCallback,
		 ApplyTransparencyToBodyCB, (XtPointer)gui);
  
   DEBUG_TRACE_OUT printf("Done with build_transparency_form\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_lighting_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the widgets for controlling the lighting.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_lighting_form(main_gui_t *gui)
{
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered build_lighting_form\n");

  gui->lighting_panel.form = (Widget)XmCreateForm(gui->inner_control_form, "Lightingform", NULL, 0);
  XtVaSetValues(gui->lighting_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);
   
  gui->lighting_panel.lights_frame = (Widget)XmCreateFrame(gui->lighting_panel.form, "lights_frame", NULL, 0);
  XtVaSetValues(gui->lighting_panel.lights_frame,
		XmNshadowType, XmSHADOW_ETCHED_IN,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_NONE,		
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 5,
		XmNleftOffset, 5,
		XmNtopOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  XtManageChild(gui->lighting_panel.lights_frame);  

  gui->lighting_panel.lights_form = (Widget)XmCreateForm(gui->lighting_panel.lights_frame, "Lights_form", NULL, 0);
  XtVaSetValues(gui->lighting_panel.lights_form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(gui->lighting_panel.lights_form);
  
  gui->lighting_panel.lights_label = (Widget)XtVaCreateManagedWidget ("Lights",
								      xmLabelWidgetClass, gui->lighting_panel.lights_frame,
								      XmNchildType,XmFRAME_TITLE_CHILD,
								      NULL);
  
  gui->lighting_panel.light_1 = (Widget)XtVaCreateManagedWidget("Front Right",
								xmToggleButtonWidgetClass, gui->lighting_panel.lights_form,
								XmNleftAttachment, XmATTACH_FORM,
								XmNrightAttachment, XmATTACH_NONE,
								XmNtopAttachment, XmATTACH_FORM,
								XmNbottomAttachment, XmATTACH_NONE,
								XmNtopOffset, 0,
								XmNleftOffset, 15,
								XmNspacing, 12,
								XmNset, TRUE,
								NULL);
  gui->lighting_panel.light_2 = (Widget)XtVaCreateManagedWidget("Front Left",
								xmToggleButtonWidgetClass, gui->lighting_panel.lights_form,
								XmNleftAttachment, XmATTACH_FORM,
								XmNrightAttachment, XmATTACH_NONE,
								XmNtopAttachment, XmATTACH_WIDGET,
								XmNtopWidget, gui->lighting_panel.light_1,
								XmNbottomAttachment, XmATTACH_NONE,
								XmNtopOffset, 0,
								XmNleftOffset, 15,
								XmNspacing, 12,
								NULL);
  gui->lighting_panel.light_3 = (Widget)XtVaCreateManagedWidget("Back Left",
								xmToggleButtonWidgetClass, gui->lighting_panel.lights_form,
								XmNleftAttachment, XmATTACH_FORM,
								XmNrightAttachment, XmATTACH_NONE,
								XmNtopAttachment, XmATTACH_WIDGET,
								XmNtopWidget, gui->lighting_panel.light_2,
								XmNbottomAttachment, XmATTACH_NONE,
								XmNtopOffset, 0,
								XmNleftOffset, 15,
								XmNspacing, 12,
								XmNset, TRUE,
								NULL);
  gui->lighting_panel.light_4 = (Widget)XtVaCreateManagedWidget("Back Right",
								xmToggleButtonWidgetClass, gui->lighting_panel.lights_form,
								XmNleftAttachment, XmATTACH_FORM,
								XmNrightAttachment, XmATTACH_NONE,
								XmNtopAttachment, XmATTACH_WIDGET,
								XmNtopWidget, gui->lighting_panel.light_3,
								XmNbottomAttachment, XmATTACH_NONE,
								XmNtopOffset, 0,
								XmNleftOffset,15,
								XmNspacing, 12,
								NULL);
  gui->lighting_panel.light_5 = (Widget)XtVaCreateManagedWidget("Top",
								xmToggleButtonWidgetClass, gui->lighting_panel.lights_form,
								XmNleftAttachment, XmATTACH_FORM,
								XmNrightAttachment, XmATTACH_NONE,
								XmNtopAttachment, XmATTACH_WIDGET,
								XmNtopWidget, gui->lighting_panel.light_4,
								XmNbottomAttachment, XmATTACH_NONE,
								XmNtopOffset, 0,
								XmNleftOffset, 15,
								XmNspacing, 12,
								NULL);
  
  XtAddCallback(gui->lighting_panel.light_1, XmNvalueChangedCallback, (XtCallbackProc)Light_toggledCB, (XtPointer)gui);
  XtAddCallback(gui->lighting_panel.light_2, XmNvalueChangedCallback, (XtCallbackProc)Light_toggledCB, (XtPointer)gui);
  XtAddCallback(gui->lighting_panel.light_3, XmNvalueChangedCallback, (XtCallbackProc)Light_toggledCB, (XtPointer)gui);
  XtAddCallback(gui->lighting_panel.light_4, XmNvalueChangedCallback, (XtCallbackProc)Light_toggledCB, (XtPointer)gui);
  XtAddCallback(gui->lighting_panel.light_5, XmNvalueChangedCallback, (XtCallbackProc)Light_toggledCB, (XtPointer)gui);

  xmstr = XmStringCreateLocalized("Ambient Lighting (%)");
  gui->lighting_panel.ambient_slider = (Widget)XtVaCreateManagedWidget("Ambientslider",
								       xmScaleWidgetClass, gui->lighting_panel.form,
								       XmNrightAttachment, XmATTACH_FORM,
								       XmNrightOffset, 5,
								       XmNleftAttachment, XmATTACH_FORM,
								       XmNleftOffset, 5,
								       XmNtopAttachment, XmATTACH_WIDGET,
								       XmNtopWidget, gui->lighting_panel.lights_frame, 
								       XmNtopOffset, 0,
								       XmNbottomAttachment, XmATTACH_NONE,
								       XmNvalue, 40,
								       XmNminimum, 0,
								       XmNmaximum, 100,
								       XmNshowValue, True,
								       XmNscaleWidth, 175,
								       XmNscaleHeight, 10,
								       XmNorientation, XmHORIZONTAL,
								       XmNtitleString, xmstr,
								       NULL);
  XmStringFree(xmstr);
  
  XtAddCallback(gui->lighting_panel.ambient_slider,XmNvalueChangedCallback, Ambient_changedCB, (XtPointer)gui);

  gui->lighting_panel.default_l_button = (Widget)XtVaCreateManagedWidget("DEFAULT",
									 xmPushButtonWidgetClass, gui->lighting_panel.form,
									 XmNleftAttachment, XmATTACH_FORM,
									 XmNrightAttachment, XmATTACH_FORM,
									 XmNbottomAttachment, XmATTACH_NONE,
									 XmNtopAttachment, XmATTACH_WIDGET,
									 XmNtopWidget, gui->lighting_panel.ambient_slider,
									 XmNleftOffset, 5,
									 XmNtopOffset,  5,
									 XmNrightOffset, 5,
									 NULL);
  
  XtAddCallback(gui->lighting_panel.default_l_button, XmNactivateCallback, (XtCallbackProc)ResetLightingCB, (XtPointer)gui);
  
  DEBUG_TRACE_OUT printf("Done with build_lighting_form\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_axis_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the widgets for controlling the axis
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_axis_form(main_gui_t *gui)
{  
  int i;
  XmString xmstr;
  Pixmap pixmap;

  DEBUG_TRACE_IN  printf("Entered build_axis_form\n");
  
  gui->axis_panel.form = (Widget)XmCreateForm(gui->inner_control_form, "Axisform", NULL, 0);
  XtVaSetValues(gui->axis_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);

  gui->axis_panel.axis_type_label = XtVaCreateManagedWidget("Axis Type", xmLabelWidgetClass,
							    gui->axis_panel.form,
							    XmNleftAttachment,XmATTACH_FORM,
							    XmNrightAttachment, XmATTACH_FORM,
							    XmNbottomAttachment, XmATTACH_NONE,
							    XmNtopAttachment, XmATTACH_FORM,
							    NULL); 
  
  pixmap = XCreatePixmapFromBitmapData(gui->display,
				       RootWindowOfScreen(XtScreen(gui->form)),
				       (char *)f_axis_bits, 
				       f_axis_width, f_axis_height,
				       gui->bg, gui->fg,
				       DefaultDepthOfScreen(XtScreen(gui->form)));

  /* Full axis */
  gui->axis_panel.axis_b[1] = XtVaCreateManagedWidget("at1",
						      xmDrawnButtonWidgetClass,gui->axis_panel.form,
						      XmNleftAttachment, XmATTACH_FORM,
						      XmNtopAttachment, XmATTACH_WIDGET,
						      XmNtopWidget, gui->axis_panel.axis_type_label,
						      XmNleftOffset, 0,
						      XmNlabelType,        XmPIXMAP,
						      XmNlabelPixmap,      pixmap,
						      XmNbottomAttachment, XmATTACH_NONE,
						      XmNrightAttachment, XmATTACH_NONE,
						      XmNshadowType, XmSHADOW_OUT,
						      NULL); 
  pixmap = XCreatePixmapFromBitmapData(gui->display,
				       RootWindowOfScreen(XtScreen(gui->form)),
				       (char *)thin_axis_bits, 
				       thin_axis_width, thin_axis_height,
				       gui->bg, gui->fg,
				       DefaultDepthOfScreen(XtScreen(gui->form)));

  /* Thin axis */
  gui->axis_panel.axis_b[2] = XtVaCreateManagedWidget("at2",
						      xmDrawnButtonWidgetClass,gui->axis_panel.form,
						      XmNleftAttachment, XmATTACH_WIDGET,
						      XmNleftWidget, gui->axis_panel.axis_b[1],
						      XmNtopAttachment, XmATTACH_WIDGET,
						      XmNtopWidget, gui->axis_panel.axis_type_label,
						      XmNleftOffset, 0,
						      XmNlabelType,        XmPIXMAP,
						      XmNlabelPixmap,      pixmap,
						      XmNbottomAttachment, XmATTACH_NONE,
						      XmNrightAttachment, XmATTACH_NONE,
						      XmNshadowType, XmSHADOW_OUT,
						      NULL); 
  
  pixmap = XCreatePixmapFromBitmapData(gui->display,
				       RootWindowOfScreen(XtScreen(gui->form)),
				       (char *)thick_axis_bits, 
				       thick_axis_width, thick_axis_height,
				       gui->bg, gui->fg,
				       DefaultDepthOfScreen(XtScreen(gui->form)));

  /* Thick */
  gui->axis_panel.axis_b[3] = XtVaCreateManagedWidget("at3",
						      xmDrawnButtonWidgetClass,gui->axis_panel.form,
						      XmNleftAttachment, XmATTACH_WIDGET,
						      XmNleftWidget, gui->axis_panel.axis_b[2],
						      XmNtopAttachment, XmATTACH_WIDGET,
						      XmNtopWidget, gui->axis_panel.axis_type_label,
						      XmNleftOffset, 0,
						      XmNlabelType,        XmPIXMAP,
						      XmNlabelPixmap,      pixmap,
						      XmNbottomAttachment, XmATTACH_NONE,
						      XmNrightAttachment, XmATTACH_NONE,
						      XmNshadowType, XmSHADOW_OUT,
						      NULL); 

  pixmap = XCreatePixmapFromBitmapData(gui->display,
				       RootWindowOfScreen(XtScreen(gui->form)),
				       (char *)off_bits, 
				       off_width, off_height,
				       gui->bg, gui->fg,
				       DefaultDepthOfScreen(XtScreen(gui->form)));

  /* Axis off */
  gui->axis_panel.axis_b[0] = XtVaCreateManagedWidget("at0",
						      xmDrawnButtonWidgetClass,gui->axis_panel.form,
						      XmNleftAttachment, XmATTACH_WIDGET,
						      XmNleftWidget, gui->axis_panel.axis_b[3],
						      XmNtopAttachment, XmATTACH_WIDGET,
						      XmNtopWidget, gui->axis_panel.axis_type_label,
						      XmNleftOffset, 0,
						      XmNlabelType,        XmPIXMAP,
						      XmNlabelPixmap,      pixmap,
						      XmNbottomAttachment, XmATTACH_NONE,
						      XmNrightAttachment, XmATTACH_NONE,
						      XmNshadowType, XmSHADOW_OUT,
						      NULL); 
  
  for (i=0;i<4;i++)
  {
      XtAddCallback(gui->axis_panel.axis_b[i],XmNactivateCallback, Axis_typeCB,(XtPointer)gui);

      /* Set the shadow of the one that is selected */
      if( gui->axis_type == i )
          XtVaSetValues( gui->axis_panel.axis_b[i], XmNshadowType, XmSHADOW_IN, NULL );
  }
  
  gui->axis_panel.divider = XtVaCreateManagedWidget("divider",
						    xmSeparatorWidgetClass, gui->axis_panel.form,
						    XmNorientation, XmHORIZONTAL,
						    XmNseparatorType, XmSHADOW_ETCHED_OUT,
						    XmNrightAttachment, XmATTACH_FORM,
						    XmNleftAttachment, XmATTACH_FORM,
						    XmNbottomAttachment, XmATTACH_NONE,
						    XmNtopAttachment, XmATTACH_WIDGET,
						    XmNtopWidget, gui->axis_panel.axis_b[0],
						    XmNtopOffset, 0,
						    XmNrightOffset, 5,
						    XmNleftOffset, 5,
						    NULL); 

  gui->axis_panel.axis_labels = (Widget)XtVaCreateManagedWidget("Axes Labels",
								xmToggleButtonWidgetClass, gui->axis_panel.form,
								XmNleftAttachment, XmATTACH_FORM,
								XmNrightAttachment, XmATTACH_NONE,
								XmNtopAttachment, XmATTACH_WIDGET,
								XmNtopWidget, gui->axis_panel.divider,
								XmNleftOffset, 10,
								XmNtopOffset,  10,
								XmNrightOffset, 5,
								NULL);
  XtAddCallback (gui->axis_panel.axis_labels, XmNvalueChangedCallback, AxisLabelToggleCB, (XtPointer)gui);

  /* Arm the button if the preferences tell us to */
  if( gui->axis_labels_on )
      XmToggleButtonSetState( gui->axis_panel.axis_labels, True, False );
  
  DEBUG_TRACE_OUT  printf("Done with build_axis_form\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_clipping_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: build the widgets that make up the clipping form
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_clipping_form(main_gui_t *gui)
{
  int i;

  gui->clipping_panel.form = (Widget)XmCreateForm(gui->inner_control_form, 
						  "Clippingform", NULL, 0);
  XtVaSetValues(gui->clipping_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);

  gui->clipping_panel.s_win_frame = (Widget)XmCreateFrame(gui->clipping_panel.form, "clip_frame", NULL, 0);
  XtVaSetValues(gui->clipping_panel.s_win_frame,
		XmNshadowType, XmSHADOW_ETCHED_IN,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_NONE,		
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 5,
		XmNleftOffset, 5,
		XmNtopOffset, 5,
		NULL);
  XtManageChild(gui->clipping_panel.s_win_frame);  

  gui->clipping_panel.clip_label = (Widget)XtVaCreateManagedWidget("Clipped Bodies",
								   xmLabelWidgetClass, gui->clipping_panel.s_win_frame,
								   XmNchildType,XmFRAME_TITLE_CHILD,
								   NULL);
  
  gui->clipping_panel.s_win = XtVaCreateManagedWidget("Scrolled Win",
						      xmScrolledWindowWidgetClass, gui->clipping_panel.s_win_frame,
						      XmNscrollingPolicy, XmAUTOMATIC,
						      XmNscrollBarDisplayPolicy, XmAS_NEEDED,
						      XmNleftAttachment, XmATTACH_FORM,
						      XmNtopAttachment, XmATTACH_FORM,
						      XmNrightAttachment, XmATTACH_FORM,
						      XmNbottomAttachment, XmATTACH_FORM,
						      XmNwidth, 125,
						      XmNheight, 150,
						      NULL);
  gui->clipping_panel.s_win_form = XtVaCreateManagedWidget("scroll_win_form",
							   xmFormWidgetClass, gui->clipping_panel.s_win,
							   XmNleftAttachment, XmATTACH_FORM,
							   XmNrightAttachment, XmATTACH_FORM,
							   XmNbottomAttachment, XmATTACH_FORM,
							   XmNtopAttachment, XmATTACH_FORM,
							   NULL); 

  gui->clipping_panel.numeric_divider = (Widget)XtVaCreateManagedWidget("ndivider",
									xmSeparatorWidgetClass, gui->clipping_panel.form,
									XmNorientation, XmHORIZONTAL,
									XmNseparatorType, XmSHADOW_ETCHED_OUT,
									XmNtopAttachment, XmATTACH_WIDGET,
									XmNtopWidget, gui->clipping_panel.s_win_frame,
									XmNtopOffset, 10,
									XmNrightAttachment, XmATTACH_FORM,
									XmNleftAttachment, XmATTACH_FORM,
									XmNbottomAttachment, XmATTACH_NONE,
									XmNrightOffset, 5,
									XmNleftOffset, 5,
									NULL);

  gui->clipping_panel.numeric_button = (Widget)XtVaCreateManagedWidget("Set Clipping",
								       xmPushButtonWidgetClass, gui->clipping_panel.form,
								       XmNleftAttachment, XmATTACH_FORM,
								       XmNrightAttachment, XmATTACH_FORM,
								       XmNbottomAttachment, XmATTACH_NONE,
								       XmNtopAttachment, XmATTACH_WIDGET,
								       XmNtopWidget, gui->clipping_panel.numeric_divider,
								       XmNleftOffset, 5,
								       XmNbottomOffset,  5,
								       XmNrightOffset, 5,
								       XmNtopOffset, 5,
								       NULL); 
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_polygon_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the widgets for controlling the polygonal rendering
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_polygon_form(main_gui_t *gui)
{
  int i;

  DEBUG_TRACE_IN printf("Entered build_polygon_form\n");
  
  gui->polygon_panel.form = (Widget)XmCreateForm(gui->inner_control_form,"Polygonform",NULL,0);
  XtVaSetValues(gui->polygon_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);

  
  gui->polygon_panel.algorithm_label = XtVaCreateManagedWidget("Surface Polygon Algorithm :", xmLabelWidgetClass,
							       gui->polygon_panel.form,
							       XmNleftAttachment,XmATTACH_FORM,
							       XmNrightAttachment, XmATTACH_NONE,
							       XmNbottomAttachment, XmATTACH_NONE,
							       XmNtopAttachment, XmATTACH_FORM,
							       XmNtopOffset, 3,
							       NULL);
  gui->polygon_panel.algorithm_pane = (Widget)XmCreatePulldownMenu(gui->polygon_panel.form,"normal_pane",NULL,0);
  gui->polygon_panel.algorithm_menu = (Widget)XtVaCreateManagedWidget("Algorithm Menu",xmRowColumnWidgetClass,gui->polygon_panel.form,
                XmNmarginHeight,       0,
                XmNmarginWidth,        0,
                XmNpacking,            XmPACK_TIGHT,
                XmNpopupEnabled,       TRUE,
                XmNrowColumnType,      XmMENU_OPTION,
                XmNspacing,            0,
                XmNsubMenuId, gui->polygon_panel.algorithm_pane,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->polygon_panel.algorithm_label,
		XmNtopOffset, 0,
		XmNleftOffset, 10,
		XmNrightOffset, 5,
		NULL);
  
  gui->polygon_panel.algorithm_type[0] = (Widget)XtCreateManagedWidget("8-Cell w/Surface Normals",xmPushButtonWidgetClass,
								       gui->polygon_panel.algorithm_pane, NULL, 0);
  gui->polygon_panel.algorithm_type[1] = (Widget)XtCreateManagedWidget("8-Cell w/Vertex Normals",xmPushButtonWidgetClass,
								       gui->polygon_panel.algorithm_pane, NULL, 0);
  gui->polygon_panel.algorithm_type[2] = (Widget)XtCreateManagedWidget("Marching Cubes",xmPushButtonWidgetClass,
								       gui->polygon_panel.algorithm_pane, NULL, 0);		
  
  XtAddCallback(gui->polygon_panel.algorithm_type[0], XmNactivateCallback, (XtCallbackProc)Polygonal_AlgorithmCB, (XtPointer)gui);
  XtAddCallback(gui->polygon_panel.algorithm_type[1], XmNactivateCallback, (XtCallbackProc)Polygonal_AlgorithmCB, (XtPointer)gui);
  XtAddCallback(gui->polygon_panel.algorithm_type[2], XmNactivateCallback, (XtCallbackProc)Polygonal_AlgorithmCB, (XtPointer)gui);

    
  XtVaSetValues(gui->polygon_panel.algorithm_menu,XmNmenuHistory,
		gui->polygon_panel.algorithm_type[gui->polygonal_algorithm], NULL);

  
  /*
    gui->polygon_panel.normal_type_label = XtVaCreateManagedWidget("Normal Type :", xmLabelWidgetClass,
    gui->polygon_panel.form,
    XmNleftAttachment,XmATTACH_FORM,
    XmNrightAttachment, XmATTACH_NONE,
    XmNbottomAttachment, XmATTACH_NONE,
    XmNtopAttachment, XmATTACH_FORM,
    XmNtopOffset, 3,
    NULL);

    gui->polygon_panel.normal_pane = (Widget)XmCreatePulldownMenu(gui->polygon_panel.form,"normal_pane",NULL,0);
    gui->polygon_panel.normal_menu = (Widget)XtVaCreateManagedWidget("Normal Menu",xmRowColumnWidgetClass,gui->polygon_panel.form,
    XmNmarginHeight,       0,
    XmNmarginWidth,        0,
    XmNpacking,            XmPACK_TIGHT,
    XmNpopupEnabled,       TRUE,
    XmNrowColumnType,      XmMENU_OPTION,
    XmNspacing,            0,
    XmNsubMenuId, gui->polygon_panel.normal_pane,
    XmNleftAttachment, XmATTACH_FORM,
    XmNrightAttachment, XmATTACH_FORM,
    XmNbottomAttachment, XmATTACH_NONE,
    XmNtopAttachment, XmATTACH_WIDGET,
    XmNtopWidget, gui->polygon_panel.normal_type_label,
    XmNtopOffset, 0,
    XmNleftOffset, 10,
    XmNrightOffset, 5,
    NULL);
    
    gui->polygon_panel.normal_type[0] = (Widget)XtCreateManagedWidget("Surface",xmPushButtonWidgetClass,
    gui->polygon_panel.normal_pane, NULL, 0);
    gui->polygon_panel.normal_type[1] = (Widget)XtCreateManagedWidget("Vertex",xmPushButtonWidgetClass,
    gui->polygon_panel.normal_pane, NULL, 0);		
    
    XtAddCallback(gui->polygon_panel.normal_type[0], XmNactivateCallback, (XtCallbackProc)Polygonal_Normal_TypeCB, (XtPointer)gui);
    XtAddCallback(gui->polygon_panel.normal_type[1], XmNactivateCallback, (XtCallbackProc)Polygonal_Normal_TypeCB, (XtPointer)gui);
    
    
    XtVaSetValues(gui->polygon_panel.normal_menu,XmNmenuHistory,
    gui->polygon_panel.normal_type[gui->polygonal_normal_type], NULL);
  */


  DEBUG_TRACE_OUT printf("Done with build_polygon_form\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_particles_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: builds the particle form
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_particles_form(main_gui_t *gui)
{
  XColor color;
  Pixmap pixmap;

  DEBUG_TRACE_IN printf("Entered build_particles_form\n");

  init_line_types(gui);
  
  gui->particle_panel.form = (Widget)XmCreateForm(gui->inner_control_form, "Particlesform", 
					NULL, 0);
  XtVaSetValues(gui->particle_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);

  build_particle_type_dialog(gui);  

  gui->particle_panel.part_toggle = XtVaCreateManagedWidget("Display Particles",
							    xmToggleButtonWidgetClass, gui->particle_panel.form,
							    XmNtopAttachment, XmATTACH_FORM,
							    XmNleftAttachment, XmATTACH_FORM,
							    XmNleftOffset, 5,
							    /*XmNtopWidget, gui->particle_panel.load_paths,*/
							    XmNtopOffset, 1,
							    XmNset, TRUE,
							    NULL);

  XtAddCallback(gui->particle_panel.part_toggle, XmNvalueChangedCallback, Draw_ParticlesToggledCB, (XtPointer)gui);
  
  gui->particle_panel.anti = XtVaCreateManagedWidget("Antialiasing",
						     xmToggleButtonWidgetClass, gui->particle_panel.form,
						     XmNtopAttachment, XmATTACH_WIDGET,
						     XmNleftAttachment, XmATTACH_FORM,
						     XmNleftOffset, 15,
						     XmNtopWidget, gui->particle_panel.part_toggle,
						     XmNtopOffset, 1,
						     NULL);
  XtAddCallback(gui->particle_panel.anti, XmNvalueChangedCallback,AntialiasingToggledCB,(XtPointer)gui);
  
  gui->particle_panel.part_frame = XtVaCreateManagedWidget("part_frame",
							   xmFrameWidgetClass, gui->particle_panel.form,
							   XmNtopAttachment, XmATTACH_WIDGET,
							   XmNtopWidget, gui->particle_panel.anti,
							   XmNtopOffset, 2,
							   XmNrightAttachment, XmATTACH_FORM,
							   XmNrightOffset, 5,
							   XmNleftAttachment, XmATTACH_FORM,
							   XmNleftOffset, 5,
							   XmNbottomAttachment, XmATTACH_FORM,
							   XmNbottomOffset, 5,
							   NULL);
  gui->particle_panel.part_form = XtVaCreateManagedWidget("part_form",
							  xmFormWidgetClass, gui->particle_panel.part_frame,
							  XmNtopAttachment, XmATTACH_FORM,
							  XmNleftAttachment, XmATTACH_FORM,
							  XmNrightAttachment, XmATTACH_FORM,
							  XmNbottomAttachment, XmATTACH_FORM,
							  NULL);
  
  
  pixmap = get_pixmap_from_line_type(gui,gui->line_types[GAMMA].color.pixel, 
				     gui->line_types[GAMMA].type,
				     gui->line_types[GAMMA].boldness);
  gui->particle_panel.colorbx[GAMMA] = XtVaCreateManagedWidget("gamma_color",
							       xmDrawnButtonWidgetClass, gui->particle_panel.part_form,
							       XmNlabelType,        XmPIXMAP,
							       XmNlabelPixmap,      pixmap,
							       XmNtopAttachment, XmATTACH_FORM,
							       XmNtopOffset,5,
							       XmNleftAttachment, XmATTACH_FORM,
							       XmNleftOffset, 5,
							       XmNwidth, 60,
							       XmNheight, 25,
							       XmNbackground, gui->line_types[GAMMA].color.pixel,
							       NULL);
  XtAddCallback (gui->particle_panel.colorbx[GAMMA], XmNactivateCallback,Show_Particle_type_dialogCB, (XtPointer)gui);
  
  gui->particle_panel.toggle[GAMMA] = XtVaCreateManagedWidget("Gamma",
							      xmToggleButtonWidgetClass, gui->particle_panel.part_form,
							      XmNleftAttachment, XmATTACH_WIDGET,
							      XmNleftWidget, gui->particle_panel.colorbx[GAMMA],
							      XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
							      XmNtopWidget, gui->particle_panel.colorbx[GAMMA], 
							      XmNtopOffset, 0,
							      XmNrightAttachment, XmATTACH_FORM,
							      XmNset, TRUE,
							      NULL);
  XtAddCallback(gui->particle_panel.toggle[GAMMA], XmNvalueChangedCallback, Rays_ToggledCB, (XtPointer)gui);


  /*
    pixmap = get_pixmap_from_line_type(gui,gui->line_types[1].color.pixel, 
    gui->line_types[1].type,
    gui->line_types[1].boldness);
    gui->particle_panel.gm_color = XtVaCreateManagedWidget("gm_color",
    xmDrawnButtonWidgetClass, gui->particle_panel.part_form,
    XmNlabelType,        XmPIXMAP,
    XmNlabelPixmap,      pixmap,
    XmNtopAttachment, XmATTACH_WIDGET,
    XmNtopWidget, gui->particle_panel.gl_color,
    XmNtopOffset,5,
    XmNleftAttachment, XmATTACH_FORM,
    XmNleftOffset, 5,
    XmNwidth, 60,
    XmNheight, 25,
    XmNbackground, gui->line_types[1].color.pixel,
    NULL);
    XtAddCallback (gui->particle_panel.gm_color, XmNactivateCallback,
    Show_Particle_type_dialogCB, (XtPointer)gui);
    gui->particle_panel.gamma_med = XtVaCreateManagedWidget("Gamma Med",
    xmToggleButtonWidgetClass, gui->particle_panel.part_form,
    XmNleftAttachment, XmATTACH_WIDGET,
    XmNleftWidget, gui->particle_panel.gm_color,
    XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
    XmNtopWidget, gui->particle_panel.gm_color,
    XmNtopOffset, 0,
    XmNrightAttachment, XmATTACH_FORM,
    XmNset, TRUE,
    NULL);
    XtAddCallback(gui->particle_panel.gamma_med, XmNvalueChangedCallback,Rays_ToggledCB, (XtPointer)gui);
    
    pixmap = get_pixmap_from_line_type(gui,gui->line_types[2].color.pixel, 
    gui->line_types[2].type,
    gui->line_types[2].boldness);
    gui->particle_panel.gh_color = XtVaCreateManagedWidget("gh_color",
    xmDrawnButtonWidgetClass, gui->particle_panel.part_form,
    XmNlabelType,        XmPIXMAP,
    XmNlabelPixmap,      pixmap,
    XmNtopAttachment, XmATTACH_WIDGET,
    XmNtopWidget, gui->particle_panel.gm_color,
    XmNtopOffset,5,
    XmNleftAttachment, XmATTACH_FORM,
    XmNleftOffset, 5,
    XmNwidth, 60,
    XmNheight, 25,
    XmNbackground, BlackPixel(gui->display,DefaultScreen(gui->display)),
    NULL); 
    XtAddCallback (gui->particle_panel.gh_color, XmNactivateCallback, Show_Particle_type_dialogCB, (XtPointer)gui);  
    gui->particle_panel.gamma_high = XtVaCreateManagedWidget("Gamma High",
    xmToggleButtonWidgetClass, gui->particle_panel.part_form,
    XmNleftAttachment, XmATTACH_WIDGET,
    XmNleftWidget, gui->particle_panel.gm_color,		   
    XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
    XmNrightAttachment, XmATTACH_FORM,
    XmNtopWidget, gui->particle_panel.gh_color,
    XmNtopOffset, 0,
    XmNset, TRUE,
    NULL);
    XtAddCallback(gui->particle_panel.gamma_high, XmNvalueChangedCallback,Rays_ToggledCB, (XtPointer)gui);
  */  
  
  
  pixmap = get_pixmap_from_line_type(gui,gui->line_types[NEUTRON_LOW].color.pixel, 
				     gui->line_types[NEUTRON_LOW].type,
				     gui->line_types[NEUTRON_LOW].boldness);
  gui->particle_panel.colorbx[NEUTRON_LOW] = XtVaCreateManagedWidget("neutron_low_color",
								     xmDrawnButtonWidgetClass, gui->particle_panel.part_form,
								     XmNlabelType,        XmPIXMAP,
								     XmNlabelPixmap,      pixmap,
								     XmNtopAttachment, XmATTACH_WIDGET,
								     XmNtopWidget, gui->particle_panel.colorbx[GAMMA],
								     XmNtopOffset,5,
								     XmNleftAttachment, XmATTACH_FORM,
								     XmNleftOffset, 5,
								     XmNwidth, 60,
								     XmNheight, 25,
								     XmNbackground, gui->line_types[NEUTRON_LOW].color.pixel,
								     NULL);
  XtAddCallback (gui->particle_panel.colorbx[NEUTRON_LOW], XmNactivateCallback,Show_Particle_type_dialogCB, (XtPointer)gui);     
  gui->particle_panel.toggle[NEUTRON_LOW] = XtVaCreateManagedWidget("Neutron Low",
								    xmToggleButtonWidgetClass, gui->particle_panel.part_form,
								    XmNleftAttachment, XmATTACH_WIDGET,
								    XmNleftWidget, gui->particle_panel.colorbx[NEUTRON_LOW],
								    XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
								    XmNtopWidget, gui->particle_panel.colorbx[NEUTRON_LOW],
								    XmNtopOffset, 0,
								    XmNrightAttachment, XmATTACH_FORM,
								    XmNset, TRUE,
								    NULL);
  XtAddCallback(gui->particle_panel.toggle[NEUTRON_LOW], XmNvalueChangedCallback,Rays_ToggledCB, (XtPointer)gui);
  
  pixmap = get_pixmap_from_line_type(gui,gui->line_types[NEUTRON_MED].color.pixel, 
				     gui->line_types[NEUTRON_MED].type,
				     gui->line_types[NEUTRON_MED].boldness);
  gui->particle_panel.colorbx[NEUTRON_MED] = XtVaCreateManagedWidget("neutron_med_color",
								     xmDrawnButtonWidgetClass, gui->particle_panel.part_form,
								     XmNlabelType,        XmPIXMAP,
								     XmNlabelPixmap,      pixmap,
								     XmNtopAttachment, XmATTACH_WIDGET,
								     XmNtopWidget, gui->particle_panel.colorbx[NEUTRON_LOW],
								     XmNtopOffset,5,
								     XmNleftAttachment, XmATTACH_FORM,
								     XmNleftOffset, 5,
								     XmNwidth, 60,
								     XmNheight, 25,
								     XmNbackground, gui->line_types[NEUTRON_MED].color.pixel,
								     NULL);
  XtAddCallback (gui->particle_panel.colorbx[NEUTRON_MED], XmNactivateCallback, Show_Particle_type_dialogCB, (XtPointer)gui);   
  gui->particle_panel.toggle[NEUTRON_MED] = XtVaCreateManagedWidget("Neutron Med",
							    xmToggleButtonWidgetClass, gui->particle_panel.part_form,
							    XmNleftAttachment, XmATTACH_WIDGET,
							    XmNleftWidget, gui->particle_panel.colorbx[NEUTRON_MED],
							    XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
							    XmNtopWidget, gui->particle_panel.colorbx[NEUTRON_MED],
							    XmNtopOffset, 0,
							    XmNrightAttachment, XmATTACH_FORM,
							    XmNset, TRUE,
							    NULL);
  XtAddCallback(gui->particle_panel.toggle[NEUTRON_MED], XmNvalueChangedCallback, Rays_ToggledCB, (XtPointer)gui);
  
  pixmap = get_pixmap_from_line_type(gui,gui->line_types[NEUTRON_HIGH].color.pixel, 
				     gui->line_types[NEUTRON_HIGH].type,
				     gui->line_types[NEUTRON_HIGH].boldness);
  gui->particle_panel.colorbx[NEUTRON_HIGH] = XtVaCreateManagedWidget("neutron_high_color",
								      xmDrawnButtonWidgetClass, gui->particle_panel.part_form,
								      XmNlabelType,        XmPIXMAP,
								      XmNlabelPixmap,      pixmap,
								      XmNtopAttachment, XmATTACH_WIDGET,
								      XmNtopWidget, gui->particle_panel.colorbx[NEUTRON_MED],
								      XmNtopOffset,5,
								      XmNleftAttachment, XmATTACH_FORM,
								      XmNleftOffset, 5,
								      XmNwidth, 60,
								      XmNheight, 25,
								      XmNbackground, gui->line_types[NEUTRON_HIGH].color.pixel,
								      NULL);
  XtAddCallback (gui->particle_panel.colorbx[NEUTRON_HIGH], XmNactivateCallback, Show_Particle_type_dialogCB, (XtPointer)gui);     
  gui->particle_panel.toggle[NEUTRON_HIGH] = XtVaCreateManagedWidget("Neutron High",
								     xmToggleButtonWidgetClass, gui->particle_panel.part_form,
								     XmNleftAttachment, XmATTACH_WIDGET,
								     XmNleftWidget, gui->particle_panel.colorbx[NEUTRON_HIGH],
								     XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
								     XmNtopWidget, gui->particle_panel.colorbx[NEUTRON_HIGH],
								     XmNtopOffset, 0,
								     XmNrightAttachment, XmATTACH_FORM,
								     XmNset, TRUE,
								     NULL);
  XtAddCallback(gui->particle_panel.toggle[NEUTRON_HIGH], XmNvalueChangedCallback, Rays_ToggledCB, (XtPointer)gui);
  
  pixmap = get_pixmap_from_line_type(gui,gui->line_types[BEAM].color.pixel, 
				     gui->line_types[BEAM].type,
				     gui->line_types[BEAM].boldness);
  gui->particle_panel.colorbx[BEAM] = XtVaCreateManagedWidget("beam_color",
							      xmDrawnButtonWidgetClass, gui->particle_panel.part_form,
							      XmNlabelType,        XmPIXMAP,
							      XmNlabelPixmap,      pixmap,
							      XmNtopAttachment, XmATTACH_WIDGET,
							      XmNtopWidget, gui->particle_panel.colorbx[NEUTRON_HIGH],
							      XmNtopOffset,5,
							      XmNleftAttachment, XmATTACH_FORM,
							      XmNleftOffset, 5,
							      XmNwidth, 60,
							      XmNheight, 25,
							      XmNbackground, gui->line_types[BEAM].color.pixel,
							      NULL);
  XtAddCallback (gui->particle_panel.colorbx[BEAM], XmNactivateCallback, Show_Particle_type_dialogCB, (XtPointer)gui);     
  gui->particle_panel.toggle[BEAM] = XtVaCreateManagedWidget("Beam",
							     xmToggleButtonWidgetClass, gui->particle_panel.part_form,
							     XmNleftAttachment, XmATTACH_WIDGET,
							     XmNleftWidget, gui->particle_panel.colorbx[BEAM],
							     XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
							     XmNtopWidget, gui->particle_panel.colorbx[BEAM],
							     XmNtopOffset, 0,
							     XmNrightAttachment, XmATTACH_FORM,
							     XmNset, TRUE,
							     NULL);
  XtAddCallback(gui->particle_panel.toggle[BEAM], XmNvalueChangedCallback, Rays_ToggledCB, (XtPointer)gui);
  
  pixmap = get_pixmap_from_line_type(gui,gui->line_types[LOST].color.pixel, 
				     gui->line_types[LOST].type,
				     gui->line_types[LOST].boldness);
  gui->particle_panel.colorbx[LOST] = XtVaCreateManagedWidget("lost_color",
							      xmDrawnButtonWidgetClass, gui->particle_panel.part_form,
							      XmNlabelType,        XmPIXMAP,
							      XmNlabelPixmap,      pixmap,
							      XmNtopAttachment, XmATTACH_WIDGET,
							      XmNtopWidget, gui->particle_panel.colorbx[BEAM],
							      XmNtopOffset, 5,
							      XmNleftAttachment, XmATTACH_FORM,
							      XmNleftOffset, 5,
							      XmNwidth, 60,
							      XmNheight, 25,
							      XmNbackground, gui->line_types[LOST].color.pixel,
							      NULL);
  XtAddCallback (gui->particle_panel.colorbx[LOST], XmNactivateCallback, Show_Particle_type_dialogCB, (XtPointer)gui);         
  gui->particle_panel.toggle[LOST] = XtVaCreateManagedWidget("Lost",
							     xmToggleButtonWidgetClass, gui->particle_panel.part_form,
							     XmNleftAttachment, XmATTACH_WIDGET,
							     XmNleftWidget, gui->particle_panel.colorbx[LOST],
							     XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
							     XmNtopWidget, gui->particle_panel.colorbx[LOST],
							     XmNtopOffset, 0,
							     XmNrightAttachment, XmATTACH_FORM,
							     XmNset, TRUE,
							     NULL);
  XtAddCallback(gui->particle_panel.toggle[LOST], XmNvalueChangedCallback,Rays_ToggledCB, (XtPointer)gui);
  
  DEBUG_TRACE_OUT printf("Done with build_particles_form\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_slice_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the widgets for controlling slices
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_slice_form(main_gui_t *gui)
{
  int i;
  XmString xmstr;
  char out_string[25];

  DEBUG_TRACE_IN printf("Entered build_slice_form\n");
  
  gui->slice_panel.form = (Widget)XmCreateForm(gui->inner_control_form, 
					   "Sliceform", NULL, 0);
  XtVaSetValues(gui->slice_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);

  /*  gui->slice_panel.load_raw_button = XtVaCreateManagedWidget("Load Raw Images",
      xmPushButtonWidgetClass, gui->slice_panel.form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNleftOffset, 5,
      XmNrightAttachment, XmATTACH_FORM,
      XmNrightOffset, 5,
      XmNtopAttachment, XmATTACH_FORM,
      XmNtopOffset, 10,
      NULL);
      XtAddCallback(gui->slice_panel.load_raw_button, XmNactivateCallback,Load_RawsCB, (XtPointer)gui);*/

  gui->slice_panel.s_toggle = (Widget)XtVaCreateManagedWidget("Inlay Slice",
							      xmToggleButtonWidgetClass, gui->slice_panel.form,
							      XmNleftAttachment, XmATTACH_FORM,
							      XmNrightAttachment, XmATTACH_NONE,
							      /*XmNtopAttachment, XmATTACH_WIDGET,
								XmNtopWidget, gui->slice_panel.load_raw_button,*/
							      XmNtopAttachment, XmATTACH_FORM,
							      XmNtopOffset, 10,			
							      XmNbottomAttachment, XmATTACH_NONE,
							      XmNtopOffset, 5,
							      XmNleftOffset, 15,
							      XmNspacing, 12,
							      XmNset, FALSE,
							      NULL);
  XtAddCallback(gui->slice_panel.s_toggle, XmNvalueChangedCallback, Inlay_sliceToggledCB, (XtPointer)gui);

  gui->slice_panel.divider = XtVaCreateManagedWidget("div",
						     xmSeparatorWidgetClass, gui->slice_panel.form,
						     XmNorientation, XmHORIZONTAL,
						     XmNseparatorType, XmSHADOW_ETCHED_OUT,
						     XmNtopAttachment, XmATTACH_WIDGET,
						     XmNbottomAttachment, XmATTACH_NONE,
						     XmNtopWidget, gui->slice_panel.s_toggle,
						     XmNleftAttachment, XmATTACH_FORM,
						     XmNrightAttachment, XmATTACH_FORM,
						     XmNrightOffset, 5,
						     XmNtopOffset, 5,
						     XmNleftOffset, 5,
						     NULL);  
  
  gui->slice_panel.dir_frame = XtVaCreateManagedWidget("Dir Frame",
						       xmFrameWidgetClass, gui->slice_panel.form,
						       XmNshadowType, XmSHADOW_ETCHED_IN,
						       XmNleftAttachment, XmATTACH_FORM,
						       XmNrightAttachment, XmATTACH_FORM,
						       XmNtopAttachment, XmATTACH_WIDGET,
						       XmNtopWidget, gui->slice_panel.divider,
						       XmNleftOffset, 5,
						       XmNrightOffset, 5,
						       XmNtopOffset, 10,
						       /*XmNheight, 100,*/
						       NULL);
  
  gui->slice_panel.rowcol = XmCreateRadioBox(gui->slice_panel.dir_frame, "rowcol",NULL, 0);
  XtVaSetValues(gui->slice_panel.rowcol,
		XmNnumColumns, 1,
		NULL);
  XtManageChild(gui->slice_panel.rowcol);
  gui->slice_panel.ax_s_toggle = (Widget)XtCreateManagedWidget("IS Slice",
							       xmToggleButtonWidgetClass, gui->slice_panel.rowcol,
							       NULL, 0);
  XtVaSetValues(gui->slice_panel.ax_s_toggle,XmNset, TRUE,NULL);
  XtAddCallback(gui->slice_panel.ax_s_toggle, XmNvalueChangedCallback, Slice_Dir_ToggledCB, (XtPointer)gui);
   
  gui->slice_panel.cor_s_toggle = (Widget)XtCreateManagedWidget("PA Slice",
					       xmToggleButtonWidgetClass, gui->slice_panel.rowcol,
					       NULL, 0);
  XtAddCallback(gui->slice_panel.cor_s_toggle, XmNvalueChangedCallback,	Slice_Dir_ToggledCB, (XtPointer)gui);  

  gui->slice_panel.sag_s_toggle = (Widget)XtCreateManagedWidget("RL Slice",
								xmToggleButtonWidgetClass, gui->slice_panel.rowcol,
								NULL, 0);
  XtAddCallback(gui->slice_panel.sag_s_toggle, XmNvalueChangedCallback, Slice_Dir_ToggledCB, (XtPointer)gui);  


  gui->slice_panel.slice_win_toggle = XtVaCreateManagedWidget("External Window", 
							      xmPushButtonWidgetClass, gui->slice_panel.form,
							      XmNtopAttachment, XmATTACH_WIDGET,
							      XmNtopWidget, gui->slice_panel.dir_frame,
							      XmNtopOffset, 5,
							      XmNleftAttachment, XmATTACH_FORM,
							      XmNleftOffset, 5,
							      NULL);
  XtAddCallback(gui->slice_panel.slice_win_toggle,XmNactivateCallback, Show_Slice_Win_DialogCB,(XtPointer)gui);

  if (gui->cm)
    sprintf(out_string,"Slice IS = %.2f (cm)", convert_world_y_to_slice_z(gui,0.0)/10.0);
  else
    sprintf(out_string,"Slice IS = %.2f (mm)", convert_world_y_to_slice_z(gui,0.0));
  xmstr = XmStringCreateLocalized(out_string);
  gui->slice_panel.loc_label = (Widget)XtVaCreateManagedWidget("label", 
							       xmLabelWidgetClass,  gui->slice_panel.form,
							       XmNleftAttachment,XmATTACH_FORM,
							       XmNrightAttachment, XmATTACH_NONE,
							       XmNbottomAttachment, XmATTACH_NONE,
							       XmNtopAttachment, XmATTACH_WIDGET,
							       XmNtopWidget, gui->slice_panel.slice_win_toggle,
							       XmNleftOffset, 10,
							       XmNrightOffset, 5,
							       XmNtopOffset, 5,
							       XmNlabelString, xmstr,
							       NULL);
  XmStringFree(xmstr);
  
  gui->slice_panel.s_slider = (Widget)XtVaCreateManagedWidget("Inlaid Slice Slider",
							      xmScaleWidgetClass, gui->slice_panel.form,
							      XmNrightAttachment, XmATTACH_NONE,
							      XmNrightOffset, 5,
							      XmNleftAttachment, XmATTACH_FORM,	 
							      XmNleftOffset, 10,
							      XmNtopAttachment, XmATTACH_WIDGET,
							      XmNtopWidget, gui->slice_panel.loc_label,
							      XmNbottomAttachment, XmATTACH_NONE,
							      XmNtopOffset, 5,
							      XmNvalue, 0,
							      XmNminimum, -150,
							      XmNmaximum, 150,
							      XmNscaleWidth, 175,
							      XmNscaleHeight, 15,
							      XmNscaleMultiple, 1,
							      XmNorientation, XmHORIZONTAL,
							      NULL);
  XtAddCallback(gui->slice_panel.s_slider, XmNdragCallback,         (XtCallbackProc)Slice_value_ChangedCB, (XtPointer)gui);
  XtAddCallback(gui->slice_panel.s_slider, XmNvalueChangedCallback, (XtCallbackProc)Slice_value_ChangedCB, (XtPointer)gui);
  /*XtAddCallback(gui->slice_panel.s_slider, XmNvalueChangedCallback, (XtCallbackProc)Unset_High_Lists,      (XtPointer)gui);*/

  DEBUG_TRACE_OUT printf("Done with build_slice_form\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_contours_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the widgets for displaying the contours
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_contours_form(main_gui_t *gui)
{  
  int i;
  char contour_init_string[256];
  char temp_string[5];
  char *init_levels = "70";
  char *names[8] = {"20","30","40","50","60","70","80","90"};
  XmString xmstr;
  Pixmap pixmap;

  DEBUG_TRACE_IN printf("Entered build_contours_form\n");

  gui->contour_panel.form = (Widget)XmCreateForm(gui->inner_control_form, 
						 "Contoursform", NULL, 0);
  XtVaSetValues(gui->contour_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);

  gui->contour_panel.contour_surfaces_button = XtVaCreateManagedWidget("Build Contour Surfaces",
								       xmPushButtonWidgetClass,gui->contour_panel.form,
								       XmNleftAttachment, XmATTACH_FORM,
								       XmNrightAttachment, XmATTACH_FORM,
								       XmNbottomAttachment, XmATTACH_NONE,
								       XmNtopAttachment, XmATTACH_FORM,
								       XmNleftOffset, 5,
								       XmNrightOffset, 5,
								       XmNtopOffset,10,
								       NULL); 
  XtAddCallback(gui->contour_panel.contour_surfaces_button, XmNactivateCallback, build_polygonal_contour_surfacesCB, (XtPointer)gui);  

  gui->contour_panel.edit_contour_levels_button = XtVaCreateManagedWidget("Edit Contour Levels",
									  xmPushButtonWidgetClass,gui->contour_panel.form,
									  XmNleftAttachment, XmATTACH_FORM,
									  XmNrightAttachment, XmATTACH_FORM,
									  XmNbottomAttachment, XmATTACH_NONE,
									  XmNtopAttachment, XmATTACH_WIDGET,
									  XmNtopWidget, gui->contour_panel.contour_surfaces_button,
									  XmNleftOffset, 5,
									  XmNrightOffset, 5,
									  XmNtopOffset,10,
									  NULL); 
  
  XtAddCallback(gui->contour_panel.edit_contour_levels_button, XmNactivateCallback, Show_Colorwash_Levels_PopupCB, (XtPointer)gui);  
  
  
  gui->contour_panel.dose_display_toggle = XtVaCreateManagedWidget("Color With Doses",
								   xmToggleButtonWidgetClass, gui->contour_panel.form,
								   XmNtopAttachment, XmATTACH_WIDGET,
								   XmNtopWidget, gui->contour_panel.edit_contour_levels_button,
								   XmNleftAttachment, XmATTACH_FORM,
								   XmNleftOffset, 5,
								   NULL);  
  
  XtAddCallback(gui->contour_panel.dose_display_toggle,XmNvalueChangedCallback,Dose_Display_ToggledCB,(XtPointer)gui);

  if (gui->colorwash_texture_with_dose) XtVaSetValues(gui->contour_panel.dose_display_toggle,XmNset, TRUE ,NULL);
  else XtVaSetValues(gui->contour_panel.dose_display_toggle,XmNset, FALSE ,NULL);

  /* Becomes sensitive when a contour file has been loaded */
  XtSetSensitive( gui->contour_panel.dose_display_toggle, False );
  

  gui->contour_panel.dose_outline_toggle = XtVaCreateManagedWidget("Outlines Only",
								   xmToggleButtonWidgetClass, gui->contour_panel.form,
								   XmNtopAttachment, XmATTACH_WIDGET,
								   XmNtopWidget, gui->contour_panel.dose_display_toggle,
								   XmNleftAttachment, XmATTACH_FORM,
								   XmNleftOffset, 5,
								   NULL);  
  
  XtAddCallback(gui->contour_panel.dose_outline_toggle,XmNvalueChangedCallback,Dose_Outlines_ToggledCB,(XtPointer)gui);
  
  if (gui->colorwash_outlines) XtVaSetValues(gui->contour_panel.dose_outline_toggle,XmNset, TRUE ,NULL);
  else XtVaSetValues(gui->contour_panel.dose_outline_toggle,XmNset, FALSE ,NULL);

  /* Becomes sensitive when a contour file has been loaded */
  XtSetSensitive( gui->contour_panel.dose_outline_toggle, False );
  

  gui->contour_panel.dose_component_pane = (Widget)XmCreatePulldownMenu(gui->contour_panel.form,
									"dose component pane",NULL,0);
  gui->contour_panel.dose_component_menu = (Widget)XtVaCreateManagedWidget("Dose Component Menu",xmRowColumnWidgetClass,gui->contour_panel.form,
                XmNmarginHeight,       0,
                XmNmarginWidth,        0,
                XmNpacking,            XmPACK_TIGHT,
                XmNpopupEnabled,       TRUE,
                XmNrowColumnType,      XmMENU_OPTION,
                XmNspacing,            0,
                XmNsubMenuId, gui->contour_panel.dose_component_pane,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 10,
		XmNrightAttachment, XmATTACH_NONE,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->contour_panel.dose_outline_toggle,
		XmNtopOffset, 0,
		NULL);

  gui->contour_panel.dose_component[0] = (Widget)XtCreateManagedWidget("Boron Dose",
						    xmPushButtonWidgetClass,
						    gui->contour_panel.dose_component_pane, NULL, 0);
  gui->contour_panel.dose_component[1] = (Widget)XtCreateManagedWidget("Gamma Dose",
						    xmPushButtonWidgetClass,
						    gui->contour_panel.dose_component_pane, NULL, 0);
  gui->contour_panel.dose_component[2] = (Widget)XtCreateManagedWidget("Nitrogen Dose",
						    xmPushButtonWidgetClass,
						    gui->contour_panel.dose_component_pane, NULL, 0);
  gui->contour_panel.dose_component[3] = (Widget)XtCreateManagedWidget("Fast Dose",
						    xmPushButtonWidgetClass,
						    gui->contour_panel.dose_component_pane, NULL, 0);
  gui->contour_panel.dose_component[4] = (Widget)XtCreateManagedWidget("Group 1 Fluence",
						    xmPushButtonWidgetClass,
						    gui->contour_panel.dose_component_pane, NULL, 0);
  gui->contour_panel.dose_component[5] = (Widget)XtCreateManagedWidget("Group 2 Fluence",
						    xmPushButtonWidgetClass,
						    gui->contour_panel.dose_component_pane, NULL, 0);
  gui->contour_panel.dose_component[6] = (Widget)XtCreateManagedWidget("Thermal Fluence",
						    xmPushButtonWidgetClass,
						    gui->contour_panel.dose_component_pane, NULL, 0);
  gui->contour_panel.dose_component[7] = (Widget)XtCreateManagedWidget("Other Dose",
						    xmPushButtonWidgetClass,
						    gui->contour_panel.dose_component_pane, NULL, 0);
  gui->contour_panel.dose_component[8] = (Widget)XtCreateManagedWidget("Total Dose",
						    xmPushButtonWidgetClass,
						    gui->contour_panel.dose_component_pane, NULL, 0);
  
  XtAddCallback(gui->contour_panel.dose_component[0],XmNactivateCallback,Dose_Component_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->contour_panel.dose_component[1],XmNactivateCallback,Dose_Component_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->contour_panel.dose_component[2],XmNactivateCallback,Dose_Component_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->contour_panel.dose_component[3],XmNactivateCallback,Dose_Component_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->contour_panel.dose_component[4],XmNactivateCallback,Dose_Component_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->contour_panel.dose_component[5],XmNactivateCallback,Dose_Component_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->contour_panel.dose_component[6],XmNactivateCallback,Dose_Component_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->contour_panel.dose_component[7],XmNactivateCallback,Dose_Component_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->contour_panel.dose_component[8],XmNactivateCallback,Dose_Component_ChangedCB,(XtPointer)gui);
  

  gui->legend.num_levels = 8;
  gui->legend.sw = XtVaCreateManagedWidget("legend frame",
					   xmScrolledWindowWidgetClass, gui->contour_panel.form,
					   XmNscrollingPolicy, XmAUTOMATIC,
					   XmNscrollBarDisplayPolicy, XmAS_NEEDED,
					   XmNshadowType, XmSHADOW_ETCHED_IN,
					   XmNleftAttachment, XmATTACH_FORM,
					   XmNleftOffset, 10,
					   XmNrightAttachment, XmATTACH_FORM,
					   XmNbottomAttachment, XmATTACH_NONE,
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNtopWidget,gui->contour_panel.dose_component_menu,
					   XmNtopOffset, 0,
					   XmNheight, 150,
					   NULL);
  gui->legend.form = XtVaCreateManagedWidget("legend form",
					     xmFormWidgetClass, gui->legend.sw,
					     NULL);
  gui->legend.main_label = XtVaCreateManagedWidget ("Colorwash Legend",
						    xmLabelWidgetClass, gui->legend.form,
						    XmNleftAttachment, XmATTACH_FORM,
						    XmNleftOffset, 15,
						    XmNtopAttachment, XmATTACH_FORM,
						    XmNtopOffset, 5,
						    NULL);
  
  gui->legend.rc = NULL;
  gui->legend.color_box = NULL;
  gui->legend.label = NULL;
  gui->legend.levels = NULL;

  build_colorwash_levels_popup(gui);  

  strcpy(contour_init_string,(char *)" ");
  for (i=MAX_CONTOUR_LEVEL-1;i>-1;i--){
    if (gui->contour_pref[i].filled){
      sprintf(temp_string,"%d ",i);
      /*printf("adding %d to the string\n",i);*/
      strcat(contour_init_string,temp_string);
      init_color(gui,gui->contour_pref[i].color.red,
		 gui->contour_pref[i].color.green,
		 gui->contour_pref[i].color.blue,
		 &gui->contour_pref[i].color);
    }
  }

  /*printf("the init_string is : %s\n",contour_init_string);*/

  XmTextSetString(gui->colorwash_level_popup.text_box,contour_init_string);

  rebuild_contour_colorwash_legend(gui);

  gui->contour_panel.apply_legend_button = XtVaCreateManagedWidget("Apply Legend",
								   xmPushButtonWidgetClass,gui->contour_panel.form,
								   XmNleftAttachment, XmATTACH_FORM,
								   XmNrightAttachment, XmATTACH_FORM,
								   XmNbottomAttachment, XmATTACH_NONE,
								   XmNtopAttachment, XmATTACH_WIDGET,
								   XmNtopWidget, gui->legend.sw, 
								   XmNleftOffset, 5,
								   XmNrightOffset, 5,
								   XmNtopOffset,5,
								   NULL); 
  
  XtAddCallback(gui->contour_panel.apply_legend_button, XmNactivateCallback, Apply_Colorwash_Legend_ColorsCB, (XtPointer)gui);  



  DEBUG_TRACE_OUT printf("Done with build_contours_form\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_mouse_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the widgets for displaying the contours
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_mouse_form(main_gui_t *gui)
{  
  int i;
  XmString xmstr;
  Pixmap pixmap;

  DEBUG_TRACE_IN printf("Entered build_mouse_form\n");
  
  gui->mouse_panel.form = (Widget)XmCreateForm(gui->inner_control_form, 
					       "Mouse_form", NULL, 0);
  XtVaSetValues(gui->mouse_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);

   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)pull_bits, 
					pull_width, pull_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));

    gui->mouse_panel.label = XtVaCreateManagedWidget("Control Method",
						    xmLabelWidgetClass,gui->mouse_panel.form,
						    XmNleftAttachment, XmATTACH_FORM,
						    XmNleftOffset, 3,
						    XmNtopAttachment, XmATTACH_FORM,
						    XmNtopOffset, 10,
						    NULL);
    /*    
	  gui->mouse_panel.mouse_b[0] = XtVaCreateManagedWidget("mb1",
	  xmDrawnButtonWidgetClass,gui->mouse_panel.form,
	  XmNleftAttachment, XmATTACH_FORM,
	  XmNtopAttachment, XmATTACH_WIDGET,
	  XmNtopWidget, gui->mouse_panel.label,
	  XmNleftOffset, 20,
	  XmNtopOffset, 20,
	  XmNlabelType,        XmPIXMAP,
	  XmNlabelPixmap,      pixmap,
	  XmNbottomAttachment, XmATTACH_NONE,
	  XmNrightAttachment, XmATTACH_NONE,
	  XmNshadowType, XmSHADOW_OUT,
	  NULL); 
	  
	  XtVaSetValues(gui->mouse_panel.mouse_b[0],XmNsensitive,FALSE,NULL);
    */      

   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)sliders_bits, 
					sliders_width, sliders_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->mouse_panel.mouse_b[0] = XtVaCreateManagedWidget("mb2",
							 xmDrawnButtonWidgetClass,gui->mouse_panel.form,
							 XmNleftAttachment, XmATTACH_FORM,
							 XmNleftOffset, 20,
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->mouse_panel.label,
							 XmNtopOffset, 20,
       							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 XmNbottomAttachment, XmATTACH_NONE,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNshadowType, XmSHADOW_OUT,
							 NULL); 
   
   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)mouse_b_bits, 
					mouse_b_width, mouse_b_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->mouse_panel.mouse_b[1] = XtVaCreateManagedWidget("mb3",
							 xmDrawnButtonWidgetClass,gui->mouse_panel.form,
							 XmNleftAttachment, XmATTACH_WIDGET,
							 XmNleftWidget, gui->mouse_panel.mouse_b[0],
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->mouse_panel.label,
							 XmNtopOffset, 20,
							 XmNleftOffset, 0,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 XmNbottomAttachment, XmATTACH_NONE,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNshadowType, XmSHADOW_IN,
							 NULL); 
   
   			       
   for (i=0;i<2;i++)
     XtAddCallback(gui->mouse_panel.mouse_b[i],XmNactivateCallback, Mouse_control_changedCB,(XtPointer)gui);
   	   

   gui->mouse_panel.r_label = XtVaCreateManagedWidget("Rotational Locks",
						      xmLabelWidgetClass, gui->mouse_panel.form,
						      XmNtopAttachment, XmATTACH_WIDGET,
						      XmNtopWidget, gui->mouse_panel.mouse_b[1],
						      XmNleftAttachment, XmATTACH_FORM,
						      XmNleftOffset, 25,
						      XmNtopOffset, 15,
						      NULL);

   gui->mouse_panel.lock[0] = XtVaCreateManagedWidget("Lock Bodies",
						      xmToggleButtonWidgetClass, gui->mouse_panel.form,
						      XmNtopAttachment, XmATTACH_WIDGET,
						      XmNtopWidget, gui->mouse_panel.r_label,
						      XmNleftAttachment, XmATTACH_FORM,
						      XmNleftOffset, 25,
						      NULL);
   gui->mouse_panel.lock[1] = XtVaCreateManagedWidget("Lock Clip Planes",
						      xmToggleButtonWidgetClass, gui->mouse_panel.form,
						      XmNtopAttachment, XmATTACH_WIDGET,
						      XmNtopWidget, gui->mouse_panel.lock[0],
						      XmNleftAttachment, XmATTACH_FORM,
						      XmNleftOffset, 25,
						      NULL);
   gui->mouse_panel.lock[2] = XtVaCreateManagedWidget("Lock Slice Inlay",
						      xmToggleButtonWidgetClass, gui->mouse_panel.form,
						      XmNtopAttachment, XmATTACH_WIDGET,
						      XmNtopWidget, gui->mouse_panel.lock[1],
						      XmNleftAttachment, XmATTACH_FORM,
						      XmNleftOffset, 25,
						      NULL);
   XtVaSetValues(gui->mouse_panel.lock[2],XmNsensitive,FALSE,NULL);   
   
   for (i=0;i<3;i++){
       XtAddCallback(gui->mouse_panel.lock[i],XmNvalueChangedCallback, Locking_ChangedCB,(XtPointer)gui);
     }

  DEBUG_TRACE_OUT printf("Done with build_mouse_form\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_view_params_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the widgets for displaying the viewing parameters
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_view_params_form(main_gui_t *gui)
{  
  int i;
  XmString xmstr;
  Pixmap pixmap;
  Widget temp_slider;


  DEBUG_TRACE_IN printf("Entered view_params_form\n");
  
  gui->view_params_panel.form = (Widget)XmCreateForm(gui->inner_control_form, 
				       "View_params_form", NULL, 0);
  XtVaSetValues(gui->view_params_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);

  gui->view_params_panel.buff_frame = XtVaCreateManagedWidget("Buffer Frame",
				       xmFrameWidgetClass, gui->view_params_panel.form,
				       XmNshadowType, XmSHADOW_ETCHED_IN,
				       XmNleftAttachment, XmATTACH_FORM,
				       XmNrightAttachment, XmATTACH_FORM,
				       XmNtopAttachment, XmATTACH_FORM,
				       XmNleftOffset, 5,
				       XmNrightOffset, 5,
				       XmNtopOffset, 10,
				       NULL);				       

    gui->view_params_panel.rowcol = XmCreateRadioBox(gui->view_params_panel.buff_frame, "rowcol",NULL, 0);
  XtVaSetValues(gui->view_params_panel.rowcol,
		XmNnumColumns, 1,
		NULL);
  XtManageChild(gui->view_params_panel.rowcol);
  gui->view_params_panel.double_buff_button = (Widget)XtCreateManagedWidget("Double Buffered",
									    xmToggleButtonWidgetClass, gui->view_params_panel.rowcol,
									    NULL, 0);
  XtVaSetValues(gui->view_params_panel.double_buff_button,XmNset, TRUE,NULL);
  XtAddCallback(gui->view_params_panel.double_buff_button, XmNvalueChangedCallback, Buffering_ToggledCB, (XtPointer)gui);
   
  gui->view_params_panel.single_buff_button = (Widget)XtCreateManagedWidget("Single Buffered",
									    xmToggleButtonWidgetClass, gui->view_params_panel.rowcol,
									    NULL, 0);
  XtAddCallback(gui->view_params_panel.single_buff_button, XmNvalueChangedCallback, Buffering_ToggledCB, (XtPointer)gui);  
  
  
  gui->view_params_panel.frame_rate_toggle = XtVaCreateManagedWidget("Show Frame Rate",
								     xmToggleButtonWidgetClass, gui->view_params_panel.form,
								     XmNtopAttachment, XmATTACH_WIDGET,
								     XmNtopWidget, gui->view_params_panel.buff_frame,
								     XmNtopOffset, 10,
								     XmNleftAttachment, XmATTACH_FORM,
								     XmNleftOffset, 10,
								     XmNset, TRUE,
								     NULL);
  XtAddCallback(gui->view_params_panel.frame_rate_toggle, XmNvalueChangedCallback, Show_Frame_Rate_ToggledCB, (XtPointer)gui);  

  gui->view_params_panel.speed_test_button = XtVaCreateManagedWidget("Test Polygon Speed",
								     xmPushButtonWidgetClass, gui->view_params_panel.form,
								     XmNtopAttachment, XmATTACH_WIDGET,
								     XmNtopWidget, gui->view_params_panel.frame_rate_toggle,
								     XmNtopOffset, 10,
								     XmNleftAttachment, XmATTACH_FORM,
								     XmNleftOffset, 10,
								     NULL);
  XtAddCallback(gui->view_params_panel.speed_test_button, XmNactivateCallback, Test_SpeedCB, (XtPointer)gui);  

  gui->view_params_panel.center_mass_button = XtVaCreateManagedWidget("Show Center Mass for Bodies",
								      xmPushButtonWidgetClass, gui->view_params_panel.form,
								      XmNtopAttachment, XmATTACH_WIDGET,
								      XmNtopWidget, gui->view_params_panel.speed_test_button,
								      XmNtopOffset, 10,
								      XmNleftAttachment, XmATTACH_FORM,
								      XmNleftOffset, 10,
								      NULL);
  XtAddCallback(gui->view_params_panel.center_mass_button, XmNactivateCallback, Display_Centers_of_Mass_for_BodiesCB, (XtPointer)gui);  

  /*
  temp_slider = (Widget)XtVaCreateManagedWidget("Inlaid Slice Slider",
						xmScaleWidgetClass, gui->view_params_panel.form,
						XmNrightAttachment, XmATTACH_NONE,
						XmNleftAttachment, XmATTACH_FORM,	 
						XmNleftOffset, 30,
						XmNtopAttachment, XmATTACH_WIDGET,
						XmNtopWidget, gui->view_params_panel.center_mass_button,
						XmNbottomAttachment, XmATTACH_NONE,
						XmNtopOffset, 5,
						XmNvalue, 0,
						XmNminimum, 0,
						XmNmaximum, 255,
						XmNscaleWidth, 20,
						XmNscaleHeight, 300,
						XmNshowArrows, TRUE,
						XmNshowValue, TRUE,
						XmNscaleMultiple, 1,
						XmNorientation, XmVERTICAL,
						NULL);
  XtAddCallback(temp_slider, XmNdragCallback, Cell8_ChangedCB, (XtPointer)gui);
  XtAddCallback(temp_slider,XmNvalueChangedCallback, (XtCallbackProc)Cell8_ChangedCB,(XtPointer)gui);   
  */


  DEBUG_TRACE_OUT printf("Entered view_params_form\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_beam_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the widgets for animation
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_beam_form(main_gui_t *gui)
{  
  int i;
  Pixmap pixmap;
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered build_beam_form\n");

  gui->beam_panel.form = (Widget)XmCreateForm(gui->inner_control_form, 
					      "Beam_form", NULL, 0);
  XtVaSetValues(gui->beam_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);


  
  gui->beam_panel.which_beam_label = XtVaCreateManagedWidget("Beam in Use", xmLabelWidgetClass,
							     gui->beam_panel.form,
							     XmNleftAttachment,XmATTACH_FORM,
							     XmNleftOffset, 10,
							     XmNrightAttachment, XmATTACH_NONE,
							     XmNbottomAttachment, XmATTACH_NONE,
							     XmNtopAttachment, XmATTACH_FORM,
							     XmNtopOffset, 3,
							     NULL);

  gui->beam_panel.which_beam_pane = (Widget)XmCreatePulldownMenu(gui->beam_panel.form,
								 "which beam pane",NULL,0);
  gui->beam_panel.which_beam_menu = (Widget)XtVaCreateManagedWidget("Which Beam Menu",xmRowColumnWidgetClass,gui->beam_panel.form,
                XmNmarginHeight,       0,
                XmNmarginWidth,        0,
                XmNpacking,            XmPACK_TIGHT,
                XmNpopupEnabled,       TRUE,
                XmNrowColumnType,      XmMENU_OPTION,
                XmNspacing,            0,
                XmNsubMenuId, gui->beam_panel.which_beam_pane,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 10,
		XmNrightAttachment, XmATTACH_NONE,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->beam_panel.which_beam_label,
		XmNtopOffset, 0,
		NULL);


  gui->beam_panel.wb[0] = (Widget)XtCreateManagedWidget("Beam From File",
							xmPushButtonWidgetClass,
							gui->beam_panel.which_beam_pane, NULL, 0);
  gui->beam_panel.wb[1] = (Widget)XtCreateManagedWidget("Interactive Beam",
							xmPushButtonWidgetClass,
							gui->beam_panel.which_beam_pane, NULL, 0);
  
  XtAddCallback(gui->beam_panel.wb[0],XmNactivateCallback,Which_Beam_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->beam_panel.wb[1],XmNactivateCallback,Which_Beam_ChangedCB,(XtPointer)gui);

  gui->beam_panel.bv_button = XtVaCreateManagedWidget("Beam Line View", 
						      xmToggleButtonWidgetClass, gui->beam_panel.form,
						      XmNtopAttachment, XmATTACH_WIDGET,
						      XmNtopWidget, gui->beam_panel.which_beam_menu,
						      XmNtopOffset, 2,
						      XmNleftAttachment, XmATTACH_FORM,
						      XmNleftOffset, 5,
						      NULL);
  
  XtAddCallback(gui->beam_panel.bv_button,XmNvalueChangedCallback, Beam_Line_ViewCB,(XtPointer)gui);

  
  gui->beam_panel.beam_slider = (Widget)XtVaCreateManagedWidget("Beam_Slider",xmScaleWidgetClass,
								gui->beam_panel.form,
								XmNleftAttachment, XmATTACH_FORM,
								XmNleftOffset, 5,
								XmNtopAttachment, XmATTACH_WIDGET,
								XmNtopWidget, gui->beam_panel.bv_button,
								XmNbottomAttachment, XmATTACH_NONE,
								XmNtopOffset, 0,
								XmNvalue, 0,
								XmNminimum, -500,
								XmNmaximum, 500,
								XmNscaleWidth, 15,
								XmNscaleHeight, 200,
								XmNshowValue, TRUE,
								XmNorientation, XmVERTICAL,
								NULL);  
  
  XtAddCallback(gui->beam_panel.beam_slider, XmNdragCallback, Beam_Line_View_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->beam_panel.beam_slider, XmNvalueChangedCallback, Beam_Line_View_ChangedCB,(XtPointer)gui);

  gui->beam_panel.beam_slice_slider = (Widget)XtVaCreateManagedWidget("Beam_Slice_Slider",xmScaleWidgetClass,
								      gui->beam_panel.form,
								      XmNleftAttachment, XmATTACH_WIDGET,
								      XmNleftWidget, gui->beam_panel.beam_slider,
								      XmNleftOffset, 10,
								      XmNtopAttachment, XmATTACH_WIDGET,
								      XmNtopWidget, gui->beam_panel.bv_button,
								      XmNbottomAttachment, XmATTACH_NONE,
								      XmNtopOffset, 0,
								      XmNvalue, 0,
								      XmNminimum, -500,
								      XmNmaximum, 500,
								      XmNscaleWidth, 15,
								      XmNscaleHeight, 200,
								      XmNshowValue, TRUE,
								      XmNorientation, XmVERTICAL,
								      NULL);
  XtAddCallback(gui->beam_panel.beam_slice_slider, XmNdragCallback, Beam_Slice_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->beam_panel.beam_slice_slider, XmNvalueChangedCallback, Beam_Slice_ChangedCB,(XtPointer)gui);

  if (gui->cm){
    XtVaSetValues(gui->beam_panel.beam_slider, XmNdecimalPoints, 1, NULL);
    XtVaSetValues(gui->beam_panel.beam_slice_slider, XmNdecimalPoints, 1, NULL);
  }
		
   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)off_bits, 
					off_width, off_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->beam_panel.cross_hair_b[0] = XtVaCreateManagedWidget("ch0",
							     xmDrawnButtonWidgetClass,gui->beam_panel.form,
							     XmNleftAttachment, XmATTACH_WIDGET,
							     XmNleftWidget, gui->beam_panel.beam_slice_slider,
							     XmNleftOffset, 25,
							     XmNtopAttachment, XmATTACH_WIDGET,
							     XmNtopWidget, gui->beam_panel.bv_button,
							     XmNtopOffset, 30,
							     XmNlabelType,        XmPIXMAP,
							     XmNlabelPixmap,      pixmap,
							     XmNbottomAttachment, XmATTACH_NONE,
							     XmNrightAttachment, XmATTACH_NONE,
							     XmNshadowType, XmSHADOW_IN,
							     NULL); 

   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)crossh1_bits, 
					crossh1_width, crossh1_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->beam_panel.cross_hair_b[1] = XtVaCreateManagedWidget("ch1",
							     xmDrawnButtonWidgetClass,gui->beam_panel.form,
							     XmNleftAttachment, XmATTACH_WIDGET,
							     XmNleftWidget, gui->beam_panel.beam_slice_slider,
							     XmNleftOffset, 25,
							     XmNtopAttachment, XmATTACH_WIDGET,
							     XmNtopWidget, gui->beam_panel.cross_hair_b[0],
							     XmNlabelType,        XmPIXMAP,
							     XmNlabelPixmap,      pixmap,
							     XmNbottomAttachment, XmATTACH_NONE,
							     XmNrightAttachment, XmATTACH_NONE,
							     XmNshadowType, XmSHADOW_OUT,
							     NULL); 
   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)crossh2_bits, 
					crossh2_width, crossh2_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->beam_panel.cross_hair_b[2] = XtVaCreateManagedWidget("ch2",
					     xmDrawnButtonWidgetClass,gui->beam_panel.form,
					     XmNleftAttachment, XmATTACH_WIDGET,
					     XmNleftWidget, gui->beam_panel.beam_slice_slider,
					     XmNleftOffset, 25,
					     XmNtopAttachment, XmATTACH_WIDGET,
					     XmNtopWidget, gui->beam_panel.cross_hair_b[1],
					     XmNlabelType,        XmPIXMAP,
					     XmNlabelPixmap,      pixmap,
					     XmNbottomAttachment, XmATTACH_NONE,
					     XmNrightAttachment, XmATTACH_NONE,
					     XmNshadowType, XmSHADOW_OUT,
					     NULL); 
   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)crossh3_bits, 
					crossh3_width, crossh3_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->beam_panel.cross_hair_b[3] = XtVaCreateManagedWidget("ch3",
							     xmDrawnButtonWidgetClass,gui->beam_panel.form,
							     XmNleftAttachment, XmATTACH_WIDGET,
							     XmNleftWidget, gui->beam_panel.beam_slice_slider,
							     XmNleftOffset, 25,
							     XmNtopAttachment, XmATTACH_WIDGET,
							     XmNtopWidget, gui->beam_panel.cross_hair_b[2],
							     XmNlabelType,        XmPIXMAP,
							     XmNlabelPixmap,      pixmap,
							     XmNbottomAttachment, XmATTACH_NONE,
							     XmNrightAttachment, XmATTACH_NONE,
							     XmNshadowType, XmSHADOW_OUT,
							     NULL); 

   for (i=0;i<4;i++)
     XtAddCallback(gui->beam_panel.cross_hair_b[i],XmNactivateCallback, Cross_Hair_ChangedCB, (XtPointer)gui);

   gui->beam_panel.beam_slice_toggle = (Widget)XtVaCreateManagedWidget("Inlay Beam Slice",
								      xmToggleButtonWidgetClass, gui->beam_panel.form,
								      XmNleftAttachment, XmATTACH_FORM,
								      XmNrightAttachment, XmATTACH_NONE,
								      XmNtopAttachment, XmATTACH_WIDGET,
								      XmNtopWidget, gui->beam_panel.beam_slider,
								      XmNbottomAttachment, XmATTACH_NONE,
								      XmNtopOffset, 0,
								      XmNleftOffset, 15,
								      XmNspacing, 12,
								      XmNset, FALSE,
								      NULL);   
  
  XtAddCallback(gui->beam_panel.beam_slice_toggle,XmNvalueChangedCallback, Beam_Slice_ToggledCB, (XtPointer)gui);

  gui->beam_panel.beam_clip_toggle = (Widget)XtVaCreateManagedWidget("Clip With Beam Slice",
								     xmToggleButtonWidgetClass, gui->beam_panel.form,
								     XmNleftAttachment, XmATTACH_FORM,
								     XmNrightAttachment, XmATTACH_NONE,
								     XmNtopAttachment, XmATTACH_WIDGET,
								     XmNtopWidget, gui->beam_panel.beam_slice_toggle,
								     XmNbottomAttachment, XmATTACH_NONE,
								     XmNtopOffset, 0,
								     XmNleftOffset, 15,
								     XmNspacing, 12,
								     XmNset, FALSE,
								     NULL);   
  XtAddCallback(gui->beam_panel.beam_clip_toggle,XmNvalueChangedCallback, Beam_Clip_ToggledCB, (XtPointer)gui);

  gui->beam_panel.ibeam_control_b = XtVaCreateManagedWidget("I-Beam Controls", 
							    xmPushButtonWidgetClass, gui->beam_panel.form,
							    XmNtopAttachment, XmATTACH_WIDGET,
							    XmNtopWidget, gui->beam_panel.beam_clip_toggle,
							    XmNtopOffset, 0,
							    XmNleftAttachment, XmATTACH_FORM,
							    XmNleftOffset, 15,
							    NULL);
  
  XtAddCallback(gui->beam_panel.ibeam_control_b,XmNactivateCallback, Show_IBeam_Controls_DialogCB,(XtPointer)gui);


  gui->beam_panel.apeture_toggle = (Widget)XtVaCreateManagedWidget("Use Aperture",
								   xmToggleButtonWidgetClass, gui->beam_panel.form,
								   XmNleftAttachment, XmATTACH_FORM,
								   XmNrightAttachment, XmATTACH_NONE,
								   XmNtopAttachment, XmATTACH_WIDGET,
								   XmNtopWidget, gui->beam_panel.ibeam_control_b,
								   XmNbottomAttachment, XmATTACH_NONE,
								   XmNtopOffset, 0,
								   XmNleftOffset, 15,
								   XmNspacing, 12,
								   XmNset, FALSE,
								   NULL);   
  XtAddCallback(gui->beam_panel.apeture_toggle,XmNvalueChangedCallback,	Apeture_ToggledCB, (XtPointer)gui);

  gui->beam_panel.apeture_button = XtVaCreateManagedWidget("Aperture Settings", 
							   xmPushButtonWidgetClass, gui->beam_panel.form,
							   XmNtopAttachment, XmATTACH_WIDGET,
							   XmNtopWidget, gui->beam_panel.apeture_toggle,
							   XmNtopOffset, 0,
							   XmNleftAttachment, XmATTACH_FORM,
							   XmNleftOffset, 15,
							   NULL);
  
  XtAddCallback(gui->beam_panel.apeture_button,XmNactivateCallback, Show_Apeture_Controls_DialogCB,(XtPointer)gui);


  DEBUG_TRACE_OUT printf("Done with build_beam_form\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_ray_track_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: display some rays & info from the tracking program.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_ray_track_form(main_gui_t *gui)
{  
  int i; 

  DEBUG_TRACE_IN printf("Entered build_ray_track_form\n");

  gui->ray_track_panel.form = (Widget)XmCreateForm(gui->inner_control_form, 
						   "Ray_Track_form", NULL, 0);
  XtVaSetValues(gui->ray_track_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);

  /* removed 4-26-99 CLA, doesn't do anything productive
  gui->ray_track_panel.ray_track_button = XtVaCreateManagedWidget("Track Rays",
								  xmPushButtonWidgetClass,gui->ray_track_panel.form,
								  XmNleftAttachment, XmATTACH_FORM,
								  XmNrightAttachment, XmATTACH_FORM,
								  XmNbottomAttachment, XmATTACH_FORM,
								  XmNtopAttachment, XmATTACH_NONE,
								  XmNleftOffset, 5,
								  XmNrightOffset, 5,
								  XmNbottomOffset,5,
								  NULL);
  
  
  gui->ray_track_panel.ray_track_file_button = XtVaCreateManagedWidget("Load Ray Track File",
								       xmPushButtonWidgetClass,gui->ray_track_panel.form,
								       XmNleftAttachment, XmATTACH_FORM,
								       XmNrightAttachment, XmATTACH_FORM,
								       XmNbottomAttachment, XmATTACH_WIDGET,
								       XmNbottomWidget, gui->ray_track_panel.ray_track_button,
								       XmNtopAttachment, XmATTACH_NONE,
								       XmNleftOffset, 5,
								       XmNrightOffset, 5,
								       XmNbottomOffset,15,
								       NULL);  
  */
  /*XtAddCallback(gui->ray_track_panel.ray_track_file_button, XmNactivateCallback, SelectRayTrackFileCallback, (XtPointer)gui);*/

  /*gui->ray_track_panel.rt_dialog = (Widget)build_ray_tracking_dialog(gui->ray_track_panel.ray_track_button);*/

  /*XtAddCallback(gui->ray_track_panel.ray_track_button, XmNactivateCallback,
    Show_ray_tracking_dialogCB,(XtPointer)gui); */

  DEBUG_TRACE_OUT printf("Done with build_ray_track_form\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_texture_form
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the widgets for controlling texture
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_texture_form(main_gui_t *gui)
{  
  int i;

  DEBUG_TRACE_IN printf("Entered build_texture_form\n");
  
  gui->texture_panel.form = (Widget)XmCreateForm(gui->inner_control_form, 
				       "Texture_form", NULL, 0);
  XtVaSetValues(gui->texture_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->options_select_form,
		XmNrightAttachment, XmATTACH_FORM,		
		NULL);

  gui->texture_panel.tex_frame = XtVaCreateManagedWidget("Frame",
							 xmFrameWidgetClass, gui->texture_panel.form,
							 XmNshadowType, XmSHADOW_ETCHED_IN,
							 XmNleftAttachment, XmATTACH_FORM,
							 XmNrightAttachment, XmATTACH_FORM,
							 XmNtopAttachment, XmATTACH_FORM,
							 XmNleftOffset, 5,
							 XmNrightOffset, 5,
							 XmNtopOffset, 5,
							 /*XmNheight, 100,*/
							 NULL);
  
  gui->texture_panel.tex_rowcol = XmCreateRadioBox(gui->texture_panel.tex_frame, "texrowcol",NULL, 0);
  XtVaSetValues(gui->texture_panel.tex_rowcol,XmNnumColumns, 1,NULL);
  XtManageChild(gui->texture_panel.tex_rowcol);
  gui->texture_panel.tex[0] = (Widget)XtCreateManagedWidget("NEAREST",
							    xmToggleButtonWidgetClass, gui->texture_panel.tex_rowcol,
							    NULL, 0);
  XtAddCallback(gui->texture_panel.tex[0], XmNvalueChangedCallback, Tex_type_toggledCB, (XtPointer)gui);
  gui->texture_panel.tex[1] = (Widget)XtCreateManagedWidget("LINEAR",
					 xmToggleButtonWidgetClass, gui->texture_panel.tex_rowcol,
					 NULL, 0);
  XtAddCallback(gui->texture_panel.tex[1], XmNvalueChangedCallback, Tex_type_toggledCB, (XtPointer)gui);
   
  if (gui->texture_nearest)
    XtVaSetValues(gui->texture_panel.tex[0],XmNset, TRUE,NULL);
  else
    XtVaSetValues(gui->texture_panel.tex[1],XmNset, TRUE,NULL);

  gui->texture_panel.tex_mod_frame = XtVaCreateManagedWidget("Frame",
							     xmFrameWidgetClass, gui->texture_panel.form,
							     XmNshadowType, XmSHADOW_ETCHED_IN,
							     XmNleftAttachment, XmATTACH_FORM,
							     XmNrightAttachment, XmATTACH_FORM,
							     XmNtopAttachment, XmATTACH_WIDGET,
							     XmNtopWidget, gui->texture_panel.tex_frame,
							     XmNleftOffset, 5,
							     XmNrightOffset, 5,
							     XmNtopOffset, 5,
							     NULL);
  gui->texture_panel.tex_mod_rowcol = XmCreateRadioBox(gui->texture_panel.tex_mod_frame, "texrowcol",NULL, 0);
  XtVaSetValues(gui->texture_panel.tex_mod_rowcol,XmNnumColumns, 1,NULL);
  XtManageChild(gui->texture_panel.tex_mod_rowcol);

  gui->texture_panel.tex_mod[0] = (Widget)XtCreateManagedWidget("REPEAT",
								xmToggleButtonWidgetClass, gui->texture_panel.tex_mod_rowcol,
								NULL, 0);
  XtAddCallback(gui->texture_panel.tex_mod[0], XmNvalueChangedCallback, Tex_mod_toggledCB, (XtPointer)gui);
  gui->texture_panel.tex_mod[1] = (Widget)XtCreateManagedWidget("CLAMP",
								xmToggleButtonWidgetClass, gui->texture_panel.tex_mod_rowcol,
								NULL, 0);
  XtAddCallback(gui->texture_panel.tex_mod[1], XmNvalueChangedCallback, Tex_mod_toggledCB, (XtPointer)gui);

   if (gui->texture_clamp)
     XtVaSetValues(gui->texture_panel.tex_mod[1],XmNset, TRUE,NULL);
   else
     XtVaSetValues(gui->texture_panel.tex_mod[0],XmNset, TRUE,NULL);

   gui->texture_panel.gamma_toggle = XtVaCreateManagedWidget("Gamma Correction",
							     xmToggleButtonWidgetClass, gui->texture_panel.form,
							     XmNleftAttachment, XmATTACH_FORM,
							     XmNrightAttachment, XmATTACH_FORM,
							     XmNtopAttachment, XmATTACH_WIDGET,
							     XmNtopWidget, gui->texture_panel.tex_mod_frame,
							     XmNleftOffset, 5,
							     XmNrightOffset, 5,
							     XmNtopOffset, 5,
							     /*XmNheight, 100,*/
							     NULL);
   XtAddCallback(gui->texture_panel.gamma_toggle,XmNvalueChangedCallback, Gamma_ToggledCB, (XtPointer)gui);

   if (gui->texture_gamma_correction) XtVaSetValues(gui->texture_panel.gamma_toggle,XmNset, TRUE ,NULL);
   else XtVaSetValues(gui->texture_panel.gamma_toggle,XmNset, FALSE ,NULL);
   
   gui->texture_panel.alpha_cul_frame = XtVaCreateManagedWidget("Frame",
								xmFrameWidgetClass, gui->texture_panel.form,
								XmNshadowType, XmSHADOW_ETCHED_IN,
								XmNleftAttachment, XmATTACH_FORM,
								XmNrightAttachment, XmATTACH_FORM,
								XmNtopAttachment, XmATTACH_WIDGET,
								XmNtopWidget, gui->texture_panel.gamma_toggle,
								XmNleftOffset, 5,
								XmNrightOffset, 5,
								XmNtopOffset, 5,
								/*XmNheight, 100,*/
								NULL);
   gui->texture_panel.alpha_cul_form = XtVaCreateManagedWidget("Alpha_cul_form",
							       xmFormWidgetClass,gui->texture_panel.alpha_cul_frame,
							       NULL);

   gui->texture_panel.alpha_cul = XtVaCreateManagedWidget("Alpha Culling",
							  xmToggleButtonWidgetClass,gui->texture_panel.alpha_cul_form,
							  XmNleftAttachment, XmATTACH_FORM,
							  XmNtopAttachment, XmATTACH_FORM,
							  NULL);
  XtAddCallback(gui->texture_panel.alpha_cul, XmNvalueChangedCallback, Alpha_Culling_toggledCB, (XtPointer)gui);
  if (gui->alpha_culling_on) XtVaSetValues(gui->texture_panel.alpha_cul,XmNset,TRUE,NULL);

  gui->texture_panel.alpha_slider = (Widget)XtVaCreateManagedWidget("alpha_slider",xmScaleWidgetClass,
				     gui->texture_panel.alpha_cul_form,
				     XmNrightAttachment, XmATTACH_NONE,
				     XmNleftAttachment, XmATTACH_FORM,	 
				     XmNleftOffset, 5,
				     XmNshowValue, TRUE,
				     XmNtopAttachment, XmATTACH_WIDGET,
				     XmNtopWidget, gui->texture_panel.alpha_cul,
				     XmNbottomAttachment, XmATTACH_FORM,
				     XmNbottomOffset, 0,
				     XmNvalue, 50,
     				     XmNminimum, 0,
				     XmNmaximum, 256,
				     XmNscaleWidth, 150,
			             XmNscaleHeight, 15,
                                     XmNscaleMultiple, 10,
				     XmNorientation, XmHORIZONTAL,
				     NULL);
  XtAddCallback(gui->texture_panel.alpha_slider, XmNdragCallback, Alpha_value_ChangedCB, (XtPointer)gui);
  XtAddCallback(gui->texture_panel.alpha_slider,XmNvalueChangedCallback, Alpha_value_ChangedCB,(XtPointer)gui);

  gui->texture_panel.vr_button = XtVaCreateManagedWidget("Volume Render", 
							 xmPushButtonWidgetClass, gui->texture_panel.form,
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->texture_panel.alpha_cul_frame,
							 XmNtopOffset, 5,
							 XmNleftAttachment, XmATTACH_FORM,
							 XmNleftOffset, 5,
							 NULL);
  XtAddCallback(gui->texture_panel.vr_button,XmNactivateCallback, Volume_Rendering_ToggledCB, (XtPointer)gui);
  
  gui->texture_panel.surface_tex_toggle = XtVaCreateManagedWidget("Texture Polygonal Bodies", 
								  xmToggleButtonWidgetClass, gui->texture_panel.form,
								  XmNtopAttachment, XmATTACH_WIDGET,
								  XmNtopWidget, gui->texture_panel.vr_button,
								  XmNtopOffset, 5,
								  XmNleftAttachment, XmATTACH_FORM,
								  XmNleftOffset, 5,
								  NULL);
  XtAddCallback(gui->texture_panel.surface_tex_toggle,XmNvalueChangedCallback, Surface_Texturing_ToggledCB, (XtPointer)gui);
  
  DEBUG_TRACE_OUT printf("Done with build_texture_form\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_main_window
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the main viewing window
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_main_window(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered build_main_window\n");
 /********************************************************/
 /** GL Frame - frame around the gl rendering window.   
 /*******************************************************/
  gui->gl_frame = XmCreateFrame(gui->display_form, "gl_frame", NULL, 0);
  XtVaSetValues(gui->gl_frame,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 20,
		XmNleftOffset, 20,
		XmNtopOffset, 20,
		XmNbottomOffset, 20,
		NULL);
  XtManageChild(gui->gl_frame);

 /*************************************************/
 /** glxarea - The main GL rendering widget
 /**   the left side of the main window.
 /*************************************************/
 
  gui->glxarea = XtVaCreateManagedWidget("glxarea",
					 DRAWING_AREA_TYPE, gui->gl_frame,
					 GLwNvisualInfo, gui->visinfo,
					 GLwNallocateOtherColors, FALSE,
					 GLwNinstallColormap, FALSE,
					 XmNheight, gui->mainwindowsize,
					 XmNwidth, gui->mainwindowsize,
					 NULL);
  
  DEBUG_TRACE_OUT printf("Done with build_main_window\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_top_level_forms
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the forms on which everything else sits
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_top_level_forms(main_gui_t *gui)
{

  DEBUG_TRACE_IN printf("Entered build_top_level_forms\n");

  gui->message_panel.form = XmCreateForm(gui->form, "Messageform", NULL, 0);
  XtVaSetValues(gui->message_panel.form,
		XmNbottomAttachment, XmATTACH_NONE,
		/*XmNtopAttachment, XmATTACH_WIDGET,  
		  XmNtopWidget, gui->menubar,*/             /* MTC 8/26/99 */
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 0,
		XmNleftOffset,  0,
		XmNtopOffset, 20,
		NULL);
  XtManageChild(gui->message_panel.form);

  /* Form to hold the multi-views */
  gui->multiViewForm = XmCreateForm( gui->form, "multiViewForm", NULL, 0 );
  XtVaSetValues( gui->multiViewForm,
                 XmNtopAttachment, XmATTACH_WIDGET,
                 XmNtopWidget, gui->message_panel.form,
                 XmNbottomAttachment, XmATTACH_FORM,
                 XmNleftAttachment, XmATTACH_FORM,
                 XmNleftOffset, 10,
                 NULL );
  XtManageChild( gui->multiViewForm );
  
 /*************************************************/
 /** Display Form - holds the main rendering 
 /** window.
 /*************************************************/
  gui->display_form = XmCreateForm(gui->form, "Displayform", NULL, 0);
  XtVaSetValues(gui->display_form,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNleftAttachment, XmATTACH_WIDGET,
                XmNleftWidget, gui->multiViewForm,
		XmNtopAttachment, XmATTACH_WIDGET,
                XmNtopWidget, gui->message_panel.form,
		NULL);
  XtManageChild(gui->display_form);


  

 /*************************************************/
 /** Divider - divides the controlform and 
 /**    display form.
 /*************************************************/
  gui->control_display_divider = XtVaCreateManagedWidget("divider",
							 xmSeparatorWidgetClass, gui->form,
							 XmNorientation, XmVERTICAL,
							 XmNseparatorType, XmSHADOW_ETCHED_OUT,
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->message_panel.form,
                                                         XmNbottomAttachment, XmATTACH_FORM,
                                                         XmNleftAttachment, XmATTACH_WIDGET,
                                                         XmNleftWidget, gui->display_form,
							 NULL);
  
  /*************************************************/
  /** Controls Form - right side of main window **/
  /**   has all of the controls built on it. **/
  /*************************************************/
  gui->top_control_form = XmCreateForm(gui->form, "TOPControlsform", NULL, 0);
  XtVaSetValues(gui->top_control_form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->message_panel.form,
		XmNrightAttachment, XmATTACH_FORM,
                XmNleftAttachment, XmATTACH_WIDGET,
                XmNleftWidget, gui->control_display_divider,
		NULL);
  XtManageChild(gui->top_control_form);

  gui->controls_sw = XtVaCreateManagedWidget("Scrolled Win",
					     xmScrolledWindowWidgetClass, gui->top_control_form,
					     XmNscrollingPolicy, XmAUTOMATIC,
					     XmNscrollBarDisplayPolicy, XmAS_NEEDED,
					     XmNtopAttachment, XmATTACH_FORM,
					     XmNrightAttachment, XmATTACH_FORM,
					     XmNbottomAttachment, XmATTACH_FORM,
					     XmNleftAttachment, XmATTACH_FORM,
					     XmNwidth, 250,
					     XmNvisualPolicy, XmVARIABLE,
					     NULL);

  gui->controls_form = XmCreateForm(gui->controls_sw, "Controlsform", NULL, 0);
  XtManageChild(gui->controls_form);

  DEBUG_TRACE_OUT printf("Done with build_top_level_forms\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_controls
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: this builds the control frames 
%%%           which contain the various control panels and forms.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_controls(main_gui_t *gui)
{	  	  
  DEBUG_TRACE_IN printf("Entered build_controls\n");
 /*************************************************/
 /** controls frame - frame around the controls
 /*************************************************/     		  
 gui->controls_frame = XtVaCreateManagedWidget("Controls",
					       xmFrameWidgetClass, gui->controls_form,
					       XmNshadowType, XmSHADOW_ETCHED_IN,
					       XmNleftAttachment, XmATTACH_FORM,
					       XmNrightAttachment, XmATTACH_FORM,
					       XmNbottomAttachment, XmATTACH_FORM,
					       XmNtopAttachment, XmATTACH_WIDGET,
					       XmNtopWidget, gui->bodylistframe,
					       XmNtopOffset, 20,
					       XmNleftOffset, 5,
					       XmNrightOffset, 5,
					       XmNbottomOffset, 20,
					       NULL);
 
 /*************************************************/
 /** Inner_control_form - form on which all the buttons **/
 /**   for controls are placed. **/
 /*************************************************/
 gui->inner_control_form = XmCreateForm(gui->controls_frame, "inner_controls_form", 
					NULL, 0);
 XtVaSetValues(gui->inner_control_form,
	       XmNbottomAttachment, XmATTACH_FORM,
	       XmNleftAttachment, XmATTACH_FORM,
	       XmNtopAttachment, XmATTACH_FORM,
	       XmNrightAttachment, XmATTACH_FORM,		
	       NULL);
 XtManageChild(gui->inner_control_form);

 gui->options_select_form = XmCreateForm(gui->inner_control_form, 
					 "options_select_form", NULL, 0);
 XtVaSetValues(gui->options_select_form,
	       XmNbottomAttachment, XmATTACH_NONE,
	       XmNleftAttachment, XmATTACH_FORM,
	       XmNtopAttachment, XmATTACH_FORM,
	       XmNrightAttachment, XmATTACH_FORM,		
	       NULL);
 XtManageChild(gui->options_select_form);
 
 DEBUG_TRACE_OUT printf("Done with build_controls\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_body_list
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the list widget that is located at the top
%%%           right of the window, which holds the body names,
%%%           and allows for the toggling of bodies.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_body_list(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered build_body_list\n");
 /*************************************************/
 /** bodylistframe - frame around the bodylist
 /*************************************************/
  gui->bodylistframe = XtVaCreateManagedWidget("Body List",
					       xmFrameWidgetClass, gui->controls_form,
					       XmNshadowType, XmSHADOW_IN,
					       XmNleftAttachment, XmATTACH_FORM,
					       XmNrightAttachment, XmATTACH_FORM,
					       XmNtopAttachment, XmATTACH_FORM,
					       XmNleftOffset, 5,
					       XmNrightOffset, 5,
					       XmNtopOffset, 10,
					       NULL);
  /*************************************************/
  /** bodylist label - title on the bodylist **/
  /*************************************************/
  gui->bodylist_label = XtVaCreateManagedWidget ("Bodies",xmLabelWidgetClass, 
						 gui->bodylistframe,
						 XmNchildType,XmFRAME_TITLE_CHILD,
						 NULL);  

  /*************************************************/
  /** Bodylist - contains the names of all the **/
  /**   bodies for the file, this list allows the **/
  /**   user to toggle the bodies on and off **/
  /*************************************************/
  gui->bodylist = (Widget)XmCreateScrolledList(gui->bodylistframe,"BodyList", NULL,0);
  XtManageChild(gui->bodylist);
  XtVaSetValues(gui->bodylist,
		XmNselectionPolicy, XmMULTIPLE_SELECT,
		XmNleftAttachment,XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftOffset , 5,		 
		XmNrightOffset , 5,		 
		XmNbottomOffset , 5,
		XmNtopOffset , 5,	   
		XmNvisibleItemCount, 6,
		NULL);
  
  DEBUG_TRACE_OUT printf("Done with build_body_list\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_sliders
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the sliders at the bottom of the view window
%%%           which can be used for rotation.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_sliders(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered build_sliders\n");    
  /*************************************************/
  /** Slider Divider - divides the advanced_form and **/
  /**    display form. **/
  /*************************************************/
  gui->slider_panel.slider_divider = XtVaCreateManagedWidget("slider_divider",
						xmSeparatorWidgetClass, gui->form,
						XmNorientation, XmHORIZONTAL,
						XmNseparatorType, XmSHADOW_ETCHED_OUT,
						XmNtopAttachment, XmATTACH_WIDGET,
						XmNtopWidget, gui->display_form,
						XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
						XmNleftWidget, gui->display_form,
						XmNrightAttachment, XmATTACH_WIDGET,
                                                XmNrightWidget, gui->control_display_divider,
						NULL);
  XtUnmanageChild(gui->slider_panel.slider_divider);

  /*************************************************/
  /** Slider Form - bottom of  display form **/
  /**   has all of the controls built on it. **/
  /*************************************************/  
  gui->slider_panel.form = XmCreateForm(gui->form, "Sliderform", NULL, 0);
  XtVaSetValues(gui->slider_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->slider_panel.slider_divider,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, gui->control_display_divider, 
		XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
		XmNleftWidget, gui->display_form,		
		NULL);
  
  if (gui->mouse_control_method == SLIDER)
    {
      XtManageChild(gui->slider_panel.slider_divider); 
      XtManageChild(gui->slider_panel.form);
    }

  /********************************************************/
  /** Slider Frame   **/
  /*******************************************************/   
  gui->slider_panel.slider_frame = XmCreateFrame(gui->slider_panel.form, "slider_frame", NULL, 0);
  XtVaSetValues(gui->slider_panel.slider_frame,
		XmNshadowType, XmSHADOW_ETCHED_IN,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 20,
		XmNleftOffset, 20,
		XmNtopOffset, 10,
		XmNbottomOffset, 20,
		NULL);
  XtManageChild(gui->slider_panel.slider_frame);
  
 /*************************************************/
 /** Slider label - title on the control frame
 /*************************************************/			    
 gui->slider_panel.slider_label = XtVaCreateManagedWidget ("X, Y, Z Rotation",
							   xmLabelWidgetClass, 
							   gui->slider_panel.slider_frame,
							   XmNchildType,XmFRAME_TITLE_CHILD,
							   NULL);
 /**********************************************************/
 /* Build the sliders!  **/
 /***********************************************************/
 gui->slider_panel.slider_inner_form = XtVaCreateManagedWidget("slider_inner_form",
							       xmFormWidgetClass,
							       gui->slider_panel.slider_frame, NULL );
 							       

 /********************************************************/
 /** Slider x  Frame   **/
 /*******************************************************/
 gui->slider_panel.slider_z_label = XtVaCreateManagedWidget("IS axis", xmLabelWidgetClass,
							    gui->slider_panel.slider_inner_form,
							    XmNleftAttachment,XmATTACH_FORM,
							    XmNleftOffset, 5,
							    XmNrightAttachment, XmATTACH_NONE,
							    XmNbottomAttachment, XmATTACH_FORM,
							    XmNbottomOffset, 5,
							    XmNtopAttachment, XmATTACH_NONE,
							    NULL);
 gui->slider_panel.slider_z_numeric_label = XtVaCreateManagedWidget("0", xmLabelWidgetClass,
								    gui->slider_panel.slider_inner_form,
								    XmNleftAttachment, XmATTACH_NONE,
								    XmNrightOffset, 10,
								    XmNrightAttachment, XmATTACH_FORM,
								    XmNbottomAttachment, XmATTACH_FORM,
								    XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
								    XmNtopWidget, gui->slider_panel.slider_z_label,
								    NULL);
 gui->slider_panel.slider_z = XtVaCreateManagedWidget("Zslider",xmScaleWidgetClass,
						      gui->slider_panel.slider_inner_form,
						      XmNrightAttachment, XmATTACH_FORM,
						      /*XmNrightWidget, slider_z_numeric_label,*/
						      XmNrightOffset, 55,
						      XmNleftAttachment, XmATTACH_WIDGET,
						      XmNleftWidget, gui->slider_panel.slider_z_label,
						      XmNleftOffset, 10,
						      XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
						      XmNtopWidget, gui->slider_panel.slider_z_label,
						      XmNtopOffset, 5,
						      XmNvalue, 0,
						      XmNminimum, -180,
						      XmNmaximum, 180,
						      XmNscaleHeight, 15,
						      XmNorientation, XmHORIZONTAL,
						      NULL);
 
 gui->slider_panel.slider_y_label = XtVaCreateManagedWidget("PA axis", xmLabelWidgetClass,
							    gui->slider_panel.slider_inner_form,
							    XmNleftAttachment,XmATTACH_FORM,
							    XmNleftOffset, 5,
							    XmNrightAttachment, XmATTACH_NONE,
							    XmNbottomAttachment, XmATTACH_WIDGET,
							    XmNbottomWidget, gui->slider_panel.slider_z_label,
							    XmNbottomOffset, 5,
							    XmNtopAttachment, XmATTACH_NONE,
							    NULL);
 gui->slider_panel.slider_y_numeric_label = XtVaCreateManagedWidget("0", xmLabelWidgetClass,
								    gui->slider_panel.slider_inner_form,
								    XmNleftAttachment,XmATTACH_NONE,
								    XmNrightAttachment, XmATTACH_FORM,
								    XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
								    XmNtopOffset, 2,
								    XmNrightOffset, 10,
								    XmNtopWidget, gui->slider_panel.slider_y_label,
								    NULL);
 
 gui->slider_panel.slider_y = XtVaCreateManagedWidget("Yslider",xmScaleWidgetClass,
						      gui->slider_panel.slider_inner_form,
						      XmNrightAttachment, XmATTACH_FORM,
						      /*XmNrightWidget, slider_y_numeric_label,*/
						      XmNleftAttachment, XmATTACH_WIDGET,
						      XmNleftWidget, gui->slider_panel.slider_y_label,
						      XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
						      XmNtopWidget, gui->slider_panel.slider_y_label,
						      XmNtopOffset, 5,
						      XmNleftOffset, 10,
						      XmNrightOffset, 55,
						      XmNvalue, 0,
						      XmNminimum, -180,
						      XmNmaximum, 180,
						      XmNscaleHeight, 15,
						      XmNorientation, XmHORIZONTAL,
						      NULL);
 
 gui->slider_panel.slider_x_label = XtVaCreateManagedWidget("RL axis", xmLabelWidgetClass,
					       gui->slider_panel.slider_inner_form,
					       XmNleftAttachment,XmATTACH_FORM,
					       XmNleftOffset, 5,
					       XmNrightAttachment, XmATTACH_NONE,
					       XmNbottomAttachment, XmATTACH_WIDGET,
					       XmNbottomWidget, gui->slider_panel.slider_y_label,
					       XmNbottomOffset, 5,
					       XmNtopAttachment, XmATTACH_FORM,
					       XmNtopOffset, 5,
					       NULL);  
 
 gui->slider_panel.slider_x_numeric_label = XtVaCreateManagedWidget("0", xmLabelWidgetClass,
						       gui->slider_panel.slider_inner_form,
						       XmNleftAttachment,XmATTACH_NONE,
						       XmNrightAttachment, XmATTACH_FORM,
						       XmNbottomAttachment, XmATTACH_NONE,
						       XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
						       XmNtopWidget, gui->slider_panel.slider_x_label,
						       XmNrightOffset, 10,
						       XmNtopOffset, 2,
						       NULL);
 gui->slider_panel.slider_x = XtVaCreateManagedWidget("Xslider",xmScaleWidgetClass,
					 gui->slider_panel.slider_inner_form,
					 XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
					 XmNtopWidget, gui->slider_panel.slider_x_label,
					 XmNtopOffset, 5,
					 XmNrightAttachment, XmATTACH_FORM,
					 /*XmNrightWidget, gui->slider_x_numeric_label,*/
					 XmNleftAttachment, XmATTACH_WIDGET,
					 XmNleftWidget,gui->slider_panel.slider_x_label,
					 XmNrightOffset, 55,
					 XmNleftOffset, 10,
					 XmNbottomOffset, 5,
					 XmNtopOffset, 5,
					 XmNvalue,  0,
					 XmNminimum, -180,
					 XmNmaximum, 180,
					 XmNscaleHeight, 15,
					 XmNorientation, XmHORIZONTAL,
					 NULL);

  DEBUG_TRACE_OUT printf("Done with build_sliders\n");    
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_multi_view
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the multi_view widgets which when on 
%%%           are located on the left side of the main window.
%%%           it includes the form which is toggled, and the 
%%%           three different camera views.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_multi_view(main_gui_t *gui)
{

  DEBUG_TRACE_IN printf("Entered build_multi_view\n");

  /*************************************************/
  /** advanced Divider - divides the advanced_form and **/
  /**    display form. **/
  /*************************************************/
  gui->multiview_panel.divider = XtVaCreateManagedWidget("advanced_divider",
							 xmSeparatorWidgetClass, gui->multiViewForm,
							 XmNorientation, XmVERTICAL,
							 XmNseparatorType, XmSHADOW_ETCHED_OUT,
							 XmNtopAttachment, XmATTACH_FORM,
                                                         XmNbottomAttachment, XmATTACH_FORM,
							 XmNrightAttachment, XmATTACH_FORM,
                                                         XmNrightOffset, 5,
                                                         NULL);
  if (!gui->multi_view) XtUnmanageChild(gui->multiview_panel.divider);

 /*************************************************/
 /** Advanced Form - holds the main rendering 
 /** window.
 /*************************************************/
  gui->multiview_panel.form = XmCreateForm(gui->multiViewForm, "Multiform", NULL, 0);
  XtVaSetValues(gui->multiview_panel.form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, gui->multiview_panel.divider,
                XmNrightOffset, 5,
		NULL);
  
  if (gui->multi_view) XtManageChild(gui->multiview_panel.form);

 /********************************************************/
 /** Advance lview Frame   
 /*******************************************************/
  gui->multiview_panel.lview_frame = XmCreateFrame(gui->multiview_panel.form, "gl_frame", NULL, 0);
  XtVaSetValues(gui->multiview_panel.lview_frame,
		XmNshadowType, XmSHADOW_IN,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 10,
		XmNtopOffset, 20,
		XmNwidth, gui->multiwindowsize,
		XmNheight, gui->multiwindowsize,
		NULL);
  XtManageChild(gui->multiview_panel.lview_frame);  


 /*************************************************/
  /** Lview label - title on the control frame **/
 /*************************************************/			      
  gui->multiview_panel.lview_label = XtVaCreateManagedWidget ("Left Camera",xmLabelWidgetClass, 
							      gui->multiview_panel.lview_frame,
							      XmNchildType,XmFRAME_TITLE_CHILD,
							      NULL);

  /*************************************************/
  /** gl_lview_area -  rendering widget **/
  /**   the left side of the main window. **/
  /*************************************************/
  gui->multiview_panel.gl_lview_area = XtVaCreateManagedWidget("gl_lview_area",
							       DRAWING_AREA_TYPE, gui->multiview_panel.lview_frame,
							       GLwNvisualInfo, gui->visinfo,
							       GLwNallocateOtherColors, FALSE,
							       GLwNinstallColormap, FALSE,
							       NULL);


  /********************************************************/
  /** Advance rview Frame   **/
  /*******************************************************/
  gui->multiview_panel.rview_frame = XmCreateFrame(gui->multiview_panel.form, 
						   "rview_frame", NULL, 0);
  XtVaSetValues(gui->multiview_panel.rview_frame,
		XmNshadowType, XmSHADOW_IN,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->multiview_panel.lview_frame,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 10,
		XmNtopOffset, 10,
		XmNwidth, gui->multiwindowsize,
		XmNheight, gui->multiwindowsize,
		NULL);
  XtManageChild(gui->multiview_panel.rview_frame);  

 /*************************************************/
  /** Rview label - title on the control frame **/
 /*************************************************/			      
  gui->multiview_panel.rview_label = XtVaCreateManagedWidget ("Right Camera",xmLabelWidgetClass, 
							      gui->multiview_panel.rview_frame,
							      XmNchildType,XmFRAME_TITLE_CHILD,
							      NULL);

  /*************************************************/
  /** gl_rview_area -  rendering widget **/
  /**   the left side of the main window. **/
  /*************************************************/
  gui->multiview_panel.gl_rview_area = XtVaCreateManagedWidget("gl_rview_area",
							       DRAWING_AREA_TYPE,  
							       gui->multiview_panel.rview_frame,
							       GLwNvisualInfo, gui->visinfo,
							       GLwNallocateOtherColors, FALSE,
							       GLwNinstallColormap, FALSE,
							       NULL);
 
  /********************************************************/
  /** Advance tview Frame    **/
  /*******************************************************/
  gui->multiview_panel.tview_frame = XmCreateFrame(gui->multiview_panel.form, 
						   "advanced_tview_frame", NULL, 0);

  XtVaSetValues(gui->multiview_panel.tview_frame,
		XmNshadowType, XmSHADOW_IN,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->multiview_panel.rview_frame,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 10,
		XmNtopOffset, 10,
		XmNwidth, gui->multiwindowsize,
		XmNheight, gui->multiwindowsize,
		NULL);
  XtManageChild(gui->multiview_panel.tview_frame);  

  /*************************************************/
  /** Tview label - title on the control frame **/
  /*************************************************/			      
  gui->multiview_panel.tview_label = XtVaCreateManagedWidget ("Top Camera",xmLabelWidgetClass, 
							      gui->multiview_panel.tview_frame,
							      XmNchildType,XmFRAME_TITLE_CHILD,
							      NULL);
  
  /*************************************************/
  /** gl_tview_area -  rendering widget **/
  /**   the left side of the main window. **/
  /*************************************************/
  gui->multiview_panel.gl_tview_area = XtVaCreateManagedWidget("gl_tview_area",
							       DRAWING_AREA_TYPE, 
							       gui->multiview_panel.tview_frame,
							       GLwNvisualInfo, gui->visinfo,
							       GLwNallocateOtherColors, FALSE,
							       GLwNinstallColormap, FALSE,
							       NULL);

  DEBUG_TRACE_OUT printf("Done with build_multi_view\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_options_selector
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the buttons which switch the control panels
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_options_selector(main_gui_t *gui)
{
  Pixmap pixmap;
  int i;

  DEBUG_TRACE_IN printf("Entered build_options_selector\n");
  
  gui->options_panel.cp_label =  XtVaCreateManagedWidget("Control Panels",
							 xmLabelWidgetClass, gui->options_select_form,
							 XmNleftAttachment, XmATTACH_FORM,
							 XmNleftOffset, 20,
							 XmNtopAttachment, XmATTACH_FORM,
							 NULL);

   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)view_bits, view_width, view_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[0] = XtVaCreateManagedWidget("cp0",
							 xmDrawnButtonWidgetClass,gui->options_select_form,
							 XmNleftAttachment, XmATTACH_FORM,
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->options_panel.cp_label,
							 XmNleftOffset, 0,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 XmNbottomAttachment, XmATTACH_NONE,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNshadowType, XmSHADOW_IN,
							 NULL); 
   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)color_bits, color_width, color_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[1] = XtVaCreateManagedWidget("cp1",
							 xmDrawnButtonWidgetClass,gui->options_select_form,
							 XmNleftAttachment, XmATTACH_WIDGET,
							 XmNleftWidget, gui->options_panel.panel[0],
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->options_panel.cp_label,
							 XmNleftOffset, 0,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 XmNbottomAttachment, XmATTACH_NONE,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNshadowType, XmSHADOW_OUT,
							 NULL);    
   
   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)transparency_bits, 
					transparency_width, transparency_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[2] = XtVaCreateManagedWidget("cp2",
							 xmDrawnButtonWidgetClass,gui->options_select_form,
							 XmNleftAttachment, XmATTACH_WIDGET,
							 XmNleftWidget, gui->options_panel.panel[1],
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->options_panel.cp_label,
							 XmNleftOffset, 0,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 XmNbottomAttachment, XmATTACH_NONE,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNshadowType, XmSHADOW_OUT,
							 NULL); 

   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)lighting_bits, 
					lighting_width, lighting_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[3] = XtVaCreateManagedWidget("cp3",
							 xmDrawnButtonWidgetClass,gui->options_select_form,
							 XmNleftAttachment, XmATTACH_WIDGET,
							 XmNleftWidget, gui->options_panel.panel[2],
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->options_panel.cp_label,
							 XmNleftOffset, 0,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 XmNbottomAttachment, XmATTACH_NONE,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNshadowType, XmSHADOW_OUT,
							 NULL); 
   

   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)f_axis_bits, 
					f_axis_width, f_axis_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[4] = XtVaCreateManagedWidget("cp4",
							 xmDrawnButtonWidgetClass,gui->options_select_form,
							 XmNleftAttachment, XmATTACH_WIDGET,
							 XmNleftWidget, gui->options_panel.panel[3],
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->options_panel.cp_label,
							 XmNleftOffset, 0,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 XmNbottomAttachment, XmATTACH_NONE,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNshadowType, XmSHADOW_OUT,
							 NULL); 
   

   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)clipping_bits, 
					clipping_width, clipping_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[5] = XtVaCreateManagedWidget("cp5",
							 xmDrawnButtonWidgetClass,gui->options_select_form,
							 XmNleftAttachment, XmATTACH_FORM,
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->options_panel.panel[0],
							 XmNleftOffset, 0,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 XmNbottomAttachment, XmATTACH_NONE,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNshadowType, XmSHADOW_OUT,
							 NULL); 

   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)polys_bits, 
					polys_width, polys_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[6] = XtVaCreateManagedWidget("cp6",
							 xmDrawnButtonWidgetClass,gui->options_select_form,
							 XmNleftAttachment, XmATTACH_WIDGET,
							 XmNleftWidget, gui->options_panel.panel[5],
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->options_panel.panel[1],
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 XmNbottomAttachment, XmATTACH_NONE,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNshadowType, XmSHADOW_OUT,
							 NULL); 

   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)particles_bits, 
					particles_width, particles_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[7] = XtVaCreateManagedWidget("cp7",
							 xmDrawnButtonWidgetClass,gui->options_select_form,
							 XmNshadowType, XmSHADOW_IN,
							 XmNleftAttachment, XmATTACH_WIDGET,
							 XmNleftWidget, gui->options_panel.panel[6],
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->options_panel.panel[1],
							 XmNleftOffset, 0,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 XmNbottomAttachment, XmATTACH_NONE,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNshadowType, XmSHADOW_OUT,
							 NULL); 

   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)slice_bits, 
					slice_width, slice_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[8] = XtVaCreateManagedWidget("cp8",
							 xmDrawnButtonWidgetClass,gui->options_select_form,
							 XmNleftAttachment, XmATTACH_WIDGET,
							 XmNleftWidget, gui->options_panel.panel[7],
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->options_panel.panel[3],
							 XmNleftOffset, 0,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 XmNbottomAttachment, XmATTACH_NONE,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNshadowType, XmSHADOW_OUT,
							 NULL); 
   
   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)contours_bits, 
					contours_width, contours_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[9] = XtVaCreateManagedWidget("cp9",
							 xmDrawnButtonWidgetClass,gui->options_select_form,
							 XmNleftAttachment, XmATTACH_WIDGET,
							 XmNleftWidget, gui->options_panel.panel[8],
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->options_panel.panel[4],
							 XmNleftOffset, 0,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 XmNbottomAttachment, XmATTACH_NONE,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNshadowType, XmSHADOW_OUT,
							 NULL); 
   
   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)mouse_bits, 
					mouse_width, mouse_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[10] = XtVaCreateManagedWidget("cp10",
							  xmDrawnButtonWidgetClass,gui->options_select_form,
							  XmNleftAttachment, XmATTACH_FORM,
							  XmNtopAttachment, XmATTACH_WIDGET,
							  XmNtopWidget, gui->options_panel.panel[5],
							  XmNleftOffset, 0,
							  XmNlabelType,        XmPIXMAP,
							  XmNlabelPixmap,      pixmap,
							  XmNbottomAttachment, XmATTACH_NONE,
							  XmNrightAttachment, XmATTACH_NONE,
							  XmNshadowType, XmSHADOW_OUT,
							  NULL); 

   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)view_params_bits, 
					view_params_width, view_params_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[11] = XtVaCreateManagedWidget("cp11",
							  xmDrawnButtonWidgetClass,gui->options_select_form,
							  XmNleftAttachment, XmATTACH_WIDGET,
							  XmNleftWidget, gui->options_panel.panel[10],
							  XmNtopAttachment, XmATTACH_WIDGET,
							  XmNtopWidget, gui->options_panel.panel[6],
							  XmNlabelType,        XmPIXMAP,
							  XmNlabelPixmap,      pixmap,
							  XmNbottomAttachment, XmATTACH_NONE,
							  XmNrightAttachment, XmATTACH_NONE,
							  XmNshadowType, XmSHADOW_OUT,
							  NULL); 
   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)beam_sight_bits, 
					beam_sight_width, beam_sight_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[12] = XtVaCreateManagedWidget("cp12",
							  xmDrawnButtonWidgetClass,gui->options_select_form,
							  XmNleftAttachment, XmATTACH_WIDGET,
							  XmNleftWidget, gui->options_panel.panel[11],
							  XmNtopAttachment, XmATTACH_WIDGET,
							  XmNtopWidget, gui->options_panel.panel[7],
							  XmNlabelType,        XmPIXMAP,
							  XmNlabelPixmap,      pixmap,
							  XmNbottomAttachment, XmATTACH_NONE,
							  XmNrightAttachment, XmATTACH_NONE,
							  XmNshadowType, XmSHADOW_OUT,
							  NULL); 
   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)empty_bits, 
					empty_width, empty_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[13] = XtVaCreateManagedWidget("cp13",
							  xmDrawnButtonWidgetClass,gui->options_select_form,
							  XmNleftAttachment, XmATTACH_WIDGET,
							  XmNleftWidget, gui->options_panel.panel[12],
							  XmNtopAttachment, XmATTACH_WIDGET,
							  XmNtopWidget, gui->options_panel.panel[8],
							  XmNlabelType,        XmPIXMAP,
							  XmNlabelPixmap,      pixmap,
							  XmNbottomAttachment, XmATTACH_NONE,
							  XmNrightAttachment, XmATTACH_NONE,
							  XmNshadowType, XmSHADOW_OUT,
							  NULL); 
   
   pixmap = XCreatePixmapFromBitmapData(gui->display,
					RootWindowOfScreen(XtScreen(gui->form)),
					(char *)texture_bits, 
					texture_width, texture_height,
					gui->bg, gui->fg,
					DefaultDepthOfScreen(XtScreen(gui->form)));
   gui->options_panel.panel[14] = XtVaCreateManagedWidget("cp14",
							  xmDrawnButtonWidgetClass,gui->options_select_form,
							  XmNleftAttachment, XmATTACH_WIDGET,
							  XmNleftWidget, gui->options_panel.panel[13],
							  XmNtopAttachment, XmATTACH_WIDGET,
							  XmNtopWidget, gui->options_panel.panel[9],
							  XmNlabelType,        XmPIXMAP,
							  XmNlabelPixmap,      pixmap,
							  XmNbottomAttachment, XmATTACH_NONE,
							  XmNrightAttachment, XmATTACH_NONE,
							  XmNshadowType, XmSHADOW_OUT,
							  NULL); 
   
   for (i=0;i<15;i++)
     XtAddCallback(gui->options_panel.panel[i],XmNactivateCallback, Control_Panel_changedCB, (XtPointer)gui);
   
   
   gui->options_panel.divider1 = XtVaCreateManagedWidget("divider1",
							 xmSeparatorWidgetClass, gui->options_select_form,
							 XmNorientation, XmHORIZONTAL,
							 XmNseparatorType, XmSHADOW_ETCHED_OUT,
							 XmNrightAttachment, XmATTACH_FORM,
							 XmNleftAttachment, XmATTACH_FORM,
							 XmNbottomAttachment, XmATTACH_NONE,
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->options_panel.panel[10],
							 XmNtopOffset, 0,
							 XmNrightOffset, 5,
							 XmNleftOffset, 5,
							 NULL);

  gui->options_panel.divider2 = XtVaCreateManagedWidget("divider1",
							xmSeparatorWidgetClass, gui->options_select_form,
							XmNorientation, XmHORIZONTAL,
							XmNseparatorType, XmSHADOW_ETCHED_OUT,
							XmNrightAttachment, XmATTACH_FORM,
							XmNleftAttachment, XmATTACH_FORM,
							XmNbottomAttachment, XmATTACH_NONE,
							XmNtopAttachment, XmATTACH_WIDGET,
							XmNtopWidget, gui->options_panel.divider1,
							XmNtopOffset, 3,
							XmNrightOffset, 5,
							XmNleftOffset, 5,
							NULL);
  
  DEBUG_TRACE_OUT printf("Done with build_options_selector\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_message_pad
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds the message label at the top of the main
%%%           window which displays help messages and current
%%%           activity messages.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_message_pad(main_gui_t *gui)
{
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered build_message_pad\n");
  
  gui->message_panel.frame = XmCreateFrame(gui->message_panel.form, "message_frame", NULL, 0);
  XtVaSetValues(gui->message_panel.frame,
		XmNshadowType, XmSHADOW_ETCHED_IN,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,	 
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightOffset, 2,
		XmNleftOffset, 2,
		XmNtopOffset, 2,
		XmNbottomOffset, 2,
		NULL);
  XtManageChild(gui->message_panel.frame);  
  
  gui->message_panel.inner_form = XtVaCreateManagedWidget("message_form",
							  xmFormWidgetClass, gui->message_panel.frame,
							  NULL);
 
  xmstr = XmStringCreateLocalized("----- ");
  gui->message_panel.uv_pad = (Widget)XtVaCreateManagedWidget ("uv_file",
							       xmLabelWidgetClass, gui->message_panel.inner_form,
							       XmNrightAttachment, XmATTACH_FORM,
							       XmNleftAttachment, XmATTACH_NONE,
							       XmNlabelString, xmstr,
							       XmNforeground, /*gui->fg,*/WhitePixel(gui->display,DefaultScreen(gui->display)),
							       XmNbackground, gui->hl,
							       NULL);
  XmStringFree(xmstr);

  xmstr = XmStringCreateLocalized("Sera3D v 1.0");
  gui->message_panel.pad = (Widget)XtVaCreateManagedWidget ("messages",
							    xmLabelWidgetClass, gui->message_panel.inner_form,
							    XmNrightAttachment, XmATTACH_WIDGET,
							    XmNrightWidget, gui->message_panel.uv_pad,
							    XmNleftAttachment, XmATTACH_FORM,
							    XmNlabelString, xmstr,
							    XmNforeground, /*gui->fg,*/WhitePixel(gui->display,DefaultScreen(gui->display)),
							    XmNbackground, gui->hl,
							    NULL);

  if (!gui->messages_on) XtUnmanageChild(gui->message_panel.frame);
  XmStringFree(xmstr);

  DEBUG_TRACE_OUT printf("Done with build_message_pad\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : CreatePopupMenu
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent 
%%%
%%%  Purpose: builds the popup window for switch the mouse control
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void CreatePopupMenu(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered CreatePopupMenu\n");

  gui->popup_menu.menu = (Widget)XmCreatePopupMenu (gui->display_form,"Mouse_control_Popup",NULL,0);
  XtAddEventHandler(gui->display_form, ButtonPressMask,FALSE,PostMenu, (XtPointer)gui);
  
  XtCreateManagedWidget("Mouse Controls", xmLabelWidgetClass, gui->popup_menu.menu, NULL, 0);
  XtCreateManagedWidget("separator", xmSeparatorWidgetClass, gui->popup_menu.menu, NULL, 0);
  
  /*gui->popup_menu.button[0] = XtCreateManagedWidget ("Pull Axis", xmPushButtonWidgetClass, gui->popup_menu.menu, NULL, 0);
  XtAddCallback(gui->popup_menu.button[0], XmNactivateCallback, Mouse_control_changedCB, (XtPointer)gui);
  XtVaSetValues(gui->popup_menu.button[0], XmNsensitive,FALSE, NULL);
  */
 
  gui->popup_menu.button[1] = XtCreateManagedWidget ("Sliders", xmPushButtonWidgetClass, gui->popup_menu.menu, NULL, 0);
  XtAddCallback(gui->popup_menu.button[1], XmNactivateCallback, Mouse_control_changedCB, (XtPointer)gui);
  
  gui->popup_menu.button[2] = XtCreateManagedWidget ("Mouse Buttons", xmPushButtonWidgetClass, gui->popup_menu.menu, NULL, 0);
  XtAddCallback(gui->popup_menu.button[2], XmNactivateCallback, Mouse_control_changedCB, (XtPointer)gui);  

  DEBUG_TRACE_OUT printf("Done with CreatePopupMenu\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : PostMenu
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (eh)
%%%
%%%  Purpose: brings up the mouse control popup
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void PostMenu(Widget w, XtPointer clientData, XEvent *event, Boolean *flag)
{
  main_gui_t *gui = (main_gui_t *)clientData;

  DEBUG_TRACE_IN printf("Entered PostMenu\n");
 
  if (event->xbutton.button == Button3){
    XmMenuPosition (gui->popup_menu.menu, (XButtonPressedEvent *) event);
    XtManageChild(gui->popup_menu.menu);
  }

  DEBUG_TRACE_OUT printf("Done with PostMenu\n");
}
