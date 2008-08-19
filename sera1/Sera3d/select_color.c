#include "sera3d.h"


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
int Select_Color(main_gui_t *gui,XColor *color)
{
  static int first_time = 1;
 
  DEBUG_TRACE_IN printf("Entered Select_Color\n");

  if (first_time)
    {
      build_select_color_popup(gui);
      first_time = 0;
    }
 
  XtPopup(gui->select_color_popup.shell,XtGrabNone);
  
  gui->select_color_popup.done = 0;
  gui->select_color_popup.cancelled = 0;
  
  while (!gui->select_color_popup.done)
    XtAppProcessEvent(gui->app, XtIMAll);

  if (!gui->select_color_popup.cancelled){
    color->red   = gui->select_color_popup.returned_color.red;
    color->green = gui->select_color_popup.returned_color.green;
    color->blue  = gui->select_color_popup.returned_color.blue;
    color->flags = DoRed | DoGreen | DoBlue;
    init_color(gui,color->red,color->green,color->blue,color);
    DEBUG_TRACE_OUT printf("Done with Select_Color");
    return 1;
  }else{ 
    DEBUG_TRACE_OUT printf("Done with Select_Color, cancelled");
    return 0;
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
void build_select_color_popup(main_gui_t *gui)
{
  gui->select_color_popup.shell = XtCreatePopupShell("Find Color",
						     topLevelShellWidgetClass,gui->toplevel,
						     NULL,0);
  
  XtPopdown(gui->select_color_popup.shell);
  gui->select_color_popup.form = XtVaCreateManagedWidget("form",
							 xmFormWidgetClass, gui->select_color_popup.shell,
					/*		 XmNheight, 170, */
							 NULL);
  gui->select_color_popup.top_form = 
    XtVaCreateManagedWidget("top_form",
			    xmFormWidgetClass,
			    gui->select_color_popup.form,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_FORM,
			    XmNleftAttachment, XmATTACH_FORM,
			    NULL);

  gui->select_color_popup.button_form = 
    XtVaCreateManagedWidget("select_color_popup_button_form",
			    xmFormWidgetClass,
			    gui->select_color_popup.form,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNbottomAttachment, XmATTACH_FORM,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_FORM,
			    XmNtopWidget, gui->select_color_popup.top_form,
			    NULL);
  
  gui->select_color_popup.red_slider = (Widget)XtVaCreateManagedWidget("Redslider",
								       xmScaleWidgetClass,gui->select_color_popup.top_form,
								       XmNrightAttachment, XmATTACH_NONE,
								       XmNleftAttachment, XmATTACH_FORM,
								       XmNleftOffset, 10,
								       XmNtopAttachment, XmATTACH_FORM,
								       XmNtopOffset, 10,
								       XmNtitleString, XmStringCreateLocalized("Red"),
								       XmNvalue, 0,
								       XmNminimum, 0,
								       XmNmaximum, 65535,
								       XmNscaleWidth, 150,
								       XmNscaleHeight, 15,
								       XmNorientation, XmHORIZONTAL,
								       /*XmNtopShadowColor, r.pixel,
									 XmNhighlightColor, r.pixel,*/
								       NULL);
  gui->select_color_popup.green_slider = (Widget)XtVaCreateManagedWidget("Greenslider",
									 xmScaleWidgetClass,gui->select_color_popup.top_form,
									 XmNrightAttachment, XmATTACH_NONE,
									 XmNleftAttachment, XmATTACH_FORM,
									 XmNleftOffset, 10,
									 XmNtopAttachment, XmATTACH_WIDGET,
									 XmNtopWidget, gui->select_color_popup.red_slider,
									 XmNtopOffset, 5,
									 XmNtitleString, XmStringCreateLocalized("Green"),
									 XmNvalue, 0,
									 XmNminimum, 0,
									 XmNmaximum, 65535,
									 XmNscaleWidth, 150,
									 XmNscaleHeight, 15,
									 XmNorientation, XmHORIZONTAL,
									 /*XmNtopShadowColor, r.pixel,
									   XmNhighlightColor, r.pixel,*/
									 NULL);
  gui->select_color_popup.blue_slider = (Widget)XtVaCreateManagedWidget("Blueslider",
									xmScaleWidgetClass,gui->select_color_popup.top_form,
									XmNrightAttachment, XmATTACH_NONE,
									XmNleftAttachment, XmATTACH_FORM,
									XmNleftOffset, 10,
									XmNtopAttachment, XmATTACH_WIDGET,
									XmNtopWidget, gui->select_color_popup.green_slider,
									XmNtopOffset, 5,
									XmNtitleString, XmStringCreateLocalized("Blue"),
									XmNvalue, 0,
									XmNminimum, 0,
									XmNmaximum, 65535,
									XmNscaleWidth, 150,
									XmNscaleHeight, 15,
									XmNorientation, XmHORIZONTAL,
									/*XmNtopShadowColor, r.pixel,
									  XmNhighlightColor, r.pixel,*/
									NULL);

  XtAddCallback(gui->select_color_popup.red_slider,XmNvalueChangedCallback, Select_Color_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->select_color_popup.green_slider,XmNvalueChangedCallback, Select_Color_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->select_color_popup.blue_slider,XmNvalueChangedCallback,  Select_Color_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->select_color_popup.red_slider,XmNdragCallback, Select_Color_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->select_color_popup.green_slider,XmNdragCallback, Select_Color_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->select_color_popup.blue_slider,XmNdragCallback,  Select_Color_ChangedCB,(XtPointer)gui);

  gui->select_color_popup.swatch = 
    XtVaCreateManagedWidget("swatch",
			    xmDrawnButtonWidgetClass,
			    gui->select_color_popup.top_form,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, gui->select_color_popup.red_slider,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_FORM,
			    XmNrightOffset, 10,
			    XmNwidth, 40,
			    XmNleftOffset, 10,
			    XmNtopOffset, 10,
			    XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
			    XmNbottomWidget, gui->select_color_popup.blue_slider,
			    NULL);
  
  gui->select_color_popup.apply_button = 
    XtVaCreateManagedWidget("Apply",
			    xmPushButtonWidgetClass, gui->select_color_popup.button_form,
			    XmNtopAttachment, XmATTACH_FORM,
			    /*XmNtopWidget, gui->select_color_popup.swatch,*/
			    XmNtopOffset, 5,
			    XmNrightAttachment, XmATTACH_FORM,
			    XmNrightOffset, 5,
			    XmNbottomAttachment, XmATTACH_FORM,
			    XmNbottomOffset, 5,
			    NULL);
  XtAddCallback(gui->select_color_popup.apply_button, XmNactivateCallback, Select_Color_Popup_ApplyCB,(XtPointer)gui);
  
  gui->select_color_popup.cancel_button = 
    XtVaCreateManagedWidget("Cancel",
			    xmPushButtonWidgetClass, gui->select_color_popup.button_form,
			    XmNtopAttachment, XmATTACH_FORM,
			    /*	XmNtopWidget, gui->select_color_popup.swatch,*/
			    XmNtopOffset, 5,
			    XmNrightAttachment, XmATTACH_WIDGET,
			    XmNrightWidget, gui->select_color_popup.apply_button,
			    XmNrightOffset, 5,
			    XmNbottomAttachment, XmATTACH_FORM,
			    XmNbottomOffset, 5,
			    NULL);
  XtAddCallback(gui->select_color_popup.cancel_button, XmNactivateCallback,  Select_Color_Popup_CancelCB,(XtPointer)gui);
 
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
void Select_Color_Popup_CancelCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;

  gui->select_color_popup.cancelled = 1;
  gui->select_color_popup.done = 1;
  XtPopdown(gui->select_color_popup.shell);
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
void Select_Color_Popup_ApplyCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;

  gui->select_color_popup.done = 1;
  XtPopdown(gui->select_color_popup.shell);
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
void Select_Color_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) calldata;

  DEBUG_TRACE_IN printf("Entered Select_Color_ChangedCB\n");
  
  if (w == gui->select_color_popup.red_slider){
    gui->select_color_popup.returned_color.red = cbs->value;
  }else if (w == gui->select_color_popup.green_slider){
    gui->select_color_popup.returned_color.green = cbs->value;
  }else if (w == gui->select_color_popup.blue_slider){
    gui->select_color_popup.returned_color.blue = cbs->value;
  }
  init_color(gui,gui->select_color_popup.returned_color.red,
	     gui->select_color_popup.returned_color.green,
	     gui->select_color_popup.returned_color.blue,
	     &gui->select_color_popup.returned_color);

  XtVaSetValues(gui->select_color_popup.swatch,XmNbackground,gui->select_color_popup.returned_color.pixel,NULL);

  DEBUG_TRACE_OUT printf("Done with Select_Color_ChangedCB\n");
}

