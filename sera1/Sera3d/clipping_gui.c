#include "sera3d.h"

void Interactive_Clip_ToggleCB(Widget w, XtPointer clientData, XtPointer callData);
void Draw_Clip_Planes_ToggleCB(Widget w, XtPointer clientData, XtPointer callData);
static void Update_textCB(Widget w, XtPointer clientData, XtPointer callData);
static void NumericModifiedCallback(Widget w, XtPointer clientData, XtPointer callData);
static void UpdateScaleCB(Widget w, XtPointer clientData, XtPointer callData);
static void ClippingDialogOKCB (Widget w, XtPointer clientData, XtPointer callDat);
static void ClippingDialogCancelCB (Widget w, XtPointer clientData, XtPointer callData);



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : fill_body_clipping_with_bodies
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: when a file is loaded, the body names are each
%%%           placed on a toggle button, which toggle the clipping
%%%           of that body on and off. Clipping is defaulted on 
%%%           for every body.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void fill_body_clipping_with_bodies(main_gui_t *gui)
{
  int i;

  DEBUG_TRACE_IN printf("Entered fill_body_clipping_with_bodies\n");

  for (i=1;i<gui->num_bodies;i++){
    if (i == 1)
      gui->clipping_panel.clip_bods[i] = XtVaCreateManagedWidget(gui->bod[i].name,
					     xmToggleButtonWidgetClass,
					     gui->clipping_panel.s_win_form,
					     XmNtopAttachment, XmATTACH_FORM,
					     XmNleftAttachment, XmATTACH_FORM,
					     XmNtopOffset, 5,
					     XmNleftAttachment, 5,
					     XmNset, TRUE,
					     NULL);
    else
      gui->clipping_panel.clip_bods[i] = XtVaCreateManagedWidget(gui->bod[i].name,
					     xmToggleButtonWidgetClass,
					     gui->clipping_panel.s_win_form,
					     XmNtopAttachment, XmATTACH_WIDGET,
					     XmNtopWidget, gui->clipping_panel.clip_bods[i-1],
					     XmNleftAttachment, XmATTACH_FORM,
					     XmNtopOffset, 5,
					     XmNleftAttachment, 5,
					     XmNset, TRUE,
					     NULL);
    
    XtAddCallback(gui->clipping_panel.clip_bods[i], XmNvalueChangedCallback, 
		  (XtCallbackProc)Body_Clipping_ChangedCB, (XtPointer)gui);
  }

  DEBUG_TRACE_OUT printf("Done with fill_body_clipping_with_bodies\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : fill_con_clipping_with_contours
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: when a file is loaded, the body names are each
%%%           placed on a toggle button, which toggle the clipping
%%%           of that body on and off. Clipping is defaulted on 
%%%           for every body.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void fill_contour_clipping_with_contours(){
int i;

DEBUG_TRACE_IN printf("Entered fill_contour_clipping_with_contours\n");

for (i=0;i<num_contours;i++){
if (i == 0)
clip_cons[i] = XtVaCreateManagedWidget(con[i].name,
xmToggleButtonWidgetClass,
s_win_form,
XmNtopAttachment, XmATTACH_WIDGET,
XmNtopWidget, clip_bods[num_bodies-1],
XmNleftAttachment, XmATTACH_FORM,
XmNtopOffset, 5,
XmNleftAttachment, 5,
XmNset, TRUE,
NULL);
else
clip_cons[i] = XtVaCreateManagedWidget(con[i].name,
xmToggleButtonWidgetClass,
s_win_form,
XmNtopAttachment, XmATTACH_WIDGET,
XmNtopWidget, clip_cons[i-1],
XmNleftAttachment, XmATTACH_FORM,
XmNtopOffset, 5,
XmNleftAttachment, 5,
XmNset, TRUE,
NULL);

XtAddCallback(clip_cons[i], XmNvalueChangedCallback,
(XtCallbackProc)Con_Clipping_ChangedCB,(XtPointer)i);
}

DEBUG_TRACE_OUT printf("Done with fill_contour_clipping_with_contours\n");
}
*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : remove_bodies_from_body_clipping 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: when a file is closed, the body clipping toggles
%%%            are removed. 
%%%                       
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void remove_bodies_from_body_clipping(main_gui_t *gui)
{
  int i;

  DEBUG_TRACE_IN printf("Entered remove_bodies_from_body_clipping\n");

  for (i=1;i<gui->num_bodies;i++)
    {
      XtRemoveCallback(gui->clipping_panel.clip_bods[i], XmNvalueChangedCallback,
		    (XtCallbackProc)Body_Clipping_ChangedCB, (XtPointer)gui);
      XtDestroyWidget(gui->clipping_panel.clip_bods[i]);
    }

  DEBUG_TRACE_OUT printf("Done with remove_bodies_from_body_clipping\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Show_clipping_dialogCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: brings up the clipping dialog box
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Show_clipping_dialogCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  Boolean tf_inter, tf_planes;
  int i,j;

  DEBUG_TRACE_IN printf("Entered Show_Clipping_dialogCB\n");

  XtVaGetValues(gui->clipping_dialog.inter_toggle, XmNset,&tf_inter,NULL);
  if (tf_inter &&  gui->fast_rotation_type == 1)
    gui->draw_high_lists = 1;

  XtVaGetValues(gui->clipping_dialog.d_planes, XmNset,&tf_planes,NULL);
  if (tf_planes && tf_inter) gui->draw_clip_planes = 1;


  for (i=0;i<3;i++)
    {
      gui->tempclip[i].on = gui->clip[i].on;
      gui->tempclip[i].f_capped = gui->clip[i].f_capped;
      gui->tempclip[i].b_capped = gui->clip[i].b_capped;
      for (j=0;j<4;j++)
	{
	  gui->tempclip[i].f_eqn[j] = gui->clip[i].f_eqn[j];
	  gui->tempclip[i].b_eqn[j] = gui->clip[i].b_eqn[j];
	}
    }
  XtManageChild(gui->clipping_dialog.dialog);

  gui->clipping_mode = 1;

  if (gui->interactive_clipping && gui->fast_rotation_type != 3){
    XtRemoveEventHandler(gui->glxarea,ButtonPressMask, FALSE, Set_High_Lists, (XtPointer)gui); 
    XtRemoveEventHandler(gui->glxarea,ButtonReleaseMask, FALSE, Unset_High_Lists, (XtPointer)gui); 
    draw_all(gui);
  }

  /* MTC 5/19/99 -- Explicitly call the Update_textCB to update the slider labels */
  Update_textCB ( gui->clipping_dialog.pos_slider, ( XtPointer ) gui, NULL );
  Update_textCB ( gui->clipping_dialog.neg_slider, ( XtPointer ) gui, NULL );
  
  DEBUG_TRACE_OUT printf("Done with Show_Clipping_dialogCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_clipping_dialog
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent
%%%
%%%  Purpose: builds the dialog for controlling the clipping
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_clipping_dialog(main_gui_t *gui)
{
  char out_string[25];
  XmString xmstr;
  int min_x,max_x, min_y,max_y, min_z, max_z;
  Arg al[5];
  int ac = 0;
  

  DEBUG_TRACE_IN printf("Entered build_clipping_dialog\n");

  min_x = (int)((-128*gui->x_size)*100.0);
  max_x = (int)((128*gui->x_size)*100.0);

  min_y = (int)((-128*gui->y_size)*100.0);
  max_y = (int)((128*gui->y_size)*100.0);

  max_z = (int)((0+gui->num_slices/2.0*gui->z_spacing)*100.0);
  min_z = (int)((0-gui->num_slices/2.0*gui->z_spacing)*100.0);

  xmstr = XmStringCreateLocalized( "Detailed Clipping" );
  XtSetArg( al[ac], XmNdialogTitle,  xmstr ); ac++;

  gui->clipping_dialog.dialog = (Widget)XmCreateMessageDialog(gui->form, "Detailed Clipping", al, ac);
  XmStringFree( xmstr );

  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->clipping_dialog.dialog,XmDIALOG_SYMBOL_LABEL));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->clipping_dialog.dialog,XmDIALOG_MESSAGE_LABEL));
  xmstr = XmStringCreateLocalized("Apply");
  XtVaSetValues((Widget)XmMessageBoxGetChild(gui->clipping_dialog.dialog,XmDIALOG_OK_BUTTON),
		XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);
  
  XtAddCallback(gui->clipping_dialog.dialog, XmNokCallback,
		ClippingDialogOKCB,(XtPointer)gui);
  XtAddCallback(gui->clipping_dialog.dialog, XmNcancelCallback,
		ClippingDialogCancelCB, (XtPointer)gui);
  
  /*XtVaGetValues(gui->form, 
    XmNforeground, &fg,
    XmNbackground, &bg,
    NULL);*/

  gui->clipping_dialog.main_form = XtVaCreateManagedWidget ("main_form", 
							    xmFormWidgetClass, gui->clipping_dialog.dialog, 
							    NULL);
  
  xmstr = XmStringCreateLocalized("Clipping Controls");
  gui->clipping_dialog.title = XtVaCreateManagedWidget("Clipping",
						       xmLabelWidgetClass, gui->clipping_dialog.main_form,
						       XmNtopAttachment, XmATTACH_FORM,
						       XmNtopOffset, 5,
						       XmNleftAttachment, XmATTACH_FORM,
						       XmNleftOffset, 100,
						       XmNrightAttachment, XmATTACH_NONE,
						       XmNleftAttachment, XmATTACH_NONE,
						       XmNlabelString, xmstr,
						       NULL);
  XmStringFree(xmstr);

  gui->clipping_dialog.inter_toggle = XtVaCreateManagedWidget("Interactive clipping",
							      xmToggleButtonWidgetClass,gui->clipping_dialog.main_form,
							      XmNtopAttachment, XmATTACH_WIDGET,
							      XmNtopWidget, gui->clipping_dialog.title,
							      XmNtopOffset, 5,
							      XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
							      XmNleftWidget, gui->clipping_dialog.title,
							      XmNleftOffset, 10,
							      NULL);
  XtAddCallback(gui->clipping_dialog.inter_toggle, XmNvalueChangedCallback,
		Interactive_Clip_ToggleCB, (XtPointer)gui);
  
  gui->clipping_dialog.d_planes = XtVaCreateManagedWidget("draw planes",
							  xmToggleButtonWidgetClass, gui->clipping_dialog.main_form,
							  XmNtopAttachment, XmATTACH_WIDGET,
							  XmNtopWidget, gui->clipping_dialog.inter_toggle,
							  XmNtopOffset, 0,
							  XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
							  XmNleftWidget, gui->clipping_dialog.inter_toggle,
							  XmNleftOffset, 10,
							  NULL);
  XtAddCallback(gui->clipping_dialog.d_planes, XmNvalueChangedCallback,
		Draw_Clip_Planes_ToggleCB, (XtPointer)gui);
  XtUnmanageChild(gui->clipping_dialog.d_planes);

  gui->clipping_dialog.dir_frame = XtVaCreateManagedWidget("dir_frame",
							   xmFrameWidgetClass, gui->clipping_dialog.main_form,
							   XmNshadowType, XmSHADOW_ETCHED_IN,
							   XmNtopAttachment, XmATTACH_WIDGET,
							   XmNtopWidget, gui->clipping_dialog.inter_toggle,
							   XmNtopOffset, 30,
							   XmNleftAttachment, XmATTACH_FORM,
							   XmNleftOffset, 10,
							   NULL);
  
  gui->clipping_dialog.dir_form = XtVaCreateManagedWidget("dir_form",
							  xmFormWidgetClass, gui->clipping_dialog.dir_frame,
							  NULL);

  xmstr = XmStringCreateLocalized("  IS  ");
  gui->clipping_dialog.ax_push = XtVaCreateManagedWidget("Axial",
							 xmDrawnButtonWidgetClass, gui->clipping_dialog.dir_form,
							 XmNleftAttachment, XmATTACH_FORM,
							 XmNtopAttachment, XmATTACH_FORM,
							 XmNbottomAttachment, XmATTACH_FORM,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNshadowType, XmSHADOW_IN,
							 XmNlabelType, XmSTRING,
							 XmNlabelString, xmstr,
							 NULL);
  XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("  PA  ");
  gui->clipping_dialog.cor_push = XtVaCreateManagedWidget("Coronal",
							  xmDrawnButtonWidgetClass, gui->clipping_dialog.dir_form,
							  XmNleftAttachment, XmATTACH_WIDGET,
							  XmNleftWidget, gui->clipping_dialog.ax_push,
							  XmNtopAttachment, XmATTACH_FORM,
							  XmNbottomAttachment, XmATTACH_FORM,
							  XmNrightAttachment, XmATTACH_NONE,
							  XmNshadowType, XmSHADOW_OUT,
							  XmNlabelType, XmSTRING,
							  XmNlabelString, xmstr,
							  NULL);
  XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("  RL  ");
  gui->clipping_dialog.sag_push = XtVaCreateManagedWidget("Sagittal",
							  xmDrawnButtonWidgetClass, gui->clipping_dialog.dir_form,
							  XmNleftAttachment, XmATTACH_WIDGET,
							  XmNleftWidget, gui->clipping_dialog.cor_push,
							  XmNtopAttachment, XmATTACH_FORM,
							  XmNbottomAttachment, XmATTACH_FORM,
							  XmNrightAttachment, XmATTACH_FORM,
							  XmNshadowType, XmSHADOW_OUT,
							  XmNlabelType, XmSTRING,
							  XmNlabelString, xmstr,
							  NULL);
  XmStringFree(xmstr);
  
  XtAddCallback(gui->clipping_dialog.ax_push, XmNactivateCallback,  Clip_Direction_ChangedCB, (XtPointer)gui);
  XtAddCallback(gui->clipping_dialog.cor_push, XmNactivateCallback, Clip_Direction_ChangedCB, (XtPointer)gui);
  XtAddCallback(gui->clipping_dialog.sag_push, XmNactivateCallback, Clip_Direction_ChangedCB, (XtPointer)gui);

  gui->clipping_dialog.ctrl_frame = XtVaCreateManagedWidget("ctrl_frame",
							    xmFrameWidgetClass, gui->clipping_dialog.main_form,
							    XmNshadowType, XmSHADOW_ETCHED_IN,
							    XmNtopAttachment, XmATTACH_WIDGET,
							    XmNtopWidget, gui->clipping_dialog.dir_frame,
							    XmNtopOffset, 15,
							    XmNleftAttachment, XmATTACH_FORM,
							    XmNleftOffset, 10,
							    NULL);
  
  gui->clipping_dialog.ctrl_form = XtVaCreateManagedWidget("ctrl_form",
				  xmFormWidgetClass, gui->clipping_dialog.ctrl_frame,
				  NULL);

  gui->clipping_dialog.clip_toggle = XtVaCreateManagedWidget("IS Clipping On",
							     xmToggleButtonWidgetClass, gui->clipping_dialog.ctrl_form,
							     XmNtopAttachment, XmATTACH_FORM,
							     XmNtopOffset, 10,
							     XmNleftAttachment, XmATTACH_FORM,
							     XmNleftOffset, 20,
							     NULL);
  XtAddCallback(gui->clipping_dialog.clip_toggle, XmNvalueChangedCallback,
		(XtCallbackProc)Clipping_ToggledCB, (XtPointer)gui);

  gui->clipping_dialog.pos_label = XtVaCreateManagedWidget("Above IS = 128.00 (mm)",
							   xmLabelWidgetClass, gui->clipping_dialog.ctrl_form,
							   XmNtopAttachment, XmATTACH_WIDGET,
							   XmNtopWidget, gui->clipping_dialog.clip_toggle,
							   XmNtopOffset, 10,
							   XmNleftAttachment, XmATTACH_FORM,
							   XmNleftOffset, 15,
							   NULL);
  

  gui->clipping_dialog.neg_label = XtVaCreateManagedWidget("Below IS = -128.00 (mm)",
							   xmLabelWidgetClass, gui->clipping_dialog.ctrl_form,
							   XmNtopAttachment, XmATTACH_WIDGET,
							   XmNtopWidget, gui->clipping_dialog.pos_label,
							   XmNtopOffset, 3,
							   XmNleftAttachment, XmATTACH_FORM,
							   XmNleftOffset, 15,
							   NULL);

  gui->clipping_dialog.neg_slider = (Widget)XtVaCreateManagedWidget("Neg_slider",
								    xmScaleWidgetClass, gui->clipping_dialog.ctrl_form,
								    XmNrightAttachment, XmATTACH_NONE,
								    XmNleftAttachment, XmATTACH_FORM,
								    XmNleftOffset, 50,
								    XmNtopAttachment, XmATTACH_WIDGET,
								    XmNtopWidget, gui->clipping_dialog.neg_label,
								    XmNtopOffset, 45,
								    XmNvalue, -12800,
								    XmNminimum, -12800,
								    XmNmaximum, 12800,
								    XmNscaleWidth, 15,
								    XmNscaleHeight, 256,
								    XmNorientation, XmVERTICAL,
								    /*XmNshowValue, TRUE,*/
								    NULL);
  
  gui->clipping_dialog.pos_slider = (Widget)XtVaCreateManagedWidget("Pos_slider",
								    xmScaleWidgetClass, gui->clipping_dialog.ctrl_form,
								    XmNrightAttachment, XmATTACH_NONE,
								    XmNleftAttachment, XmATTACH_WIDGET,
								    XmNleftWidget, gui->clipping_dialog.neg_slider,
								    XmNleftOffset, 50,
								    XmNtopAttachment, XmATTACH_WIDGET,
								    XmNtopWidget, gui->clipping_dialog.neg_label,
								    XmNtopOffset, 45,
								    XmNvalue, 12800,
								    XmNminimum, -12800,
								    XmNmaximum, 12800,
								    XmNscaleWidth, 15,
								    XmNscaleHeight, 256,
								    XmNorientation, XmVERTICAL,
								    /*XmNshowValue, TRUE,*/
								    NULL);
  
  if (gui->cm){
    sprintf(out_string,"%s %.2f (cm)","Above IS =  ", convert_world_y_to_slice_z(gui,128.0)/10.0);
    xmstr = XmStringCreateLocalized(out_string);
    XtVaSetValues(gui->clipping_dialog.pos_label, XmNlabelString, xmstr, NULL);
    XmStringFree(xmstr);
    sprintf(out_string,"%s %.2f (cm)","Below IS =  ", convert_world_y_to_slice_z(gui,-128.0)/10.0);
    xmstr = XmStringCreateLocalized(out_string);
    XtVaSetValues(gui->clipping_dialog.neg_label, XmNlabelString, xmstr, NULL);
    XmStringFree(xmstr);
  }else{
    sprintf(out_string,"%s %.2f (mm)","Above IS =  ", convert_world_y_to_slice_z(gui,128.0));
    xmstr = XmStringCreateLocalized(out_string);
    XtVaSetValues(gui->clipping_dialog.pos_label, XmNlabelString, xmstr, NULL);
    XmStringFree(xmstr);
    sprintf(out_string,"%s %.2f (mm)","Below IS =  ",  convert_world_y_to_slice_z(gui,-128.0));
    xmstr = XmStringCreateLocalized(out_string);
    XtVaSetValues(gui->clipping_dialog.neg_label, XmNlabelString, xmstr, NULL);
    XmStringFree(xmstr);
  }
  XtAddCallback(gui->clipping_dialog.pos_slider, XmNvalueChangedCallback, Clipping_ChangedCB, (XtPointer)gui);
  XtAddCallback(gui->clipping_dialog.pos_slider, XmNdragCallback, Clipping_ChangedCB, (XtPointer)gui);
  
  XtAddCallback(gui->clipping_dialog.neg_slider, XmNvalueChangedCallback, Clipping_ChangedCB, (XtPointer)gui);
  XtAddCallback( gui->clipping_dialog.neg_slider, XmNdragCallback, Clipping_ChangedCB, (XtPointer)gui);
  
  XtAddCallback ( gui->clipping_dialog.pos_slider, XmNvalueChangedCallback, (XtCallbackProc)Update_textCB, (XtPointer)gui);
  XtAddCallback (gui->clipping_dialog. pos_slider, XmNdragCallback, (XtCallbackProc)Update_textCB, (XtPointer)gui);
  
  XtAddCallback ( gui->clipping_dialog.neg_slider, XmNvalueChangedCallback, (XtCallbackProc)Update_textCB, (XtPointer)gui);
  XtAddCallback ( gui->clipping_dialog.neg_slider, XmNdragCallback, (XtCallbackProc)Update_textCB, (XtPointer)gui);
       
  gui->clipping_dialog.pixmap[0] = XCreatePixmapFromBitmapData(XtDisplay(gui->form),
					  RootWindowOfScreen(XtScreen(gui->form)),
					  (char *)below_bits, below_width, below_height,
					  gui->fg, gui->bg,
					  DefaultDepthOfScreen(XtScreen(gui->form)));
  gui->clipping_dialog.pixmap[1] = XCreatePixmapFromBitmapData(XtDisplay(gui->form),
					  RootWindowOfScreen(XtScreen(gui->form)),
					  (char *)above_bits, above_width, above_height,
					  gui->fg, gui->bg,
					  DefaultDepthOfScreen(XtScreen(gui->form)));
  gui->clipping_dialog.pixmap[2] = XCreatePixmapFromBitmapData(XtDisplay(gui->form),
					  RootWindowOfScreen(XtScreen(gui->form)),
					  (char *)behind_bits, behind_width, behind_height,
					  gui->fg, gui->bg,
					  DefaultDepthOfScreen(XtScreen(gui->form)));
  gui->clipping_dialog.pixmap[3] = XCreatePixmapFromBitmapData(XtDisplay(gui->form),
		      RootWindowOfScreen(XtScreen(gui->form)),
					  (char *)front_bits, front_width, front_height,
					  gui->fg, gui->bg,
					  DefaultDepthOfScreen(XtScreen(gui->form)));
  gui->clipping_dialog.pixmap[4] = XCreatePixmapFromBitmapData(XtDisplay(gui->form),
					  RootWindowOfScreen(XtScreen(gui->form)),
					  (char *)left_bits, left_width, left_height,
					  gui->fg, gui->bg,
					  DefaultDepthOfScreen(XtScreen(gui->form)));
  gui->clipping_dialog.pixmap[5] = XCreatePixmapFromBitmapData(XtDisplay(gui->form),
					  RootWindowOfScreen(XtScreen(gui->form)),
					  (char *)right_bits, right_width, right_height,
					  gui->fg, gui->bg,
					  DefaultDepthOfScreen(XtScreen(gui->form)));


  gui->clipping_dialog.neg_pic = XtVaCreateManagedWidget("label", xmLabelGadgetClass, gui->clipping_dialog.ctrl_form,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      gui->clipping_dialog.pixmap[0],
							 XmNleftAttachment,   XmATTACH_FORM,
							 XmNleftOffset, 35,
							 XmNbottomAttachment,    XmATTACH_WIDGET,
							 XmNbottomWidget, gui->clipping_dialog.neg_slider,
							 XmNbottomOffset, 3,
							 XmNrightAttachment, XmATTACH_NONE,
							 XmNtopAttachment, XmATTACH_NONE,
							 NULL);       
  gui->clipping_dialog.pos_pic = XtVaCreateManagedWidget("label", xmLabelGadgetClass, gui->clipping_dialog.ctrl_form,
				    XmNlabelType,        XmPIXMAP,
				    XmNlabelPixmap,      gui->clipping_dialog.pixmap[1],
				    XmNleftAttachment,   XmATTACH_WIDGET,
				    XmNleftWidget, gui->clipping_dialog.neg_pic,
				    XmNleftOffset, 25,
				    XmNbottomAttachment,    XmATTACH_WIDGET,
				    XmNbottomWidget, gui->clipping_dialog.pos_slider,
				    XmNbottomOffset, 3,
				    XmNrightAttachment, XmATTACH_NONE,
				    XmNtopAttachment, XmATTACH_NONE,
				    NULL);       

  gui->clipping_dialog.neg_cap = XtVaCreateManagedWidget("Cap",
							 xmToggleButtonWidgetClass, gui->clipping_dialog.ctrl_form,
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->clipping_dialog.neg_slider,
							 XmNtopOffset, 10,
							 XmNleftAttachment, XmATTACH_FORM,
							 XmNleftOffset, 35,
                                                         XmNsensitive, False, /* wait for qsh file to be loaded */
							 NULL);
  gui->clipping_dialog.pos_cap = XtVaCreateManagedWidget("Cap ",
							 xmToggleButtonWidgetClass, gui->clipping_dialog.ctrl_form,
							 XmNtopAttachment, XmATTACH_WIDGET,
							 XmNtopWidget, gui->clipping_dialog.pos_slider,
							 XmNtopOffset, 10,
							 XmNleftAttachment, XmATTACH_WIDGET,
							 XmNleftWidget, gui->clipping_dialog.neg_cap,
							 XmNleftOffset, 10,
                                                         XmNsensitive, False, /* wait for qsh file to be loaded */
							 NULL);
  XtAddCallback(gui->clipping_dialog.neg_cap, XmNvalueChangedCallback, Capping_ChangedCB, (XtPointer)gui);
  XtAddCallback(gui->clipping_dialog.pos_cap, XmNvalueChangedCallback, Capping_ChangedCB, (XtPointer)gui);

  gui->clipping_dialog.num_box = (Widget)XmCreateTextField(gui->clipping_dialog.ctrl_form, "input", NULL,0);
  XtVaSetValues(gui->clipping_dialog.num_box,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->clipping_dialog.neg_cap,
		XmNtopOffset, 8,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 20,
		XmNleftAttachment, XmATTACH_NONE,
		XmNbottomAttachment, XmATTACH_NONE,
		/*XmNwidth, 10,*/
		NULL);
  XtManageChild(gui->clipping_dialog.num_box);

  gui->clipping_dialog.num_neg = (Widget)XtVaCreateManagedWidget("Below",
								 xmPushButtonWidgetClass, gui->clipping_dialog.ctrl_form,
								 XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
								 XmNleftWidget, gui->clipping_dialog.num_box,
								 XmNrightAttachment, XmATTACH_NONE,
								 XmNbottomAttachment, XmATTACH_NONE,
								 XmNtopAttachment, XmATTACH_WIDGET,
								 XmNtopWidget, gui->clipping_dialog.num_box,
								 NULL);
  gui->clipping_dialog.num_pos = (Widget)XtVaCreateManagedWidget("Above",
								 xmPushButtonWidgetClass, gui->clipping_dialog.ctrl_form,
								 XmNleftAttachment, XmATTACH_WIDGET,
								 XmNleftWidget, gui->clipping_dialog.num_neg,
								 XmNrightAttachment, XmATTACH_NONE,
								 XmNbottomAttachment, XmATTACH_NONE,
								 XmNtopAttachment, XmATTACH_WIDGET,
								 XmNtopWidget, gui->clipping_dialog.num_box,
								 NULL);

  XtAddCallback (gui->clipping_dialog.num_box, XmNmodifyVerifyCallback, NumericModifiedCallback, (XtPointer)gui);
  XtAddCallback (gui->clipping_dialog.num_pos, XmNactivateCallback, UpdateScaleCB, (XtPointer)gui);
  XtAddCallback (gui->clipping_dialog.num_neg, XmNactivateCallback, UpdateScaleCB, (XtPointer)gui);

  DEBUG_TRACE_OUT printf("Done with build_clipping_dialog\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : NumericModifiedCallback
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: as characters are typed into the numeric box,
%%%           this procedure checks to make sure that they are
%%%           are valid numbers.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static void NumericModifiedCallback(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *) callData;

  DEBUG_TRACE_IN printf("Entered NumericModifiedCallback\n");

  if (cbs->text->ptr)
    {
      char *string = cbs->text->ptr;
      int i;
 
      for (i=0;i<cbs->text->length; i++)
	if (!isdigit(string[i]) && (string[i] != '.') && (string[i] != '-'))
	  {
	    cbs->doit = FALSE;
	    DT_error(gui->toplevel,"Not a valid value",NULL,NULL);
	  }
    }

  DEBUG_TRACE_OUT printf("Done with NumericModifiedCallback\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : UpdateScaleCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: after the numeric text box is used to set the 
%%%           clipping, the slider must be updated.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static void UpdateScaleCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  Widget slider; 
  int ok = 1, is_slider_pos;
  float value;
  char *text;
  
  DEBUG_TRACE_IN printf("Entered UpdateScaleCB\n");

  if (w == gui->clipping_dialog.num_pos) slider = gui->clipping_dialog.pos_slider;
  else slider = gui->clipping_dialog.neg_slider;

  text = XmTextFieldGetString(gui->clipping_dialog.num_box);
  sscanf(text,"%f",&value);
  
  /*printf("just got the text field value : %f\n",value);*/
  if (gui->clipping_dialog.current_dir == 1) value = convert_slice_z_to_world_y(gui,value);
  /*printf("converted  value is : %f\n",value);*/ 
  value *= 100.0;


  if (strcmp(XtName(slider),"Pos_slider") == 0) is_slider_pos = 1;
  else is_slider_pos = 0;

  if (value < -15000 || value > 15000){
    DT_error(gui->toplevel,"Value out of bounds",NULL,NULL);
    return;
  }

  if (ok){
    XmScaleSetValue(slider, (int)(value+.5));
    Update_textCB(slider,clientData, callData);
    
    if (is_slider_pos){
      gui->clip[gui->clipping_dialog.current_dir-1].f_eqn[3] = value/100.0;	  
      /*printf("set the f_eqn to : %f\n",value/100.0);*/
    }else{
      gui->clip[gui->clipping_dialog.current_dir-1].b_eqn[3] = -value/100.0;
      /*printf("set the b_eqn to : %f\n",-value/100.0);*/
    }
    switch(gui->clipping_dialog.current_dir-1){
    case 0:
      if (is_slider_pos) glClipPlane(GL_CLIP_PLANE0, gui->clip[0].f_eqn);
      else glClipPlane(GL_CLIP_PLANE1, gui->clip[0].b_eqn);
      break;
    case 1:
      if (is_slider_pos)  glClipPlane(GL_CLIP_PLANE2, gui->clip[1].f_eqn);
      else  glClipPlane(GL_CLIP_PLANE3, gui->clip[1].b_eqn);
      break;
    case 2:
      if (is_slider_pos)  glClipPlane(GL_CLIP_PLANE4, gui->clip[2].f_eqn);
      else  glClipPlane(GL_CLIP_PLANE5, gui->clip[2].b_eqn);
      break;
    }
  }else{
    DT_error(gui->toplevel,"Value not allowed",NULL,NULL);
    return;
  }
  if (gui->interactive_clipping) draw_all(gui);

  DEBUG_TRACE_OUT printf("Done withNumericModifiedCallback\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Update_textCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: when the clipping values have changed, the 
%%%           label at the top of the dialog displays
%%%           the current value so it must be updated.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static void Update_textCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmString xmstr;
  int pos_value,neg_value;
  char out_string[50],meas[5];
  float denom;
  char pos_label[16], neg_label[16];
  char name[8];
  int dir;
  
  DEBUG_TRACE_IN printf("Entered Update_textCB\n");

  XtVaGetValues (gui->clipping_dialog.pos_slider, XmNvalue, &pos_value, NULL);
  XtVaGetValues (gui->clipping_dialog.neg_slider, XmNvalue, &neg_value, NULL);

  if (gui->cm){ 
    denom = 1000.0; strcpy(meas,"(cm)");
  }else{
    denom = 100.0; strcpy(meas,"(mm)");
  }

  dir = gui->clipping_dialog.current_dir;
  strcpy( name, gui->axisLabels[dir-1].name );
  
  switch( gui->axisLabels[dir-1].first )
  {
      case 'I':
      case 'S':
          strcpy( pos_label, "Above" );
          strcpy( neg_label, "Below" );
          break;
          
      case 'P':
      case 'A':
          strcpy( pos_label, "In Front" );
          strcpy( neg_label, "Behind"   );
          break;
          
      case 'R':
      case 'L':
          strcpy( pos_label, "Right Of" );
          strcpy( neg_label, "Left Of" );
          break;
  }
  
  /*printf("updating text, the pos val is : %d\n",pos_value);*/

  switch(gui->clipping_dialog.current_dir){
  case 1:
    if (gui->cm) denom = 10.0;
    else denom = 1.0;
    if (w == gui->clipping_dialog.pos_slider){
      sprintf(out_string,"%s %s =  %.2f %s", pos_label, name, convert_world_y_to_slice_z(gui,(float)(pos_value)/100.0)/denom, meas);
      xmstr = XmStringCreateLocalized(out_string);
      XtVaSetValues(gui->clipping_dialog.pos_label, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
    }else {
      sprintf(out_string,"%s %s =  %.2f %s", neg_label, name, convert_world_y_to_slice_z(gui,(float)(neg_value)/100.0)/denom, meas);
      xmstr = XmStringCreateLocalized(out_string);
      XtVaSetValues(gui->clipping_dialog.neg_label, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
    }
    break;
  case 2:
    if (w == gui->clipping_dialog.pos_slider){
      sprintf(out_string,"%s %s =  %.2f %s", pos_label, name, (float)(pos_value)/denom,meas);
      xmstr = XmStringCreateLocalized(out_string);
      XtVaSetValues(gui->clipping_dialog.pos_label, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
    }else {
      sprintf(out_string,"%s %s =  %.2f %s", neg_label, name, (float)(neg_value)/denom,meas);
      xmstr = XmStringCreateLocalized(out_string);
      XtVaSetValues(gui->clipping_dialog.neg_label, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
    }
    break;
  case 3:
    if (w == gui->clipping_dialog.pos_slider){
      sprintf(out_string,"%s %s =  %.2f %s", pos_label, name, (float)(pos_value)/denom,meas);
      xmstr = XmStringCreateLocalized(out_string);
      XtVaSetValues(gui->clipping_dialog.pos_label, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
    }else {
      sprintf(out_string,"%s %s =  %.2f %s", neg_label, name, (float)(neg_value)/denom,meas);
      xmstr = XmStringCreateLocalized(out_string);
      XtVaSetValues(gui->clipping_dialog.neg_label, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
    }
    break;
  }
  DEBUG_TRACE_OUT printf("Done with Update_textCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : ClippingDialogCancelCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: cancels the clipping settings 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static void ClippingDialogCancelCB (Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  gui->draw_clip_planes = 0;
  gui->clipping_mode = 0;

  DEBUG_TRACE_IN printf("Entered ClippingDialogCancelCB\n");

  if (gui->interactive_clipping && gui->fast_rotation_type != 3){
    XtAddEventHandler(gui->glxarea,ButtonPressMask, FALSE, Set_High_Lists, (XtPointer)gui);  	
    XtAddEventHandler(gui->glxarea,ButtonReleaseMask, FALSE, Unset_High_Lists, (XtPointer)gui); 
  }
  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with ClippingDialogCancelCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : ClippingDialogOKCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: when interactive clipping is on, this just
%%%           basically closes the clipping dialog,
%%%           when interactive clipping is off, it copies
%%%           all the new clipping info into the clip
%%%           structures and applies them to the rendering, then
%%%           closes the dialog.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static void ClippingDialogOKCB (Widget w, XtPointer clientData, XtPointer callData)
{  
  main_gui_t *gui = (main_gui_t *)clientData;
  Boolean tf;
  int i,j;

  DEBUG_TRACE_IN printf("Entered ClippingDialogOKCB\n");

  DisplayBusyCursor(gui->form);
  
  gui->draw_clip_planes = 0;
  gui->clipping_mode = 0;
  
  XtVaGetValues(gui->clipping_dialog.inter_toggle, XmNset,&tf,NULL);
  /*if (tf && gui->view_style == VIEW_STYLE_UNIVELS) glEnable(GL_LIGHTING);*/  
  if (tf) gui->draw_high_lists = 0;
  
  if (!gui->interactive_clipping){
    for (i=0;i<3;i++){
      gui->clip[i].on       = gui->tempclip[i].on;
      gui->clip[i].f_capped = gui->tempclip[i].f_capped;
      gui->clip[i].b_capped = gui->tempclip[i].b_capped;
      for (j=0;j<4;j++){
	gui->clip[i].f_eqn[j] = gui->tempclip[i].f_eqn[j];
	gui->clip[i].b_eqn[j] = gui->tempclip[i].b_eqn[j];
      }
    }
    glClipPlane(GL_CLIP_PLANE0, gui->clip[0].f_eqn);
    glClipPlane(GL_CLIP_PLANE1, gui->clip[0].b_eqn);
    glClipPlane(GL_CLIP_PLANE2, gui->clip[1].f_eqn);  
    glClipPlane(GL_CLIP_PLANE3, gui->clip[1].b_eqn);
    glClipPlane(GL_CLIP_PLANE4, gui->clip[2].f_eqn);
    glClipPlane(GL_CLIP_PLANE5, gui->clip[2].b_eqn);

    if (gui->clip[0].on){
      glEnable(GL_CLIP_PLANE0); glEnable(GL_CLIP_PLANE1);
    }else{
      glDisable(GL_CLIP_PLANE0); glDisable(GL_CLIP_PLANE1);
    }
    if (gui->clip[1].on){
      glEnable(GL_CLIP_PLANE2); glEnable(GL_CLIP_PLANE3);
    }else {
      glDisable(GL_CLIP_PLANE2); glDisable(GL_CLIP_PLANE3);
    }
    if (gui->clip[1].on){
      glEnable(GL_CLIP_PLANE4); glEnable(GL_CLIP_PLANE5);
    }else{
      glDisable(GL_CLIP_PLANE4); glDisable(GL_CLIP_PLANE5);
    }
  }
  else if (gui->fast_rotation_type != 3){
    XtAddEventHandler(gui->glxarea,ButtonPressMask, FALSE, Set_High_Lists, (XtPointer)gui); 
    XtAddEventHandler(gui->glxarea,ButtonReleaseMask, FALSE,	Unset_High_Lists, (XtPointer)gui);
  } 
  draw_all(gui);
  RemoveBusyCursor(gui->form);

  DEBUG_TRACE_OUT printf("Done with ClippingDialogOKCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Interactive_Clip_ToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the Interactive Clipping on or off
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Interactive_Clip_ToggleCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int i,j;
  XmString xmstr;
  XmToggleButtonCallbackStruct * cbs = (XmToggleButtonCallbackStruct *) callData;
  
  DEBUG_TRACE_IN printf("Entered Interactive_Clip_ToggleCB\n");

  if (cbs->set){ 
    
    if (gui->fast_rotation_type < 3) gui->draw_high_lists = 1;
    gui->interactive_clipping = 1;
    XtManageChild(gui->clipping_dialog.d_planes);
    
    if (gui->fast_rotation_type != 3){
      XtRemoveEventHandler(gui->glxarea,ButtonPressMask, FALSE, Set_High_Lists, (XtPointer)gui); 
      XtRemoveEventHandler(gui->glxarea,ButtonReleaseMask, FALSE, Unset_High_Lists, (XtPointer)gui); 
    }
    XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->clipping_dialog.dialog, XmDIALOG_CANCEL_BUTTON));
    
    xmstr = XmStringCreateLocalized("Done");
    XtVaSetValues((Widget)XmMessageBoxGetChild(gui->clipping_dialog.dialog,XmDIALOG_OK_BUTTON),
		  XmNlabelString, xmstr,
		  NULL);
    XmStringFree(xmstr);

    for (i=0;i<3;i++)
    {
        gui->clip[i].on       = gui->tempclip[i].on;
        gui->clip[i].f_capped = gui->tempclip[i].f_capped;
        gui->clip[i].b_capped = gui->tempclip[i].b_capped;

        for (j=0;j<4;j++)
        {
            gui->clip[i].f_eqn[j] = gui->tempclip[i].f_eqn[j];
            gui->clip[i].b_eqn[j] = gui->tempclip[i].b_eqn[j];
        }
    }
    /*if (gui->view_style == VIEW_STYLE_UNIVELS && gui->fast_rotation_type == 1) glDisable(GL_LIGHTING);*/
  }else{
    gui->draw_high_lists = 0;
    gui->interactive_clipping = 0;
    XtUnmanageChild(gui->clipping_dialog.d_planes);
    
    if (gui->fast_rotation_type != 3){
      XtAddEventHandler(gui->glxarea,ButtonPressMask, FALSE,	Set_High_Lists, (XtPointer)gui); 
      XtAddEventHandler(gui->glxarea,ButtonReleaseMask, FALSE, Unset_High_Lists, (XtPointer)gui); 
    }
    
    xmstr = XmStringCreateLocalized("Apply");
    XtVaSetValues((Widget)XmMessageBoxGetChild(gui->clipping_dialog.dialog,XmDIALOG_OK_BUTTON),
		  XmNlabelString, xmstr,
		  NULL);
    XmStringFree(xmstr);
    
    XtManageChild((Widget)XmMessageBoxGetChild(gui->clipping_dialog.dialog,
					       XmDIALOG_CANCEL_BUTTON));
    
    for (i=0;i<3;i++){
      gui->tempclip[i].on       = gui->clip[i].on;
      gui->tempclip[i].f_capped = gui->clip[i].f_capped;
      gui->tempclip[i].b_capped = gui->clip[i].b_capped;
      for (j=0;j<4;j++){
	gui->tempclip[i].f_eqn[j] = gui->clip[i].f_eqn[j];
	gui->tempclip[i].b_eqn[j] = gui->clip[i].b_eqn[j];
      }
    }
    /*if (gui->view_style == VIEW_STYLE_UNIVELS) glEnable(GL_LIGHTING);*/
  }
  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Interactive_Clip_ToggleCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Draw_Clip_Planes_ToggleCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the drawing of clipping planes
%%%           while interactive clipping is on
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Draw_Clip_Planes_ToggleCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmToggleButtonCallbackStruct * cbs = (XmToggleButtonCallbackStruct *) callData;
  
  DEBUG_TRACE_IN printf("Entered Draw_Clip_Planes_ToggleCB\n");

  if (cbs->set) gui->draw_clip_planes = 1;
  else gui->draw_clip_planes = 0;
  
  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Draw_Clip_Planes_ToggleCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Clip_Direction_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb) direction comes through clienData
%%%
%%%  Purpose: switches the controls in the clipping dialog
%%%            to a different clip direction.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Clip_Direction_ChangedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int dir;
  static int first_time = 1,old_dir;
  XmString xmstr;
  char out_string[50];
  float pos_clip, neg_clip;

  char name[16];
  char label[64];
  char pos_label[64], neg_label[64];
  int pix1 = 0, pix2 = 1;
  
  DEBUG_TRACE_IN printf("Entered Clip_Direction_ChangedCB\n");

  if      ( w == gui->clipping_dialog.ax_push )    dir = 1;
  else if ( w == gui->clipping_dialog.cor_push)    dir = 2;
  else                                             dir = 3;

  if (first_time){
    old_dir = 1; first_time = 0;
  }

  if (old_dir != dir || gui->reset_clipping){
    gui->clipping_dialog.current_dir = dir;
    if (gui->interactive_clipping){
      pos_clip = gui->clip[dir-1].f_eqn[3]; 
      neg_clip = gui->clip[dir-1].b_eqn[3]; 
    }else{ 
      pos_clip = gui->tempclip[dir-1].f_eqn[3]; 
      neg_clip = gui->tempclip[dir-1].b_eqn[3]; 
    }

    /* Determine labels for buttons */
    strcpy( name, gui->axisLabels[dir-1].name );
    
    switch( gui->axisLabels[dir-1].first )
    {
        case 'I':
        case 'S':
            strcpy( pos_label, "Above" );
            strcpy( neg_label, "Below" );
            pix1 = 0;
            pix2 = 1;
            break;
            
        case 'P':
        case 'A':
            strcpy( pos_label, "In Front" );
            strcpy( neg_label, "Behind"   );
            pix1 = 2;
            pix2 = 3;            
            break;
            
        case 'R':
        case 'L':
            strcpy( pos_label, "Right Of" );
            strcpy( neg_label, "Left Of" );
            pix1 = 4;
            pix2 = 5;            
            break;
    }

    switch (gui->clipping_dialog.current_dir){
    case 1:
      XtVaSetValues(gui->clipping_dialog.ax_push, XmNshadowType, XmSHADOW_IN,NULL);

      sprintf( label, "%s Clipping On", name );
      xmstr = XmStringCreateLocalized( label );
      XtVaSetValues(gui->clipping_dialog.clip_toggle, XmNlabelString, xmstr,NULL);
      XmStringFree(xmstr);

      xmstr = XmStringCreateLocalized( pos_label );
      XtVaSetValues(gui->clipping_dialog.num_pos, XmNlabelString, xmstr,NULL);
      XmStringFree(xmstr);
      xmstr = XmStringCreateLocalized( neg_label );
      XtVaSetValues(gui->clipping_dialog.num_neg, XmNlabelString, xmstr,NULL);
      XmStringFree(xmstr);
      
      if (gui->clip[0].on) XtVaSetValues(gui->clipping_dialog.clip_toggle,XmNset,TRUE,NULL);
      else XtVaSetValues(gui->clipping_dialog.clip_toggle,XmNset,FALSE,NULL);
      
      if (gui->clip[0].f_capped) XtVaSetValues(gui->clipping_dialog.pos_cap,XmNset,TRUE,NULL);
      else XtVaSetValues(gui->clipping_dialog.pos_cap,XmNset,FALSE,NULL);
      if (gui->clip[0].f_capped) XtVaSetValues(gui->clipping_dialog.neg_cap,XmNset,TRUE,NULL);
      else XtVaSetValues(gui->clipping_dialog.neg_cap,XmNset,FALSE,NULL);

      if (gui->cm) sprintf(out_string,"%s %s =  %.2f (cm)",
                           pos_label, name,
                           convert_world_y_to_slice_z(gui,pos_clip)/10.0);
      
      else
          sprintf(out_string,"%s %s =  %.2f (mm)",
                  pos_label, name,
                  convert_world_y_to_slice_z(gui,pos_clip));
      
      xmstr = XmStringCreateLocalized(out_string);
      XtVaSetValues(gui->clipping_dialog.pos_label, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
      
      if (gui->cm)
          sprintf(out_string,"%s %s =  %.2f (cm)",
                  neg_label, name,
                  convert_world_y_to_slice_z(gui,-neg_clip)/10.0);

      else
          sprintf(out_string,"%s %s =  %.2f (mm)",
                  neg_label, name,
                  convert_world_y_to_slice_z(gui,-neg_clip));
      
      xmstr = XmStringCreateLocalized(out_string);
      XtVaSetValues(gui->clipping_dialog.neg_label, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
      
      break;
    case 2:
      XtVaSetValues(gui->clipping_dialog.cor_push, XmNshadowType, XmSHADOW_IN,NULL);

      sprintf( label, "%s Clipping On", name );
      xmstr = XmStringCreateLocalized( label );
      XtVaSetValues(gui->clipping_dialog.clip_toggle, XmNlabelString, xmstr,NULL);
      XmStringFree(xmstr);
      
      xmstr = XmStringCreateLocalized( pos_label );
      XtVaSetValues(gui->clipping_dialog.num_pos, XmNlabelString, xmstr,NULL);
      XmStringFree(xmstr);
      xmstr = XmStringCreateLocalized( neg_label );
      XtVaSetValues(gui->clipping_dialog.num_neg, XmNlabelString, xmstr,NULL);
      XmStringFree(xmstr);
      
      if (gui->clip[1].on) XtVaSetValues(gui->clipping_dialog.clip_toggle,XmNset,TRUE,NULL);
      else XtVaSetValues(gui->clipping_dialog.clip_toggle,XmNset,FALSE,NULL);
      
      if (gui->clip[1].f_capped) XtVaSetValues(gui->clipping_dialog.pos_cap,XmNset,TRUE,NULL);
      else XtVaSetValues(gui->clipping_dialog.pos_cap,XmNset,FALSE,NULL);
      if (gui->clip[1].f_capped) XtVaSetValues(gui->clipping_dialog.neg_cap,XmNset,TRUE,NULL);
      else XtVaSetValues(gui->clipping_dialog.neg_cap,XmNset,FALSE,NULL);
      
      if (gui->cm)
          sprintf(out_string,"%s %s =  %.2f (cm)",
                  pos_label, name,
                  pos_clip/10.0);
      
      else
          sprintf(out_string,"%s %s =  %.2f (mm)",
                  pos_label, name,
                  pos_clip);
      
      xmstr = XmStringCreateLocalized(out_string);
      XtVaSetValues(gui->clipping_dialog.pos_label, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
      
      if (gui->cm)
          sprintf(out_string,"%s %s =  %.2f (cm)",
                  neg_label, name,
                  -neg_clip/10.0);
      
      else
          sprintf(out_string,"%s %s =  %.2f (mm)",
                  neg_label, name,
                  -neg_clip);
      
      xmstr = XmStringCreateLocalized(out_string);
      XtVaSetValues(gui->clipping_dialog.neg_label, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
      
      break;
    case 3:
      XtVaSetValues(gui->clipping_dialog.sag_push, XmNshadowType, XmSHADOW_IN,NULL);

      sprintf( label, "%s Clipping On", name );      
      xmstr = XmStringCreateLocalized( label );
      XtVaSetValues(gui->clipping_dialog.clip_toggle, XmNlabelString, xmstr,NULL);
      XmStringFree(xmstr);
      
      xmstr = XmStringCreateLocalized( pos_label );
      XtVaSetValues(gui->clipping_dialog.num_pos, XmNlabelString, xmstr,NULL);
      XmStringFree(xmstr);
      xmstr = XmStringCreateLocalized( neg_label );
      XtVaSetValues(gui->clipping_dialog.num_neg, XmNlabelString, xmstr,NULL);
      XmStringFree(xmstr);
      
      if (gui->clip[2].on) XtVaSetValues(gui->clipping_dialog.clip_toggle,XmNset,TRUE,NULL);
      else XtVaSetValues(gui->clipping_dialog.clip_toggle,XmNset,FALSE,NULL);
      
      if (gui->clip[2].f_capped) XtVaSetValues(gui->clipping_dialog.pos_cap,XmNset,TRUE,NULL);
      else XtVaSetValues(gui->clipping_dialog.pos_cap,XmNset,FALSE,NULL);
      if (gui->clip[2].f_capped) XtVaSetValues(gui->clipping_dialog.neg_cap,XmNset,TRUE,NULL);
      else XtVaSetValues(gui->clipping_dialog.neg_cap,XmNset,FALSE,NULL);
      
      if (gui->cm)
          sprintf(out_string,"%s %s =  %.2f (cm)",
                  pos_label, name,
                  pos_clip/10.0);
      
      else
          sprintf(out_string,"%s %s =  %.2f (mm)",
                  pos_label, name,
                  pos_clip);
      
      xmstr = XmStringCreateLocalized(out_string);
      XtVaSetValues(gui->clipping_dialog.pos_label, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
      
      if (gui->cm)
          sprintf(out_string,"%s %s =  %.2f (cm)",
                  neg_label, name,
                  -neg_clip/10.0);
      
      else
          sprintf(out_string,"%s %s =  %.2f (mm)",
                  neg_label, name,
                  -neg_clip);
      
      xmstr = XmStringCreateLocalized(out_string);
      XtVaSetValues(gui->clipping_dialog.neg_label, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);
      
      break;
    }

    XtVaSetValues(gui->clipping_dialog.neg_pic,XmNlabelPixmap,gui->clipping_dialog.pixmap[pix1],NULL);
    XtVaSetValues(gui->clipping_dialog.pos_pic,XmNlabelPixmap,gui->clipping_dialog.pixmap[pix2],NULL);

    if( !gui->reset_clipping )
    {
        XmScaleSetValue(gui->clipping_dialog.neg_slider, -(int)(neg_clip*100.0+.5));
        XmScaleSetValue(gui->clipping_dialog.pos_slider, (int)(pos_clip*100.0+.5));
    }

    
    if( !gui->reset_clipping )
    {
        switch(old_dir)
        {
            case 1:
                XtVaSetValues(gui->clipping_dialog.ax_push, XmNshadowType, XmSHADOW_OUT,NULL);break;
            case 2:
                XtVaSetValues(gui->clipping_dialog.cor_push, XmNshadowType, XmSHADOW_OUT,NULL);break;
            case 3:
                XtVaSetValues(gui->clipping_dialog.sag_push, XmNshadowType, XmSHADOW_OUT,NULL);break;
        }
    }
    else
    {
        XtVaSetValues(gui->clipping_dialog.ax_push,  XmNshadowType, XmSHADOW_IN, NULL);
        XtVaSetValues(gui->clipping_dialog.cor_push, XmNshadowType, XmSHADOW_OUT,NULL);
        XtVaSetValues(gui->clipping_dialog.sag_push, XmNshadowType, XmSHADOW_OUT,NULL);        
    }
    
    old_dir = dir;
  }

  DEBUG_TRACE_OUT printf("Done with Clip_Direction_ChangedCB\n");
}
