#include "sera3d.h"

int ibeam_endpoint = 0;
Widget x_s,y_s,z_s;


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Beam_Line_ViewCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: sets the camera to the loaded beam line
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Beam_Line_ViewCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)calldata;


  DEBUG_TRACE_IN printf("Entered Beam_Line_ViewCB\n");


  if ((gui->num_particle_paths == 0) && !gui->ibeam){
    DT_error(gui->toplevel,"You must first load a particle track file (.pp)",NULL,NULL);
    XtVaSetValues(w, XmNset, FALSE, NULL);
    DEBUG_TRACE_OUT printf("Done with Beam_Line_ViewCB\n");
    return;
  }

  DisplayBusyCursor(gui->form);
  if (cbs->set){
    gui->beam_line_view = 1;
    lock_all_rotations(gui);
    
    if (gui->ibeam){
      gui->cam.x = gui->ibeam_sx;
      gui->cam.y = gui->ibeam_sy;
      gui->cam.z = gui->ibeam_sz;
      
      gui->cam.at_x =  gui->ibeam_ex;
      gui->cam.at_y =  gui->ibeam_ey;
      gui->cam.at_z =  gui->ibeam_ez;
    }else{
      /*
      cam_pos_x = (float)particle_paths[beam_num][0];
      cam_pos_y = (float)particle_paths[beam_num][2];
      cam_pos_z = (float)particle_paths[beam_num][1];
      
      at_pos_x = (float)particle_paths[beam_num][3];
      at_pos_y = (float)particle_paths[beam_num][5];
      at_pos_z = (float)particle_paths[beam_num][4];
      */
      gui->cam.x = (float)gui->particle_path[gui->beam_num].start_x + gui->beam_position_val * gui->beam_slope_x;
      gui->cam.y = (float)gui->particle_path[gui->beam_num].start_z + gui->beam_position_val * gui->beam_slope_y;
      gui->cam.z = (float)gui->particle_path[gui->beam_num].start_y + gui->beam_position_val * gui->beam_slope_z;
      
      gui->cam.at_x =  (float)gui->particle_path[gui->beam_num].end_x + gui->beam_position_val * gui->beam_slope_x;
      gui->cam.at_y =  (float)gui->particle_path[gui->beam_num].end_z + gui->beam_position_val * gui->beam_slope_y;
      gui->cam.at_z =  (float)gui->particle_path[gui->beam_num].end_y + gui->beam_position_val * gui->beam_slope_z;
    }

    
    if ((gui->cam.x - gui->cam.at_x == 0.0) && (gui->cam.z - gui->cam.at_z == 0.0)){
      printf(" we were looking down the y axis, changing the cam up\n");
      gui->cam.up_x = 0; gui->cam.up_y = 0; gui->cam.up_z = 1;
    }
    
    gui->rotation_y = 0; gui->rotation_x = 0; gui->rotation_z = 0;
    
    glLoadIdentity ();	/*  clear the matrix	*/
    gluLookAt(gui->cam.x,        gui->cam.y,        gui->cam.z,
	      gui->cam.at_x,     gui->cam.at_y,     gui->cam.at_z,
	      gui->cam.up_x,     gui->cam.up_y,     gui->cam.up_z);  

    if (gui->apeture_on) glEnable(GL_STENCIL_TEST);


  }else{
    
    if (gui->apeture_on) glDisable(GL_STENCIL_TEST);

    unlock_all_rotations(gui);
    
    ResetButtonCB(NULL,clientdata,NULL);
  }
  draw_all(gui);
  RemoveBusyCursor(gui->form);

  DEBUG_TRACE_OUT printf("Done with Beam_Line_ViewCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Beam_Line_View_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: adjusts the beam line camera (slides up & down the 
%%%           beam line)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Beam_Line_View_ChangedCB(Widget w, XtPointer clientdata, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;

  DEBUG_TRACE_IN printf("Entered Beam_Line_View_ChangedCB\n");

  if ((gui->num_particle_paths == 0) && !gui->ibeam){
    DT_error(gui->toplevel,"You must first load a particle track file (.pp)",NULL,NULL);
    DEBUG_TRACE_OUT printf("Done with Beam_Line_View_ChangedCB\n");
    return;
  }
  
  gui->beam_position_val = (float)cbs->value;
  
  if (gui->beam_line_view){    
    if (gui->ibeam){
      gui->cam.x = gui->ibeam_sx + (float)cbs->value * gui->beam_slope_x;
      gui->cam.y = gui->ibeam_sy + (float)cbs->value * gui->beam_slope_y;
      gui->cam.z = gui->ibeam_sz + (float)cbs->value * gui->beam_slope_z;
      
      gui->cam.at_x =  gui->ibeam_ex;
      gui->cam.at_y =  gui->ibeam_ey;
      gui->cam.at_z =  gui->ibeam_ez;
    }else{
      gui->cam.x = (float)gui->particle_path[gui->beam_num].start_x + (float)cbs->value * gui->beam_slope_x;
      gui->cam.y = (float)gui->particle_path[gui->beam_num].start_z + (float)cbs->value * gui->beam_slope_y;
      gui->cam.z = (float)gui->particle_path[gui->beam_num].start_y + (float)cbs->value * gui->beam_slope_z;
      
      gui->cam.at_x =  (float)gui->particle_path[gui->beam_num].end_x + (float)cbs->value * gui->beam_slope_x;
      gui->cam.at_y =  (float)gui->particle_path[gui->beam_num].end_z + (float)cbs->value * gui->beam_slope_y;
      gui->cam.at_z =  (float)gui->particle_path[gui->beam_num].end_y + (float)cbs->value * gui->beam_slope_z;
    }
    gui->rotation_y = 0; gui->rotation_x = 0; gui->rotation_z = 0;
    
    glLoadIdentity ();	/*  clear the matrix	*/
    gluLookAt(gui->cam.x,       gui->cam.y,       gui->cam.z,
	      gui->cam.at_x,    gui->cam.at_y,    gui->cam.at_z,
	      gui->cam.up_x,    gui->cam.up_y,    gui->cam.up_z);  
    
    draw_all(gui);
  }
  DEBUG_TRACE_OUT printf("Done With Beam_Line_View_ChangedCB\n");
}
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : draw_beam_slice
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: the position of the slice
%%%
%%%  Purpose: draws a "slice" or plane through the 3d texture map
%%%           perpendicular to the beam
%%%          
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw_beam_slice(main_gui_t *gui,float x0, float y0, float z0)
{
  float a,b,c,mag_h, mag_v,tx,ty,tz;
  float h_x,h_y,h_z, v_x, v_y, v_z;
  float val = 150.0;
  int was_on = 0;
  int blend_was_on = 0;

  DEBUG_TRACE_IN printf("Entered draw_beam_slice\n");

  if (gui->alpha_culling_on) glEnable(GL_ALPHA_TEST); 
  glEnable(GL_TEXTURE_3D_EXT);  
  if (glIsEnabled(GL_LIGHTING)){
    was_on = 1; glDisable(GL_LIGHTING);
  }
  if (glIsEnabled(GL_BLEND)){
    blend_was_on = 1; glDisable(GL_BLEND);
  }

  if (gui->beam_clip) glDisable(GL_CLIP_PLANE3);
  
  a = gui->beam_slope_x; b = gui->beam_slope_y; c = gui->beam_slope_z;
  mag_h = sqrt(c*c+a*a);

  if (mag_h>0.0) {
    mag_v = sqrt((a*b)*(a*b)+(a*a+c*c)*(a*a+c*c)+(b*c)*(b*c)); 
    h_x = -c/mag_h;  h_y = 0;  h_z = a/mag_h;
    v_x = (a*b)/mag_v;  v_y = -(a*a+c*c)/mag_v;  v_z = (b*c)/mag_v;
  } else { /* looking straight down y -- or thereabouts */
    mag_h = 1.0;
    mag_v = 1.0;
    h_x = 1.0;  h_y = 0.0;  h_z = 0.0;
    v_x = 0.0;  v_y = 0.0;  v_z = 1.0;
  }
  
  glColor3f(1,1,1);
  glBegin(GL_QUADS);
  
   convert_world_coord_to_texture_coord(gui, x0+h_x*val+v_x*val, y0+h_y*val+v_y*val, z0+h_z*val+v_z*val, &tx, &ty, &tz);				       
   glTexCoord3f(tx,ty,tz);
   glVertex3f(x0+h_x*val+v_x*val, y0+h_y*val+v_y*val, z0+h_z*val+v_z*val);

   convert_world_coord_to_texture_coord(gui, x0+h_x*val-v_x*val, y0+h_y*val-v_y*val, z0+h_z*val-v_z*val,  &tx, &ty, &tz);				       
   glTexCoord3f(tx,ty,tz);
   glVertex3f(x0+h_x*val-v_x*val, y0+h_y*val-v_y*val, z0+h_z*val-v_z*val);

   convert_world_coord_to_texture_coord(gui, x0-h_x*val-v_x*val, y0-h_y*val-v_y*val, z0-h_z*val-v_z*val, &tx, &ty, &tz);				       
   glTexCoord3f(tx,ty,tz);
   glVertex3f(x0-h_x*val-v_x*val, y0-h_y*val-v_y*val, z0-h_z*val-v_z*val);

   convert_world_coord_to_texture_coord(gui, x0-h_x*val+v_x*val, y0-h_y*val+v_y*val, z0-h_z*val+v_z*val, &tx, &ty, &tz);				       
   glTexCoord3f(tx,ty,tz);
   glVertex3f(x0-h_x*val+v_x*val, y0-h_y*val+v_y*val, z0-h_z*val+v_z*val);
  glEnd();

  glDisable(GL_ALPHA_TEST);
  glDisable(GL_TEXTURE_3D_EXT);
  if (was_on) glEnable(GL_LIGHTING);
  if (blend_was_on) glEnable(GL_BLEND);
  if (gui->beam_clip) glEnable(GL_CLIP_PLANE3);

  DEBUG_TRACE_OUT printf("Done with draw_beam_slice\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Beam_Slice_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: the position of the slice
%%%
%%%  Purpose: draws a "slice" or plane through the 3d texture map
%%%           perpendicluar to the beam at the slider location.
%%%          
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Beam_Slice_ChangedCB(Widget w, XtPointer clientdata, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  float slope_x,slope_y,slope_z;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;

  DEBUG_TRACE_IN printf("Entered Beam_Slice_ChangedCB\n");

  if ((gui->num_particle_paths == 0) && !gui->ibeam){
    DT_error(gui->toplevel,"You must first load a particle track file (.pp)",NULL,NULL);
    DEBUG_TRACE_OUT printf("Done with Beam_Line_View_ChangedCB\n");
    return;
  }

  gui->beam_slice_position_val = (float)cbs->value;

  if (gui->ibeam){
    gui->beam_slice_x = gui->ibeam_sx + (float)cbs->value * gui->beam_slope_x;
    gui->beam_slice_y = gui->ibeam_sy + (float)cbs->value * gui->beam_slope_y;
    gui->beam_slice_z = gui->ibeam_sz + (float)cbs->value * gui->beam_slope_z;
  }else{
    gui->beam_slice_x = (float)gui->particle_path[gui->beam_num].start_x + (float)cbs->value * gui->beam_slope_x;
    gui->beam_slice_y = (float)gui->particle_path[gui->beam_num].start_z + (float)cbs->value * gui->beam_slope_y;
    gui->beam_slice_z = (float)gui->particle_path[gui->beam_num].start_y + (float)cbs->value * gui->beam_slope_z;
  }
  gui->beam_eqn[3] = -(gui->beam_slope_x*gui->beam_slice_x +
		       gui->beam_slope_y*gui->beam_slice_y +
		       gui->beam_slope_z*gui->beam_slice_z);

  if (gui->beam_slice || gui->beam_clip)
    draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Beam_Slice_ChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Beam_Slice_ToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the beam_slice on & off
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Beam_Slice_ToggledCB (Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;
  
  DEBUG_TRACE_IN printf("Entered Beam_Slice_ToggledCB\n");

  if (cbs ->set) gui->beam_slice = 1;
  else gui->beam_slice = 0;

  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Beam_Slice_ToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Beam_Clip_ToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the clipping with the beam slice
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Beam_Clip_ToggledCB (Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int was_enabled=0;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;
  
  DEBUG_TRACE_IN printf("Entered Beam_Clip_ToggledCB\n");

  if (cbs ->set){ 
    gui->beam_clip = 1;
    
    gui->beam_eqn[0] = gui->beam_slope_x;
    gui->beam_eqn[1] = gui->beam_slope_y;
    gui->beam_eqn[2] = gui->beam_slope_z;
    
    gui->beam_eqn[3] = -(gui->beam_slope_x*gui->beam_slice_x +
			 gui->beam_slope_y*gui->beam_slice_y +
			 gui->beam_slope_z*gui->beam_slice_z);
  }else{
    gui->beam_clip = 0;
    glDisable(GL_CLIP_PLANE3);       
  }
  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Beam_Clip_ToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Which_Beam_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles between the file beam and interactive beam
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Which_Beam_ChangedCB (Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  
  DEBUG_TRACE_IN printf("Entered Which_Beam_ChangedCB\n");

 
  if (w == gui->beam_panel.wb[0]){
    if (gui->ibeam != 0) gui->ibeam = 0;

  if ((gui->num_particle_paths == 0)) return;

    calculate_beam_slope(gui,gui->particle_path[gui->beam_num].start_x,
			 gui->particle_path[gui->beam_num].start_z,
			 gui->particle_path[gui->beam_num].start_y,
			 gui->particle_path[gui->beam_num].end_x,
			 gui->particle_path[gui->beam_num].end_z,
			 gui->particle_path[gui->beam_num].end_y);
  }else{
    if (gui->ibeam != 1) gui->ibeam = 1;
    calculate_beam_slope(gui,gui->ibeam_sx,gui->ibeam_sy,gui->ibeam_sz,
			 gui->ibeam_ex,gui->ibeam_ey,gui->ibeam_ez);
  }
  gui->beam_eqn[0] = gui->beam_slope_x;
  gui->beam_eqn[1] = gui->beam_slope_y;
  gui->beam_eqn[2] = gui->beam_slope_z;
  gui->beam_eqn[3] = -(gui->beam_slope_x*gui->beam_slice_x +
		       gui->beam_slope_y*gui->beam_slice_y +
		       gui->beam_slope_z*gui->beam_slice_z);
  glClipPlane(GL_CLIP_PLANE3, gui->beam_eqn);

  /*printf("ibeam is now : %d\n",ibeam);*/
  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Which_Beam_ChangedCB\n");
}
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : calculate beam slope
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: the position of the slice
%%%
%%%  Purpose: calculate beam slope
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void calculate_beam_slope(main_gui_t *gui,float x1, float y1, float z1,
			  float x2, float y2, float z2)
{
  DEBUG_TRACE_IN printf("Entered calculate_beam_slope\n");

  gui->beam_slope_x = x2-x1; 
  gui->beam_slope_y = y2-y1; 
  gui->beam_slope_z = z2-z1;
  normalize(&gui->beam_slope_x,&gui->beam_slope_y,&gui->beam_slope_z); 

  DEBUG_TRACE_OUT printf("Done with calculate_beam_slope\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_ibeam_controls_dialog
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent widget
%%%
%%%  Purpose: builds the ibeam control window
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
Widget build_ibeam_controls_dialog(main_gui_t *gui)
{
  int i;
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered build_ibeam_controls_dialog\n");

  /*printf("building the ibeam window\n");*/
  gui->ibeam_dialog.dialog = (Widget)XmCreateMessageDialog(gui->beam_panel.form, "I-Beam Win",NULL,0);

  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->ibeam_dialog.dialog,XmDIALOG_SYMBOL_LABEL));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->ibeam_dialog.dialog,XmDIALOG_MESSAGE_LABEL));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->ibeam_dialog.dialog,XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->ibeam_dialog.dialog,XmDIALOG_HELP_BUTTON));
  
  xmstr = XmStringCreateLocalized("DONE");
  XtVaSetValues((Widget)XmMessageBoxGetChild(gui->ibeam_dialog.dialog,XmDIALOG_OK_BUTTON),
		XmNlabelString, xmstr, NULL);
  
  /*XtAddCallback(dialog, XmNokCallback,SliceWinDialogOKCB,NULL);*/
 
  gui->ibeam_dialog.main_form = 
    XtVaCreateManagedWidget ("main_form", 
			     xmFormWidgetClass, 
			     gui->ibeam_dialog.dialog, 
			     XmNrightAttachment, XmATTACH_FORM,
			     XmNleftAttachment, XmATTACH_FORM,
			     NULL);
  
  gui->ibeam_dialog.endpt_label = 
    XtVaCreateManagedWidget("Endpoint", xmLabelWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNleftAttachment,XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNbottomAttachment, XmATTACH_NONE,
			    XmNtopAttachment, XmATTACH_FORM,
			    XmNtopOffset, 3,
			    XmNleftOffset, 5,
			    NULL);

  gui->ibeam_dialog.endpt_pane = (Widget)XmCreatePulldownMenu(gui->ibeam_dialog.main_form,"endpt_pane",NULL,0);
  gui->ibeam_dialog.endpt_menu = 
    (Widget)XtVaCreateManagedWidget("Order",xmRowColumnWidgetClass,gui->ibeam_dialog.main_form,
                                    XmNmarginHeight,       0,
                                    XmNmarginWidth,        0,
                                    XmNpacking,            XmPACK_TIGHT,
                                    XmNpopupEnabled,       TRUE,
                                    XmNrowColumnType,      XmMENU_OPTION,
                                    XmNspacing,            0,
                                    XmNsubMenuId, gui->ibeam_dialog.endpt_pane,
		                    XmNleftAttachment, XmATTACH_WIDGET,
		                    XmNleftWidget, gui->ibeam_dialog.endpt_label,
		                    XmNrightAttachment, XmATTACH_FORM,
		                    XmNbottomAttachment, XmATTACH_NONE,
		                    XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		                    XmNtopWidget, gui->ibeam_dialog.endpt_label,
		                    XmNtopOffset, 0,
		                    XmNleftOffset, 5,
		                    XmNrightOffset, 5,
		                    NULL);
  
  
  gui->ibeam_endpoint = 0;
  gui->ibeam_dialog.endpt[0] = 
    (Widget)XtCreateManagedWidget("Starting Point",
				  xmPushButtonWidgetClass,
				  gui->ibeam_dialog.endpt_pane, NULL, 0);

  gui->ibeam_dialog.endpt[1] = 
    (Widget)XtCreateManagedWidget("End Point",xmPushButtonWidgetClass,
				  gui->ibeam_dialog.endpt_pane, NULL, 0);		
  
  XtAddCallback(gui->ibeam_dialog.endpt[0],XmNactivateCallback,	
		IBeam_Endpt_ChangedCB, (XtPointer)gui);
  XtAddCallback(gui->ibeam_dialog.endpt[1],XmNactivateCallback, 
		IBeam_Endpt_ChangedCB, (XtPointer)gui);

  xmstr = XmStringCreateLocalized("RL Position");
  gui->ibeam_dialog.x_s = 
    (Widget)XtVaCreateManagedWidget("RL Position",xmScaleWidgetClass,
				    gui->ibeam_dialog.main_form,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNleftOffset, 10,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, gui->ibeam_dialog.endpt_menu,
				    XmNtopOffset, 5,
				    XmNvalue, 0,
				    XmNminimum, -200,
				    XmNmaximum, 200,
				    XmNscaleWidth, 200,
				    XmNscaleHeight, 15,
				    XmNorientation, XmHORIZONTAL,
				    XmNshowValue, TRUE,
				    XmNtitleString, xmstr,
				    NULL);
  XmStringFree(xmstr);
  
  xmstr = XmStringCreateLocalized("PA Position");
  gui->ibeam_dialog.y_s = 
    (Widget)XtVaCreateManagedWidget("PA Position",xmScaleWidgetClass,
				    gui->ibeam_dialog.main_form,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNleftOffset, 10,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, gui->ibeam_dialog.x_s,
				    XmNtopOffset, 5,
				    XmNvalue, 0,
				    XmNminimum, -200,
				    XmNmaximum, 200,
				    XmNscaleWidth, 200,
				    XmNscaleHeight, 15,
				    XmNorientation, XmHORIZONTAL,
				    XmNshowValue, TRUE,
				    XmNtitleString, xmstr,
				    NULL);
  XmStringFree(xmstr);
  xmstr = XmStringCreateLocalized("IS Position");
  gui->ibeam_dialog.z_s = 
    (Widget)XtVaCreateManagedWidget("IS Position",xmScaleWidgetClass,
				    gui->ibeam_dialog.main_form,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_FORM,
				    XmNleftOffset, 10,
				    XmNtopAttachment, XmATTACH_WIDGET,
				    XmNtopWidget, gui->ibeam_dialog.y_s,
				    XmNtopOffset, 20,
				    XmNvalue, 0,
				    XmNminimum, -200,
				    XmNmaximum, 200,
				    XmNscaleWidth, 200,
				    XmNscaleHeight, 15,
				    XmNorientation, XmHORIZONTAL,
				    XmNshowValue, TRUE,
				    XmNtitleString, xmstr,
				    NULL);
  XmStringFree(xmstr);

  /*xmstr = XmStringCreateLtoR("SeraCalc Position\nT\nZb\nPhi Theta",XmFONTLIST_DEFAULT_TAG);
  
  gui->ibeam_dialog.rttmc_data = 
    XtVaCreateManagedWidget("rttmc_data",xmLabelWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNleftOffset, 10,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->ibeam_dialog.z_s,
			    XmNlabelString, xmstr,
			    NULL);*/

  xmstr = XmStringCreateLtoR("P-A",XmFONTLIST_DEFAULT_TAG);

  gui->ibeam_dialog.pa_l = 
    XtVaCreateManagedWidget("pa_l",xmLabelWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->ibeam_dialog.z_s,
                            XmNtopOffset, 15,
			    XmNlabelString, xmstr,
			    NULL);

  XmStringFree(xmstr);

  gui->ibeam_dialog.pa_t = 
    XtVaCreateManagedWidget("pa_t",xmTextFieldWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, gui->ibeam_dialog.pa_l,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->ibeam_dialog.z_s,
                            XmNtopOffset, 15,
			    XmNwidth, 70,
			    NULL);

  xmstr = XmStringCreateLtoR("R-L",XmFONTLIST_DEFAULT_TAG);

  gui->ibeam_dialog.rl_l = 
    XtVaCreateManagedWidget("rl_l",xmLabelWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, gui->ibeam_dialog.pa_t,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->ibeam_dialog.z_s,
                            XmNtopOffset, 15,
			    XmNlabelString, xmstr,
			    NULL);

  XmStringFree(xmstr);

  gui->ibeam_dialog.rl_t = 
    XtVaCreateManagedWidget("rl_t",xmTextFieldWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, gui->ibeam_dialog.rl_l,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->ibeam_dialog.z_s,
                            XmNtopOffset, 15,
			    XmNwidth, 70,
			    NULL);


  xmstr = XmStringCreateLtoR("I-S  ",XmFONTLIST_DEFAULT_TAG);

  gui->ibeam_dialog.is_l = 
    XtVaCreateManagedWidget("is_l",xmLabelWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, gui->ibeam_dialog.rl_t,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->ibeam_dialog.z_s,
                            XmNtopOffset, 15,
			    XmNlabelString, xmstr,
			    NULL);

  XmStringFree(xmstr);

  gui->ibeam_dialog.is_t = 
    XtVaCreateManagedWidget("is_t",xmTextFieldWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, gui->ibeam_dialog.is_l,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->ibeam_dialog.z_s,
                            XmNtopOffset, 15,
			    XmNwidth, 70,
			    NULL);

  xmstr = XmStringCreateLtoR("Zb",XmFONTLIST_DEFAULT_TAG);

  gui->ibeam_dialog.zb_l = 
    XtVaCreateManagedWidget("zb_l",xmLabelWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNleftAttachment, XmATTACH_FORM,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->ibeam_dialog.pa_t,
			    XmNlabelString, xmstr,
			    NULL);

  XmStringFree(xmstr);

  gui->ibeam_dialog.zb_t = 
    XtVaCreateManagedWidget("zb_t",xmTextFieldWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			    XmNleftWidget, gui->ibeam_dialog.pa_t,
                            XmNleftOffset, 0,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->ibeam_dialog.pa_t,
			    XmNwidth, 70,
			    NULL);

  xmstr = XmStringCreateLtoR("phi",XmFONTLIST_DEFAULT_TAG);

  gui->ibeam_dialog.phi_l = 
    XtVaCreateManagedWidget("phi_l",xmLabelWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, gui->ibeam_dialog.zb_t,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->ibeam_dialog.pa_t,
			    XmNlabelString, xmstr,
			    NULL);

  XmStringFree(xmstr);

  gui->ibeam_dialog.phi_t = 
    XtVaCreateManagedWidget("phi_t",xmTextFieldWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, gui->ibeam_dialog.phi_l,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->ibeam_dialog.pa_t,
			    XmNwidth, 70,
			    NULL);

  xmstr = XmStringCreateLtoR("Theta",XmFONTLIST_DEFAULT_TAG);

  gui->ibeam_dialog.theta_l = 
    XtVaCreateManagedWidget("theta_l",xmLabelWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, gui->ibeam_dialog.phi_t,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->ibeam_dialog.pa_t,
			    XmNlabelString, xmstr,
			    NULL);

  XmStringFree(xmstr);

  gui->ibeam_dialog.theta_t = 
    XtVaCreateManagedWidget("theta_t",xmTextFieldWidgetClass,
			    gui->ibeam_dialog.main_form,
			    XmNleftAttachment, XmATTACH_WIDGET,
			    XmNleftWidget, gui->ibeam_dialog.theta_l,
			    XmNrightAttachment, XmATTACH_NONE,
			    XmNtopAttachment, XmATTACH_WIDGET,
			    XmNtopWidget, gui->ibeam_dialog.pa_t,
			    XmNwidth, 70,
			    NULL);


  XtAddCallback(gui->ibeam_dialog.pa_t,XmNlosingFocusCallback,
		IBeam_Text_ActivatedCB, (XtPointer)gui);
  XtAddCallback(gui->ibeam_dialog.rl_t,XmNlosingFocusCallback,
		IBeam_Text_ActivatedCB, (XtPointer)gui);
  XtAddCallback(gui->ibeam_dialog.is_t,XmNlosingFocusCallback,
		IBeam_Text_ActivatedCB, (XtPointer)gui);
  XtAddCallback(gui->ibeam_dialog.zb_t,XmNlosingFocusCallback,
		IBeam_Text_ActivatedCB, (XtPointer)gui);
  XtAddCallback(gui->ibeam_dialog.phi_t,XmNlosingFocusCallback,
		IBeam_Text_ActivatedCB, (XtPointer)gui);
  XtAddCallback(gui->ibeam_dialog.theta_t,XmNlosingFocusCallback,
		IBeam_Text_ActivatedCB, (XtPointer)gui);
		


  XtAddCallback(gui->ibeam_dialog.x_s, XmNdragCallback, 
		IBeam_Position_ChangedCB, (XtPointer)gui);
  XtAddCallback(gui->ibeam_dialog.x_s, XmNvalueChangedCallback, 
		IBeam_Position_ChangedCB, (XtPointer)gui);
  XtAddCallback(gui->ibeam_dialog.y_s, XmNdragCallback, 
		IBeam_Position_ChangedCB, (XtPointer)gui);
  XtAddCallback(gui->ibeam_dialog.y_s, XmNvalueChangedCallback, 
		IBeam_Position_ChangedCB, (XtPointer)gui);
  XtAddCallback(gui->ibeam_dialog.z_s, XmNdragCallback, 
		IBeam_Position_ChangedCB, (XtPointer)gui);
  XtAddCallback(gui->ibeam_dialog.z_s, XmNvalueChangedCallback, 
		IBeam_Position_ChangedCB, (XtPointer)gui);

  DEBUG_TRACE_OUT printf("Done with build_ibeam_controls_dialog\n");    
  return ( gui->ibeam_dialog.dialog );
}

/* Procedure: set_ibeam_sliders 

   Parameters: gui pointer

   Purpose: Updates the slider values to reflect current conditions.

*/

void set_ibeam_sliders(main_gui_t * gui){

  if (gui->ibeam_endpoint ==  0){
    XtVaSetValues(gui->ibeam_dialog.x_s,XmNvalue,(int)gui->ibeam_sx,NULL);
    XtVaSetValues(gui->ibeam_dialog.z_s,XmNvalue,(int)gui->ibeam_sy,NULL);
    XtVaSetValues(gui->ibeam_dialog.y_s,XmNvalue,(int)gui->ibeam_sz,NULL);
  }else{
    XtVaSetValues(gui->ibeam_dialog.x_s,XmNvalue,(int)gui->ibeam_ex,NULL);
    XtVaSetValues(gui->ibeam_dialog.z_s,XmNvalue,(int)gui->ibeam_ey,NULL);
    XtVaSetValues(gui->ibeam_dialog.y_s,XmNvalue,(int)gui->ibeam_ez,NULL);
  }

}

/* updates the information that needs to be updated when sliders 
   or text boxes change the data for the ibeam. */
void set_new_slider_values(main_gui_t * gui)
{
  calculate_beam_slope(gui,gui->ibeam_sx,gui->ibeam_sy,gui->ibeam_sz,
		       gui->ibeam_ex,gui->ibeam_ey,gui->ibeam_ez);
  
  
  gui->beam_slice_x = gui->ibeam_sx + 
    gui->beam_slice_position_val * gui->beam_slope_x;
  gui->beam_slice_y = gui->ibeam_sy + 
    gui->beam_slice_position_val * gui->beam_slope_y;
  gui->beam_slice_z = gui->ibeam_sz + 
    gui->beam_slice_position_val * gui->beam_slope_z;
  gui->beam_eqn[3] = -(gui->beam_slope_x * gui->beam_slice_x +
		       gui->beam_slope_y * gui->beam_slice_y +
		       gui->beam_slope_z * gui->beam_slice_z);
  
  
  if (gui->beam_line_view){
    
    gui->cam.x = gui->ibeam_sx + gui->beam_position_val * gui->beam_slope_x;
    gui->cam.y = gui->ibeam_sy + gui->beam_position_val * gui->beam_slope_y;
    gui->cam.z = gui->ibeam_sz + gui->beam_position_val * gui->beam_slope_z;
    
    
    gui->cam.at_x =  gui->ibeam_ex;
    gui->cam.at_y =  gui->ibeam_ey;
    gui->cam.at_z =  gui->ibeam_ez;
    
    glLoadIdentity (); 
    gluLookAt(gui->cam.x,      gui->cam.y,     gui->cam.z,
	      gui->cam.at_x,   gui->cam.at_y,  gui->cam.at_z,
	      gui->cam.up_x,   gui->cam.up_y,  gui->cam.up_z);  
  }
  
  
  /* printf("updating the beam equation\n");*/
  gui->beam_eqn[0] = gui->beam_slope_x;
  gui->beam_eqn[1] = gui->beam_slope_y;
  gui->beam_eqn[2] = gui->beam_slope_z;
  gui->beam_eqn[3] = -(gui->beam_slope_x * gui->beam_slice_x +
		       gui->beam_slope_y * gui->beam_slice_y +
		       gui->beam_slope_z * gui->beam_slice_z);
  glClipPlane(GL_CLIP_PLANE3, gui->beam_eqn);
  
  draw_all(gui);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure :
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void IBeam_Endpt_ChangedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  DEBUG_TRACE_IN printf("Entered IBeam_Endpt_ChangedCB\n");

  if (w == gui->ibeam_dialog.endpt[0]) gui->ibeam_endpoint = 0;
  else gui->ibeam_endpoint = 1;

  set_ibeam_sliders(gui);

  DEBUG_TRACE_OUT printf("Done with IBeam_Endpt_ChangedCB\n");
}

/* Called when the textboxes in the ibeam dialog lose focus.  Updates
   the data from the textbox. */
void IBeam_Text_ActivatedCB(Widget w, XtPointer clientData, XtPointer callData){
  main_gui_t *gui = (main_gui_t *)clientData;
  char * text = XmTextFieldGetString(w);
  double value = atof(text);
  double xp, yp, zp, zb, phi, theta;
  double sx = gui->ibeam_sx, sy = gui->ibeam_sz, sz = gui->ibeam_sy;
  double ex = gui->ibeam_ex, ey = gui->ibeam_ez, ez = gui->ibeam_ey;

  XtFree(text);

  zb = sqrt((sx-ex)*(sx-ex)+(sy-ey)*(sy-ey)+(sz-ez)*(sz-ez))/10;
  if(fabs(zb) < .00001)
    phi = 0;
  else
    phi = acos((sz-ez)/(zb*10));
  theta = -atan2(ey-sy,ex-sx);
  xp = ey/10;
  yp = ex/10;
  zp = ez/10;
 
  if(w == gui->ibeam_dialog.pa_t){
    xp = value;
  } else if(w == gui->ibeam_dialog.rl_t){
    yp = value;
  } else if(w == gui->ibeam_dialog.is_t){
    zp = value;
  } else if(w == gui->ibeam_dialog.zb_t){
    zb = value;
  } else if(w == gui->ibeam_dialog.phi_t){
    phi = value*M_PI/180;
  } else if(w == gui->ibeam_dialog.theta_t){
    theta = value*M_PI/180;
  }

  gui->ibeam_sx = (yp-zb*sin(phi)*cos(-theta))*10;
  gui->ibeam_sy = (zp-zb*-cos(phi))*10;
  gui->ibeam_sz = (xp-zb*sin(phi)*sin(-theta))*10;
  
  gui->ibeam_ex = yp*10;
  gui->ibeam_ey = zp*10;
  gui->ibeam_ez = xp*10;

  set_ibeam_sliders(gui);

  set_new_slider_values(gui);


}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure :
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void IBeam_Position_ChangedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int dir;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;
  char label_s[40];
  XmString xmstr;
  float temp;
  /*dir = (int)clientData;*/

  DEBUG_TRACE_IN printf("Entered IBeam_Position_ChangedCB\n");

  if (w == gui->ibeam_dialog.x_s) dir = 0;
  else if (w == gui->ibeam_dialog.y_s) dir = 1;
  else dir = 2;

  if (gui->cm)temp = 10.0;
  else temp = 1.0;

  if (gui->ibeam){
    
    if (dir == 0 /*strstr(XtName(w),"R")*/){
      if (gui->ibeam_endpoint == 0){ 
	gui->ibeam_sx = (float)cbs->value;
      }else{
	gui->ibeam_ex = (float)cbs->value;
      }
      sprintf(label_s,"%c-%c Position : %.2f",
              gui->axisLabels[COLUMN_AXIS].first,
              gui->axisLabels[COLUMN_AXIS].last,
              (float)cbs->value/temp);
    }else if (dir == 2 /*strstr(XtName(w),"I")*/){
      if (gui->ibeam_endpoint == 0) gui->ibeam_sy = (float)cbs->value;
      else gui->ibeam_ey = (float)cbs->value;
      sprintf(label_s,"%c-%c Position : %.2f",
              gui->axisLabels[SLICE_AXIS].first,
              gui->axisLabels[SLICE_AXIS].last,
              convert_world_y_to_slice_z(gui,(float)cbs->value)/temp);
    }else{
      if (gui->ibeam_endpoint == 0) gui->ibeam_sz = (float)cbs->value;
      else gui->ibeam_ez = (float)cbs->value;
      sprintf(label_s,"%c-%c Position : %.2f",
              gui->axisLabels[ROW_AXIS].last,
              gui->axisLabels[ROW_AXIS].first,
              (float)cbs->value/temp);
    }

    xmstr = XmStringCreateLocalized(label_s);
    XtVaSetValues(w,XmNtitleString,xmstr,NULL);
    XmStringFree(xmstr);


    {
      double xp, yp, zp, zb, phi, theta;
      double sx = gui->ibeam_sx, sy = gui->ibeam_sz, sz = gui->ibeam_sy;
      double ex = gui->ibeam_ex, ey = gui->ibeam_ez, ez = gui->ibeam_ey;
      char text[256];
      XmString xmstr;
      zb = sqrt((sx-ex)*(sx-ex)+(sy-ey)*(sy-ey)+(sz-ez)*(sz-ez))/10;
      phi = acos((sz-ez)/(zb*10))*180/M_PI;
      theta = -atan2(ey-sy,ex-sx)*180/M_PI;
      xp = ey/10;
      yp = ex/10;
      zp = ez/10;
      /*sprintf(text,
	"SeraCalc Data:\nT %f,%f,%f \nZb %f \nphi %f  theta %f \n",
	xp,yp,zp,zb,phi,theta);*/
      /*printf("%s",text);*/
      /*xmstr = XmStringCreateLtoR(text, XmFONTLIST_DEFAULT_TAG);*/
      sprintf(text,"%-.2f",xp);
      XtVaSetValues(gui->ibeam_dialog.pa_t,
		    XmNvalue, text, NULL);
      sprintf(text,"%-.2f",yp);
      XtVaSetValues(gui->ibeam_dialog.rl_t,
		    XmNvalue, text, NULL);
      sprintf(text,"%-.2f",zp);
      XtVaSetValues(gui->ibeam_dialog.is_t,
		    XmNvalue, text, NULL);

      sprintf(text,"%-.2f",zb);
      XtVaSetValues(gui->ibeam_dialog.zb_t,
		    XmNvalue, text, NULL);
      sprintf(text,"%-.2f",phi);
      XtVaSetValues(gui->ibeam_dialog.phi_t,
		    XmNvalue, text, NULL);
      sprintf(text,"%-.2f",theta);
      XtVaSetValues(gui->ibeam_dialog.theta_t,
		    XmNvalue, text, NULL);


      /*XtVaSetValues(gui->ibeam_dialog.rttmc_data,
	XmNlabelString, xmstr, NULL);*/
		    /*XmStringFree(xmstr);*/

    }

    set_new_slider_values(gui);
 
  }

  DEBUG_TRACE_OUT printf("Done with IBeam_Position_ChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Show_IBeam_Controls_DialogCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: brings up the I-Beam controls window
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Show_IBeam_Controls_DialogCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  static int first_time = 1;
  static Widget ibeam_win_dialog;
  Pixel pixel;

  DEBUG_TRACE_IN printf("Entered Show_IBeam_Controls_DialogCB\n");

  if(first_time) {
    build_ibeam_controls_dialog(gui);
    first_time = 0;
  }

  changeLabels( gui, BEAM_PANEL );
  
  if (!XtIsManaged(gui->ibeam_dialog.dialog)){
    XtManageChild(gui->ibeam_dialog.dialog);
  }else{
    XtUnmanageChild(gui->ibeam_dialog.dialog);
  }

  draw_all(gui);
  DEBUG_TRACE_OUT printf("Done with Show_IBeam_Controls_DialogCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Show_Apeture_Controls_DialogCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: brings up the I-Beam controls window
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Show_Apeture_Controls_DialogCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  static int first_time = 1;
  static Widget apeture_dialog;
  Pixel pixel;
  /*printf("Entered Show_Apeture_Controls_DialogCB\n");*/

  DEBUG_TRACE_IN printf("Entered Show_Apeture_Controls_DialogCB\n");

  if(first_time) {
    build_aperture_controls_dialog(gui);
    first_time = 0;
  }

  if (!XtIsManaged(gui->aperture_dialog.dialog)){
    XtManageChild(gui->aperture_dialog.dialog);
  }else{
    XtUnmanageChild(gui->aperture_dialog.dialog);
  }

  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Show_Apeture_Controls_DialogCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_apeture_controls_dialog
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent widget
%%%
%%%  Purpose: builds the apeture control window
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_aperture_controls_dialog(main_gui_t *gui)
{
  int i;

  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered build_apeture_controls_dialog\n");

  /*printf("building the ibeam window\n");*/
  gui->aperture_dialog.dialog = (Widget)XmCreateMessageDialog(gui->beam_panel.form, "Aperture Win",NULL,0);

  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->aperture_dialog.dialog,XmDIALOG_SYMBOL_LABEL));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->aperture_dialog.dialog,XmDIALOG_MESSAGE_LABEL));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->aperture_dialog.dialog,XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->aperture_dialog.dialog,XmDIALOG_HELP_BUTTON));
  
  xmstr = XmStringCreateLocalized("DONE");
  XtVaSetValues((Widget)XmMessageBoxGetChild(gui->aperture_dialog.dialog,XmDIALOG_OK_BUTTON),
		XmNlabelString, xmstr, NULL);
  XmStringFree(xmstr);
  /*XtAddCallback(dialog, XmNokCallback,SliceWinDialogOKCB,NULL);*/
 
  gui->aperture_dialog.main_form = XtVaCreateManagedWidget ("main_form", 
				       xmFormWidgetClass, gui->aperture_dialog.dialog, 
				       NULL);

  gui->aperture_dialog.a_slider = XtVaCreateManagedWidget("Viewing Aperture",
				     xmScaleWidgetClass, gui->aperture_dialog.main_form,
				     XmNrightAttachment, XmATTACH_NONE,
				     XmNleftAttachment, XmATTACH_FORM,
				     XmNleftOffset, 10,
				     XmNtopAttachment, XmATTACH_FORM,
				     XmNtopOffset, 10,
				     XmNvalue,gui->apeture_val,
				     XmNminimum, 1,
				     XmNmaximum, 400,
				     XmNscaleWidth, 300,
				     XmNscaleHeight, 15,
				     XmNshowValue, TRUE,
				     XmNorientation, XmHORIZONTAL,
				     NULL);

  gui->aperture_dialog.r_a_slider = XtVaCreateManagedWidget("Ring Aperture",
				     xmScaleWidgetClass, gui->aperture_dialog.main_form,
				     XmNrightAttachment, XmATTACH_NONE,
				     XmNleftAttachment, XmATTACH_FORM,
				     XmNleftOffset, 10,
				     XmNtopAttachment, XmATTACH_WIDGET,
				     XmNtopWidget, gui->aperture_dialog.a_slider,
				     XmNtopOffset, 10,
				     XmNvalue,gui->ring_apeture_val,
				     XmNminimum, 1,
				     XmNmaximum, 200,
				     XmNscaleWidth, 300,
				     XmNscaleHeight, 15,
				     XmNshowValue, TRUE,
				     XmNorientation, XmHORIZONTAL,
				     NULL);

  XtAddCallback(gui->aperture_dialog.a_slider, XmNvalueChangedCallback, Apeture_ChangedCB,(XtPointer)gui);
  XtAddCallback(gui->aperture_dialog.r_a_slider, XmNvalueChangedCallback, 
		Ring_Apeture_ChangedCB,(XtPointer)gui);

  if (gui->cm){
    xmstr = XmStringCreateLocalized("View Aperture Diameter (cm)");
    XtVaSetValues(gui->aperture_dialog.a_slider,XmNdecimalPoints, 1, NULL);
    XmStringFree(xmstr);
    xmstr = XmStringCreateLocalized("Ring Aperture Diameter (cm)");
    XtVaSetValues(gui->aperture_dialog.r_a_slider,XmNdecimalPoints, 1, NULL);
    XmStringFree(xmstr);
  }else{ 
    xmstr = XmStringCreateLocalized("View Aperture Diameter (mm)");
    XtVaSetValues(gui->aperture_dialog.a_slider,XmNtitleString,xmstr,NULL);
    XmStringFree(xmstr);  
    xmstr = XmStringCreateLocalized("Ring Aperture Diameter (mm)");
    XtVaSetValues(gui->aperture_dialog.r_a_slider,XmNtitleString,xmstr,NULL);
    XmStringFree(xmstr);
  }

  DEBUG_TRACE_OUT printf("Done with build_apeture_controls_dialog\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Ring Apeture Changed Callback
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent widget
%%%
%%%  Purpose: changes the ring  apeture
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Ring_Apeture_ChangedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int val;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;  

  DEBUG_TRACE_IN printf("Entered Ring_Apeture_ChangedCB\n");

  gui->ring_apeture_val = cbs->value;
  /*printf("set the ring_apeture to : %d\n",ring_apeture_val);*/

  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Ring_Apeture_ChangedCB\n");
}
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Apeture Changed Callback
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent widget
%%%
%%%  Purpose: changes the  apeture
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Apeture_ChangedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int val;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;  

  DEBUG_TRACE_IN printf("Entered Apeture_ChangedCB\n");

  if (gui->beam_line_view){
    val = cbs->value;
    
    gui->apeture_val = val;
    
    /*printf("setting the apeture to : %d\n",val);*/
    
    if (gui->apeture_on){
      glMatrixMode (GL_PROJECTION);	/*  prepare for and then  */ 
      glLoadIdentity();
      glOrtho(-val/2,val/2,-val/2,val/2,1,1000);
      glMatrixMode (GL_MODELVIEW);	/*  back to modelview matrix	*/
      
      glLoadIdentity();
      gluLookAt(gui->cam.x,     gui->cam.y,     gui->cam.z,
		gui->cam.at_x,  gui->cam.at_y,  gui->cam.at_z,
		gui->cam.up_x,  gui->cam.up_y,  gui->cam.up_z);  
      
    
    draw_all(gui);
    }
  }
  DEBUG_TRACE_OUT printf("Done with Apeture_ChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Apeture toggled Callback
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent widget
%%%
%%%  Purpose: toggles the  apeture
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Apeture_ToggledCB(Widget w,XtPointer clientdata,XtPointer calldata)
{  
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)calldata;

  DEBUG_TRACE_IN printf("Entered Apeture_ToggledCB\n");

  if (gui->beam_line_view){
    /*printf("Toggling the Apeture\n");*/
    
    if (cbs->set){
      gui->apeture_on = 1;
      glEnable(GL_STENCIL_TEST);

      glMatrixMode (GL_PROJECTION);	
      glLoadIdentity();
      glOrtho(-gui->apeture_val/2,gui->apeture_val/2,
	      -gui->apeture_val/2,gui->apeture_val/2,1,1000);
      glMatrixMode (GL_MODELVIEW);	
      


    }else{
      gui->apeture_on = 0;
      glDisable(GL_STENCIL_TEST);

      glMatrixMode (GL_PROJECTION);	/*  prepare for and then  */ 
      glLoadIdentity ();	/*  define the projection  */
      glFrustum (-20, 20, -20, 20, gui->front_z, gui->back_z);      
      glMatrixMode (GL_MODELVIEW);	/*  back to modelview matrix	*/     
    }

      glLoadIdentity();
      gluLookAt(gui->cam.x,     gui->cam.y,     gui->cam.z,
		gui->cam.at_x,  gui->cam.at_y,  gui->cam.at_z,
		gui->cam.up_x,  gui->cam.up_y,  gui->cam.up_z);  

    draw_all(gui);
  }
  DEBUG_TRACE_OUT printf("Done with Apeture_ToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : draw_ring_apeture
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw_ring_apeture(main_gui_t *gui)
{
  static int first_time = 1;
  int was_on = 0;

  DEBUG_TRACE_IN printf("Entered draw_ring_apeture\n");

  if (first_time){
    gui->ring_apeture = gluNewQuadric();
  }
  if (glIsEnabled(GL_LIGHTING)){was_on =1; glDisable(GL_LIGHTING);}
  if (gui->beam_clip) glDisable(GL_CLIP_PLANE3);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  glOrtho(-gui->apeture_val/2,gui->apeture_val/2,
	  -gui->apeture_val/2,gui->apeture_val/2,1,1000);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();
  gluLookAt(0,0,250,0,0,0,0,1,0);
  
  glTranslatef(0,0,200);
  glColor3f(1,1,1);
  gluDisk(gui->ring_apeture,
	  gui->ring_apeture_val/2, gui->ring_apeture_val/2 + .3,
	  50,50);

  glMatrixMode (GL_PROJECTION);	
  glPopMatrix();
  glMatrixMode (GL_MODELVIEW);	
  glPopMatrix();

  if (was_on) glEnable(GL_LIGHTING);
  if (gui->beam_clip) glEnable(GL_CLIP_PLANE3);

  DEBUG_TRACE_OUT printf("Done with draw_ring_apeture\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : draw_apeture_into_stencil_buffer
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw_apeture_into_stencil_buffer(main_gui_t *gui)
{
  GLUquadricObj *sphere;
  int i;

  DEBUG_TRACE_IN printf("Entered draw_ring_apeture_into_stencil_buffer\n");

  glClear(GL_STENCIL_BUFFER_BIT);
  glEnable(GL_STENCIL_TEST);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  glOrtho(-3.0,3.0,-3.0,3.0,-1.0,1.0);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();
  
  glStencilFunc(GL_ALWAYS, 0x1,0x1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
  glBegin(GL_QUADS);
     glVertex3f(-1.0,0.0,0.0);
     glVertex3f(0.0,1.0,0.0);
     glVertex3f(1.0,0.0,0.0);
     glVertex3f(0.0,-1.0,0.0);
  glEnd();

  glMatrixMode (GL_PROJECTION);	
  glPopMatrix();
  glMatrixMode (GL_MODELVIEW);	
  glPopMatrix();

  /*
  glClear(GL_STENCIL_BUFFER_BIT);
  glEnable(GL_STENCIL_TEST);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  glOrtho(-100.0, 100.0, -100.0, 100.0, 1.0, 100.0);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();
  gluLookAt(0,0,10,0,0,0,0,1,0);
  
  glClearStencil(0x0);
  glStencilFunc (GL_ALWAYS, 0x1, 0x1);
  glStencilOp (GL_REPLACE, GL_REPLACE, GL_REPLACE);
  
  glColorMask( GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE );
  sphere = gluNewQuadric();

  for (i = -100; i< 100; i+=2)
    gluSphere(sphere,50.0,50.0,i);

  glColorMask( GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE );
  
  glMatrixMode (GL_PROJECTION);	
  glPopMatrix();
  glMatrixMode (GL_MODELVIEW);	
  glPopMatrix();
  
  glStencilMask(GL_FALSE);
  glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
  glStencilFunc(GL_EQUAL,0x1,0x1);
  */



  /*glDisable(GL_STENCIL_TEST);*/

  DEBUG_TRACE_OUT printf("Done with draw_ring_apeture_into_stencil_buffer\n");
}
