#include "sera3d.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : init_clipping_planes
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: sets up the clipping plane equations
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_clipping_planes(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered init_clipping_planes\n");

  gui->clip[0].f_eqn[0] = 0.0; gui->clip[0].f_eqn[1] = -1.0;
  gui->clip[0].f_eqn[2] = 0.0; gui->clip[0].f_eqn[3] = 128.0;

  gui->clip[0].b_eqn[0] = 0.0; gui->clip[0].b_eqn[1] = 1.0;
  gui->clip[0].b_eqn[2] = 0.0; gui->clip[0].b_eqn[3] = 128.0;

  gui->clip[1].f_eqn[0] = 0.0; gui->clip[1].f_eqn[1] = 0.0;
  gui->clip[1].f_eqn[2] = -1.0;gui->clip[1].f_eqn[3] = 128.0;

  gui->clip[1].b_eqn[0] = 0.0; gui->clip[1].b_eqn[1] = 0.0;
  gui->clip[1].b_eqn[2] = 1.0; gui->clip[1].b_eqn[3] = 128.0;

  gui->clip[2].f_eqn[0] = -1.0; gui->clip[2].f_eqn[1] = 0.0;
  gui->clip[2].f_eqn[2] = 0.0; gui->clip[2].f_eqn[3] = 128.0;

  gui->clip[2].b_eqn[0] = 1.0; gui->clip[2].b_eqn[1] = 0.0;
  gui->clip[2].b_eqn[2] = 0.0; gui->clip[2].b_eqn[3] = 128.0;

  glClipPlane(GL_CLIP_PLANE0, gui->clip[0].f_eqn);
  glClipPlane(GL_CLIP_PLANE1, gui->clip[0].b_eqn);

  glClipPlane(GL_CLIP_PLANE2, gui->clip[1].f_eqn);  
  glClipPlane(GL_CLIP_PLANE3, gui->clip[1].b_eqn);

  glClipPlane(GL_CLIP_PLANE4, gui->clip[2].f_eqn);
  glClipPlane(GL_CLIP_PLANE5, gui->clip[2].b_eqn);

  DEBUG_TRACE_OUT printf("Done with init_clipping_planes\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Clipping_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb) clip direction passed through clientData
%%%              1- axial
%%%              2- coronal
%%%              3- sagittal
%%%
%%%  Purpose: this is a callback for the clipping slider. 
%%%           The slider changes the clipping equation to the slider
%%%           value so that the clipping plane moves along with it.
%%%           if the clipping is being done interactively it will
%%%           affect the clip structure which is used to redraw
%%%           otherwise is only changed the tempclip which is not 
%%%           used until the "done" button is pressed"
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Clipping_ChangedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;
  int dir;

  dir = gui->clipping_dialog.current_dir;

  DEBUG_TRACE_IN printf("Entered Clipping_ChangedCB\n");

  if (gui->interactive_clipping){
    if (strcmp(XtName(w),"Pos_slider") == 0)
      gui->clip[dir-1].f_eqn[3] = (float)(cbs->value)/100.0;
    else if (strcmp(XtName(w),"Neg_slider") == 0)
      gui->clip[dir-1].b_eqn[3] = -(float)(cbs->value)/100.0;
  }else{
    if (strcmp(XtName(w),"Pos_slider") == 0)
      gui->tempclip[dir-1].f_eqn[3] = (float)(cbs->value)/100.0;
    else if (strcmp(XtName(w),"Neg_slider") == 0)
      gui->tempclip[dir-1].b_eqn[3] = -(float)(cbs->value)/100.0;
  }

  if (gui->interactive_clipping){
    switch(dir){
    case 1:
      glClipPlane(GL_CLIP_PLANE0, gui->clip[0].f_eqn);
      glClipPlane(GL_CLIP_PLANE1, gui->clip[0].b_eqn);
      break;
    case 2:
      glClipPlane(GL_CLIP_PLANE2, gui->clip[1].f_eqn);
      glClipPlane(GL_CLIP_PLANE3, gui->clip[1].b_eqn);
      break;
    case 3:
      glClipPlane(GL_CLIP_PLANE4, gui->clip[2].f_eqn);
      glClipPlane(GL_CLIP_PLANE5, gui->clip[2].b_eqn);
      break;
    }
    
    if (gui->clip[dir-1].on)
      draw_all(gui);
  }
  DEBUG_TRACE_OUT printf("Done with Clipping_ChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : set_clipping 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: this procedure is used with the unset_clipping 
%%%           to temporarily enable any clipping planes that may
%%%           be turned off by unset_clipping
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_clipping(main_gui_t *gui)
{
  int i;

  DEBUG_TRACE_IN printf("Entered set_clipping\n");

  if(gui->clip[0].on){
    glClipPlane(GL_CLIP_PLANE0, gui->clip[0].f_eqn);
    glClipPlane(GL_CLIP_PLANE1, gui->clip[0].b_eqn);
    glEnable(GL_CLIP_PLANE0);
    glEnable(GL_CLIP_PLANE1);
  }
  if(gui->clip[1].on || gui->beam_clip){
    if (!gui->beam_clip)
      glClipPlane(GL_CLIP_PLANE2, gui->clip[1].f_eqn);
    
    if (!gui->beam_clip){
      glClipPlane(GL_CLIP_PLANE3, gui->clip[1].b_eqn);
    }else{
      glClipPlane(GL_CLIP_PLANE3, gui->beam_eqn);
    }
    if (!gui->beam_clip)
      glEnable(GL_CLIP_PLANE2);
    glEnable(GL_CLIP_PLANE3);
  }
  if(gui->clip[2].on){
    glClipPlane(GL_CLIP_PLANE4, gui->clip[2].f_eqn);
    glClipPlane(GL_CLIP_PLANE5, gui->clip[2].b_eqn);
    glEnable(GL_CLIP_PLANE4);
    glEnable(GL_CLIP_PLANE5);
  }

  DEBUG_TRACE_OUT printf("Done with set_clipping\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : unset_clipping
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: temporarily shuts off the clipping planes
%%%           in order for a particular object to be drawn
%%%           and not be clipped, while other objects are clipped
%%%           in the rendering window.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void unset_clipping(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered unset_clipping\n");

  if(gui->clip[0].on){
    glDisable(GL_CLIP_PLANE0);
    glDisable(GL_CLIP_PLANE1);
  }
  if(gui->clip[1].on || gui->beam_clip){
    glDisable(GL_CLIP_PLANE2);      
    glDisable(GL_CLIP_PLANE3);
  }
  if(gui->clip[2].on){
    glDisable(GL_CLIP_PLANE4);
    glDisable(GL_CLIP_PLANE5);
  }

  DEBUG_TRACE_OUT printf("Done with set_clipping\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Body_Clipping_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb) the body struct is passed through clientData
%%%
%%%  Purpose: toggles whether the clipping planes affect the 
%%%           current body.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Body_Clipping_ChangedCB(Widget w, XtPointer clientData,XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmToggleButtonCallbackStruct * cbs = (XmToggleButtonCallbackStruct *) callData;
  Body_t *bod;
  int i;

  DEBUG_TRACE_IN printf("Entered Body_Clipping_ChangedCB\n");

  for (i=0;i<gui->num_bodies+gui->num_contour_surfaces;i++){
    if (i<gui->num_bodies){
      if (strcmp(XtName(w),gui->bod[i].name) == 0){
	bod = &gui->bod[i];
      break;
      }
    }
    else{
      if (strcmp(XtName(w),gui->bod[(i-gui->num_bodies)+MAX_BODS].name) == 0){
	bod = &gui->bod[(i-gui->num_bodies)+MAX_BODS];
	/*printf("setting the body to # %d\n",(i-gui->num_bodies)+MAX_BODS);*/
	break;
      }
    }
    
  }


  DisplayBusyCursor(gui->mainwindow);
  
  if (cbs->set) bod->clipped = 1;
  else bod->clipped = 0;
  
  if (bod->enabled)
    draw_all(gui);
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Body_Clipping_ChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Con_Clipping_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb) the body struct is passed through clientData
%%%
%%%  Purpose: toggles whether the clipping planes affect the 
%%%           current body.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
  void Con_Clipping_ChangedCB(Widget w, XtPointer clientData,XtPointer callData){
  XmToggleButtonCallbackStruct * cbs =  (XmToggleButtonCallbackStruct *) callData;
  int con_num;
  con_num = (int)clientData;

  DEBUG_TRACE_IN printf("Entered Con_Clipping_ChangedCB\n");

  DisplayBusyCursor(mainwindow);
  
  if (cbs->set) con[con_num].clipped = 1;
  else con[con_num].clipped = 0;

  if (con[con_num].enabled)
    draw_all();
  RemoveBusyCursor(mainwindow);

  DEBUG_TRACE_OUT printf("Done with Con_Clipping_ChangedCB\n");
}
*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Clipping_ToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb) clip direction passed through clienData
%%%
%%%  Purpose: toggles clipping planes on or off for one of the 
%%%           three directions axial,coronal,sagittal
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Clipping_ToggledCB(Widget w, XtPointer clientData,	XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  Widget other = (Widget)clientData;
  int dir;
  XmToggleButtonCallbackStruct * cbs = (XmToggleButtonCallbackStruct *) callData;
  
  DEBUG_TRACE_IN printf("Entered Clipping_ToggledCB\n");

  dir = gui->clipping_dialog.current_dir;
  
  DisplayBusyCursor(gui->mainwindow);
  
  if (gui->interactive_clipping){
    if (!cbs->set) gui->clip[dir-1].on = 0;
    else gui->clip[dir-1].on = 1; 
  }else{
    if (!cbs->set) gui->tempclip[dir-1].on = 0;
    else gui->tempclip[dir-1].on = 1; 
  }

  if (gui->interactive_clipping){
    switch (dir){
    case 1:
      if (!gui->clip[dir-1].on){
	glDisable(GL_CLIP_PLANE0); glDisable(GL_CLIP_PLANE1);
      }
      else{
	glEnable(GL_CLIP_PLANE0); glEnable(GL_CLIP_PLANE1);
      }
      break;
    case 2:
      if (!gui->clip[dir-1].on){
	glDisable(GL_CLIP_PLANE2); glDisable(GL_CLIP_PLANE3);
      }
      else{
	glEnable(GL_CLIP_PLANE2); glEnable(GL_CLIP_PLANE3);
      }
      break;
    case 3:
      if (!gui->clip[dir-1].on){
	glDisable(GL_CLIP_PLANE4); glDisable(GL_CLIP_PLANE5);
      }
      else{
	glEnable(GL_CLIP_PLANE4); glEnable(GL_CLIP_PLANE5);
      }
      break;
	}
    draw_all(gui);
  }
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Clipping_ToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Capping_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb) direction passed through clienData
%%%
%%%  Purpose: toggles whether capping is wanted on the current
%%%           clipping plane.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Capping_ChangedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int dir;
  XmToggleButtonCallbackStruct * cbs = (XmToggleButtonCallbackStruct *) callData;  
  dir = gui->clipping_dialog.current_dir;
  
  DEBUG_TRACE_IN printf("Entered Capping_ChangedCB\n");

  if (cbs ->set){
    if (strcmp(XtName(w),"Cap ") == 0){   
      if (gui->interactive_clipping) gui->clip[dir-1].f_capped = 1;
      else gui->tempclip[dir-1].f_capped = 1;
    }else
      if (gui->interactive_clipping) gui->clip[dir-1].b_capped = 1;
      else gui->tempclip[dir-1].b_capped = 1;
  }else{
    if (strcmp(XtName(w),"Cap ") == 0)
      if (gui->interactive_clipping) gui->clip[dir-1].f_capped = 0;
      else gui->tempclip[dir-1].f_capped = 0;
    else
      if (gui->interactive_clipping) gui->clip[dir-1].b_capped = 0;
      else gui->tempclip[dir-1].b_capped = 0;
  }    

  DEBUG_TRACE_OUT printf("Done with Capping_ChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Draw_clip_planes
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: direction
%%%              0 : axial
%%%              1 : coronal
%%%              2 : sagittal
%%%
%%%  Purpose: draws the two clip planes for the passed direction.
%%%           the color is set to the corresponding axis to the 
%%%            direction.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Draw_clip_planes(main_gui_t *gui,int dir)
{
  int was_on = 0;

  DEBUG_TRACE_IN printf("Entered Draw_clip_planes\n");

  if (glIsEnabled(GL_LIGHTING)){
    was_on = 1;
    glDisable(GL_LIGHTING);
  }

  switch (dir){
  case 0:
    glDisable(GL_CLIP_PLANE0);glDisable(GL_CLIP_PLANE1);
    glPushMatrix();
      set_color(Z_AXIS_COLOR);
      glScalef(128, 1, 128);
      glPushMatrix();
        glTranslatef(0,gui->clip[0].f_eqn[3],0);
	glCallList(AXIAL_SQUARE);
      glPopMatrix();
      glPushMatrix();
        glTranslatef(0,-gui->clip[0].b_eqn[3],0);
	glCallList(AXIAL_SQUARE);
      glPopMatrix();
    glPopMatrix();
    glEnable(GL_CLIP_PLANE0);glEnable(GL_CLIP_PLANE1);
    break;
  case 1:
    glDisable(GL_CLIP_PLANE2);glDisable(GL_CLIP_PLANE3);
    glPushMatrix();
      set_color(Y_AXIS_COLOR);
      glScalef(128,gui->num_slices+4,1);
      glPushMatrix();
        glTranslatef(0,0,gui->clip[1].f_eqn[3]);
	glCallList(CORONAL_SQUARE);
      glPopMatrix();
      glPushMatrix();
        glTranslatef(0,0,-gui->clip[1].b_eqn[3]);
	glCallList(CORONAL_SQUARE);
      glPopMatrix();
    glPopMatrix();
    glEnable(GL_CLIP_PLANE2);glEnable(GL_CLIP_PLANE3);
    break;
  case 2:
    glDisable(GL_CLIP_PLANE4);glDisable(GL_CLIP_PLANE5);
    glPushMatrix();
      set_color(X_AXIS_COLOR);
      glScalef(1,gui->num_slices+4, 128);
      glPushMatrix();
        glTranslatef(gui->clip[2].f_eqn[3],0,0);
	glCallList(SAGITTAL_SQUARE);
      glPopMatrix();
      glPushMatrix();
        glTranslatef(-gui->clip[2].b_eqn[3],0,0);
	glCallList(SAGITTAL_SQUARE);
      glPopMatrix();
    glPopMatrix();
    glEnable(GL_CLIP_PLANE4);glEnable(GL_CLIP_PLANE5);
    break;
  }
  if (was_on) glEnable(GL_LIGHTING);

  DEBUG_TRACE_OUT printf("Done with Draw_clip_planes\n");
}
