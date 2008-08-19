#include "sera3d.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : reverse_lights
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: takes the positions of the lights and swaps them
%%%           to the reverse vector.
%%%           it is currently a temporary fix to why the mesh 
%%%           nurbs are lit backwards.  Reverse the lights and
%%%           they look correct.  Hope to fix soon.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void reverse_lights(main_gui_t *gui)
{
  int i;

  DEBUG_TRACE_IN printf("Entered reverse_lights\n");
  
  for (i=0;i<4;i++)
    {
      gui->light1_position[i] = -gui->light1_position[i];
      gui->light2_position[i] = -gui->light2_position[i];
      gui->light3_position[i] = -gui->light3_position[i];
      gui->light4_position[i] = -gui->light4_position[i];
      gui->light5_position[i] = -gui->light5_position[i];
    }

  DEBUG_TRACE_OUT printf("Done with reverse_lights\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : set_lights_normal
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: sets the lights to the original positions, used 
%%%           to undo the reverse lights procedure.  Again, this
%%%           should be fixed, and neither procedure will be 
%%%           needed.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_lights_normal(main_gui_t *gui)
{

  DEBUG_TRACE_IN printf("Entered set_lights_normal\n");

  gui->light1_position[0] = 1.0; gui->light1_position[1] = 1.0;
  gui->light1_position[2] = 1.0; gui->light1_position[3] = 0.0;

  /*gui->light1_position[0] = 1.0; gui->light1_position[1] = 1.0;
    gui->light1_position[2] = 1.0; gui->light1_position[3] = 0.0;*/

  gui->light2_position[0] = -1.0; gui->light2_position[1] = 1.0;
  gui->light2_position[2] =  1.0; gui->light2_position[3] = 0.0;

  gui->light3_position[0] = -1.0; gui->light3_position[1] = 1.0;
  gui->light3_position[2] = -1.0; gui->light3_position[3] = 0.0;

  gui->light4_position[0] = 1.0; gui->light4_position[1] = 1.0;
  gui->light4_position[2] = -1.0; gui->light4_position[3] = 0.0;
 
  gui->light5_position[0] = 0.0; gui->light5_position[1] = 1.0;
  gui->light5_position[2] = 0.0; gui->light5_position[3] = 0.0;

  DEBUG_TRACE_OUT printf("Done withset_lights_normal\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : init_lights
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: initialzes the gl lights
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_lights(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered init_lights\n");

  set_lights_normal(gui);
  
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, gui->lmodel_ambient);
  glLightModeli (GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
  glLightModeli (GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);

  glLightfv(GL_LIGHT1, GL_DIFFUSE, gui->light_diffuse);
  glLightfv(GL_LIGHT2, GL_DIFFUSE, gui->light_diffuse);
  glLightfv(GL_LIGHT3, GL_DIFFUSE, gui->light_diffuse);
  glLightfv(GL_LIGHT4, GL_DIFFUSE, gui->light_diffuse);
  glLightfv(GL_LIGHT5, GL_DIFFUSE, gui->light_diffuse);

  glLightfv(GL_LIGHT1, GL_POSITION, gui->light1_position);
  glLightfv(GL_LIGHT2, GL_POSITION, gui->light2_position);
  glLightfv(GL_LIGHT3, GL_POSITION, gui->light3_position);
  glLightfv(GL_LIGHT4, GL_POSITION, gui->light4_position);
  glLightfv(GL_LIGHT5, GL_POSITION, gui->light5_position);

  glEnable(GL_LIGHT1);
  glEnable(GL_LIGHT3);

  DEBUG_TRACE_OUT printf("Done with init_lights\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : set_light_positions
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: assigns the light positions to the gl lights
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_light_positions(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered set_light_positions\n");

  glLightfv(GL_LIGHT1, GL_POSITION, gui->light1_position);
  glLightfv(GL_LIGHT2, GL_POSITION, gui->light2_position);
  glLightfv(GL_LIGHT3, GL_POSITION, gui->light3_position);
  glLightfv(GL_LIGHT4, GL_POSITION, gui->light4_position);
  glLightfv(GL_LIGHT5, GL_POSITION, gui->light5_position);

  DEBUG_TRACE_OUT printf("Done with set_light_positions\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Light_toggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the lights off and on
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Light_toggledCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmToggleButtonCallbackStruct * cbs = (XmToggleButtonCallbackStruct *) callData;
  
  DEBUG_TRACE_IN printf("Entered Light_toggledCB\n");

  DisplayBusyCursor(gui->mainwindow);

  if (strcmp(XtName(w),"Front Right")==0)
    {
      if (!cbs->set) glDisable(GL_LIGHT1);
      else glEnable(GL_LIGHT1);
    }  
  else if(strcmp(XtName(w),"Front Left")==0)
    {
      if (!cbs->set) glDisable(GL_LIGHT2);
      else glEnable(GL_LIGHT2);
    }
  else if(strcmp(XtName(w),"Back Left")==0)
    {
      if (!cbs->set) glDisable(GL_LIGHT3);
      else glEnable(GL_LIGHT3);
    }
  else if(strcmp(XtName(w),"Back Right")==0)
    {
      if (!cbs->set) glDisable(GL_LIGHT4);
      else glEnable(GL_LIGHT4);
    }
  else if(strcmp(XtName(w),"Top")==0)
    {
      if (!cbs->set) glDisable(GL_LIGHT5);
      else glEnable(GL_LIGHT5);
    }

  draw_all(gui);

  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Light_toggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Ambient_changedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: sets the ambient lighting to the value of the 
%%%           ambient slider.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Ambient_changedCB (Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;

  DEBUG_TRACE_IN printf("Entered Ambient_changedCB\n");

  DisplayBusyCursor(gui->mainwindow);

  gui->lmodel_ambient[0] = ((float)cbs->value)/100.0;
  gui->lmodel_ambient[1] = ((float)cbs->value)/100.0;
  gui->lmodel_ambient[2] = ((float)cbs->value)/100.0;
  gui->lmodel_ambient[3] = 1.0;

  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, gui->lmodel_ambient);

  draw_all(gui);

  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Ambient_changedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : ResetLightingCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: sets the lighting controls to the default
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void ResetLightingCB (Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  Boolean set;

  DEBUG_TRACE_IN printf("Entered ResetLightingCB\n");

  DisplayBusyCursor(gui->form);
  
  XtVaGetValues(gui->lighting_panel.light_1,XmNset, &set,NULL);

  if (!set){
    glEnable(GL_LIGHT1);
    XtVaSetValues(gui->lighting_panel.light_1, XmNset, TRUE, NULL);
  }
  XtVaGetValues(gui->lighting_panel.light_2,XmNset, &set, NULL);
  if (set){
    glDisable(GL_LIGHT2);
    XtVaSetValues(gui->lighting_panel.light_2, XmNset, FALSE, NULL);
  }
  XtVaGetValues(gui->lighting_panel.light_3,XmNset, &set, NULL);
  if (!set){
    glEnable(GL_LIGHT3);
    XtVaSetValues(gui->lighting_panel.light_3, XmNset, TRUE, NULL);
  }
  XtVaGetValues(gui->lighting_panel.light_4, XmNset, &set, NULL);
  if (set){
    glDisable(GL_LIGHT4);
    XtVaSetValues(gui->lighting_panel.light_4, XmNset, FALSE, NULL);
  }
  XtVaGetValues(gui->lighting_panel.light_5,XmNset, &set, NULL);
  if (set){
    glDisable(GL_LIGHT5);
    XtVaSetValues(gui->lighting_panel.light_5, XmNset, FALSE, NULL);
  }
  XmScaleSetValue(gui->lighting_panel.ambient_slider, 40);

  /*************************************************/
  /** Redraw with the reset lighting.
  /*************************************************/
  draw_all(gui);
  RemoveBusyCursor(gui->form);

  DEBUG_TRACE_OUT printf("Done with ResetLightingCB\n");
}

