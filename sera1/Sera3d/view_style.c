#include <stdio.h>
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
void View_Style_changedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  GLfloat lmodel_ambient[] = { 0.7, 0.7, 0.7, 1.0 };
  clock_t t1,t2;
  int i;
  int vs;
  static int old_vs = 1;

  DEBUG_TRACE_IN printf("Entered View_Style_changedCB\n");

  DisplayBusyCursor(gui->form);

  for (i=0;i<4;i++)
    if (w == gui->view_panel.view_s[i])
      vs = i;
  
  vs++;

  XtVaSetValues(gui->view_panel.view_s[vs-1], XmNshadowType, XmSHADOW_IN,NULL);
  XtVaSetValues(gui->view_panel.view_s[old_vs-1], XmNshadowType, XmSHADOW_OUT,NULL);

  /*printf("the old view style was : %d, the new is : %d\n",old_vs,view_style);*/

  if (vs != old_vs)
    Set_Viewing_Style(gui, vs);


      /*
      switch(gui->polygonal_algorithm){
      case VERTEXCELL_WITH_SURFACE_NORMALS:
	build_polygonal_bodies_from_data_block_using_8_cell(gui);
	break;
      case VERTEXCELL_WITH_VERTEX_NORMALS:
	build_polygonal_bodies_from_data_block_using_8_cell_with_normal_mapping(gui);
	break;
      case MARCHING_CUBES:
	build_polygonal_bodies_from_data_block_using_marching_cubes(gui);
	break;
      }
      */      

  old_vs = gui->view_style;
  draw_all(gui);

  RemoveBusyCursor(gui->form);
  RemoveMessage(gui,".");

  DEBUG_TRACE_OUT printf("Done with View_Style_changedCB\n");
}



void Set_Viewing_Style(main_gui_t *gui, int vs){
  gui->view_style = vs;


  switch(gui->view_style){
  case VIEW_STYLE_WIREFRAME:
    PostMessage(gui,"Building Point Model . . .");      
    glDisable(GL_LIGHTING);
    glDeleteLists(1,gui->num_bodies);	
    break;
    
  case VIEW_STYLE_OUTLINE:
    PostMessage(gui,"Building Outline Model this will take a minute . . .");
    
    glDisable(GL_LIGHTING);
    glDeleteLists(1,gui->num_bodies);      
    build_complete_body_from_data_block(gui,BODY_STYLE_OUTLINE,1);
    break;
    
  case VIEW_STYLE_SOLID:
    PostMessage(gui,"Building Solid Model this will take a minute . . .");
    glDeleteLists(1,gui->num_bodies);
    glDisable(GL_LIGHTING);
    build_complete_body_from_data_block(gui,BODY_STYLE_SOLID,1);
    break;
  case VIEW_STYLE_POLYGONAL:
    PostMessage(gui,"Building Polygonal Model this will take a minute . . .");
    glDeleteLists(1,gui->num_bodies);
    glEnable(GL_LIGHTING);
    
    build_polygonal_bodies(gui);
    
    
  }
}
