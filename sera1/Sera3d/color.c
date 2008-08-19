#include "sera3d.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : body_color_changedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: takes the new value of the color sliders and 
%%%           displays the color in the color swatch
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void body_color_changedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;
  XColor color;
  int r,g,b;
  Pixel pixel;

  DEBUG_TRACE_IN printf("Entered body_color_changedCB\n");

  XtVaGetValues(gui->color_panel.r_slider,XmNvalue,&r,NULL);
  XtVaGetValues(gui->color_panel.g_slider,XmNvalue,&g,NULL);
  XtVaGetValues(gui->color_panel.b_slider,XmNvalue,&b,NULL);  

  init_color(gui,r,g,b,&color);
  XtVaSetValues(gui->color_panel.main_color_swatch,XmNbackground, color.pixel,NULL);

  DEBUG_TRACE_OUT printf("Done with body_color_changedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : ColorPresetCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: when a color preset is clicked in the list,
%%%           this procedure sets the color swatch (and
%%%           sliders) to the 
%%%           preset color, which then may be applied to the 
%%%           body.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void ColorPresetCB (Widget w ,XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *) callData;
  XColor color;
  int r,g,b;

  DEBUG_TRACE_IN printf("Entered ColorPresetCB\n");

  switch (cbs->item_position)
    {
    case 1:  r = 45000; g = 45000; b = 30000;break;
    case 2:  r = 40000; g = 40000; b = 25000;break;
    case 3:  r = 50000; g = 50000; b = 50000;break;
    case 4:  r = 40000; g = 40000; b = 40000;break;
    case 5:  r = 0;     g = 50000; b = 20000;break;
    case 6:  r = 0;     g = 40000; b = 20000;break;
    case 7:  r = 65000; g = 0;     b = 65000;break;
    case 8:  r = 0;     g = 65000; b = 65000;break;
    case 9:  r = 65000; g = 0;     b = 0;    break;
    case 10: r = 65000; g = 30000; b = 15000;break;
    case 11: r = 65000; g = 0;     b = 0;    break;
    case 12: r = 0;     g = 65000; b = 0;    break;
    case 13: r = 0;     g = 0;     b = 65000;break;
    case 14: r = 0;     g = 65000; b = 65000;break;
    case 15: r = 65000; g = 0;     b = 65000;break;
    case 16: r = 65000; g = 65000; b = 0;    break;
    case 17: r = 65000; g = 65000; b = 65000;break;
    }
  
  XmScaleSetValue(gui->color_panel.r_slider, r);
  XmScaleSetValue(gui->color_panel.g_slider, g);
  XmScaleSetValue(gui->color_panel.b_slider, b);
  
  init_color(gui,r,g,b,&color);
  XtVaSetValues(gui->color_panel.main_color_swatch,XmNbackground, color.pixel,NULL);

  DEBUG_TRACE_OUT printf("Done with ColorPresetCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : TransparencyPresetCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: when the preset is clicked in the preset list,
%%%           the slider is set to that preset, and that 
%%%           transparency may be applied to the body
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void TransparencyPresetCB (Widget w ,XtPointer clientData, XtPointer callData)
{  
  main_gui_t *gui = (main_gui_t *)clientData;
  XmListCallbackStruct *cbs = (XmListCallbackStruct *) callData;
  XColor color;
  int t;

  DEBUG_TRACE_IN printf("Entered TransparencyPresetCB\n");

  switch (cbs->item_position)
    {
    case 1:  t = 100;break;
    case 2:  t = 90;break;
    case 3:  t = 80;break;
    case 4:  t = 70;break;
    case 5:  t = 60;break;
    case 6:  t = 50;break;
    case 7:  t = 40;break;
    case 8:  t = 30;break;
    case 9:  t = 20; break;
    case 10: t = 10;break;
    }
  XmScaleSetValue(gui->transparency_panel.t_slider, t); 

  DEBUG_TRACE_OUT printf("Done with TransparencyPresetCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : ApplyColorToBody
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: gets the color from the rgb sliders and 
%%%           assigns that color to the body.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void ApplyColorToBodyCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int r,g,b;
  Pixel pixel;

  DEBUG_TRACE_IN printf("Entered ApplyColorToBodyCB\n");

  DisplayBusyCursor(gui->mainwindow);

  XtVaGetValues(gui->color_panel.r_slider,XmNvalue,&r,NULL);
  XtVaGetValues(gui->color_panel.g_slider,XmNvalue,&g,NULL);
  XtVaGetValues(gui->color_panel.b_slider,XmNvalue,&b,NULL);  

  gui->bod[gui->selected_body].r = r; 
  gui->bod[gui->selected_body].g = g; 
  gui->bod[gui->selected_body].b = b; 

  if( gui->view_style != VIEW_STYLE_POLYGONAL && gui->view_style != VIEW_STYLE_WIREFRAME) {
    /* Rebuild is required to display */
    Set_Viewing_Style(gui, gui->view_style);
  }
  draw_all(gui);
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with ApplyColorToBodyCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : ApplyTransparencyToBodyCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: takes the value of the transparency slider
%%%           and applies it to the body.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void ApplyTransparencyToBodyCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int trans;
  int i, no_transparent_bodies = 1;

  DEBUG_TRACE_IN printf("Entered ApplyTransparencyToBodyCB\n");

  DisplayBusyCursor(gui->mainwindow);
 
  XtVaGetValues(gui->transparency_panel.t_slider,XmNvalue, &trans, NULL);

  gui->bod[gui->selected_body].t = trans;

  if (gui->view_style == VIEW_STYLE_POLYGONAL || 
      gui->num_contour_surfaces > 0){
    for (i=1;i<gui->num_bodies;i++){
      if (gui->bod[i].t != 100){
	no_transparent_bodies = 0; 
	break;
      }
    }
    for (i=0;i<gui->num_contour_surfaces;i++){
      if (gui->bod[i+MAX_BODS].t != 100){
	no_transparent_bodies = 0; 
	break;
      }
    }    
    if (no_transparent_bodies){
      if (glIsEnabled(GL_BLEND)){ 
	glDisable(GL_BLEND);
      }
    }else{
      if (!glIsEnabled(GL_BLEND)){
	glEnable(GL_BLEND);
      }
    }
  }
  draw_all(gui);
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with ApplyTransparencyToBodyCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : set_material
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: the material #, the transparency
%%%
%%%  Purpose: sets the material based on the presets for
%%%           several material #s
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

void set_material(main_gui_t *gui,int material,float trans)
{
  GLfloat material_diffuse[4];
  GLfloat material_specular[4];
  GLfloat material_shininess;

  DEBUG_TRACE_IN printf("Entered set_material\n");

  if (material < 50){
    material_diffuse[0] = (float)gui->bod[material].r/65535.0;
    material_diffuse[1] = (float)gui->bod[material].g/65535.0;
    material_diffuse[2] = (float)gui->bod[material].b/65535.0;
    material_diffuse[3] = trans;
    
    material_specular[0] = 1;material_specular[1] = 1;
    material_specular[2] = 1;material_specular[3] = trans;
    material_shininess = 75.0;

  }else{
    switch(material)
      {
      case X_AXIS_COLOR:
	material_diffuse[0] = 1.0; material_diffuse[1] = 0.0; 
	material_diffuse[2] = 0.0; material_diffuse[3] = trans;
	break;
      case Y_AXIS_COLOR:
	material_diffuse[0] = 1.0; material_diffuse[1] = 1.0;
	material_diffuse[2] = 0.0; material_diffuse[3] = trans;
	break;
      case Z_AXIS_COLOR:
	material_diffuse[0] = 0.0; material_diffuse[1] = 0.0;
	material_diffuse[2] = 1.0; material_diffuse[3] = trans;
	break;
	/*case SKIN:
	  material_diffuse[0] = .785; material_diffuse[1] = .75;
	  material_diffuse[2] = .461; material_diffuse[3] = trans;
	  break;
	  case SKULL:
	  material_diffuse[0] = .8; material_diffuse[1] = .8;
	  material_diffuse[2] = .8; material_diffuse[3] = trans;
	  break;
	  case BRAIN:
	  material_diffuse[0] = 1.0; material_diffuse[1] = 0.0;
	  material_diffuse[2] = 0.0; material_diffuse[3] = trans;
	  break;*/
      }
    
    material_specular[0] = 1; material_specular[1] = 1;
    material_specular[2] = 1; material_specular[3] = trans;
    material_shininess = 75.0;
  }
  glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE,material_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,material_specular);
  glMaterialfv(GL_FRONT_AND_BACK,GL_SHININESS,&material_shininess);

  DEBUG_TRACE_OUT printf("Done with set_material\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : set_full_material
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: r,g,b, and transparency for the material
%%%
%%%  Purpose: simply takes the params and sets the glmaterial
%%%           accordingly
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_full_material(float r, float g, float b,float trans)
{
  GLfloat material_diffuse[4];
  GLfloat material_specular[4];
  GLfloat material_shininess;

  DEBUG_TRACE_IN printf("Entered set_full_material\n");

  material_diffuse[0] = r; material_diffuse[1] = g;
  material_diffuse[2] = b; material_diffuse[3] = trans;
     
  material_specular[0] = 1;material_specular[1] = 1;
  material_specular[2] = 1;material_specular[3] = trans;
     
  material_shininess = 75.0;

  glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE,material_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,material_specular);
  glMaterialfv(GL_FRONT_AND_BACK,GL_SHININESS,&material_shininess);

  DEBUG_TRACE_OUT printf("Done with set_full_material\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : set_color
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: takes a color #
%%%
%%%  Purpose: set the color based on preset color #s
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_color(int color)
{
  DEBUG_TRACE_IN printf("Entered set_color\n");

  switch(color)
    {
    case 1: glColor3f(.7,.7,.7); break;
    case 2: glColor3f(0,.8,.3);  break;
    case 3: glColor3f(.5,.5,1);  break;
    case 4: glColor3f(1,1,0);    break;
    case 5: glColor3f(0,1,1);    break;
    case 6: glColor3f(.3,.6,.6); break;
    case 7: glColor3f(.3,.7,.2); break;
    case X_AXIS_COLOR: glColor3f(1,0,0); break;
    case Y_AXIS_COLOR: glColor3f(1,1,0); break;
    case Z_AXIS_COLOR: glColor3f(0,0,1); break;
    default: glColor3f(1,1,1);
    }

  DEBUG_TRACE_OUT printf("Done with set_color\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : set_body_colors_and_transparency
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: if no preference colors are defined for a body,
%%%            then the default colors need to be defined.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_default_body_colors_and_transparency(main_gui_t *gui)
{
  int i;
  int different = 1;
  
  DEBUG_TRACE_IN printf("Entered set_default_body_colors_and_transparency\n");

printf("in set_default_body_colors_and_transparency\n");
  for (i = 1; i<=gui->num_bodies; i++)
    {
      if (strstr(gui->bod[i].name,"scalp")){
	gui->bod[i].r = 47054; gui->bod[i].g = 45874; gui->bod[i].b = 28704;
      }else if (strstr(gui->bod[i].name,"head")){
	gui->bod[i].r = 47054; gui->bod[i].g = 45874; gui->bod[i].b = 28704;
      }else if (strstr(gui->bod[i].name,"skull")){
	gui->bod[i].r = 45874; gui->bod[i].g = 45874; gui->bod[i].b = 45874;
      }else if (strstr(gui->bod[i].name,"brain")){
	gui->bod[i].r = 0; gui->bod[i].g = 52428; gui->bod[i].b = 19660;
      }else if (strstr(gui->bod[i].name,"target")){
	gui->bod[i].r = 65535; gui->bod[i].g = 0; gui->bod[i].b = 65535;
      }else if (strstr(gui->bod[i].name,"tumor")){
	gui->bod[i].r = 45874; gui->bod[i].g = 13107; gui->bod[i].b = 6554;
      }else{
	switch(different++)
	  {
	  case 1:
	    gui->bod[i].r = 32767; gui->bod[i].g = 32767; gui->bod[i].b = 0;
	    break;
	  case 2:
	    gui->bod[i].r = 0; gui->bod[i].g = 32767; gui->bod[i].b = 32767;	     
	    break;
	  case 3:
	    gui->bod[i].r = 32767; gui->bod[i].g = 0; gui->bod[i].b = 32767;	     
	    break;
	  case 4:
	    gui->bod[i].r = 32767; gui->bod[i].g = 0; gui->bod[i].b = 0;	     
	    break;
	  case 5:
	    gui->bod[i].r = 0; gui->bod[i].g = 32767; gui->bod[i].b = 0;
	    break;
	  default:
	    gui->bod[7].r = 0; gui->bod[7].g = 0; gui->bod[7].b = 32767;	      
	  }
      }
      gui->bod[i].t = 100; 
    }

  DEBUG_TRACE_OUT printf("Done with set_default_body_colors_and_transparency\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : smart_set_body_colors
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: when preference colors are defined for bodies,
%%%           the bodies in the uv still need to be matched up
%%%           to these preference colors.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void smart_set_body_colors(main_gui_t *gui)
{
  int i,j;
  int different = 1;
  int default_color_index;
  
  DEBUG_TRACE_IN printf("Entered smart_set_body_colors\n");

  for (j=1;j<=gui->num_pref_bods;j++) 
    if (strcmp("default",&gui->pref_bod[j].name[0]) == 0){
      default_color_index = j; break;
    }

  for (i = 1; i<gui->num_bodies; i++) {
      for (j=1;j<=gui->num_pref_bods;j++) {
	if (strcmp(&gui->bod[i].name[0],&gui->pref_bod[j].name[0]) == 0){
	  gui->bod[i].r = gui->pref_bod[j].r;
	  gui->bod[i].g = gui->pref_bod[j].g;
	  gui->bod[i].b = gui->pref_bod[j].b;
	  gui->bod[i].t = gui->pref_bod[j].t;
	  gui->bod[i].pref = j; 
	  break;
	}

	if (j == gui->num_pref_bods){
	  gui->bod[i].r = gui->pref_bod[default_color_index].r;
	  gui->bod[i].g = gui->pref_bod[default_color_index].g;
	  gui->bod[i].b = gui->pref_bod[default_color_index].b;
	  gui->bod[i].t = gui->pref_bod[default_color_index].t;
	  gui->bod[i].pref = default_color_index; 
	}

      }
  }

  DEBUG_TRACE_OUT printf("Done with smart_set_body_colors\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : init_color
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: r,g,b, and the Xcolor returned
%%%
%%%  Purpose: given a r,g,b values, an XColor matching the 
%%%           values is returned, if the system does not have 
%%%           the XColor, the closest one is returned (256 colors)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_color(main_gui_t *gui,int r, int g, int b, XColor *color)
{
  static int first_call = 1;
  static Colormap cmap;

  DEBUG_TRACE_IN printf("Entered init_color\n");

  if (first_call){
    cmap = XDefaultColormap(gui->display, DefaultScreen(gui->display));
    first_call = 0;
  }
  color->red = r; 
  color->green = g;
  color->blue = b;
  color->flags = DoRed | DoGreen | DoBlue;
  XAllocColor(XtDisplay(gui->mainwindow),cmap, color);

  DEBUG_TRACE_OUT printf("Done with init_color\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : get_rgb_from_pixel
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: XColor
%%%
%%%  Purpose: taking the pixel member of the XColor,
%%%            the r,g,b members are determined and filled in
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void get_rgb_from_pixel(main_gui_t *gui,XColor *color)
{
  static int first_call = 1;
  static Colormap cmap;

  DEBUG_TRACE_IN printf("Entered get_rgb_from_pixel\n");
  
  if (first_call){
      cmap = XDefaultColormap(gui->display, DefaultScreen(gui->display));
      first_call = 0;
  }

  /*printf("Xcolor came in, pixel: %d, r: %d, g: %d, b: %d\n",
       color->pixel,color->red,color->green,color->blue);*/
  XQueryColor(gui->display, cmap, color);
  /*printf("Xcolor DONE, pixel: %d, r: %d, g: %d, b: %d\n",
    color->pixel,color->red,color->green,color->blue);*/

  DEBUG_TRACE_OUT printf("Done with get_rgb_from_pixel\n");
}
