#include "sera3d.h"

static void ParticleTypeDialogOKCB (Widget w, XtPointer clientData, XtPointer callData);
void Line_Color_changedCB (Widget w, XtPointer clientData, XtPointer callData);
void Particle_DefaultsCB(Widget w, XtPointer clientData, XtPointer callData);


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Line_type_changedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: sets the line type or boldness
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static void Line_type_changedCB (Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;

  DEBUG_TRACE_IN printf("Entered Line_type_changedCB\n");

  if      (strcmp(XtName(w),"line_t1") == 0) gui->particle_dialog.current_line_type = 1;
  else if (strcmp(XtName(w),"line_t2") == 0) gui->particle_dialog.current_line_type = 2;
  else if (strcmp(XtName(w),"line_t3") == 0) gui->particle_dialog.current_line_type = 3;
  else if (strcmp(XtName(w),"line_t4") == 0) gui->particle_dialog.current_line_type = 4;
  else if (strcmp(XtName(w),"bold_t1") == 0) gui->particle_dialog.current_bold_type = 1;
  else if (strcmp(XtName(w),"bold_t2") == 0) gui->particle_dialog.current_bold_type = 2;
  else if (strcmp(XtName(w),"bold_t3") == 0) gui->particle_dialog.current_bold_type = 3;

  DEBUG_TRACE_OUT printf("Done with Line_type_changedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : get_pixmap_from_line_type
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: foreground color, line type, bold type
%%%
%%%  Purpose: returns a pixmap constructed of the parameters
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
Pixmap get_pixmap_from_line_type(main_gui_t *gui,Pixel fg, int type, int bold_type)
{
  Pixmap pixmap;
 
  DEBUG_TRACE_IN printf("Entered get_pixemap_from_line_type\n");

  switch (type)
    {
    case 1:
      if (bold_type == 1)
	pixmap = XCreatePixmapFromBitmapData(gui->display,
					     RootWindowOfScreen(XtScreen(gui->form)),
					     (char *)t1b1_bits, t1b1_width, t1b1_height,
					     fg, BlackPixel(gui->display,DefaultScreen(gui->display)),/*bg.pixel,*/
					     DefaultDepthOfScreen(XtScreen(gui->form)));      
      else if (bold_type == 2)
	pixmap = XCreatePixmapFromBitmapData(gui->display,
					     RootWindowOfScreen(XtScreen(gui->form)),
					     (char *)t1b2_bits, t1b2_width, t1b2_height,
					     fg, BlackPixel(gui->display,DefaultScreen(gui->display)),/*bg.pixel,*/
					     DefaultDepthOfScreen(XtScreen(gui->form)));      
      else if (bold_type == 3)
	pixmap = XCreatePixmapFromBitmapData(gui->display,
					     RootWindowOfScreen(XtScreen(gui->form)),
					     (char *)t1b3_bits, t1b3_width, t1b3_height,
					     fg, BlackPixel(gui->display,DefaultScreen(gui->display)),/*bg.pixel,*/
					     DefaultDepthOfScreen(XtScreen(gui->form)));      
      break;
    case 2:
      if (bold_type == 1)
	pixmap = XCreatePixmapFromBitmapData(gui->display,
					     RootWindowOfScreen(XtScreen(gui->form)),
					     (char *)t2b1_bits, t2b1_width, t2b1_height,
					     fg, BlackPixel(gui->display,DefaultScreen(gui->display)),/*bg.pixel,*/
					     DefaultDepthOfScreen(XtScreen(gui->form)));      
      else if (bold_type == 2)
	pixmap = XCreatePixmapFromBitmapData(gui->display,
					     RootWindowOfScreen(XtScreen(gui->form)),
					     (char *)t2b2_bits, t2b2_width, t2b2_height,
					     fg, BlackPixel(gui->display,DefaultScreen(gui->display)),/*bg.pixel,*/
					     DefaultDepthOfScreen(XtScreen(gui->form)));      
      else if (bold_type == 3)
	pixmap = XCreatePixmapFromBitmapData(gui->display,
					     RootWindowOfScreen(XtScreen(gui->form)),
					     (char *)t2b3_bits, t2b3_width, t2b3_height,
					     fg, BlackPixel(gui->display,DefaultScreen(gui->display)),/*bg.pixel,*/
					     DefaultDepthOfScreen(XtScreen(gui->form)));      
      break;
    case 3:
      if (bold_type == 1)
	pixmap = XCreatePixmapFromBitmapData(gui->display,
					     RootWindowOfScreen(XtScreen(gui->form)),
					     (char *)t3b1_bits, t3b1_width, t3b1_height,
					     fg, BlackPixel(gui->display,DefaultScreen(gui->display)),/*bg.pixel,*/
					     DefaultDepthOfScreen(XtScreen(gui->form)));      
      else if (bold_type == 2)
	pixmap = XCreatePixmapFromBitmapData(gui->display,
					     RootWindowOfScreen(XtScreen(gui->form)),
					     (char *)t3b2_bits, t3b2_width, t3b2_height,
					     fg, BlackPixel(gui->display,DefaultScreen(gui->display)),/*bg.pixel,*/
					     DefaultDepthOfScreen(XtScreen(gui->form)));      
      else if (bold_type == 3)
	pixmap = XCreatePixmapFromBitmapData(gui->display,
					     RootWindowOfScreen(XtScreen(gui->form)),
					     (char *)t3b3_bits, t3b3_width, t3b3_height,
					     fg, BlackPixel(gui->display,DefaultScreen(gui->display)),/*bg.pixel,*/
					     DefaultDepthOfScreen(XtScreen(gui->form))); 
      break;
    case 4:
      if (bold_type == 1)
	pixmap = XCreatePixmapFromBitmapData(gui->display,
					     RootWindowOfScreen(XtScreen(gui->form)),
					     (char *)t4b1_bits, t4b1_width, t4b1_height,
					     fg, BlackPixel(gui->display,DefaultScreen(gui->display)),/*bg.pixel,*/
					     DefaultDepthOfScreen(XtScreen(gui->form)));      
      else if (bold_type == 2)
	pixmap = XCreatePixmapFromBitmapData(gui->display,
					     RootWindowOfScreen(XtScreen(gui->form)),
					     (char *)t4b2_bits, t4b2_width, t4b2_height,
					     fg, BlackPixel(gui->display,DefaultScreen(gui->display)),/*bg.pixel,*/
					     DefaultDepthOfScreen(XtScreen(gui->form)));      
      else if (bold_type == 3)
	pixmap = XCreatePixmapFromBitmapData(gui->display,
					     RootWindowOfScreen(XtScreen(gui->form)),
					     (char *)t4b3_bits, t4b3_width, t4b3_height,
					     fg, BlackPixel(gui->display,DefaultScreen(gui->display)),/*bg.pixel,*/
					     DefaultDepthOfScreen(XtScreen(gui->form)));          
      break;
    }
  
  DEBUG_TRACE_OUT printf("Done with get_pixemap_from_line_type\n");
  return pixmap;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : ParticleTypeDialogOKCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: sets the particletype to the specified values
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static void ParticleTypeDialogOKCB (Widget w, XtPointer clientData, XtPointer callData)
{ 
  main_gui_t *gui = (main_gui_t *)clientData;
  Pixmap pixmap;
  XColor color;
  Pixel pixel;
  int i,calling_widget;
  int r,g,b;
  int index;


  DEBUG_TRACE_IN printf("Entered ParticleTypeDialogOKCB\n");
  
  DisplayBusyCursor(gui->form);

  XtVaGetValues(gui->particle_dialog.r_s, XmNvalue,&r,NULL);
  XtVaGetValues(gui->particle_dialog.g_s, XmNvalue,&g,NULL);
  XtVaGetValues(gui->particle_dialog.b_s, XmNvalue,&b,NULL);  
  XtVaGetValues(gui->particle_dialog.color_swatch,XmNbackground, &pixel,NULL);
  color.red = r; color.green = g; color.blue = b; color.pixel = pixel;

  pixmap = get_pixmap_from_line_type(gui,pixel, gui->particle_dialog.current_line_type, gui->particle_dialog.current_bold_type);


  index = gui->particle_dialog.calling_widget;

  if (index < NUM_PARTICLES){
    XtVaSetValues(gui->particle_panel.colorbx[index],XmNlabelPixmap,pixmap,NULL);
    gui->line_types[index].color = color;
    gui->line_types[index].type = gui->particle_dialog.current_line_type;
    gui->line_types[index].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui);
  }else{
    index -= NUM_PARTICLES;
    XtVaSetValues(gui->preference_dialog.colorbx[index],XmNlabelPixmap,pixmap,NULL);
    gui->line_types[index].color = color;
    gui->line_types[index].type = gui->particle_dialog.current_line_type;
    gui->line_types[index].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui);
  }
  
  /*
    switch(gui->particle_dialog.calling_widget){
    case GAMMA: 
    XtVaSetValues(gui->particle_panel.gl_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[GAMMA].color = color;
    gui->line_types[GAMMA].type = gui->particle_dialog.current_line_type;
    gui->line_types[GAMMA].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui);break;
    case NEUTRON_LOW:
    XtVaSetValues(gui->particle_panel.gm_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[N].color = color;
    gui->line_types[1].type = gui->particle_dialog.current_line_type;
    gui->line_types[1].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 2:
    XtVaSetValues(gui->particle_panel.gh_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[2].color = color;
    gui->line_types[2].type = gui->particle_dialog.current_line_type;
    gui->line_types[2].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 3:
    XtVaSetValues(gui->particle_panel.nl_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[3].color = color;
    gui->line_types[3].type = gui->particle_dialog.current_line_type;
    gui->line_types[3].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 4:
    XtVaSetValues(gui->particle_panel.nm_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[4].color = color;
    gui->line_types[4].type = gui->particle_dialog.current_line_type;
    gui->line_types[4].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 5:
    XtVaSetValues(gui->particle_panel.nh_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[5].color = color;
    gui->line_types[5].type = gui->particle_dialog.current_line_type;
    gui->line_types[5].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 6:
    XtVaSetValues(gui->particle_panel.beam_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[6].color = color;
    gui->line_types[6].type = gui->particle_dialog.current_line_type;
    gui->line_types[6].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 7:
    XtVaSetValues(gui->particle_panel.lost_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[7].color = color;
    gui->line_types[7].type = gui->particle_dialog.current_line_type;
    gui->line_types[7].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 10:
    XtVaSetValues(gui->preference_dialog.gl,XmNlabelPixmap,pixmap,NULL);
    XtVaSetValues(gui->particle_panel.gl_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[0].color = color;
    gui->line_types[0].type = gui->particle_dialog.current_line_type;
    gui->line_types[0].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 11:
    XtVaSetValues(gui->preference_dialog.gm,XmNlabelPixmap,pixmap,NULL);
    XtVaSetValues(gui->particle_panel.gm_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[1].color = color;
    gui->line_types[1].type = gui->particle_dialog.current_line_type;
    gui->line_types[1].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 12:
    XtVaSetValues(gui->preference_dialog.gh,XmNlabelPixmap,pixmap,NULL);
    XtVaSetValues(gui->particle_panel.gh_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[2].color = color;
    gui->line_types[2].type = gui->particle_dialog.current_line_type;
    gui->line_types[2].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 13:
    XtVaSetValues(gui->preference_dialog.nl,XmNlabelPixmap,pixmap,NULL);
    XtVaSetValues(gui->particle_panel.nl_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[3].color = color;
    gui->line_types[3].type = gui->particle_dialog.current_line_type;
    gui->line_types[3].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 14:
    XtVaSetValues(gui->preference_dialog.nm,XmNlabelPixmap,pixmap,NULL);
    XtVaSetValues(gui->particle_panel.nm_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[4].color = color;
    gui->line_types[4].type = gui->particle_dialog.current_line_type;
    gui->line_types[4].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 15:
    XtVaSetValues(gui->preference_dialog.nh,XmNlabelPixmap,pixmap,NULL);
    XtVaSetValues(gui->particle_panel.nh_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[5].color = color;
    gui->line_types[5].type = gui->particle_dialog.current_line_type;
    gui->line_types[5].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 16:
    XtVaSetValues(gui->preference_dialog.beam,XmNlabelPixmap,pixmap,NULL);
    XtVaSetValues(gui->particle_panel.beam_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[6].color = color;
    gui->line_types[6].type = gui->particle_dialog.current_line_type;
    gui->line_types[6].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui); break;
    case 17:
    XtVaSetValues(gui->preference_dialog.lost,XmNlabelPixmap,pixmap,NULL);
    XtVaSetValues(gui->particle_panel.lost_color,XmNlabelPixmap,pixmap,NULL);
    gui->line_types[7].color = color;
    gui->line_types[7].type = gui->particle_dialog.current_line_type;
    gui->line_types[7].boldness = gui->particle_dialog.current_bold_type;
    draw_all(gui);break;
    }
  */

  RemoveBusyCursor(gui->form);

  DEBUG_TRACE_OUT printf("Done with ParticleTypeDialogOKCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Show_Particle_type_dialog
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: brings up the Particle_type_dialog
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Show_Particle_type_dialogCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  Pixel pixel;
  int type, bold_type,num;

  DEBUG_TRACE_IN printf("Entered Show_Particle_type_dialogCB\n");
 
  if (strcmp(XtName(w),"gamma_color") == 0 || strcmp(XtName(w),"Pref_gamma") == 0){
    pixel = gui->line_types[GAMMA].color.pixel;
    type = gui->line_types[GAMMA].type;
    bold_type = gui->line_types[GAMMA].boldness;
    num = GAMMA;
    if (strcmp(XtName(w),"gamma_color") == 0) gui->particle_dialog.calling_widget = GAMMA;
    else gui->particle_dialog.calling_widget = NUM_PARTICLES + GAMMA;
    /*
      }else if (strcmp(XtName(w),"gm_color") == 0 || strcmp(XtName(w),"gm") == 0){
      pixel = gui->line_types[1].color.pixel;
      type = gui->line_types[1].type;
      bold_type = gui->line_types[1].boldness;
      num = GAMMA;
      if (strcmp(XtName(w),"gm_color") == 0) gui->particle_dialog.calling_widget = 1;
      else gui->particle_dialog.calling_widget = 11;
      }else if (strcmp(XtName(w),"gh_color") == 0 || strcmp(XtName(w),"gh") == 0){
      pixel = gui->line_types[2].color.pixel;
      type = gui->line_types[2].type;
      bold_type = gui->line_types[2].boldness;
      num =2;
      if (strcmp(XtName(w),"gh_color") == 0) gui->particle_dialog.calling_widget = 2;
      else gui->particle_dialog.calling_widget = 12;
    */
  }else if (strcmp(XtName(w),"neutron_low_color") == 0 || strcmp(XtName(w),"Pref_neutron_low") == 0){
    pixel = gui->line_types[NEUTRON_LOW].color.pixel;
    type = gui->line_types[NEUTRON_LOW].type;
    bold_type = gui->line_types[NEUTRON_LOW].boldness;
    num = NEUTRON_LOW;
    if (strcmp(XtName(w),"neutron_low_color") == 0) gui->particle_dialog.calling_widget = NEUTRON_LOW;
    else gui->particle_dialog.calling_widget = NUM_PARTICLES + NEUTRON_LOW;

  }else if (strcmp(XtName(w),"neutron_med_color") == 0 || strcmp(XtName(w),"Pref_neutron_med") == 0){
    pixel = gui->line_types[NEUTRON_MED].color.pixel;
    type = gui->line_types[NEUTRON_MED].type;
    bold_type = gui->line_types[NEUTRON_MED].boldness;
    num = NEUTRON_MED;
    if (strcmp(XtName(w),"neutron_med_color") == 0) gui->particle_dialog.calling_widget = NEUTRON_MED;
    else gui->particle_dialog.calling_widget = NUM_PARTICLES + NEUTRON_MED;

  }else if (strcmp(XtName(w),"neutron_high_color") == 0 || strcmp(XtName(w),"Pref_neutron_high") == 0){
    pixel = gui->line_types[NEUTRON_HIGH].color.pixel;
    type = gui->line_types[NEUTRON_HIGH].type;
    bold_type = gui->line_types[NEUTRON_HIGH].boldness;
    num = NEUTRON_HIGH;
    if (strcmp(XtName(w),"neutron_high_color") == 0) gui->particle_dialog.calling_widget = NEUTRON_HIGH;
    else gui->particle_dialog.calling_widget = NUM_PARTICLES + NEUTRON_HIGH;

  }else if (strcmp(XtName(w),"beam_color")== 0 ||strcmp(XtName(w),"Pref_beam")== 0){
    pixel = gui->line_types[BEAM].color.pixel;
    type = gui->line_types[BEAM].type;
    bold_type = gui->line_types[BEAM].boldness;
    num = BEAM;
    if (strcmp(XtName(w),"beam_color") == 0) gui->particle_dialog.calling_widget = BEAM;
    else gui->particle_dialog.calling_widget = NUM_PARTICLES + BEAM;

  }else if (strcmp(XtName(w),"lost_color")==0 ||strcmp(XtName(w),"Pref_lost") == 0){
    pixel = gui->line_types[LOST].color.pixel;
    type = gui->line_types[LOST].type;
    bold_type = gui->line_types[LOST].boldness;
    num = LOST;
    if (strcmp(XtName(w),"lost_color") == 0) gui->particle_dialog.calling_widget = LOST;
    else gui->particle_dialog.calling_widget = NUM_PARTICLES + LOST;
  }

  XmScaleSetValue(gui->particle_dialog.r_s,gui->line_types[num].color.red);
  XmScaleSetValue(gui->particle_dialog.g_s,gui->line_types[num].color.green);
  XmScaleSetValue(gui->particle_dialog.b_s,gui->line_types[num].color.blue);
  
  XtVaSetValues(gui->particle_dialog.color_swatch, 
		XmNbackground, gui->line_types[num].color.pixel,
		NULL);
  
  switch (type){
  case 1:
    XtVaSetValues(gui->particle_dialog.line_type_menu, XmNmenuHistory, gui->particle_dialog.line_t1,NULL); break;
  case 2:
    XtVaSetValues(gui->particle_dialog.line_type_menu, XmNmenuHistory, gui->particle_dialog.line_t2,NULL); break;
  case 3:
    XtVaSetValues(gui->particle_dialog.line_type_menu, XmNmenuHistory, gui->particle_dialog.line_t3,NULL); break;
  case 4:
    XtVaSetValues(gui->particle_dialog.line_type_menu, XmNmenuHistory, gui->particle_dialog.line_t4,NULL); break;
  }
  switch (bold_type){
  case 1:
    XtVaSetValues(gui->particle_dialog.bold_type_menu, XmNmenuHistory, gui->particle_dialog.bold_t1,NULL); break;
  case 2:
    XtVaSetValues(gui->particle_dialog.bold_type_menu, XmNmenuHistory, gui->particle_dialog.bold_t2,NULL); break;
  case 3:
    XtVaSetValues(gui->particle_dialog.bold_type_menu, XmNmenuHistory, gui->particle_dialog.bold_t3,NULL); break;
  }
  
  gui->particle_dialog.current_line_type = type;
  gui->particle_dialog.current_bold_type = bold_type;
  XtManageChild(gui->particle_dialog.dialog);

  DEBUG_TRACE_OUT printf("Done with Show_Particle_type_dialogCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Line__Color_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: sets the color swatch
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Line_Color_changedCB (Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;
  XColor color;
  int r,g,b;

  DEBUG_TRACE_IN printf("Entered Line_Color_changedCB\n");

  XtVaGetValues(gui->particle_dialog.r_s, XmNvalue,&r,NULL);
  XtVaGetValues(gui->particle_dialog.g_s, XmNvalue,&g,NULL);
  XtVaGetValues(gui->particle_dialog.b_s, XmNvalue,&b,NULL);

  init_color(gui,r,g,b,&color);
  XtVaSetValues(gui->particle_dialog.color_swatch,XmNbackground, color.pixel,NULL);

  DEBUG_TRACE_OUT printf("Done with Line_Color_changedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_particle_type_dialog
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent widget
%%%
%%%  Purpose: builds and returns the particle type dialog
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_particle_type_dialog(main_gui_t *gui)
{
  int i,min_x,max_x, min_y,max_y, min_z, max_z;
  Pixmap pixmap;
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered build_particle_type_dialog\n");
  
  gui->particle_dialog.dialog = (Widget)XmCreateMessageDialog(gui->form, "Path Type",NULL,0);

  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->particle_dialog.dialog,XmDIALOG_SYMBOL_LABEL));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->particle_dialog.dialog,XmDIALOG_MESSAGE_LABEL));

  XtAddCallback(gui->particle_dialog.dialog, XmNokCallback,ParticleTypeDialogOKCB,(XtPointer)gui);
  
  gui->particle_dialog.main_form = XtVaCreateManagedWidget ("main_form", 
							    xmFormWidgetClass, gui->particle_dialog.dialog, 
							    NULL);
  
  gui->particle_dialog.color_frame = XtVaCreateManagedWidget("color_frame",
							     xmFrameWidgetClass, gui->particle_dialog.main_form,
							     XmNshadowType, XmSHADOW_ETCHED_IN,
							     XmNleftAttachment, XmATTACH_FORM,
							     XmNtopAttachment, XmATTACH_FORM,
							     XmNrightAttachment, XmATTACH_NONE,
							     XmNleftOffset, 5,
							     XmNtopOffset, 5,
							     XmNbottomOffset, 5,
							     NULL);
  gui->particle_dialog.c_label = XtVaCreateManagedWidget ("Line Color",xmLabelWidgetClass, 
							  gui->particle_dialog.color_frame,
							  XmNchildType,XmFRAME_TITLE_CHILD,
							  NULL);
  gui->particle_dialog.color_form = XtVaCreateManagedWidget("color_form",
							    xmFormWidgetClass, gui->particle_dialog.color_frame,
							    NULL);
  gui->particle_dialog.line_type_frame = XtVaCreateManagedWidget("line_type_frame",
								 xmFrameWidgetClass, gui->particle_dialog.main_form,
								 XmNshadowType, XmSHADOW_ETCHED_IN,
								 XmNleftAttachment, XmATTACH_WIDGET,
								 XmNleftWidget, gui->particle_dialog.color_frame,
								 XmNtopAttachment, XmATTACH_FORM,
								 XmNrightAttachment, XmATTACH_FORM,
								 XmNbottomAttachment, XmATTACH_FORM,
								 XmNrightOffset, 5,
								 XmNleftOffset, 5,
								 XmNtopOffset, 5,
								 XmNbottomOffset, 5,
								 NULL);
  gui->particle_dialog.lt_label = XtVaCreateManagedWidget ("Line Type",xmLabelWidgetClass, 
							   gui->particle_dialog.line_type_frame,
							   XmNchildType,XmFRAME_TITLE_CHILD,
							   NULL);
  gui->particle_dialog.line_type_form = XtVaCreateManagedWidget("line_type_form",
								xmFormWidgetClass, gui->particle_dialog.line_type_frame,
								NULL);
  
  gui->particle_dialog.color_swatch = XtVaCreateManagedWidget("color_swatch",
							      xmDrawnButtonWidgetClass,gui->particle_dialog.color_form,
							      XmNleftAttachment, XmATTACH_FORM,
							      XmNrightAttachment, XmATTACH_FORM,
							      XmNleftOffset, 20,
							      XmNrightOffset, 20,
							      XmNheight, 60,
							      XmNtopAttachment, XmATTACH_FORM,
							      XmNtopOffset, 15,
							      XmNbottomAttachment, XmATTACH_NONE,
							      NULL);
  gui->particle_dialog.r_s = (Widget)XtVaCreateManagedWidget("Rslider",xmScaleWidgetClass,
							     gui->particle_dialog.color_form,
							     XmNrightAttachment, XmATTACH_FORM,
							     XmNrightOffset, 5,
							     XmNleftAttachment, XmATTACH_FORM,
							     XmNleftOffset, 5,
							     XmNtopAttachment, XmATTACH_WIDGET,
							     XmNtopWidget, gui->particle_dialog.color_swatch,
							     XmNtopOffset, 25,
							     XmNbottomAttachment, XmATTACH_NONE,
							     XmNvalue, 0,
							     XmNminimum, 0,
							     XmNmaximum, 65535,
							     XmNscaleHeight, 15,
							     XmNorientation, XmHORIZONTAL,
							     NULL);
  XtAddCallback(gui->particle_dialog.r_s, XmNdragCallback,Line_Color_changedCB, (XtPointer)gui);
  gui->particle_dialog.g_s = (Widget)XtVaCreateManagedWidget("Gslider",xmScaleWidgetClass,
							     gui->particle_dialog.color_form,
							     XmNrightAttachment, XmATTACH_FORM,
							     XmNrightOffset, 5,
							     XmNleftAttachment, XmATTACH_FORM,
							     XmNleftOffset, 5,
							     XmNtopAttachment, XmATTACH_WIDGET,
							     XmNtopWidget, gui->particle_dialog.r_s,
							     XmNtopOffset, 10,
							     XmNbottomAttachment, XmATTACH_NONE,
							     XmNvalue, 0,
							     XmNminimum, 0,
							     XmNmaximum, 65535,
							     XmNscaleHeight, 15,
							     XmNorientation, XmHORIZONTAL,
							     NULL);
  XtAddCallback(gui->particle_dialog.g_s, XmNdragCallback,Line_Color_changedCB, (XtPointer)gui);
  gui->particle_dialog.b_s = (Widget)XtVaCreateManagedWidget("Bslider",xmScaleWidgetClass,
							     gui->particle_dialog.color_form,
							     XmNrightAttachment, XmATTACH_FORM,
							     XmNrightOffset, 5,
							     XmNleftAttachment, XmATTACH_FORM,
							     XmNleftOffset, 5,
							     XmNtopAttachment, XmATTACH_WIDGET,
							     XmNtopWidget, gui->particle_dialog.g_s,
							     XmNtopOffset, 10,
							     XmNbottomAttachment, XmATTACH_FORM,
							     XmNbottomOffset, 10,
							     XmNvalue, 0,
							     XmNminimum, 0,
							     XmNmaximum, 65535,
							     XmNscaleHeight, 15,
							     XmNorientation, XmHORIZONTAL,
							     NULL);				   
  XtAddCallback(gui->particle_dialog.b_s, XmNdragCallback,Line_Color_changedCB, (XtPointer)gui);
  
  gui->particle_dialog.line_type_pane = (Widget)XmCreatePulldownMenu(gui->particle_dialog.line_type_form,"line_type_pane",NULL, 0);
  gui->particle_dialog.line_type_menu = (Widget)XtVaCreateManagedWidget("line_type_menu",xmRowColumnWidgetClass,gui->particle_dialog.line_type_form,
                XmNmarginHeight,       0,
                XmNmarginWidth,        0,
                XmNpacking,            XmPACK_TIGHT,
                XmNpopupEnabled,       TRUE,
                XmNrowColumnType,      XmMENU_OPTION,
                XmNspacing,            0,
                XmNsubMenuId, gui->particle_dialog.line_type_pane,
		XmNtopAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopOffset, 20,
		XmNbottomOffset, 20,
		XmNrightOffset, 20,
		XmNleftOffset, 20,
		NULL);

  pixmap = XCreatePixmapFromBitmapData(gui->display,
				       RootWindowOfScreen(XtScreen(gui->form)),
				       (char *)t1b1_bits, t1b1_width, t1b1_height,
				       WhitePixel(gui->display,DefaultScreen(gui->display)), BlackPixel(gui->display,DefaultScreen(gui->display)),
				       DefaultDepthOfScreen(XtScreen(gui->form)));
  
  gui->particle_dialog.line_t1 = XtVaCreateManagedWidget("line_t1",
							 xmPushButtonWidgetClass,gui->particle_dialog.line_type_pane,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 NULL);
  XtAddCallback(gui->particle_dialog.line_t1,XmNactivateCallback,Line_type_changedCB, (XtPointer)gui);
  pixmap = XCreatePixmapFromBitmapData(XtDisplay(gui->form),
				       RootWindowOfScreen(XtScreen(gui->form)),
				       (char *)t2b1_bits, t2b1_width, t2b1_height,
				       WhitePixel(gui->display,DefaultScreen(gui->display)), BlackPixel(gui->display,DefaultScreen(gui->display)),
				       DefaultDepthOfScreen(XtScreen(gui->form)));
  gui->particle_dialog.line_t2 = XtVaCreateManagedWidget("line_t2",
							 xmPushButtonWidgetClass,gui->particle_dialog.line_type_pane,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 NULL);
  XtAddCallback(gui->particle_dialog.line_t2,XmNactivateCallback,Line_type_changedCB, (XtPointer)gui);
  pixmap = XCreatePixmapFromBitmapData(XtDisplay(gui->form),
				       RootWindowOfScreen(XtScreen(gui->form)),
				       (char *)t3b1_bits, t3b1_width, t3b1_height,
				       WhitePixel(gui->display,DefaultScreen(gui->display)), BlackPixel(gui->display,DefaultScreen(gui->display)),
				       DefaultDepthOfScreen(XtScreen(gui->form)));
  gui->particle_dialog.line_t3 = XtVaCreateManagedWidget("line_t3",
				    xmPushButtonWidgetClass,gui->particle_dialog.line_type_pane,
				    XmNlabelType,        XmPIXMAP,
				    XmNlabelPixmap,      pixmap,
				    NULL);		
  XtAddCallback(gui->particle_dialog.line_t3,XmNactivateCallback,Line_type_changedCB, (XtPointer)gui);  
  pixmap = XCreatePixmapFromBitmapData(XtDisplay(gui->form),
				       RootWindowOfScreen(XtScreen(gui->form)),
				       (char *)t4b1_bits, t4b1_width, t4b1_height,
				       WhitePixel(gui->display,DefaultScreen(gui->display)), BlackPixel(gui->display,DefaultScreen(gui->display)),
				       DefaultDepthOfScreen(XtScreen(gui->form)));
  gui->particle_dialog.line_t4 = XtVaCreateManagedWidget("line_t4",
							 xmPushButtonWidgetClass,gui->particle_dialog.line_type_pane,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 NULL);		
  XtAddCallback(gui->particle_dialog.line_t4,XmNactivateCallback,Line_type_changedCB, (XtPointer)gui);	     

  gui->particle_dialog.divider = (Widget)XtVaCreateManagedWidget("divider",
								 xmSeparatorWidgetClass, gui->particle_dialog.line_type_form,
								 XmNorientation, XmHORIZONTAL,
								 XmNseparatorType, XmSHADOW_ETCHED_OUT,
								 XmNrightAttachment, XmATTACH_FORM,
								 XmNleftAttachment, XmATTACH_FORM,
								 XmNtopAttachment, XmATTACH_WIDGET,
								 XmNtopWidget, gui->particle_dialog.line_type_menu,
								 XmNtopOffset, 15,
								 XmNrightOffset, 5,
								 XmNleftOffset, 5,
								 NULL);

  gui->particle_dialog.bold_menu_label = XtVaCreateManagedWidget("Boldness",
								 xmLabelWidgetClass, gui->particle_dialog.line_type_form,
								 XmNleftAttachment, XmATTACH_FORM,
								 XmNrightAttachment, XmATTACH_FORM,
								 XmNtopAttachment, XmATTACH_WIDGET,
								 XmNtopWidget, gui->particle_dialog.divider,
								 XmNbottomAttachment, XmATTACH_NONE,
								 XmNtopOffset, 5,
								 NULL);
  
  gui->particle_dialog.bold_type_pane = (Widget)XmCreatePulldownMenu(gui->particle_dialog.line_type_form,"bold_type_pane", NULL, 0);

  gui->particle_dialog.bold_type_menu = (Widget)XtVaCreateManagedWidget("bold_type_menu",xmRowColumnWidgetClass,gui->particle_dialog.line_type_form,
                XmNmarginHeight,       0,
                XmNmarginWidth,        0,
                XmNpacking,            XmPACK_TIGHT,
                XmNpopupEnabled,       TRUE,
                XmNrowColumnType,      XmMENU_OPTION,
                XmNspacing,            0,
		XmNsubMenuId, gui->particle_dialog.bold_type_pane,
		XmNtopAttachment, XmATTACH_WIDGET,
    		XmNtopWidget, gui->particle_dialog.divider,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopOffset, 20,
		XmNbottomOffset, 0,
		XmNrightOffset, 20,
		XmNleftOffset, 20,
		NULL);

  pixmap = XCreatePixmapFromBitmapData(gui->display,
				       RootWindowOfScreen(XtScreen(gui->form)),
				       (char *)t1b1_bits, t1b1_width, t1b1_height,
				       WhitePixel(gui->display,DefaultScreen(gui->display)), BlackPixel(gui->display,DefaultScreen(gui->display)),
				       DefaultDepthOfScreen(XtScreen(gui->form)));

  gui->particle_dialog.bold_t1 = XtVaCreateManagedWidget("bold_t1",
							 xmPushButtonWidgetClass,gui->particle_dialog.bold_type_pane,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 NULL);
  XtAddCallback(gui->particle_dialog.bold_t1,XmNactivateCallback,Line_type_changedCB, (XtPointer)gui);
  pixmap = XCreatePixmapFromBitmapData(XtDisplay(gui->form),
				       RootWindowOfScreen(XtScreen(gui->form)),
				       (char *)t1b2_bits, t1b2_width, t1b2_height,
				       WhitePixel(gui->display,DefaultScreen(gui->display)), BlackPixel(gui->display,DefaultScreen(gui->display)),
				       DefaultDepthOfScreen(XtScreen(gui->form)));
  gui->particle_dialog.bold_t2 = XtVaCreateManagedWidget("bold_t2",
							 xmPushButtonWidgetClass,gui->particle_dialog.bold_type_pane,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 NULL);
  XtAddCallback(gui->particle_dialog.bold_t2,XmNactivateCallback,Line_type_changedCB, (XtPointer)gui);
  pixmap = XCreatePixmapFromBitmapData(XtDisplay(gui->form),
				       RootWindowOfScreen(XtScreen(gui->form)),
				       (char *)t1b3_bits, t1b3_width, t1b3_height,
				       WhitePixel(gui->display,DefaultScreen(gui->display)), BlackPixel(gui->display,DefaultScreen(gui->display)),
				       DefaultDepthOfScreen(XtScreen(gui->form)));
  gui->particle_dialog.bold_t3 = XtVaCreateManagedWidget("bold_t3",
							 xmPushButtonWidgetClass, gui->particle_dialog.bold_type_pane,
							 XmNlabelType,        XmPIXMAP,
							 XmNlabelPixmap,      pixmap,
							 NULL);		
  XtAddCallback(gui->particle_dialog.bold_t3,XmNactivateCallback,Line_type_changedCB, (XtPointer)gui);  

  DEBUG_TRACE_OUT printf("Done with build_particle_type_dialog\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : init_line_types
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: fills the line_types structure with colors
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_line_types(main_gui_t *gui)
{
  int i;

  DEBUG_TRACE_IN printf("Entered init_line_types\n");

  for (i = 0; i< NUM_PARTICLES; i++)
    init_color(gui,gui->line_types[i].color.red,
	       gui->line_types[i].color.green,
	       gui->line_types[i].color.blue,
	       &gui->line_types[i].color);		     

  DEBUG_TRACE_OUT printf("Done with init_line_types\n");
}
