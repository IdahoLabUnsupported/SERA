#include "sera3d.h"



int pref_num = 1;
/*Pixmap pref_pixmap;*/
/*Pixel fg, bg;*/
  XmString xmstr;

void build_main_prefs_forms(main_gui_t *gui);
void build_window_size_prefs(main_gui_t *gui);
void build_color_prefs(main_gui_t *gui);
void build_axis_prefs(main_gui_t *gui);
void build_mouse_prefs(main_gui_t *gui);
void build_path_prefs(main_gui_t *gui);
void build_tex_prefs(main_gui_t *gui);
void build_misc_prefs(main_gui_t *gui);
void Pref_switchedCB(Widget w, XtPointer clientData, XtPointer callData);
/*void Color_prefChangedCB(Widget w, XtPointer clientData, XtPointer callData);
  void Color_pref_selectionCB(Widget w, XtPointer clientData, XtPointer callData);*/
void Which_Color_ToggledCB(Widget w, XtPointer clientData, XtPointer callData);
void Preference_Color_ChangedCB(Widget w, XtPointer clientData, XtPointer callData);


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
void SavePreferencesCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  FILE *out;
  int i, num = 650;
  Widget temp;
  Boolean tf;
  XColor color;
  char *R_file = "Sera3d/Sera3d.rsc";
  char Resource_file[256];

 DEBUG_TRACE_IN printf("Entered SavePreferencesCB\n");
 
 get_resource_path_name(Resource_file, R_file);

 printf("wrote the preferences to : %s\n",Resource_file);

 if (!(out = fopen(Resource_file,"w")))
   DT_error(gui->toplevel,"Couldn't open the output file",NULL,NULL);
 else
   {
     fprintf(out,"# Sera3d resources\n");
     
     fprintf(out,"# ***************************************\n");

     XtVaGetValues(gui->preference_dialog.mws_menu, XmNmenuHistory, &temp, NULL);
     if (strstr(XtName(temp),"550")) num = 550;
     else if (strstr(XtName(temp),"600")) num = 600;
     else if (strstr(XtName(temp),"650")) num = 650;
     else if (strstr(XtName(temp),"700")) num = 700;
     else if (strstr(XtName(temp),"750")) num = 750;
     else if (strstr(XtName(temp),"800")) num = 800;
     fprintf(out,"Main_Window_Size: %d", num);
     fprintf(out,"\t# (550, 600, 650, 700, 750, 800)\n");
	  
     XtVaGetValues(gui->preference_dialog.mls_menu, XmNmenuHistory, &temp, NULL);
     if (strstr(XtName(temp),"150")) num = 150;
     else if (strstr(XtName(temp),"175")) num = 175;
     else if (strstr(XtName(temp),"200")) num = 200;
     else if (strstr(XtName(temp),"225")) num = 225;
     else if (strstr(XtName(temp),"250")) num = 250;
     fprintf(out,"Multi_Window_Size: %d", num);
     fprintf(out,"\t# (150, 175, 200, 225, 250)\n");

     XtVaGetValues(gui->preference_dialog.ml_toggle, XmNset, &tf, NULL);
     if (tf == TRUE) fprintf(out,"Multi-View: On ");
     else fprintf(out,"Multi-View: Off ");
     fprintf(out,"\t# (On , Off)\n");

     XtVaGetValues(gui->preference_dialog.back_menu, XmNmenuHistory, &temp, NULL);
     if (strstr(XtName(temp),"Black")) fprintf(out,"Background: black ");
     else if (strstr(XtName(temp),"25")) fprintf(out,"Background: 25 ");
     else if (strstr(XtName(temp),"50")) fprintf(out,"Background: 50 ");
     else if (strstr(XtName(temp),"75")) fprintf(out,"Background: 75 ");
     else  fprintf(out,"Background: white ");
     fprintf(out,"\t# (black,25,50,75, white)\n");  
     
     /************************************************/
     fprintf(out,"# ***************************************\n");
     
     XtVaGetValues(gui->preference_dialog.axis_menu, XmNmenuHistory, &temp, NULL);
     fprintf(out,"Axis_Type: %s ", XtName(temp));
     fprintf(out,"\t\t# (Thin, Thick, Full, off)\n");
     
     XtVaGetValues(gui->preference_dialog.axis_label_toggle, XmNset, &tf, NULL);
     if (tf == TRUE) fprintf(out,"Axis_Labelling: On ");
     else fprintf(out,"Axis_Labelling: Off ");
     fprintf(out,"\t# (On , Off)\n");      
     
     /************************************************/

     fprintf(out,"# ***************************************\n");

     XtVaGetValues(gui->preference_dialog.m_menu, XmNmenuHistory, &temp, NULL);
     if (strstr(XtName(temp),"Pull"))
       fprintf(out,"Mouse_Control: Pull ");
     else if (strstr(XtName(temp),"Sliders"))
       fprintf(out,"Mouse_Control: Sliders ");
     else
       fprintf(out,"Mouse_Control: Buttons ");
     fprintf(out,"\t# (Pull, Sliders, Buttons)\n");     
     
     /************************************************/
     fprintf(out,"# ***************************************\n");    
     
     fprintf(out,"Gamma: \t%d %d %d  %d  %d ",
	     gui->line_types[GAMMA].color.red, gui->line_types[GAMMA].color.green,
	     gui->line_types[GAMMA].color.blue,
	     gui->line_types[GAMMA].type, gui->line_types[GAMMA].boldness);
     fprintf(out,"\t# (r g b  type (1-4)  boldness (1-3))\n");           
     /*
       fprintf(out,"Low_Energy_Gamma: \t%d %d %d  %d  %d ",
       gui->line_types[0].color.red, gui->line_types[0].color.green,
       gui->line_types[0].color.blue,
       gui->line_types[0].type, gui->line_types[0].boldness);
       fprintf(out,"\t# (r g b  type (1-4)  boldness (1-3))\n");           
       
       fprintf(out,"Medium_Energy_Gamma:\t %d %d %d  %d  %d ",
       gui->line_types[1].color.red, gui->line_types[1].color.green,
       gui->line_types[1].color.blue,
       gui->line_types[1].type, gui->line_types[1].boldness);
       fprintf(out,"\t# (r g b  type (1-4)  boldness (1-3))\n");           
       
       fprintf(out,"High_Energy_Gamma:\t %d %d %d  %d  %d ",
       gui->line_types[2].color.red, gui->line_types[2].color.green,
       gui->line_types[2].color.blue,
       gui->line_types[2].type, gui->line_types[2].boldness);
       fprintf(out,"\t# (r g b  type (1-4)  boldness (1-3))\n");           
     */
     
     fprintf(out,"Low_Energy_Neutron: \t%d %d %d  %d  %d ",
	     gui->line_types[NEUTRON_LOW].color.red, gui->line_types[NEUTRON_LOW].color.green,
	     gui->line_types[NEUTRON_LOW].color.blue,
	     gui->line_types[NEUTRON_LOW].type, gui->line_types[NEUTRON_LOW].boldness);
     fprintf(out,"\t# (r g b  type (1-4)  boldness (1-3))\n");           

     fprintf(out,"Medium_Energy_Neutron:\t %d %d %d  %d  %d ",
	     gui->line_types[NEUTRON_MED].color.red, gui->line_types[NEUTRON_MED].color.green,
	     gui->line_types[NEUTRON_MED].color.blue,
	     gui->line_types[NEUTRON_MED].type, gui->line_types[NEUTRON_MED].boldness);
     fprintf(out,"\t# (r g b  type (1-4)  boldness (1-3))\n");           

     fprintf(out,"High_Energy_Neutron:\t %d %d %d  %d  %d ",
	     gui->line_types[NEUTRON_HIGH].color.red,
	     gui->line_types[NEUTRON_HIGH].color.green,
	     gui->line_types[NEUTRON_HIGH].color.blue,
	     gui->line_types[NEUTRON_HIGH].type,
	     gui->line_types[NEUTRON_HIGH].boldness);
     fprintf(out,"\t# (r g b  type (1-4)  boldness (1-3))\n");           

     fprintf(out,"Beam: \t\t\t%d %d %d  %d  %d ",
	     gui->line_types[BEAM].color.red,gui->line_types[BEAM].color.green,
	     gui->line_types[BEAM].color.blue,
	     gui->line_types[BEAM].type,gui->line_types[BEAM].boldness);
     fprintf(out,"# (r g b  type (1-4)  boldness (1-3))\n");           
       
     fprintf(out,"Lost: \t\t\t%d %d %d  %d  %d ",
	     gui->line_types[LOST].color.red, gui->line_types[LOST].color.green,
	     gui->line_types[LOST].color.blue,
	     gui->line_types[LOST].type, gui->line_types[LOST].boldness);
     fprintf(out,"\t# (r g b  type (1-4)  boldness (1-3))\n");           
       
     /************************************************/
     fprintf(out,"# ***************************************\n");    

     for(i=0;i<4;i++){
       XtVaGetValues(gui->preference_dialog.rendering_quality[i], XmNset, &tf, NULL);
       if (tf){
	 switch(i){
	 case 0: fprintf(out,"rendering_quality: low"); break;
	 case 1: fprintf(out,"rendering_quality: medium"); break;
	 case 2: fprintf(out,"rendering_quality: high"); break;
	 case 3: fprintf(out,"rendering_quality: full"); break;	   
	 }
	 fprintf(out,"\t\t\t# (low,medium,high,full)\n");
	 break;
       }
     }
     for(i=0;i<4;i++){
       XtVaGetValues(gui->preference_dialog.poly_rendering_quality[i], XmNset, &tf, NULL);
       if (tf){
	 switch(i){
	 case 0: fprintf(out,"polygon_quality: low"); break;
	 case 1: fprintf(out,"polygon_quality: medium"); break;
	 case 2: fprintf(out,"polygon_quality: high"); break;
	 case 3: fprintf(out,"polygon_quality: full"); break;	   
	 }
	 fprintf(out,"\t\t\t# (low,medium,high,full)\n");
	 break;
       }
     }

     
     XtVaGetValues(gui->preference_dialog.poly_algorithm_menu, XmNmenuHistory, &temp, NULL);
     if (temp == gui->preference_dialog.poly_algorithm_type[0])
       fprintf(out,"polygonal_algorithm: 8cell_surface_normal\n"); 
     else if (temp == gui->preference_dialog.poly_algorithm_type[1])
       fprintf(out,"polygonal_algorithm: 8cell_vertex_normal\n"); 
     else if (temp == gui->preference_dialog.poly_algorithm_type[2])
       fprintf(out,"polygonal_algorithm: marching_cubes\n"); 
     

     XtVaGetValues(gui->preference_dialog.mess_toggle, XmNset, &tf, NULL);
     if (tf == FALSE)
       fprintf(out,"Messages: Off ");
     else
       fprintf(out,"Messages: On ");
     fprintf(out,"\t\t\t# (On , Off)\n");      

     XtVaGetValues(gui->preference_dialog.fr_menu, XmNmenuHistory, &temp, NULL);
     if (strstr(XtName(temp),"Wireframe"))
       fprintf(out,"Fast_Rotation: Wireframe ");
     else if (strstr(XtName(temp),"Axis"))
       fprintf(out,"Fast_Rotation: Axis ");
     else
       fprintf(out,"Fast_Rotation: Off ");
     fprintf(out,"\t\t# (Wireframe, Axis, Off)\n");     
     
     fprintf(out,"Measure_Unit: ");
     XtVaGetValues(gui->preference_dialog.measure[0],XmNset,&tf,NULL);
     if (tf == TRUE) fprintf(out,"cm");
     else fprintf(out,"mm");
     fprintf(out,"\t\t# (cm, mm)\n");

     /************************************************/
     fprintf(out,"# ***************************************\n");    
     fprintf(out,"Texture_Filtering: ");
     XtVaGetValues(gui->preference_dialog.tex_pref[0], XmNset, &tf, NULL);
     if (tf == TRUE) fprintf(out,"nearest");
     else fprintf(out,"linear");
     fprintf(out,"\t# (nearest,linear)\n");

     fprintf(out,"Texture_Mode: ");
     XtVaGetValues(gui->preference_dialog.tex_mod_pref[0], XmNset, &tf, NULL);
     if (tf == TRUE) fprintf(out,"clamp");
     else fprintf(out,"repeat");
     fprintf(out,"\t\t# (clamp,repeat)\n");

     fprintf(out,"Alpha_Culling: ");
     XtVaGetValues(gui->preference_dialog.alpha_cull_toggle, XmNset, &tf, NULL);
     if (tf == TRUE) fprintf(out,"on");
     else fprintf(out,"off");
     fprintf(out,"\t\t# (on,off)\n");


     /************************************************/
     fprintf(out,"# ***************************************\n");    

     fprintf(out,"contour_level: 95 65535 0 0  #(level r g b )\n");
     fprintf(out,"contour_level: 90 0 65535 0 #(level r g b)\n");
     fprintf(out,"contour_level: 80 0 0 65535 #(level r g b)\n");
     fprintf(out,"contour_level: 70 65535 65535 0 #(level r g b)\n");
     fprintf(out,"contour_level: 60 65535 0 65535 #(level r g b)\n");
     fprintf(out,"contour_level: 50 0 65535 65535 #(level r g b)\n");
     
     
     XtVaGetValues(gui->preference_dialog.which_color_toggle, XmNset, &tf, NULL);
     if (tf == TRUE) fprintf(out,"use_uvh_colors: true                   #(true,false)\n");
     else fprintf(out,"use_uvh_colors: false                   #(true,false)\n");

     for (i=1;i<=gui->num_pref_bods;i++)
       {
	 XtVaGetValues(gui->preference_dialog.b_colors[i][2],XmNbackground, &color.pixel, NULL);
	 get_rgb_from_pixel(gui,&color);
	 fprintf(out,"%s_color: %d %d %d %d",
		 gui->pref_bod[i].name,
		 color.red, color.green, 
		 color.blue, gui->pref_bod[i].t);
	 fprintf(out,"\t# (R G B Opacity(0-100))\n");
       }
     

     fclose(out);
   }
 DT_error(gui->toplevel,"Preferences will take effect after program restarts",NULL,NULL);

 DEBUG_TRACE_OUT printf("Done with SavePreferencesCB\n");
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
void Show_preferences_dialogCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  Pixmap pixmap;
  XColor color;
  int i;
 
  DEBUG_TRACE_IN printf("Entered Show_preferences_dialogCB\n");

  /** init the window **/
  switch (gui->mainwindowsize)
    {
    case 550: XtVaSetValues(gui->preference_dialog.mws_menu, XmNmenuHistory, gui->preference_dialog.mws_1, NULL);break;
    case 600: XtVaSetValues(gui->preference_dialog.mws_menu, XmNmenuHistory, gui->preference_dialog.mws_2, NULL);break;
    case 650: XtVaSetValues(gui->preference_dialog.mws_menu, XmNmenuHistory, gui->preference_dialog.mws_3, NULL);break;
    case 700: XtVaSetValues(gui->preference_dialog.mws_menu, XmNmenuHistory, gui->preference_dialog.mws_4, NULL);break;
    case 750: XtVaSetValues(gui->preference_dialog.mws_menu, XmNmenuHistory, gui->preference_dialog.mws_5, NULL);break;
    case 800: XtVaSetValues(gui->preference_dialog.mws_menu, XmNmenuHistory, gui->preference_dialog.mws_6, NULL);break;
    }
  switch (gui->multiwindowsize)
    {
    case 150: XtVaSetValues(gui->preference_dialog.mls_menu, XmNmenuHistory, gui->preference_dialog.mls_1, NULL);break;
    case 175: XtVaSetValues(gui->preference_dialog.mls_menu, XmNmenuHistory, gui->preference_dialog.mls_2, NULL);break;
    case 200: XtVaSetValues(gui->preference_dialog.mls_menu, XmNmenuHistory, gui->preference_dialog.mls_3, NULL);break;
    case 225: XtVaSetValues(gui->preference_dialog.mls_menu, XmNmenuHistory, gui->preference_dialog.mls_4, NULL);break;
    case 250: XtVaSetValues(gui->preference_dialog.mls_menu, XmNmenuHistory, gui->preference_dialog.mls_5, NULL);break;
    }

  if (XtIsManaged(gui->multiview_panel.form))
    XtVaSetValues(gui->preference_dialog.ml_toggle, XmNset, TRUE, NULL);
  else
    XtVaSetValues(gui->preference_dialog.ml_toggle, XmNset, FALSE, NULL);

  if (gui->background == 0.0)
    XtVaSetValues(gui->preference_dialog.back_menu, XmNmenuHistory, gui->preference_dialog.back[0], NULL);
  else if (gui->background == 0.25)
    XtVaSetValues(gui->preference_dialog.back_menu, XmNmenuHistory, gui->preference_dialog.back[1], NULL);
  else if (gui->background == 0.5)
    XtVaSetValues(gui->preference_dialog.back_menu, XmNmenuHistory, gui->preference_dialog.back[2], NULL);
  else if (gui->background == 0.75)
    XtVaSetValues(gui->preference_dialog.back_menu, XmNmenuHistory, gui->preference_dialog.back[3], NULL);
  else
    XtVaSetValues(gui->preference_dialog.back_menu, XmNmenuHistory, gui->preference_dialog.back[4], NULL);
  
  /** init the axis **/
  switch(gui->axis_type)
    {
    case 0: XtVaSetValues(gui->preference_dialog.axis_menu, XmNmenuHistory, gui->preference_dialog.a_5, NULL);break;
    case 1: XtVaSetValues(gui->preference_dialog.axis_menu, XmNmenuHistory, gui->preference_dialog.a_1, NULL);break;
    case 2: XtVaSetValues(gui->preference_dialog.axis_menu, XmNmenuHistory, gui->preference_dialog.a_2, NULL);break;
    case 3: XtVaSetValues(gui->preference_dialog.axis_menu, XmNmenuHistory, gui->preference_dialog.a_4, NULL);break;
    case 4: XtVaSetValues(gui->preference_dialog.axis_menu, XmNmenuHistory, gui->preference_dialog.a_3, NULL);break;
    }
  if (gui->axis_labels_on)  XtVaSetValues(gui->preference_dialog.axis_label_toggle, XmNset, TRUE, NULL);
  else                      XtVaSetValues(gui->preference_dialog.axis_label_toggle, XmNset, FALSE, NULL);


  switch(gui->mouse_control_method)
    {
    case SLIDER:  XtVaSetValues(gui->preference_dialog.m_menu, XmNmenuHistory, gui->preference_dialog.m_2, NULL);break;
    case MOUSE:   XtVaSetValues(gui->preference_dialog.m_menu, XmNmenuHistory, gui->preference_dialog.m_3, NULL);break;
    }


  /** init the paths **/  
  for (i = 0; i< NUM_PARTICLES; i++){
    pixmap = get_pixmap_from_line_type(gui,gui->line_types[i].color.pixel, gui->line_types[i].type,
				       gui->line_types[i].boldness);

    XtVaSetValues(gui->preference_dialog.colorbx[i], XmNlabelPixmap, pixmap, NULL);
    
    /*
      switch (i)
      {
      case 0: XtVaSetValues(gui->preference_dialog.gl, XmNlabelPixmap, pixmap, NULL);break;
      case 1: XtVaSetValues(gui->preference_dialog.gm, XmNlabelPixmap, pixmap, NULL);break;
      case 2: XtVaSetValues(gui->preference_dialog.gh, XmNlabelPixmap, pixmap, NULL);break;
      case 3: XtVaSetValues(gui->preference_dialog.nl, XmNlabelPixmap, pixmap, NULL);break;
      case 4: XtVaSetValues(gui->preference_dialog.nm, XmNlabelPixmap, pixmap, NULL);break;
      case 5: XtVaSetValues(gui->preference_dialog.nh, XmNlabelPixmap, pixmap, NULL);break;
      case 6: XtVaSetValues(gui->preference_dialog.beam, XmNlabelPixmap, pixmap, NULL);break;
      case 7: XtVaSetValues(gui->preference_dialog.lost, XmNlabelPixmap, pixmap, NULL);break;
      }
    */
  }

  
  if (gui->use_uvh_colors){
    XtVaSetValues(gui->preference_dialog.which_color_toggle, XmNset, TRUE, NULL);
    XtUnmanageChild(gui->preference_dialog.s_window);
  }else{
    XtVaSetValues(gui->preference_dialog.which_color_toggle, XmNset, FALSE, NULL);
    XtManageChild(gui->preference_dialog.s_window);
  }
  for (i=1;i<=gui->num_pref_bods;i++){
    init_color(gui,gui->pref_bod[i].r,
	       gui->pref_bod[i].g,
	       gui->pref_bod[i].b, &color);
    XtVaSetValues(gui->preference_dialog.b_colors[i][2], XmNbackground, color.pixel,NULL);
  }
  /*
  XmScaleSetValue(gui->preference_dialog.rcolor_s, gui->pref_bod[1].r);
  XmScaleSetValue(gui->preference_dialog.gcolor_s, gui->pref_bod[1].g);
  XmScaleSetValue(gui->preference_dialog.bcolor_s, gui->pref_bod[1].b);
  XmScaleSetValue(gui->preference_dialog.tcolor_s, gui->pref_bod[1].t);
  */  

  /** init the misc **/
  /*XmScaleSetValue(gui->preference_dialog.ss_slider, (int)(gui->scaling * 100.0));*/

  switch(gui->rendering_quality){
  case 1: XtVaSetValues(gui->preference_dialog.rendering_quality[3], XmNset, TRUE, NULL);break;
  case 2: XtVaSetValues(gui->preference_dialog.rendering_quality[2], XmNset, TRUE, NULL);break;
  case 3: XtVaSetValues(gui->preference_dialog.rendering_quality[1], XmNset, TRUE, NULL);break;
  case 4: XtVaSetValues(gui->preference_dialog.rendering_quality[0], XmNset, TRUE, NULL);break;
  }

  switch(gui->polygonal_rendering_quality){
  case 1: XtVaSetValues(gui->preference_dialog.poly_rendering_quality[3], XmNset, TRUE, NULL);break;
  case 2: XtVaSetValues(gui->preference_dialog.poly_rendering_quality[2], XmNset, TRUE, NULL);break;
  case 3: XtVaSetValues(gui->preference_dialog.poly_rendering_quality[1], XmNset, TRUE, NULL);break;
  case 4: XtVaSetValues(gui->preference_dialog.poly_rendering_quality[0], XmNset, TRUE, NULL);break;
  }
  
  XtVaSetValues(gui->preference_dialog.poly_algorithm_menu, 
		XmNmenuHistory, gui->preference_dialog.poly_algorithm_type[(int)gui->polygonal_algorithm],
		NULL);
  

  if(gui->messages_on) XtVaSetValues(gui->preference_dialog.mess_toggle, XmNset, TRUE, NULL);
  else XtVaSetValues(gui->preference_dialog.mess_toggle, XmNset, FALSE, NULL);
  
  switch(gui->fast_rotation_type)
    {
    case 1: XtVaSetValues(gui->preference_dialog.fr_menu, XmNmenuHistory, gui->preference_dialog.fr_1, NULL);break;
    case 2: XtVaSetValues(gui->preference_dialog.fr_menu, XmNmenuHistory, gui->preference_dialog.fr_2, NULL);break;
    case 3: XtVaSetValues(gui->preference_dialog.fr_menu, XmNmenuHistory, gui->preference_dialog.fr_3, NULL);break;      
    }

  XtManageChild(gui->preference_dialog.dialog);
  
  if (gui->cm) XtVaSetValues(gui->preference_dialog.measure[0],XmNset, TRUE, NULL);
  else XtVaSetValues(gui->preference_dialog.measure[1],XmNset, TRUE, NULL);
  
  /*********texture****************/
  if (gui->texture_nearest){ 
    XtVaSetValues(gui->preference_dialog.tex_pref[0], XmNset, TRUE, NULL);
    XtVaSetValues(gui->preference_dialog.tex_pref[1], XmNset, FALSE, NULL);
  }else{ 
    XtVaSetValues(gui->preference_dialog.tex_pref[1],XmNset, TRUE, NULL);
    XtVaSetValues(gui->preference_dialog.tex_pref[0],XmNset, FALSE, NULL);
  }
  if (gui->texture_clamp){ 
    XtVaSetValues(gui->preference_dialog.tex_mod_pref[1], XmNset, TRUE, NULL);
    XtVaSetValues(gui->preference_dialog.tex_mod_pref[0], XmNset, FALSE, NULL);
  }else{
    XtVaSetValues(gui->preference_dialog.tex_mod_pref[0],XmNset, TRUE, NULL);
    XtVaSetValues(gui->preference_dialog.tex_mod_pref[1], XmNset, FALSE, NULL);
  }
  if (gui->alpha_culling_on) 
    XtVaSetValues(gui->preference_dialog.alpha_cull_toggle, XmNset, TRUE,NULL);
  else
    XtVaSetValues(gui->preference_dialog.alpha_cull_toggle, XmNset, FALSE,NULL); 

 DEBUG_TRACE_OUT printf("Done with Show_preferences_dialogCB\n");
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
void build_preferences_dialog(main_gui_t *gui)
{

  DEBUG_TRACE_IN printf("Entered build_preferences_dialog\n");

  gui->preference_dialog.dialog = (Widget)XmCreateMessageDialog(gui->toplevel, "Preferences",NULL,0);
  XtVaSetValues( XtParent( gui->preference_dialog.dialog ),
                 XmNtitle, "Preferences",
                 NULL );
    
  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->preference_dialog.dialog,XmDIALOG_SYMBOL_LABEL));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->preference_dialog.dialog,XmDIALOG_MESSAGE_LABEL));
  
  xmstr = XmStringCreateLocalized("Save");
  XtVaSetValues((Widget)XmMessageBoxGetChild(gui->preference_dialog.dialog,XmDIALOG_OK_BUTTON),
	        XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);

  XtAddCallback(gui->preference_dialog.dialog, XmNokCallback, SavePreferencesCB, (XtPointer)gui);
		
  gui->preference_dialog.main_form = XtVaCreateManagedWidget ("main_form", 
				       xmFormWidgetClass, gui->preference_dialog.dialog, 
				       NULL);
  build_main_prefs_forms(gui);
  
  build_window_size_prefs(gui);
  build_color_prefs(gui);
  build_axis_prefs(gui);
  build_mouse_prefs(gui);
  build_path_prefs(gui);
  build_tex_prefs(gui);
  build_misc_prefs(gui); 


  DEBUG_TRACE_OUT printf("Done with build_preferences_dialog\n");
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
void build_tex_prefs(main_gui_t *gui)
{

  DEBUG_TRACE_IN printf("Entered build_tex_prefs\n");

  gui->preference_dialog.tex_frame = XtVaCreateManagedWidget("Frame",
					   xmFrameWidgetClass, gui->preference_dialog.tex_prefs_form,
					   XmNshadowType, XmSHADOW_ETCHED_IN,
					   XmNleftAttachment, XmATTACH_FORM,
					   XmNrightAttachment, XmATTACH_FORM,
					   XmNtopAttachment, XmATTACH_FORM,
					   XmNleftOffset, 5,
					   XmNrightOffset, 5,
					   XmNtopOffset, 5,
					   NULL);

  gui->preference_dialog.tex_rowcol = XmCreateRadioBox(gui->preference_dialog.tex_frame, "texrowcol",NULL, 0);
  XtVaSetValues(gui->preference_dialog.tex_rowcol,XmNnumColumns, 2,NULL);
  XtManageChild(gui->preference_dialog.tex_rowcol);
  gui->preference_dialog.tex_pref[0] = (Widget)XtCreateManagedWidget("NEAREST",
								     xmToggleButtonWidgetClass, gui->preference_dialog.tex_rowcol,
								     NULL, 0);
  gui->preference_dialog.tex_pref[1] = (Widget)XtCreateManagedWidget(
				   "LINEAR",
				   xmToggleButtonWidgetClass, gui->preference_dialog.tex_rowcol,
				   NULL, 0);

  gui->preference_dialog.tex_mod_frame = XtVaCreateManagedWidget("Frame",
				       xmFrameWidgetClass, gui->preference_dialog.tex_prefs_form,
				       XmNshadowType, XmSHADOW_ETCHED_IN,
				       XmNleftAttachment, XmATTACH_FORM,
				       XmNrightAttachment, XmATTACH_FORM,
				       XmNtopAttachment, XmATTACH_WIDGET,
				       XmNtopWidget, gui->preference_dialog.tex_frame,
				       XmNleftOffset, 5,
				       XmNrightOffset, 5,
				       XmNtopOffset, 5,
				       NULL);
  gui->preference_dialog.tex_mod_rowcol = XmCreateRadioBox(gui->preference_dialog.tex_mod_frame, "texrowcol",NULL, 0);
  XtVaSetValues(gui->preference_dialog.tex_mod_rowcol,XmNnumColumns, 2,NULL);
  XtManageChild(gui->preference_dialog.tex_mod_rowcol);
  gui->preference_dialog.tex_mod_pref[0] = (Widget)XtCreateManagedWidget("REPEAT",
									 xmToggleButtonWidgetClass, gui->preference_dialog.tex_mod_rowcol,
									 NULL, 0);
  gui->preference_dialog.tex_mod_pref[1] = (Widget)XtCreateManagedWidget("CLAMP",
									 xmToggleButtonWidgetClass, gui->preference_dialog.tex_mod_rowcol,
									 NULL, 0);
  
  gui->preference_dialog.alpha_cull_frame = XtVaCreateManagedWidget("Frame",
								    xmFrameWidgetClass, gui->preference_dialog.tex_prefs_form,
								    XmNshadowType, XmSHADOW_ETCHED_IN,
								    XmNleftAttachment, XmATTACH_FORM,
								    XmNrightAttachment, XmATTACH_FORM,
								    XmNtopAttachment, XmATTACH_WIDGET,
								    XmNtopWidget, gui->preference_dialog.tex_mod_frame,
								    XmNleftOffset, 5,
								    XmNrightOffset, 5,
								    XmNtopOffset, 5,
								    NULL);
  gui->preference_dialog.alpha_cull_form = XtVaCreateManagedWidget("Alpha_cul_form",
								   xmFormWidgetClass,gui->preference_dialog.alpha_cull_frame,
								   NULL);
  
  gui->preference_dialog.alpha_cull_toggle = XtVaCreateManagedWidget("Alpha Culling",
								     xmToggleButtonWidgetClass,gui->preference_dialog.alpha_cull_form,
								     XmNleftAttachment, XmATTACH_FORM,
								     XmNtopAttachment, XmATTACH_FORM,
								     XmNset, TRUE,
								     NULL);
  /*
    alpha_cull_slider = (Widget)XtVaCreateManagedWidget(
    "a_slider",xmScaleWidgetClass,
    alpha_cull_form,
    XmNrightAttachment, XmATTACH_NONE,
    XmNleftAttachment, XmATTACH_WIDGET,  
    XmNleftWidget, alpha_cull_toggle,
    XmNleftOffset, 5,
    XmNshowValue, TRUE,
    XmNtopAttachment, XmATTACH_FORM,
    XmNbottomAttachment, XmATTACH_FORM,
    XmNtopOffset, 5,
    XmNvalue, 50,
    XmNminimum, 0,
    XmNmaximum, 256,
    XmNscaleWidth, 150,
    XmNscaleHeight, 10,
    XmNshowArrows, TRUE,
    XmNorientation, XmHORIZONTAL,
    NULL);
  */
  DEBUG_TRACE_OUT printf("Done with build_tex_prefs\n");
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
void build_color_prefs(main_gui_t *gui)
{
  XmString xmstr;
  int i;

  DEBUG_TRACE_IN printf("Entered build_color_prefs\n");

  gui->preference_dialog.which_color_toggle = XtVaCreateManagedWidget("Use Colors in UVH file for Bodies",
								      xmToggleButtonWidgetClass,gui->preference_dialog.color_prefs_form,
								      XmNtopAttachment, XmATTACH_FORM,
								      XmNleftAttachment, XmATTACH_FORM,
								      XmNtopOffset, 15,
								      XmNleftOffset, 15,								     
								      NULL);
  XtAddCallback(gui->preference_dialog.which_color_toggle, XmNvalueChangedCallback,Which_Color_ToggledCB, (XtPointer)gui);

  gui->preference_dialog.s_window = XtVaCreateManagedWidget("Scrolled Win",
							    xmScrolledWindowWidgetClass, gui->preference_dialog.color_prefs_form,
							    XmNscrollingPolicy, XmAUTOMATIC,
							    XmNscrollBarDisplayPolicy, XmSTATIC,
							    XmNtopAttachment, XmATTACH_WIDGET,
							    XmNtopWidget, gui->preference_dialog.which_color_toggle,
							    XmNwidth, 250,
							    XmNheight, 300,
							    NULL);
  gui->preference_dialog.s_window_form = XtVaCreateManagedWidget("scroll_win_form",
								 xmFormWidgetClass, gui->preference_dialog.s_window,
								 XmNleftAttachment, XmATTACH_FORM,
								 XmNrightAttachment, XmATTACH_FORM,
								 XmNbottomAttachment, XmATTACH_FORM,
								 XmNtopAttachment, XmATTACH_FORM,
								 NULL);
  
  for (i = 1;i<gui->num_pref_bods;i++)
    {
      if (i>1)
	{
	  xmstr = XmStringCreateLocalized(gui->pref_bod[i].name);
	  gui->preference_dialog.b_colors[i][1] = XtVaCreateManagedWidget(gui->pref_bod[i].name,
									  xmLabelWidgetClass, gui->preference_dialog.s_window_form,
									  XmNleftAttachment, XmATTACH_FORM,
									  XmNtopAttachment, XmATTACH_WIDGET,
									  XmNtopWidget, gui->preference_dialog.b_colors[i-1][1],
									  XmNrightAttachment, XmATTACH_NONE,
									  XmNbottomAttachment, XmATTACH_NONE,
									  XmNlabelString, xmstr,
									  XmNleftOffset, 25,
									  XmNtopOffset, 15,
									  NULL);
	  XmStringFree(xmstr);
	  gui->preference_dialog.b_colors[i][2] = XtVaCreateManagedWidget(gui->pref_bod[i].name,
									  xmDrawnButtonWidgetClass, gui->preference_dialog.s_window_form,
									  XmNleftAttachment, XmATTACH_FORM,
									  XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
									  XmNtopWidget, gui->preference_dialog.b_colors[i][1],
									  XmNrightAttachment, XmATTACH_NONE,
									  XmNbottomAttachment, XmATTACH_NONE,
									  XmNleftOffset, 150,
									  XmNtopOffset, 0,
									  XmNheight, 20,
									  XmNwidth, 40,
									  NULL);
	  XtAddCallback(gui->preference_dialog.b_colors[i][2],XmNactivateCallback, (XtCallbackProc)Preference_Color_ChangedCB, (XtPointer)gui);

	  /*XtAddCallback(gui->preference_dialog.b_colors[i][2],XmNactivateCallback, (XtCallbackProc)Color_pref_selectionCB, (XtPointer)gui);*/
	}
      else{
	xmstr = XmStringCreateLocalized(gui->pref_bod[i].name);
	gui->preference_dialog.b_colors[i][1] = XtVaCreateManagedWidget(gui->pref_bod[i].name,
									xmLabelWidgetClass, gui->preference_dialog.s_window_form,
									XmNleftAttachment, XmATTACH_FORM,
									XmNtopAttachment, XmATTACH_FORM,
									XmNrightAttachment, XmATTACH_NONE,
									XmNbottomAttachment, XmATTACH_NONE,
									XmNlabelString, xmstr,
									XmNleftOffset, 25,
									XmNtopOffset, 15,
									NULL);
	XmStringFree(xmstr);
	gui->preference_dialog.b_colors[i][2] = XtVaCreateManagedWidget(gui->pref_bod[i].name,
									xmDrawnButtonWidgetClass, gui->preference_dialog.s_window_form,
									XmNleftAttachment, XmATTACH_FORM,
									XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
									XmNtopWidget,gui->preference_dialog.b_colors[i][1],
									XmNrightAttachment, XmATTACH_NONE,
									XmNbottomAttachment, XmATTACH_NONE,
									XmNleftOffset, 150,
									XmNtopOffset, 0,
									XmNheight, 20,
									XmNwidth, 40,
									NULL);
	XtAddCallback(gui->preference_dialog.b_colors[i][2],XmNactivateCallback, (XtCallbackProc)Preference_Color_ChangedCB, (XtPointer)gui);
	/*XtAddCallback(gui->preference_dialog.b_colors[i][2],XmNactivateCallback, (XtCallbackProc)Color_pref_selectionCB, (XtPointer)gui);*/
	}
    }
  
  xmstr = XmStringCreateLocalized(gui->pref_bod[i].name);
  gui->preference_dialog.b_colors[i][1] = XtVaCreateManagedWidget(gui->pref_bod[i].name,
								  xmLabelWidgetClass, gui->preference_dialog.s_window_form,
								  XmNleftAttachment, XmATTACH_FORM,
								  XmNtopAttachment, XmATTACH_WIDGET,
								  XmNtopWidget, gui->preference_dialog.b_colors[i-1][1],
								  XmNrightAttachment, XmATTACH_NONE,
								  XmNbottomAttachment, XmATTACH_NONE,
								  XmNlabelString, xmstr,
								  XmNleftOffset, 25,
								  XmNtopOffset, 15,
								  NULL);
  XmStringFree(xmstr);
  gui->preference_dialog.b_colors[i][2] = XtVaCreateManagedWidget(gui->pref_bod[i].name,
								  xmDrawnButtonWidgetClass, gui->preference_dialog.s_window_form,
								  XmNleftAttachment, XmATTACH_FORM,
								  XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
								  XmNtopWidget, gui->preference_dialog.b_colors[i][1],
								  XmNrightAttachment, XmATTACH_NONE,
								  XmNbottomAttachment, XmATTACH_FORM,
								  XmNleftOffset, 150,
								  XmNtopOffset, 0,
								  XmNbottomOffset, 15,
								  XmNheight, 20,
								  XmNwidth, 40,
								  NULL);
  XtAddCallback(gui->preference_dialog.b_colors[i][2],XmNactivateCallback, (XtCallbackProc)Preference_Color_ChangedCB, (XtPointer)gui);
  /*XtAddCallback(gui->preference_dialog.b_colors[i][2],XmNactivateCallback, (XtCallbackProc)Color_pref_selectionCB, (XtPointer)gui);*/
  
  
  
  /*  
  init_color(gui,65535, 0,0, &color);
  gui->preference_dialog.rcolor_s = (Widget)XtVaCreateManagedWidget("Rslider",xmScaleWidgetClass,
								    gui->preference_dialog.color_prefs_form,
								    XmNrightAttachment, XmATTACH_NONE,
								    XmNleftAttachment, XmATTACH_WIDGET,
								    XmNleftWidget, gui->preference_dialog.s_window,
								    XmNleftOffset, 50,
								    XmNtopAttachment, XmATTACH_FORM,
								    XmNtopOffset, 75,
								    XmNbottomAttachment, XmATTACH_NONE,
								    XmNvalue, 0,
								    XmNminimum, 0,
								    XmNmaximum, 65535,
								    XmNscaleWidth, 15,
								    XmNscaleHeight, 140,
								    XmNorientation, XmVERTICAL,
								    XmNtopShadowColor, color.pixel,
								    XmNhighlightColor, color.pixel,
								    NULL);
    init_color(gui,0, 65535,0, &color);
  gui->preference_dialog.gcolor_s = (Widget)XtVaCreateManagedWidget("Gslider",xmScaleWidgetClass,
								    gui->preference_dialog.color_prefs_form,
								    XmNrightAttachment, XmATTACH_NONE,
								    XmNleftAttachment, XmATTACH_WIDGET,
								    XmNleftWidget, gui->preference_dialog.rcolor_s,
								    XmNleftOffset, 20,
								    XmNtopAttachment, XmATTACH_FORM,
								    XmNtopOffset, 75,
								    XmNbottomAttachment, XmATTACH_NONE,
								    XmNvalue, 0,
								    XmNminimum, 0,
								    XmNmaximum, 65535,
								    XmNscaleWidth, 15,
								    XmNscaleHeight, 140,
								    XmNorientation, XmVERTICAL,
								    XmNtopShadowColor, color.pixel,
								    XmNhighlightColor, color.pixel,
								    NULL);
  init_color(gui,0,0,65535, &color);
  gui->preference_dialog.bcolor_s = (Widget)XtVaCreateManagedWidget("Bslider",xmScaleWidgetClass,
								    gui->preference_dialog.color_prefs_form,
								    XmNrightAttachment, XmATTACH_NONE,
								    XmNleftAttachment, XmATTACH_WIDGET,
								    XmNleftWidget, gui->preference_dialog.gcolor_s,
								    XmNleftOffset, 20,
								    XmNtopAttachment, XmATTACH_FORM,
								    XmNtopOffset, 75,
								    XmNbottomAttachment, XmATTACH_NONE,
								    XmNvalue, 0,
								    XmNminimum, 0,
								    XmNmaximum, 65535,
								    XmNscaleWidth, 15,
								    XmNscaleHeight, 140,
								    XmNorientation, XmVERTICAL,
								    XmNtopShadowColor, color.pixel,
								    XmNhighlightColor, color.pixel,
								    NULL);
  init_color(gui,40000,40000,40000, &color);
  gui->preference_dialog.tcolor_s = (Widget)XtVaCreateManagedWidget("Tslider",xmScaleWidgetClass,
								    gui->preference_dialog.color_prefs_form,
								    XmNrightAttachment, XmATTACH_NONE,
								    XmNleftAttachment, XmATTACH_WIDGET,
								    XmNleftWidget, gui->preference_dialog.bcolor_s,
								    XmNleftOffset, 35,
								    XmNtopAttachment, XmATTACH_FORM,
								    XmNtopOffset, 75,
								    XmNbottomAttachment, XmATTACH_NONE,
								    XmNvalue, 0,
								    XmNminimum, 0,
								    XmNmaximum, 100,
								    XmNscaleWidth, 15,
								    XmNscaleHeight, 140,
								    XmNorientation, XmVERTICAL,
								    XmNtopShadowColor, color.pixel,
								    XmNhighlightColor, color.pixel,
								    NULL);
  XtVaCreateManagedWidget("Opacity", 
			  xmLabelWidgetClass, gui->preference_dialog.color_prefs_form,
			  XmNleftAttachment, XmATTACH_WIDGET,
			  XmNleftWidget, gui->preference_dialog.bcolor_s,
			  XmNleftOffset, 10,
			  XmNtopAttachment, XmATTACH_WIDGET,
			  XmNtopWidget, gui->preference_dialog.tcolor_s,
			  XmNtopOffset, 10,
			  NULL);

  XtAddCallback(gui->preference_dialog.rcolor_s, XmNdragCallback, Color_prefChangedCB, (XtPointer)gui);
  XtAddCallback(gui->preference_dialog.rcolor_s, XmNvalueChangedCallback,Color_prefChangedCB, (XtPointer)gui);
  XtAddCallback(gui->preference_dialog.gcolor_s, XmNdragCallback,  Color_prefChangedCB, (XtPointer)gui);
  XtAddCallback(gui->preference_dialog.gcolor_s, XmNvalueChangedCallback, Color_prefChangedCB, (XtPointer)gui);
  XtAddCallback(gui->preference_dialog.bcolor_s, XmNdragCallback, Color_prefChangedCB, (XtPointer)gui);
  XtAddCallback(gui->preference_dialog.bcolor_s, XmNvalueChangedCallback, Color_prefChangedCB, (XtPointer)gui);
  XtAddCallback(gui->preference_dialog.tcolor_s, XmNdragCallback, Color_prefChangedCB, (XtPointer)gui);
  XtAddCallback(gui->preference_dialog.tcolor_s, XmNvalueChangedCallback, Color_prefChangedCB, (XtPointer)gui);
  */


  DEBUG_TRACE_OUT printf("Done with build_color_prefs\n");
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
void build_path_prefs(main_gui_t *gui)
{
  XmString xmstr;
  
  DEBUG_TRACE_IN printf("Entered build_path_prefs\n");
  
  xmstr = XmStringCreateLocalized("Low Energy Gamma");
  gui->preference_dialog.label[GAMMA] = XtVaCreateManagedWidget("LEG",
								xmLabelWidgetClass, gui->preference_dialog.path_prefs_form,
								XmNleftAttachment, XmATTACH_FORM,
								XmNtopAttachment, XmATTACH_FORM,
								XmNrightAttachment, XmATTACH_NONE,
								XmNbottomAttachment, XmATTACH_NONE,
								XmNlabelString, xmstr,
								XmNleftOffset, 10,
								XmNtopOffset, 14,
								NULL);
  XmStringFree(xmstr);

  gui->preference_dialog.colorbx[GAMMA] = XtVaCreateManagedWidget("Pref_gamma",
								  xmDrawnButtonWidgetClass, gui->preference_dialog.path_prefs_form,
								  XmNleftAttachment, XmATTACH_NONE,
								  XmNtopAttachment, XmATTACH_FORM,
								  XmNrightAttachment, XmATTACH_FORM,
								  XmNbottomAttachment, XmATTACH_NONE,
								  XmNrightOffset, 50,
								  XmNtopOffset, 10,
								  XmNlabelType, XmPIXMAP,
								  XmNheight, 25,
								  XmNwidth, 60,
								  NULL);
  XtAddCallback(gui->preference_dialog.colorbx[GAMMA],XmNactivateCallback,Show_Particle_type_dialogCB, (XtPointer)gui);
  /*
    xmstr = XmStringCreateLocalized("Medium Energy Gamma");
    gui->preference_dialog.gm_label = XtVaCreateManagedWidget("MEG",
    xmLabelWidgetClass, gui->preference_dialog.path_prefs_form,
    XmNleftAttachment, XmATTACH_FORM,
    XmNtopAttachment, XmATTACH_WIDGET,
    XmNtopWidget, gui->preference_dialog.gl_label,
    XmNrightAttachment, XmATTACH_NONE,
    XmNbottomAttachment, XmATTACH_NONE,
    XmNlabelString, xmstr,
    XmNleftOffset, 10,
    XmNtopOffset, 20,
    NULL);
    XmStringFree(xmstr);
    gui->preference_dialog.gm = XtVaCreateManagedWidget("gm",
    xmDrawnButtonWidgetClass, gui->preference_dialog.path_prefs_form,
    XmNleftAttachment, XmATTACH_NONE,
    XmNtopAttachment, XmATTACH_WIDGET,
    XmNtopWidget, gui->preference_dialog.gl,
    XmNrightAttachment, XmATTACH_FORM,
    XmNbottomAttachment, XmATTACH_NONE,
    XmNrightOffset, 50,
    XmNtopOffset, 10,
    XmNlabelType, XmPIXMAP,
    XmNheight, 25,
    XmNwidth, 60,
    NULL);
    XtAddCallback(gui->preference_dialog.gm,XmNactivateCallback,Show_Particle_type_dialogCB, (XtPointer)gui);
    xmstr = XmStringCreateLocalized("High Energy Gamma");
    XmStringFree(xmstr);
    
    xmstr = XmStringCreateLocalized("High Energy Gamma");
    gui->preference_dialog.gh_label = XtVaCreateManagedWidget("HEG",
    xmLabelWidgetClass, gui->preference_dialog.path_prefs_form,
    XmNleftAttachment, XmATTACH_FORM,
    XmNtopAttachment, XmATTACH_WIDGET,
    XmNtopWidget, gui->preference_dialog.gm_label,
    XmNrightAttachment, XmATTACH_NONE,
    XmNbottomAttachment, XmATTACH_NONE,
    XmNlabelString, xmstr,
    XmNleftOffset, 10,
    XmNtopOffset, 20,
    NULL);
    gui->preference_dialog.gh = XtVaCreateManagedWidget("gh",
    xmDrawnButtonWidgetClass, gui->preference_dialog.path_prefs_form,
    XmNleftAttachment, XmATTACH_NONE,
    XmNtopAttachment, XmATTACH_WIDGET,
    XmNtopWidget, gui->preference_dialog.gm,
    XmNrightAttachment, XmATTACH_FORM,
    XmNbottomAttachment, XmATTACH_NONE,
    XmNrightOffset, 50,
    XmNtopOffset, 10,
    XmNlabelType, XmPIXMAP,
    XmNheight, 25,
    XmNwidth, 60,
    NULL);
    XtAddCallback(gui->preference_dialog.gh,XmNactivateCallback, Show_Particle_type_dialogCB, (XtPointer)gui);
    XmStringFree(xmstr);
  */

    xmstr = XmStringCreateLocalized("Low Energy Neutron");
    gui->preference_dialog.label[NEUTRON_LOW] = XtVaCreateManagedWidget("LEN",
									xmLabelWidgetClass, gui->preference_dialog.path_prefs_form,
									XmNleftAttachment, XmATTACH_FORM,
									XmNtopAttachment, XmATTACH_WIDGET,
									XmNtopWidget, gui->preference_dialog.label[GAMMA],
									XmNrightAttachment, XmATTACH_NONE,
									XmNbottomAttachment, XmATTACH_NONE,
									XmNlabelString, xmstr,
									XmNleftOffset, 10,
									XmNtopOffset, 25,
									NULL);
    gui->preference_dialog.colorbx[NEUTRON_LOW] = XtVaCreateManagedWidget("Pref_neutron_low",
									  xmDrawnButtonWidgetClass, gui->preference_dialog.path_prefs_form,
									  XmNleftAttachment, XmATTACH_NONE,
									  XmNtopAttachment, XmATTACH_WIDGET,
									  XmNtopWidget, gui->preference_dialog.colorbx[GAMMA],
									  XmNrightAttachment, XmATTACH_FORM,
									  XmNbottomAttachment, XmATTACH_NONE,
									  XmNrightOffset, 50,
									  XmNtopOffset, 10,
									  XmNlabelType, XmPIXMAP,
									  XmNheight, 25,
									  XmNwidth, 60,
									  NULL);
    XtAddCallback(gui->preference_dialog.colorbx[NEUTRON_LOW],XmNactivateCallback, Show_Particle_type_dialogCB, (XtPointer)gui);
    XmStringFree(xmstr);

    xmstr = XmStringCreateLocalized("Medium Energy Neutron");
    gui->preference_dialog.label[NEUTRON_MED] = XtVaCreateManagedWidget("MEN",
									xmLabelWidgetClass, gui->preference_dialog.path_prefs_form,
									XmNleftAttachment, XmATTACH_FORM,
									XmNtopAttachment, XmATTACH_WIDGET,
									XmNtopWidget, gui->preference_dialog.label[NEUTRON_LOW],
									XmNrightAttachment, XmATTACH_NONE,
									XmNbottomAttachment, XmATTACH_NONE,
									XmNlabelString, xmstr,
									XmNleftOffset, 10,
									XmNtopOffset, 25,
									NULL);
    gui->preference_dialog.colorbx[NEUTRON_MED] = XtVaCreateManagedWidget("Pref_neutron_med",
									  xmDrawnButtonWidgetClass, gui->preference_dialog.path_prefs_form,
									  XmNleftAttachment, XmATTACH_NONE,
									  XmNtopAttachment, XmATTACH_WIDGET,
									  XmNtopWidget, gui->preference_dialog.colorbx[NEUTRON_LOW],
									  XmNrightAttachment, XmATTACH_FORM,
									  XmNbottomAttachment, XmATTACH_NONE,
									  XmNrightOffset, 50,
									  XmNtopOffset, 10,
									  XmNlabelType, XmPIXMAP,
									  XmNheight, 25,
									  XmNwidth, 60,
									  NULL);
    XtAddCallback(gui->preference_dialog.colorbx[NEUTRON_MED],XmNactivateCallback, Show_Particle_type_dialogCB, (XtPointer)gui);
    XmStringFree(xmstr);

    xmstr = XmStringCreateLocalized("High Energy Neutron");
    gui->preference_dialog.label[NEUTRON_HIGH] = XtVaCreateManagedWidget("HEN",
									 xmLabelWidgetClass, gui->preference_dialog.path_prefs_form,
									 XmNleftAttachment, XmATTACH_FORM,
									 XmNtopAttachment, XmATTACH_WIDGET,
									 XmNtopWidget, gui->preference_dialog.label[NEUTRON_MED],
									 XmNrightAttachment, XmATTACH_NONE,
									 XmNbottomAttachment, XmATTACH_NONE,
									 XmNlabelString, xmstr,
									 XmNleftOffset, 10,
									 XmNtopOffset, 25,
									 NULL);
    gui->preference_dialog.colorbx[NEUTRON_HIGH] = XtVaCreateManagedWidget("Pref_neutron_high",
									   xmDrawnButtonWidgetClass, gui->preference_dialog.path_prefs_form,
									   XmNleftAttachment, XmATTACH_NONE,
									   XmNtopAttachment, XmATTACH_WIDGET,
									   XmNtopWidget, gui->preference_dialog.colorbx[NEUTRON_MED],
									   XmNrightAttachment, XmATTACH_FORM,
									   XmNbottomAttachment, XmATTACH_NONE,
									   XmNrightOffset, 50,
									   XmNtopOffset, 10,
									   XmNlabelType, XmPIXMAP,
									   XmNheight, 25,
									   XmNwidth, 60,
									   NULL);
    XtAddCallback(gui->preference_dialog.colorbx[NEUTRON_HIGH],XmNactivateCallback, Show_Particle_type_dialogCB, (XtPointer)gui);
    XmStringFree(xmstr);

    xmstr = XmStringCreateLocalized("The Beam");
    gui->preference_dialog.label[BEAM] = XtVaCreateManagedWidget("beam",
								 xmLabelWidgetClass, gui->preference_dialog.path_prefs_form,
								 XmNleftAttachment, XmATTACH_FORM,
								 XmNtopAttachment, XmATTACH_WIDGET,
								 XmNtopWidget, gui->preference_dialog.label[NEUTRON_HIGH],
								 XmNrightAttachment, XmATTACH_NONE,
								 XmNbottomAttachment, XmATTACH_NONE,
								 XmNlabelString, xmstr,
								 XmNleftOffset, 10,
								 XmNtopOffset, 25,
								 NULL);
    gui->preference_dialog.colorbx[BEAM] = XtVaCreateManagedWidget("Pref_beam",
								   xmDrawnButtonWidgetClass, gui->preference_dialog.path_prefs_form,
								   XmNleftAttachment, XmATTACH_NONE,
								   XmNtopAttachment, XmATTACH_WIDGET,
								   XmNtopWidget, gui->preference_dialog.colorbx[NEUTRON_HIGH],
								   XmNrightAttachment, XmATTACH_FORM,
								   XmNbottomAttachment, XmATTACH_NONE,
								   XmNrightOffset, 50,
								   XmNtopOffset, 10,
								   XmNlabelType, XmPIXMAP,
								   XmNheight, 25,
								   XmNwidth, 60,
								   NULL);
    XtAddCallback(gui->preference_dialog.colorbx[BEAM],XmNactivateCallback, Show_Particle_type_dialogCB, (XtPointer)gui);
    XmStringFree(xmstr);

    xmstr = XmStringCreateLocalized("Lost Particles");
    gui->preference_dialog.label[LOST] = XtVaCreateManagedWidget("lost",
								 xmLabelWidgetClass, gui->preference_dialog.path_prefs_form,
								 XmNleftAttachment, XmATTACH_FORM,
								 XmNtopAttachment, XmATTACH_WIDGET,
								 XmNtopWidget, gui->preference_dialog.label[BEAM],
								 XmNrightAttachment, XmATTACH_NONE,
								 XmNbottomAttachment, XmATTACH_NONE,
								 XmNlabelString, xmstr,
								 XmNleftOffset, 10,
								 XmNtopOffset, 25,
								 NULL);
    gui->preference_dialog.colorbx[LOST] = XtVaCreateManagedWidget("Pref_lost",
								   xmDrawnButtonWidgetClass, gui->preference_dialog.path_prefs_form,
								   XmNleftAttachment, XmATTACH_NONE,
								   XmNtopAttachment, XmATTACH_WIDGET,
								   XmNtopWidget, gui->preference_dialog.colorbx[BEAM],
								   XmNrightAttachment, XmATTACH_FORM,
								   XmNbottomAttachment, XmATTACH_FORM,
								   XmNbottomOffset, 10,
								   XmNrightOffset, 50,
								   XmNtopOffset, 10,
								   XmNlabelType, XmPIXMAP,
								   XmNheight, 25,
								   XmNwidth, 60,
								   NULL);
    XtAddCallback(gui->preference_dialog.colorbx[LOST],XmNactivateCallback, Show_Particle_type_dialogCB, (XtPointer)gui);
    XmStringFree(xmstr);
    
    DEBUG_TRACE_OUT printf("Done with build_path_prefs\n");
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
void build_misc_prefs(main_gui_t *gui)
{
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered build_misc_prefs\n");



  gui->preference_dialog.rendering_quality_frame = XtVaCreateManagedWidget("Rendering Quality",
									   xmFrameWidgetClass, gui->preference_dialog.misc_prefs_form,
									   XmNleftAttachment, XmATTACH_FORM,
									   XmNtopAttachment, XmATTACH_FORM,
									   XmNrightAttachment, XmATTACH_NONE,
									   XmNbottomAttachment, XmATTACH_NONE,
									   XmNleftAttachment, XmATTACH_FORM,
									   XmNleftOffset, 20,
									   XmNtopOffset, 20,
									   NULL);
  gui->preference_dialog.rendering_quality_label = XtVaCreateManagedWidget ("Rendering Quality",xmLabelWidgetClass, 
									    gui->preference_dialog.rendering_quality_frame,
									    XmNchildType,XmFRAME_TITLE_CHILD,
									    NULL);
  gui->preference_dialog.rendering_quality_rc = XmCreateRadioBox(gui->preference_dialog.rendering_quality_frame, "rqrc",NULL, 0);
  XtVaSetValues(gui->preference_dialog.rendering_quality_rc,XmNnumColumns, 1,NULL);
  XtManageChild(gui->preference_dialog.rendering_quality_rc);
  gui->preference_dialog.rendering_quality[0] = (Widget)XtCreateManagedWidget("Low",
									      xmToggleButtonWidgetClass, gui->preference_dialog.rendering_quality_rc,
									      NULL, 0);
  gui->preference_dialog.rendering_quality[1] = (Widget)XtCreateManagedWidget("Medium",
									      xmToggleButtonWidgetClass, gui->preference_dialog.rendering_quality_rc,
									      NULL, 0);
  gui->preference_dialog.rendering_quality[2] = (Widget)XtCreateManagedWidget("High",
									      xmToggleButtonWidgetClass, gui->preference_dialog.rendering_quality_rc,
									      NULL, 0);
  gui->preference_dialog.rendering_quality[3] = (Widget)XtCreateManagedWidget("Full",
									      xmToggleButtonWidgetClass, gui->preference_dialog.rendering_quality_rc,
									      NULL, 0);


  gui->preference_dialog.poly_rendering_quality_frame = XtVaCreateManagedWidget("Polygon Quality",
										xmFrameWidgetClass, gui->preference_dialog.misc_prefs_form,
										XmNleftAttachment, XmATTACH_WIDGET,
										XmNleftWidget, gui->preference_dialog.rendering_quality_frame,
										XmNleftOffset, 20,
										XmNtopAttachment, XmATTACH_FORM,
										XmNrightAttachment, XmATTACH_NONE,
										XmNbottomAttachment, XmATTACH_NONE,
										XmNtopOffset, 20,
										NULL);
  gui->preference_dialog.poly_rendering_quality_label = XtVaCreateManagedWidget ("Polygon Quality",xmLabelWidgetClass, 
										 gui->preference_dialog.poly_rendering_quality_frame,
										 XmNchildType,XmFRAME_TITLE_CHILD,
										 NULL);
  gui->preference_dialog.poly_rendering_quality_rc = XmCreateRadioBox(gui->preference_dialog.poly_rendering_quality_frame, "rqrc",NULL, 0);
  XtVaSetValues(gui->preference_dialog.poly_rendering_quality_rc,XmNnumColumns, 1,NULL);
  XtManageChild(gui->preference_dialog.poly_rendering_quality_rc);
  gui->preference_dialog.poly_rendering_quality[0] = (Widget)XtCreateManagedWidget("Low",
										   xmToggleButtonWidgetClass, gui->preference_dialog.poly_rendering_quality_rc,
										   NULL, 0);
  gui->preference_dialog.poly_rendering_quality[1] = (Widget)XtCreateManagedWidget("Medium",
										   xmToggleButtonWidgetClass, gui->preference_dialog.poly_rendering_quality_rc,
										   NULL, 0);
  gui->preference_dialog.poly_rendering_quality[2] = (Widget)XtCreateManagedWidget("High",
										   xmToggleButtonWidgetClass, gui->preference_dialog.poly_rendering_quality_rc,
										   NULL, 0);
  gui->preference_dialog.poly_rendering_quality[3] = (Widget)XtCreateManagedWidget("Full",
										   xmToggleButtonWidgetClass, gui->preference_dialog.poly_rendering_quality_rc,
										   NULL, 0);


  
  gui->preference_dialog.poly_algorithm_label = XtVaCreateManagedWidget ("Polygon Algorithm Type:",
								      xmLabelWidgetClass, gui->preference_dialog.misc_prefs_form,
								      XmNleftAttachment, XmATTACH_FORM,
								      XmNtopAttachment, XmATTACH_WIDGET,
								      XmNtopWidget, gui->preference_dialog.poly_rendering_quality_frame,
								      XmNtopOffset, 10,
								      XmNleftOffset, 10,
								      NULL);					     
  gui->preference_dialog.poly_algorithm_pane = (Widget)XmCreatePulldownMenu(gui->preference_dialog.misc_prefs_form,"algorithm_pane",NULL,0);

  gui->preference_dialog.poly_algorithm_menu = (Widget)XtVaCreateManagedWidget("Algorithm",xmRowColumnWidgetClass,gui->preference_dialog.misc_prefs_form,
                XmNmarginHeight,       0,
                XmNmarginWidth,        0,
                XmNpacking,            XmPACK_TIGHT,
                XmNpopupEnabled,       TRUE,
                XmNrowColumnType,      XmMENU_OPTION,
                XmNspacing,            0,
                XmNsubMenuId, gui->preference_dialog.poly_algorithm_pane,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, gui->preference_dialog.poly_algorithm_label,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->preference_dialog.poly_rendering_quality_frame,
		XmNtopOffset, 10,
		XmNleftOffset, 10,
		NULL);
  
  
  gui->preference_dialog.poly_algorithm_type[0] = (Widget)XtCreateManagedWidget("8-Cell w/Surface Normals",xmPushButtonWidgetClass,
									     gui->preference_dialog.poly_algorithm_pane, NULL, 0);
  gui->preference_dialog.poly_algorithm_type[1] = (Widget)XtCreateManagedWidget("8-Cell w/Vertex Normals",xmPushButtonWidgetClass,
									     gui->preference_dialog.poly_algorithm_pane, NULL, 0);		
  gui->preference_dialog.poly_algorithm_type[2] = (Widget)XtCreateManagedWidget("Marching Cubes",xmPushButtonWidgetClass,
									     gui->preference_dialog.poly_algorithm_pane, NULL, 0);		
  

  gui->preference_dialog.mess_toggle = XtVaCreateManagedWidget("Messages (On the main bar)",
							       xmToggleButtonWidgetClass,gui->preference_dialog.misc_prefs_form,
							       XmNrightAttachment, XmATTACH_NONE,
							       XmNleftAttachment, XmATTACH_FORM,
							       XmNbottomAttachment, XmATTACH_NONE,
							       XmNtopAttachment, XmATTACH_WIDGET,
							       XmNtopWidget, gui->preference_dialog.poly_algorithm_menu,
							       XmNtopOffset, 20,
							       XmNleftOffset, 25,
							       NULL);




  
  xmstr = XmStringCreateLocalized("Fast Rotation :");
  gui->preference_dialog.fr_label = XtVaCreateManagedWidget("Fast Rotation :",
							    xmLabelWidgetClass, gui->preference_dialog.misc_prefs_form,
							    XmNleftAttachment, XmATTACH_FORM,
							    XmNtopAttachment, XmATTACH_WIDGET,
							    XmNtopWidget, gui->preference_dialog.mess_toggle,
							    XmNrightAttachment, XmATTACH_NONE,
							    XmNbottomAttachment, XmATTACH_NONE,
							    XmNlabelString, xmstr,
							    XmNleftOffset, 25,
							    XmNtopOffset, 20,
							    NULL);
  XmStringFree(xmstr);

  gui->preference_dialog.fr_pane = (Widget)XmCreatePulldownMenu(gui->preference_dialog.misc_prefs_form,"fr_pane",NULL,0);
  gui->preference_dialog.fr_menu = (Widget)XtVaCreateManagedWidget("fr_menu",xmRowColumnWidgetClass,gui->preference_dialog.misc_prefs_form,
                XmNmarginHeight,       0,
                XmNmarginWidth,        0,
                XmNpacking,            XmPACK_TIGHT,
                XmNpopupEnabled,       TRUE,
                XmNrowColumnType,      XmMENU_OPTION,
                XmNspacing,            0,
		XmNsubMenuId, gui->preference_dialog.fr_pane, 
		XmNrightAttachment, XmATTACH_NONE,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, gui->preference_dialog.fr_label,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gui->preference_dialog.mess_toggle,
		XmNleftOffset, 20,
		XmNtopOffset, 15,
		NULL);


  gui->preference_dialog.fr_1 = XtCreateManagedWidget ("by Wireframe",xmPushButtonWidgetClass,
						       gui->preference_dialog.fr_pane, NULL, 0);
  gui->preference_dialog.fr_2 = XtCreateManagedWidget ("by Axis",xmPushButtonWidgetClass,
						       gui->preference_dialog.fr_pane, NULL, 0);  
  gui->preference_dialog.fr_3 = XtCreateManagedWidget ("Off",xmPushButtonWidgetClass,
						       gui->preference_dialog.fr_pane, NULL, 0);  
  
  gui->preference_dialog.measure_frame = XtVaCreateManagedWidget("Measure Units",
								 xmFrameWidgetClass, gui->preference_dialog.misc_prefs_form,
								 XmNtopAttachment, XmATTACH_WIDGET,
								 XmNtopWidget, gui->preference_dialog.fr_menu,
								 XmNleftAttachment, XmATTACH_FORM,
								 XmNleftOffset, 20,
								 NULL);
  gui->preference_dialog.measure_frame_label = XtVaCreateManagedWidget ("Measure Units",xmLabelWidgetClass, 
									gui->preference_dialog.measure_frame,
									XmNchildType,XmFRAME_TITLE_CHILD,
									NULL);
  gui->preference_dialog.meas_rowcol = XmCreateRadioBox(gui->preference_dialog.measure_frame, "measrowcol",NULL, 0);
  XtVaSetValues(gui->preference_dialog.meas_rowcol,XmNnumColumns, 2,NULL);
  XtManageChild(gui->preference_dialog.meas_rowcol);
  gui->preference_dialog.measure[0] = (Widget)XtCreateManagedWidget("cm",
								    xmToggleButtonWidgetClass, gui->preference_dialog.meas_rowcol,
								    NULL, 0);
  gui->preference_dialog.measure[1] = (Widget)XtCreateManagedWidget("mm",
								    xmToggleButtonWidgetClass, gui->preference_dialog.meas_rowcol,
								    NULL, 0);

  DEBUG_TRACE_OUT printf("Done with build_misc_prefs\n");
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
void build_mouse_prefs(main_gui_t *gui)
{
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered build_mouse_prefs\n");

    xmstr = XmStringCreateLocalized("Mouse Control :");
    gui->preference_dialog.m_label = XtVaCreateManagedWidget("Mouse Control :",
				      xmLabelWidgetClass, gui->preference_dialog.mouse_prefs_form,
				      XmNleftAttachment, XmATTACH_FORM,
				      XmNtopAttachment, XmATTACH_FORM,
				      XmNrightAttachment, XmATTACH_NONE,
				      XmNbottomAttachment, XmATTACH_NONE,
				      XmNlabelString, xmstr,
				      XmNleftOffset, 25,
				      XmNtopOffset, 10,
				      NULL);
    XmStringFree(xmstr);

  gui->preference_dialog.m_pane = (Widget)XmCreatePulldownMenu(gui->preference_dialog.mouse_prefs_form,"m_pane",NULL, 0);
  gui->preference_dialog.m_menu = (Widget)XtVaCreateManagedWidget("m_menu",xmRowColumnWidgetClass,gui->preference_dialog.mouse_prefs_form,
                XmNmarginHeight,       0,
                XmNmarginWidth,        0,
                XmNpacking,            XmPACK_TIGHT,
                XmNpopupEnabled,       TRUE,
                XmNrowColumnType,      XmMENU_OPTION,
                XmNspacing,            0,
		XmNsubMenuId, gui->preference_dialog.m_pane, 
		XmNrightAttachment, XmATTACH_NONE,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, gui->preference_dialog.m_label,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftOffset, 20,
		XmNtopOffset, 5,
		NULL);

  gui->preference_dialog.m_1 = XtCreateManagedWidget ("Pull Axis",xmPushButtonWidgetClass,
						      gui->preference_dialog.m_pane, NULL, 0);
  XtVaSetValues(gui->preference_dialog.m_1,XmNsensitive, FALSE,NULL);

  gui->preference_dialog.m_2 = XtCreateManagedWidget ("Sliders",xmPushButtonWidgetClass,
						      gui->preference_dialog.m_pane, NULL, 0);  
  gui->preference_dialog.m_3 = XtCreateManagedWidget ("Mouse Buttons",xmPushButtonWidgetClass,
						      gui->preference_dialog.m_pane, NULL, 0);  
  
  DEBUG_TRACE_OUT printf("Done with build_mouse_prefs\n");
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
void build_axis_prefs(main_gui_t *gui)
{
  XmString xmstr;
  
  DEBUG_TRACE_IN printf("Entered build_axis_prefs\n");

  xmstr = XmStringCreateLocalized("Axis Type :");
  gui->preference_dialog.axis_label = XtVaCreateManagedWidget("Main Window Size :",
							      xmLabelWidgetClass, gui->preference_dialog.axis_prefs_form,
							      XmNleftAttachment, XmATTACH_FORM,
							      XmNtopAttachment, XmATTACH_FORM,
							      XmNrightAttachment, XmATTACH_NONE,
							      XmNbottomAttachment, XmATTACH_NONE,
							      XmNlabelString, xmstr,
							      XmNleftOffset, 25,
							      XmNtopOffset, 10,
							      NULL);
  XmStringFree(xmstr);

  gui->preference_dialog.axis_pane = (Widget)XmCreatePulldownMenu(gui->preference_dialog.axis_prefs_form,"axis_pane",NULL, 0);
  gui->preference_dialog.axis_menu = (Widget)XtVaCreateManagedWidget("axis_menu",xmRowColumnWidgetClass,
                                                                     gui->preference_dialog.axis_prefs_form,
                                                                     XmNmarginHeight,       0,
                                                                     XmNmarginWidth,        0,
                                                                     XmNpacking,            XmPACK_TIGHT,
                                                                     XmNpopupEnabled,       TRUE,
                                                                     XmNrowColumnType,      XmMENU_OPTION,
                                                                     XmNspacing,            0,
                                                                     XmNsubMenuId, gui->preference_dialog.axis_pane, 
                                                                     XmNrightAttachment, XmATTACH_NONE,
                                                                     XmNleftAttachment, XmATTACH_WIDGET,
                                                                     XmNleftWidget, gui->preference_dialog.axis_label,
                                                                     XmNbottomAttachment, XmATTACH_NONE,
                                                                     XmNtopAttachment, XmATTACH_FORM,
                                                                     XmNleftOffset, 8,
                                                                     XmNtopOffset, 5,
                                                                     NULL);

  gui->preference_dialog.a_1 = XtCreateManagedWidget ("Full",xmPushButtonWidgetClass,
						      gui->preference_dialog.axis_pane, NULL, 0);
  gui->preference_dialog.a_2 = XtCreateManagedWidget ("Thin",xmPushButtonWidgetClass,
						      gui->preference_dialog.axis_pane, NULL, 0);  
  gui->preference_dialog.a_3 = XtCreateManagedWidget ("Thick",xmPushButtonWidgetClass,
						      gui->preference_dialog.axis_pane, NULL, 0);  
  /*gui->preference_dialog.a_4 = XtCreateManagedWidget ("Ring",xmPushButtonWidgetClass,
    gui->preference_dialog.axis_pane, NULL, 0); */ 
  gui->preference_dialog.a_5 = XtCreateManagedWidget ("Off",xmPushButtonWidgetClass,
						      gui->preference_dialog.axis_pane, NULL, 0);  
  
  gui->preference_dialog.axis_label_toggle = XtVaCreateManagedWidget("Axis Labelling",
								     xmToggleButtonWidgetClass,gui->preference_dialog.axis_prefs_form,
								     XmNrightAttachment, XmATTACH_NONE,
								     XmNleftAttachment, XmATTACH_FORM,
								     XmNbottomAttachment, XmATTACH_FORM,
								     XmNtopAttachment, XmATTACH_WIDGET,
                                                                     XmNtopWidget, gui->preference_dialog.axis_menu,
								     XmNtopOffset, 15,
								     XmNbottomOffset, 15,
								     XmNleftOffset, 25,
								     NULL);
  
  DEBUG_TRACE_OUT printf("Done with build_axis_prefs\n");
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
void build_window_size_prefs(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered build_window_size_prefs\n");

  xmstr = XmStringCreateLocalized("Main Win Size :");
  gui->preference_dialog.mws_label = XtVaCreateManagedWidget("Main Window Size :",
							     xmLabelWidgetClass, gui->preference_dialog.ws_prefs_form,
							     XmNleftAttachment, XmATTACH_FORM,
							     XmNtopAttachment, XmATTACH_FORM,
							     XmNrightAttachment, XmATTACH_NONE,
							     XmNbottomAttachment, XmATTACH_NONE,
							     XmNlabelString, xmstr,
							     XmNleftOffset, 25,
							     XmNtopOffset, 10,
							     NULL);
  XmStringFree(xmstr);
    
  gui->preference_dialog.mws_pane = (Widget)XmCreatePulldownMenu(gui->preference_dialog.ws_prefs_form,"mws_pane",NULL, 0);
  gui->preference_dialog.mws_menu = (Widget)XtVaCreateManagedWidget("mws_menu",xmRowColumnWidgetClass,gui->preference_dialog.ws_prefs_form,
                XmNmarginHeight,       0,
                XmNmarginWidth,        0,
                XmNpacking,            XmPACK_TIGHT,
                XmNpopupEnabled,       TRUE,
                XmNrowColumnType,      XmMENU_OPTION,
                XmNspacing,            0,
		XmNsubMenuId, gui->preference_dialog.mws_pane, 
		XmNrightAttachment, XmATTACH_NONE,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, gui->preference_dialog.mws_label,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
                XmNtopWidget,     gui->preference_dialog.mws_label,
                XmNleftOffset, 10,
		XmNtopOffset, 0,
		NULL);

  gui->preference_dialog.mws_1 = XtCreateManagedWidget ("550 X 550",xmPushButtonWidgetClass,
							gui->preference_dialog.mws_pane, NULL, 0);
  gui->preference_dialog.mws_2 = XtCreateManagedWidget ("600 X 600",xmPushButtonWidgetClass,
							gui->preference_dialog.mws_pane, NULL, 0);  
  gui->preference_dialog.mws_3 = XtCreateManagedWidget ("650 X 650",xmPushButtonWidgetClass,
							gui->preference_dialog.mws_pane, NULL, 0);  
  gui->preference_dialog.mws_4 = XtCreateManagedWidget ("700 X 700",xmPushButtonWidgetClass,
							gui->preference_dialog.mws_pane, NULL, 0);  
  gui->preference_dialog.mws_5 = XtCreateManagedWidget ("750 X 750",xmPushButtonWidgetClass,
							gui->preference_dialog.mws_pane, NULL, 0);  
  gui->preference_dialog.mws_6 = XtCreateManagedWidget ("800 X 800",xmPushButtonWidgetClass,
							gui->preference_dialog.mws_pane, NULL, 0);  
  
  xmstr = XmStringCreateLocalized("Multi Win Size :");
  gui->preference_dialog.mls_label = XtVaCreateManagedWidget("Multi Window Size :",
							     xmLabelWidgetClass, gui->preference_dialog.ws_prefs_form,
							     XmNleftAttachment, XmATTACH_FORM,
							     XmNtopAttachment, XmATTACH_WIDGET,
							     XmNtopWidget, gui->preference_dialog.mws_menu,
							     XmNrightAttachment, XmATTACH_NONE,
							     XmNbottomAttachment, XmATTACH_NONE,
							     XmNlabelString, xmstr,
							     XmNtopOffset, 5,
							     XmNleftOffset, 25,
							     NULL);
  XmStringFree(xmstr);
    
  gui->preference_dialog.mls_pane = (Widget)XmCreatePulldownMenu(gui->preference_dialog.ws_prefs_form,"mls_pane",NULL, 0);
  gui->preference_dialog.mls_menu = (Widget)XtVaCreateManagedWidget("mls_menu",xmRowColumnWidgetClass,gui->preference_dialog.ws_prefs_form,
                XmNmarginHeight,       0,
                XmNmarginWidth,        0,
                XmNpacking,            XmPACK_TIGHT,
                XmNpopupEnabled,       TRUE,
                XmNrowColumnType,      XmMENU_OPTION,
                XmNspacing,            0,
		XmNsubMenuId, gui->preference_dialog.mls_pane, 
		XmNrightAttachment, XmATTACH_NONE,
		XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
		XmNleftWidget, gui->preference_dialog.mws_menu,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		XmNtopWidget, gui->preference_dialog.mls_label,
		XmNleftOffset, 0,
		XmNtopOffset, 0,
		NULL);

  gui->preference_dialog.mls_1 = XtCreateManagedWidget ("150 X 150",xmPushButtonWidgetClass,
							gui->preference_dialog.mls_pane, NULL, 0);
  gui->preference_dialog.mls_2 = XtCreateManagedWidget ("175 X 175",xmPushButtonWidgetClass,
							gui->preference_dialog.mls_pane, NULL, 0);  
  gui->preference_dialog.mls_3 = XtCreateManagedWidget ("200 X 200",xmPushButtonWidgetClass,
							gui->preference_dialog.mls_pane, NULL, 0);  
  gui->preference_dialog.mls_4 = XtCreateManagedWidget ("225 X 225",xmPushButtonWidgetClass,
							gui->preference_dialog.mls_pane, NULL, 0);  
  gui->preference_dialog.mls_5 = XtCreateManagedWidget ("250 X 250",xmPushButtonWidgetClass,
							gui->preference_dialog.mls_pane, NULL, 0);  

  gui->preference_dialog.ml_toggle = XtVaCreateManagedWidget("Multi-View",
							     xmToggleButtonWidgetClass, gui->preference_dialog.ws_prefs_form,
							     XmNleftAttachment,   XmATTACH_FORM,
                                                             XmNtopAttachment,    XmATTACH_WIDGET,
                                                             XmNtopWidget,        gui->preference_dialog.mls_menu,
                                                             XmNrightAttachment,  XmATTACH_NONE,
							     XmNbottomAttachment, XmATTACH_NONE,
							     XmNleftOffset,       40,
							     XmNtopOffset,        0,
							     NULL);
  
  xmstr = XmStringCreateLocalized("Background :");
  gui->preference_dialog.back_label = XtVaCreateManagedWidget("Background :",
							      xmLabelWidgetClass, gui->preference_dialog.ws_prefs_form,
							      XmNleftAttachment, XmATTACH_FORM,
                                                              XmNtopWidget, gui->preference_dialog.ml_toggle,
                                                              XmNtopAttachment, XmATTACH_WIDGET,
							      XmNrightAttachment, XmATTACH_NONE,
							      XmNlabelString, xmstr,
							      XmNtopOffset, 10,
							      XmNleftOffset, 25,
							      NULL);
  XmStringFree(xmstr);
    
  gui->preference_dialog.back_pane = (Widget)XmCreatePulldownMenu(gui->preference_dialog.ws_prefs_form,"back_pane",NULL, 0);
  gui->preference_dialog.back_menu = (Widget)XtVaCreateManagedWidget("back_menu",xmRowColumnWidgetClass,gui->preference_dialog.ws_prefs_form,
                XmNmarginHeight,       0,
                XmNmarginWidth,        0,
                XmNpacking,            XmPACK_TIGHT,
                XmNpopupEnabled,       TRUE,
                XmNrowColumnType,      XmMENU_OPTION,
                XmNspacing,            0,
		XmNsubMenuId, gui->preference_dialog.back_pane, 
		XmNrightAttachment, XmATTACH_NONE,
                XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
		XmNleftWidget, gui->preference_dialog.mls_menu,
		XmNbottomAttachment, XmATTACH_NONE,
                XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
                XmNtopWidget, gui->preference_dialog.back_label,
		XmNtopOffset, 0,
		NULL);

  gui->preference_dialog.back[0] = XtCreateManagedWidget ("Black",xmPushButtonWidgetClass,
							  gui->preference_dialog.back_pane, NULL, 0);
  gui->preference_dialog.back[1] = XtCreateManagedWidget ("25%",xmPushButtonWidgetClass,
							  gui->preference_dialog.back_pane, NULL, 0);  
  gui->preference_dialog.back[2] = XtCreateManagedWidget ("50%",xmPushButtonWidgetClass,
							  gui->preference_dialog.back_pane, NULL, 0);  
  gui->preference_dialog.back[3] = XtCreateManagedWidget ("75%",xmPushButtonWidgetClass,
							  gui->preference_dialog.back_pane, NULL, 0);  
  gui->preference_dialog.back[4] = XtCreateManagedWidget ("White",xmPushButtonWidgetClass,
							  gui->preference_dialog.back_pane, NULL, 0);  
  
  DEBUG_TRACE_OUT printf("Done with build_window_size_prefs\n");
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
void build_main_prefs_forms(main_gui_t *gui)
{
  XmString xmstr;
  
  DEBUG_TRACE_IN printf("Entered build_main_prefs_forms\n");

  gui->preference_dialog.sel_frame = XtVaCreateManagedWidget ("sel_frame",
							      xmFrameWidgetClass, gui->preference_dialog.main_form,
							      XmNtopAttachment, XmATTACH_FORM,
							      XmNleftAttachment, XmATTACH_FORM,
							      XmNrightAttachment, XmATTACH_FORM,
							      XmNbottomAttachment, XmATTACH_NONE,
							      XmNrightOffset, 20,
							      XmNleftOffset, 20,
							      XmNtopOffset, 20,
							      NULL);
  gui->preference_dialog.sel_form = XtVaCreateManagedWidget ("sel_form",
							     xmFormWidgetClass, gui->preference_dialog.sel_frame,
							     NULL);

  xmstr = XmStringCreateLocalized("Window Sizes");
  gui->preference_dialog.ws_button = XtVaCreateManagedWidget ("Window Sizes",
							      xmDrawnButtonWidgetClass, gui->preference_dialog.sel_form,
							      XmNtopAttachment, XmATTACH_FORM,
							      XmNleftAttachment, XmATTACH_FORM,
							      XmNrightAttachment, XmATTACH_NONE,
							      XmNbottomAttachment, XmATTACH_FORM,
							      XmNshadowType, XmSHADOW_ETCHED_IN,
							      XmNlabelType, XmSTRING,
							      XmNlabelString, xmstr,
							      NULL);
  XtAddCallback(gui->preference_dialog.ws_button,XmNactivateCallback, Pref_switchedCB, (XtPointer)gui);
  XmStringFree(xmstr);

  		  
  xmstr = XmStringCreateLocalized("Color");
  gui->preference_dialog.color_button = XtVaCreateManagedWidget ("Color",
								 xmDrawnButtonWidgetClass, gui->preference_dialog.sel_form,
								 XmNtopAttachment, XmATTACH_FORM,
								 XmNleftAttachment, XmATTACH_WIDGET,
								 XmNleftWidget, gui->preference_dialog.ws_button,
								 XmNrightAttachment, XmATTACH_NONE,
								 XmNbottomAttachment, XmATTACH_FORM,
								 XmNshadowType, XmSHADOW_ETCHED_OUT,
								 XmNlabelType, XmSTRING,
								 XmNlabelString, xmstr,
								 NULL);
  XtAddCallback(gui->preference_dialog.color_button,XmNactivateCallback, Pref_switchedCB, (XtPointer)gui);
  XmStringFree(xmstr); 

  xmstr = XmStringCreateLocalized("Axis");
  gui->preference_dialog.axis_button = XtVaCreateManagedWidget ("Axis",
								xmDrawnButtonWidgetClass, gui->preference_dialog.sel_form,
								XmNtopAttachment, XmATTACH_FORM,
								XmNleftAttachment, XmATTACH_WIDGET,
								XmNleftWidget, gui->preference_dialog.color_button,
								XmNrightAttachment, XmATTACH_NONE,
								XmNbottomAttachment, XmATTACH_FORM,
								XmNshadowType, XmSHADOW_ETCHED_OUT,
								XmNlabelType, XmSTRING,
								XmNlabelString, xmstr,
								NULL);
    XtAddCallback(gui->preference_dialog.axis_button,XmNactivateCallback, Pref_switchedCB, (XtPointer)gui);
    XmStringFree(xmstr);

  xmstr = XmStringCreateLocalized("Mouse");
  gui->preference_dialog.mouse_button = XtVaCreateManagedWidget ("Mouse",
								 xmDrawnButtonWidgetClass, gui->preference_dialog.sel_form,
								 XmNtopAttachment, XmATTACH_FORM,
								 XmNleftAttachment, XmATTACH_WIDGET,
								 XmNleftWidget, gui->preference_dialog.axis_button,
								 XmNrightAttachment, XmATTACH_NONE,
								 XmNbottomAttachment, XmATTACH_FORM,
								 XmNshadowType, XmSHADOW_ETCHED_OUT,
								 XmNlabelType, XmSTRING,
								 XmNlabelString, xmstr,
								 NULL);				       
  XtAddCallback(gui->preference_dialog.mouse_button,XmNactivateCallback, Pref_switchedCB, (XtPointer)gui);
  XmStringFree(xmstr);

  xmstr = XmStringCreateLocalized("Paths");
  gui->preference_dialog.path_button = XtVaCreateManagedWidget ("Paths",
								xmDrawnButtonWidgetClass, gui->preference_dialog.sel_form,
								XmNtopAttachment, XmATTACH_FORM,
								XmNleftAttachment, XmATTACH_WIDGET,
								XmNleftWidget, gui->preference_dialog.mouse_button,
								XmNrightAttachment, XmATTACH_NONE,
								XmNbottomAttachment, XmATTACH_FORM,
								XmNshadowType, XmSHADOW_ETCHED_OUT,
								XmNlabelType, XmSTRING,
								XmNlabelString, xmstr,
								NULL);  
  XtAddCallback(gui->preference_dialog.path_button,XmNactivateCallback, Pref_switchedCB, (XtPointer)gui);
  XmStringFree(xmstr);

  xmstr = XmStringCreateLocalized("Texture");
  gui->preference_dialog.tex_button = XtVaCreateManagedWidget ("Texture",
							       xmDrawnButtonWidgetClass, gui->preference_dialog.sel_form,
							       XmNtopAttachment, XmATTACH_FORM,
							       XmNleftAttachment, XmATTACH_WIDGET,
							       XmNleftWidget, gui->preference_dialog.path_button,
							       XmNrightAttachment, XmATTACH_NONE,
							       XmNbottomAttachment, XmATTACH_FORM,
							       XmNshadowType, XmSHADOW_ETCHED_OUT,
							       XmNlabelType, XmSTRING,
							       XmNlabelString, xmstr,
							       NULL);  
  XtAddCallback(gui->preference_dialog.tex_button,XmNactivateCallback, Pref_switchedCB, (XtPointer)gui);
  XmStringFree(xmstr);    
  
  xmstr = XmStringCreateLocalized("Misc");
  gui->preference_dialog.misc_button = XtVaCreateManagedWidget ("Misc",
								xmDrawnButtonWidgetClass, gui->preference_dialog.sel_form,
								XmNtopAttachment, XmATTACH_FORM,
								XmNleftAttachment, XmATTACH_WIDGET,
								XmNleftWidget, gui->preference_dialog.tex_button,
								XmNrightAttachment, XmATTACH_FORM,
								XmNbottomAttachment, XmATTACH_FORM,
								XmNshadowType, XmSHADOW_ETCHED_OUT,
								XmNlabelType, XmSTRING,
								XmNlabelString, xmstr,
								NULL);  
  XtAddCallback(gui->preference_dialog.misc_button,XmNactivateCallback, Pref_switchedCB, (XtPointer)gui);
  XmStringFree(xmstr);    

  gui->preference_dialog.pref_frame = XtVaCreateManagedWidget("pref_frame",
							      xmFrameWidgetClass, gui->preference_dialog.main_form,
							      XmNleftAttachment, XmATTACH_FORM,
							      XmNrightAttachment, XmATTACH_FORM,
							      XmNbottomAttachment, XmATTACH_FORM,
							      XmNtopAttachment , XmATTACH_WIDGET,
							      XmNtopWidget, gui->preference_dialog.sel_frame,
							      XmNtopOffset, 50,
							      XmNrightOffset, 20,
							      XmNleftOffset, 20,
							      XmNbottomOffset, 20,
							      /*XmNheight, 50,*/
							      NULL);
  
  gui->preference_dialog.pref_frame_label = XtVaCreateManagedWidget ("Window Size Prefs",xmLabelWidgetClass, 
								     gui->preference_dialog.pref_frame,
								     XmNchildType,XmFRAME_TITLE_CHILD,
								     NULL);
  
  gui->preference_dialog.pref_form = XtVaCreateManagedWidget("pref_form",
							     xmFormWidgetClass, gui->preference_dialog.pref_frame,
							     XmNleftAttachment, XmATTACH_FORM,
							     XmNrightAttachment, XmATTACH_FORM,
							     XmNbottomAttachment, XmATTACH_FORM,
							     XmNtopAttachment , XmATTACH_FORM,
							     NULL);
  
  gui->preference_dialog.ws_prefs_form = XtVaCreateManagedWidget("ws_form",
								 xmFormWidgetClass, gui->preference_dialog.pref_form,
								 XmNleftAttachment, XmATTACH_FORM,
								 XmNrightAttachment, XmATTACH_FORM,
								 XmNbottomAttachment, XmATTACH_FORM,
								 XmNtopAttachment , XmATTACH_FORM,
								 NULL);
  gui->preference_dialog.color_prefs_form = XtVaCreateManagedWidget("color_form",
								    xmFormWidgetClass, gui->preference_dialog.pref_form,
								    XmNleftAttachment, XmATTACH_FORM,
								    XmNrightAttachment, XmATTACH_FORM,
								    XmNbottomAttachment, XmATTACH_FORM,
								    XmNtopAttachment , XmATTACH_FORM,
								    NULL);
  XtUnmanageChild(gui->preference_dialog.color_prefs_form);
			     		     
  gui->preference_dialog.axis_prefs_form = XtVaCreateManagedWidget("axis_form",
								   xmFormWidgetClass, gui->preference_dialog.pref_form,
								   XmNleftAttachment, XmATTACH_FORM,
								   XmNrightAttachment, XmATTACH_FORM,
								   XmNbottomAttachment, XmATTACH_FORM,
								   XmNtopAttachment , XmATTACH_FORM,
								   NULL);
  
  XtUnmanageChild(gui->preference_dialog.axis_prefs_form);
  
  
  gui->preference_dialog.mouse_prefs_form = XtVaCreateManagedWidget("mouse_form",
								    xmFormWidgetClass, gui->preference_dialog.pref_form,
								    NULL);
  XtUnmanageChild(gui->preference_dialog.mouse_prefs_form);
  
  gui->preference_dialog.path_prefs_form = XtVaCreateManagedWidget("path_form",
								   xmFormWidgetClass, gui->preference_dialog.pref_form,
								   XmNrightAttachment, XmATTACH_FORM,
								   XmNleftAttachment, XmATTACH_FORM,
								   NULL);
  XtUnmanageChild(gui->preference_dialog.path_prefs_form);

  gui->preference_dialog.tex_prefs_form = XtVaCreateManagedWidget("tex_form",
								  xmFormWidgetClass, gui->preference_dialog.pref_form,
								  NULL);
  XtUnmanageChild(gui->preference_dialog.tex_prefs_form);
  
  gui->preference_dialog.misc_prefs_form = XtVaCreateManagedWidget("path_form",
								   xmFormWidgetClass, gui->preference_dialog.pref_form,
								   NULL);
  XtUnmanageChild(gui->preference_dialog.misc_prefs_form);
  
  DEBUG_TRACE_OUT printf("Done with build_main_prefs_forms\n");  
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
void Pref_switchedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmString xmstr;
  int old_pref;
  static int first_time = 1;

  DEBUG_TRACE_IN printf("Entered Pref_switchedCB\n");

  if (first_time){
    old_pref = 1;
    first_time = 0;
  }else
    old_pref = gui->preference_dialog.pref_num;


  if (strcmp(XtName(w),"Window Sizes") == 0){
    xmstr = XmStringCreateLocalized("Window Size Prefs");
    gui->preference_dialog.pref_num = 1;
  }else if (strcmp(XtName(w),"Color") == 0){
    xmstr = XmStringCreateLocalized("Color Prefs");
    gui->preference_dialog.pref_num = 7;
  }else if (strcmp(XtName(w),"Axis") == 0){
    xmstr = XmStringCreateLocalized("Axis Prefs");
    gui->preference_dialog.pref_num = 2;
  }else if (strcmp(XtName(w),"Mouse") == 0){
    xmstr = XmStringCreateLocalized("Mouse Prefs");
    gui->preference_dialog.pref_num = 4;
  }else if (strcmp(XtName(w),"Paths") == 0){
    xmstr = XmStringCreateLocalized("Particle Path Prefs");
    gui->preference_dialog.pref_num = 5;
  }else if (strcmp(XtName(w),"Misc") == 0){
    xmstr = XmStringCreateLocalized("Misc Prefs");
    gui->preference_dialog.pref_num = 6;
  }else if (strcmp(XtName(w),"Texture") == 0){
    xmstr = XmStringCreateLocalized("Tex Prefs");
    gui->preference_dialog.pref_num = 8;
  }
  else /* Default case */
  {
      xmstr = XmStringCreateLocalized("Window Size Prefs");
      gui->preference_dialog.pref_num = 1;
  }
  

  if (old_pref != gui->preference_dialog.pref_num)
    {
      switch(old_pref)
	{
	case 1: XtUnmanageChild(gui->preference_dialog.ws_prefs_form);
	        XtVaSetValues(gui->preference_dialog.ws_button, XmNshadowType, XmSHADOW_OUT,NULL);
		break;
	case 2: XtUnmanageChild(gui->preference_dialog.axis_prefs_form); 
	        XtVaSetValues(gui->preference_dialog.axis_button, XmNshadowType, XmSHADOW_OUT,NULL);
		break;
	case 4: XtUnmanageChild(gui->preference_dialog.mouse_prefs_form); 
	        XtVaSetValues(gui->preference_dialog.mouse_button, XmNshadowType, XmSHADOW_OUT,NULL);
		break;
	case 5: XtUnmanageChild(gui->preference_dialog.path_prefs_form); 
	        XtVaSetValues(gui->preference_dialog.path_button, XmNshadowType, XmSHADOW_OUT,NULL);
		break;
	case 6: XtUnmanageChild(gui->preference_dialog.misc_prefs_form); 
	        XtVaSetValues(gui->preference_dialog.misc_button, XmNshadowType, XmSHADOW_OUT,NULL);
		break;
	case 7: XtUnmanageChild(gui->preference_dialog.color_prefs_form); 
	        XtVaSetValues(gui->preference_dialog.color_button, XmNshadowType, XmSHADOW_OUT,NULL);
		break;
	case 8: XtUnmanageChild(gui->preference_dialog.tex_prefs_form); 
	        XtVaSetValues(gui->preference_dialog.tex_button, XmNshadowType, XmSHADOW_OUT,NULL);
		break;
	}
      switch(gui->preference_dialog.pref_num)
	{
	case 1: XtManageChild(gui->preference_dialog.ws_prefs_form); 
	        XtVaSetValues(gui->preference_dialog.ws_button, XmNshadowType, XmSHADOW_IN,NULL);
		break;
	case 2: XtManageChild(gui->preference_dialog.axis_prefs_form); 
	        XtVaSetValues(gui->preference_dialog.axis_button, XmNshadowType, XmSHADOW_IN,NULL);
		break;
	case 4: XtManageChild(gui->preference_dialog.mouse_prefs_form); 
	        XtVaSetValues(gui->preference_dialog.mouse_button, XmNshadowType, XmSHADOW_IN,NULL);
		break;
	case 5: XtManageChild(gui->preference_dialog.path_prefs_form); 
	        XtVaSetValues(gui->preference_dialog.path_button, XmNshadowType, XmSHADOW_IN,NULL);
		break;
	case 6: XtManageChild(gui->preference_dialog.misc_prefs_form); 
	        XtVaSetValues(gui->preference_dialog.misc_button, XmNshadowType, XmSHADOW_IN,NULL);
		break;
	case 7: XtManageChild(gui->preference_dialog.color_prefs_form); 
	        XtVaSetValues(gui->preference_dialog.color_button, XmNshadowType, XmSHADOW_IN,NULL);
		break;
	case 8: XtManageChild(gui->preference_dialog.tex_prefs_form); 
	        XtVaSetValues(gui->preference_dialog.tex_button, XmNshadowType, XmSHADOW_IN,NULL);
		break;
	}
    }

  XtVaSetValues(gui->preference_dialog.pref_frame_label,
		XmNlabelString, xmstr,
		NULL);       
 XmStringFree(xmstr);

 DEBUG_TRACE_OUT printf("Done with Pref_switchedCB\n");
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
void Which_Color_ToggledCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;
  
  DEBUG_TRACE_IN printf("Entered Which_Color_ToggledCB\n");

  if (cbs->set){
    gui->use_uvh_colors = 1;
    XtUnmanageChild(gui->preference_dialog.s_window);
  }else{
    gui->use_uvh_colors = 0;
    XtManageChild(gui->preference_dialog.s_window);
  }

  DEBUG_TRACE_OUT printf("Done with Which_Color_ToggledCB\n");
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
void Preference_Color_ChangedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XColor new_color;
  DEBUG_TRACE_IN printf("Entered Preference_Color_ChangedCB\n");

  Select_Color(gui,&new_color);

  XtVaSetValues(w,XmNbackground, new_color.pixel,NULL);

  DEBUG_TRACE_OUT printf("Done with Preference_Color_ChangedCB\n");
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
/*
  void Color_pref_selectionCB(Widget w, XtPointer clientData, XtPointer callData)
  {
  main_gui_t *gui = (main_gui_t *)clientData;
  XColor color;
  int bod_num,i;

  DEBUG_TRACE_IN printf("Entered Color_pref_selectionCB\n");

  XtVaGetValues(w, XmNbackground, &color.pixel, NULL);
  get_rgb_from_pixel(gui,&color);
  
  XtVaSetValues(gui->preference_dialog.rcolor_s, XmNvalue, color.red, NULL);
  XtVaSetValues(gui->preference_dialog.gcolor_s, XmNvalue, color.green, NULL);
  XtVaSetValues(gui->preference_dialog.bcolor_s, XmNvalue, color.blue, NULL);
  
 for (i=1;i<=gui->num_pref_bods;i++)
 {
 if (strcmp(XtName(w),gui->pref_bod[i].name) == 0)
 {
 bod_num = i; break;
 }
 }
 
 XtVaSetValues(gui->preference_dialog.b_colors[i][2], XmNvalue, gui->pref_bod[2].t,NULL);
 
 DEBUG_TRACE_OUT printf("Done with Color_pref_selectionCB\n");
}
*/
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
/*
  void Color_prefChangedCB(Widget w, XtPointer clientData, XtPointer callData)
  {
  main_gui_t *gui = (main_gui_t *)clientData;
  XColor color;
  int r,g,b,t,i;

  DEBUG_TRACE_IN printf("Entered Color_prefChangedCB\n");

  XtVaGetValues(gui->preference_dialog.rcolor_s, XmNvalue,&r,NULL);
  XtVaGetValues(gui->preference_dialog.gcolor_s, XmNvalue,&g,NULL);
  XtVaGetValues(gui->preference_dialog.bcolor_s, XmNvalue,&b,NULL);  
  XtVaGetValues(gui->preference_dialog.tcolor_s, XmNvalue,&t,NULL);

  for (i=1;i<=gui->num_pref_bods;i++)
  {
  if (strcmp(XtName(w),gui->pref_bod[i].name) == 0)
  {
  gui->bod[1].t = t; break;
  }
  }
  
  init_color(gui,r,g,b,&color);
  XtVaSetValues(w,XmNbackground, color.pixel,NULL);

  DEBUG_TRACE_OUT printf("Done with Color_prefChangedCB\n");
  }
*/
