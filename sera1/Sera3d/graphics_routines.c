#include "sera3d.h"

/**********************************************/
/** Functions local to this file
/**********************************************/
void set_global_operations(main_gui_t *gui);
void Draw_IBeam(main_gui_t *gui);
void draw_test_sphere(main_gui_t *gui);
void draw_test_eight_cell(main_gui_t *gui);
void draw_test_marching_cube_eight_cell(main_gui_t *gui);
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : draw
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: widget to draw to
%%%
%%%  Purpose: this is the main rendering routine, it calls 
%%%           the display lists, the axis routines, the particle
%%%	      paths, the slices, the clipping caps, and more.	      
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw(main_gui_t *gui,Widget w)
{
  int i,j,view_num,width,height; 
  clock_t t1,t2;
  float num_frames;
  char frame_string[10];
  char apeture_string[30];
  char ring_apeture_string[30];
  int old_slice_dir;
  int was_on = 0;
  int main_win = 1;

  DEBUG_TRACE_IN printf("Entered draw\n");

  if (gui->multi_view){
    /*************************************************/
    /** Make w the current drawing widget.
      /*************************************************/
    glXMakeCurrent(gui->display, XtWindow(w), gui->glxcontext);
    
    /*************************************************/
    /** Set the viewport size to the widget size
    /*************************************************/
    if (w != gui->glxarea){ 
      glViewport(0, 0, gui->multiwindowsize, gui->multiwindowsize);
      main_win = 0;
    }else glViewport(0, 0, gui->mainwindowsize, gui->mainwindowsize);
    
    /*************************************************/
    /** init the view_num for camera positioning
    /*************************************************/
    if (w == gui->glxarea)                            view_num = 1;
    else if (w == gui->multiview_panel.gl_lview_area) view_num = 2;
    else if (w == gui->multiview_panel.gl_rview_area) view_num = 3;
    else if (w == gui->multiview_panel.gl_tview_area) view_num = 4;
    
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity ();	/*  clear the matrix	*/
    
    /*************************************************/
    /** Set the camera position based on w.
      /*************************************************/
    switch(view_num){
    case 1:
      /*************************************************/
      /** The main view
	/*************************************************/
      gluLookAt(gui->cam.x,     gui->cam.y,    gui->cam.z,
		gui->cam.at_x,  gui->cam.at_y, gui->cam.at_z,
		gui->cam.up_x,  gui->cam.up_y, gui->cam.up_z);
      break;
    case 2:
      /*************************************************/
      /** The left side view.
	/*************************************************/
	gluLookAt(/*cam_pos*/-450,100,0,/*cam_at*/0,0,0,/*cam_up*/0,1,0);
	break;
    case 3:
      /*************************************************/
      /** The right side view.
	/*************************************************/
      gluLookAt(/*cam_pos*/450,100,0,/*cam_at*/0,0,0,/*cam_up*/0,1,0); 
      break;
    case 4:
      /*************************************************/
      /** The top view.
	/*************************************************/
      gluLookAt(/*cam_pos*/0,500,0,/*cam_at*/0,0,0,/*cam_up*/0,0,-1);  
      break;
    }
    set_light_positions(gui);
  }else{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glViewport(0, 0, gui->mainwindowsize, gui->mainwindowsize);
  }

  /*************************************************/
  /*************************************************/
  /** Drawing calls
  /*************************************************/
  /*************************************************/
  if (gui->show_frame_rate) t1 = clock();
  
  glPushMatrix();
  
  if (gui->scaling != 1.0) glScalef(gui->scaling, gui->scaling, gui->scaling);
  DEBUG_DATA printf("Set the scaling to : %f\n",gui->scaling);
		 
     /** draw crosshairs first, so if single buffered, 
     ** other objects are drawn "in" the crosshairs **/
  if (gui->crosshair_type != 0 && gui->beam_line_view && main_win) draw_crosshairs(gui);
  if (gui->beam_line_view && gui->apeture_on) draw_ring_apeture(gui);

   /*************************************************/
   /** draw the unclipped axis
   /*************************************************/  
   if (!gui->bodies_locked){
     glPushMatrix();
     set_global_operations(gui);
   }else{
     glPushMatrix();
     glRotatef(gui->locked_rx,1,0,0);
     glRotatef(gui->locked_ry,0,1,0);
     glRotatef(gui->locked_rz,0,0,1);
   }
   /*if (gui->mouse_control_method == 1) draw_pulling_axis(GL_RENDER);
     else*/ 
   if (gui->axis_type != 0) draw_generic_axis(gui);
   glPopMatrix();

   /*if (gui->loaded_ray_tracking) draw_tracked_rays();*/

  
   /*************************************************/
   /** draw the clipping caps
   /*************************************************/     
   if (!gui->draw_high_lists && !gui->bodies_locked&&!gui->clip_planes_locked){
     glPushMatrix();
     set_global_operations(gui);
     set_clipping(gui);
       old_slice_dir = gui->slice_dir;
       for (i=0;i<3;i++){
	 gui->slice_dir = i+1;
	 if (gui->clip[i].f_capped && gui->clip[i].on){ 
	   if (i > 0){
	     DEBUG_DATA printf("Drawing the front clipping cap\n");
	     /*draw_clipped_slice((int)(clip[i].f_eqn[3]));*/
	     draw_3d_textured_plane(gui,(gui->clip[i].f_eqn[3]));
	   }else{
	     DEBUG_DATA  printf("Drawing the clipping cap\n");
	     /*draw_clipped_slice((int)
	       (clip[i].f_eqn[3])+(int)z_spacing);*/
	     draw_3d_textured_plane(gui,(gui->clip[i].f_eqn[3])/*+(int)z_spacing*/);
	   }
	 }
	 if (gui->clip[i].b_capped && gui->clip[i].on){
	   if (i > 0){
	     DEBUG_DATA  printf("Drawing the clipping cap\n");
	     /*draw_clipped_slice(-(int)clip[i].b_eqn[3]-1);*/
	     draw_3d_textured_plane(gui,-gui->clip[i].b_eqn[3]-1);
	   }else{
	     DEBUG_DATA  printf("Drawing the clipping cap\n");
	     /*draw_clipped_slice(-(int)clip[i].b_eqn[3]+(int)z_spacing);*/
	     draw_3d_textured_plane(gui,-gui->clip[i].b_eqn[3]/*+(int)z_spacing*/);
	   }
	 }
       gui->slice_dir = old_slice_dir;
       }
     unset_clipping(gui);
     glPopMatrix();
   }

   /*************************************************/
   /** if interactive clipping, draw the clip planes
   /*************************************************/
   if (gui->draw_clip_planes && gui->interactive_clipping){
     if (!gui->clip_planes_locked){
       glPushMatrix();
       set_global_operations(gui);
     }
     for (i=0;i<3;i++)
       if (gui->clip[i].on)
	 Draw_clip_planes(gui,i);
     if (!gui->clip_planes_locked)
       glPopMatrix();  
   }


   /*************************************************/
   /** draw the inlaid slice
   /*************************************************/
   /*
   if (!slice_locked){
     glPushMatrix();
     set_global_operations();
   }
     if (inlay_slice){
       if (view_style > VIEW_STYLE_LINES && !draw_high_lists) glDisable(GL_LIGHTING);
       draw_unclipped_slice(cur_slice);
       if (view_style > VIEW_STYLE_LINES && !draw_high_lists) glEnable(GL_LIGHTING);
     }
   if(!slice_locked)
     glPopMatrix();
     */

   if (gui->slice_win && (gui->inlay_slice || gui->beam_slice) && main_win){
     DEBUG_DATA printf("Drawing the slice in the slice window\n");
     draw_slice_in_slice_win(gui);    
   }

   
   if (gui->inlay_slice || gui->draw_particle_paths || gui->ibeam){
     if (!gui->bodies_locked && !gui->clip_planes_locked){
       glPushMatrix();
       set_global_operations(gui);
       set_clipping(gui);
     }else if (!gui->bodies_locked && gui->clip_planes_locked){
       glPushMatrix();
       glRotatef(gui->locked_rx,1,0,0);
       glRotatef(gui->locked_ry,0,1,0);
       glRotatef(gui->locked_rz,0,0,1);
       set_clipping(gui);
       glPopMatrix();
       glPushMatrix();
       set_global_operations(gui);
     }else if (gui->bodies_locked && !gui->clip_planes_locked){
       glPushMatrix();
       set_global_operations(gui);
       set_clipping(gui);
       glPopMatrix();
       glPushMatrix();
       glRotatef(gui->locked_rx,1,0,0);
       glRotatef(gui->locked_ry,0,1,0);
       glRotatef(gui->locked_rz,0,0,1);   
     }else if (gui->bodies_locked && gui->clip_planes_locked){
       glPushMatrix();
       glRotatef(gui->locked_rx,1,0,0);
       glRotatef(gui->locked_ry,0,1,0);
       glRotatef(gui->locked_rz,0,0,1);   
       set_clipping(gui);
     }
     

     if (gui->images_loaded){
       if (gui->volume_render_on){
	 volume_render(gui); gui->volume_render_on = 0;
       }else{

	 if (gui->inlay_slice){
	   DEBUG_DATA printf("Drawing the inlaid slice\n");
	   draw_3d_textured_plane(gui,(float)gui->cur_slice);
	 }
	 if (gui->beam_slice){
	   DEBUG_DATA printf("Drawing the beam slice\n");
	   draw_beam_slice(gui,gui->beam_slice_x, gui->beam_slice_y,gui->beam_slice_z);  
	 }
       }
       
       unset_clipping(gui);
       
       /*
	 if (!(!bodies_locked&&slice_locked))
	 glPopMatrix();
	 */
     }
     
     if (gui->ibeam){
       DEBUG_DATA printf("Drawing the Interactive beam\n");
       Draw_IBeam(gui);
     }else if (gui->draw_particle_paths){
       DEBUG_DATA  printf("Drawing the Particle_Paths\n");
       Draw_particle_paths(gui);     
     }
     glPopMatrix();
   }

   /***************************************************************
   *** BODIES & Contours
   ****************************************************************/
     if (!gui->bodies_locked && !gui->clip_planes_locked){
       glPushMatrix();
       set_global_operations(gui);
       set_clipping(gui);
     }else if (!gui->bodies_locked && gui->clip_planes_locked){
       glPushMatrix();
       glRotatef(gui->locked_rx,1,0,0);
       glRotatef(gui->locked_ry,0,1,0);
       glRotatef(gui->locked_rz,0,0,1);
       set_clipping(gui);
       glPopMatrix();
       glPushMatrix();
       set_global_operations(gui);
     }else if (gui->bodies_locked && !gui->clip_planes_locked){
       glPushMatrix();
       set_global_operations(gui);
       set_clipping(gui);
       glPopMatrix();
       glPushMatrix();
       glRotatef(gui->locked_rx,1,0,0);
       glRotatef(gui->locked_ry,0,1,0);
       glRotatef(gui->locked_rz,0,0,1);   
     }else if (gui->bodies_locked && gui->clip_planes_locked){
       glPushMatrix();
       glRotatef(gui->locked_rx,1,0,0);
       glRotatef(gui->locked_ry,0,1,0);
       glRotatef(gui->locked_rz,0,0,1);   
       set_clipping(gui);
     }


     /**********************************************************************************************************/
     /**** Draw the bodies first ****/     
     /**********************************************************************************************************/
     if (gui->surface_texturing_on && gui->view_style == VIEW_STYLE_POLYGONAL){
       glEnable(GL_TEXTURE_3D_EXT);
       if (gui->alpha_culling_on) glEnable(GL_ALPHA_TEST); 
       glDisable(GL_LIGHTING);
       glDisable(GL_BLEND);
       glColor3f(1.0,1.0,1.0);
     }
        
     /*if (gui->view_style == VIEW_STYLE_POLYGONAL) draw_test_eight_cell(gui);*/
     /*if (gui->view_style == VIEW_STYLE_POLYGONAL) draw_test_marching_cube_eight_cell(gui);*/

     if (gui->view_style == VIEW_STYLE_WIREFRAME || 
	 (gui->draw_high_lists && gui->fast_rotation_type == 1)) 
       glPointSize(gui->wireframe_point_size);
     else 
       glPointSize(gui->solid_point_size);

     for (j=1;j<gui->num_bodies;j++){
       i = gui->body_order[j];


       /** set the color for the region (if texturing it just use light gray) **/
       if (gui->view_style == VIEW_STYLE_POLYGONAL && (gui->surface_texturing_on)){
	 glColor4f(.8,.8,.8,1.0);	 
       }else{
	 glColor4f((float)gui->bod[i].r/65535.0,
		   (float)gui->bod[i].g/65535.0,
		   (float)gui->bod[i].b/65535.0,
		   (float)gui->bod[i].t/100.0);    
       }
       /*
	 if ((gui->view_style == VIEW_STYLE_POLYGONAL && !gui->surface_texturing_on) || gui->view_style == VIEW_STYLE_WIREFRAME){
	 glColor4f((float)gui->bod[i].r/65535.0,
	 (float)gui->bod[i].g/65535.0,
	 (float)gui->bod[i].b/65535.0,
	 (float)gui->bod[i].t/100.0);    
	 }else if (gui->view_style == VIEW_STYLE_POLYGONAL && (gui->surface_texturing_on)){
	 printf("setting the color to gray\n");
	 glColor4f(.8,.8,.8,1.0);	 
	 }
       */

       /*** if enabled, draw it ***/
       if (gui->bod[i].enabled && ((gui->fast_rotation_type!=2) || (gui->fast_rotation_type==2 && !gui->draw_high_lists))){
	 DEBUG_DATA printf("Drawing body: %s",gui->bod[i].name);
	 if ((!gui->bod[i].clipped) || 
	     (gui->draw_high_lists && gui->interactive_clipping &&
	      gui->clipping_mode && gui->draw_clip_planes) || 
	     (!gui->draw_high_lists && gui->interactive_clipping && 
	      gui->clipping_mode && gui->draw_clip_planes && gui->fast_rotation_type == 3)){ 
	   was_on = 1; 
	   unset_clipping(gui);
	 }
	 
	 if (gui->draw_high_lists || gui->view_style == VIEW_STYLE_WIREFRAME) glCallList(WIREFRAME_LISTS+i-1);		  
	 else {
	   glCallList(i);	   
	 }
	 
	 if (was_on){
	   set_clipping(gui);
	 }
       }
     }

     if (gui->surface_texturing_on && gui->view_style == VIEW_STYLE_POLYGONAL){
       glDisable(GL_TEXTURE_3D_EXT);
       if (gui->alpha_culling_on) glDisable(GL_ALPHA_TEST); 
       glEnable(GL_LIGHTING);
       glEnable(GL_BLEND);
     }

     /**********************************************************************************************************/
     /**** now draw the contour surfaces  ****/
     /**********************************************************************************************************/
     if(!(gui->draw_high_lists && gui->fast_rotation_type==1)){ 
       for (i=0; i < gui->num_contour_surfaces; i++){
	 /*printf("in the loop for body %d\n",i);*/
	 if (gui->view_style != VIEW_STYLE_POLYGONAL) glEnable(GL_LIGHTING);
	 
	 glColor4f((float)gui->bod[i+MAX_BODS].r/65535.0,
		   (float)gui->bod[i+MAX_BODS].g/65535.0,
		   (float)gui->bod[i+MAX_BODS].b/65535.0,
		   (float)gui->bod[i+MAX_BODS].t/100.0);    
       
	 /*** if enabled, draw it ***/
	 if (gui->bod[i+MAX_BODS].enabled){
	   DEBUG_DATA printf("Drawing body: %s",gui->bod[i+MAX_BODS].name);
	   if ((!gui->bod[i+MAX_BODS].clipped) || 
	       (gui->draw_high_lists && gui->interactive_clipping &&
		gui->clipping_mode && gui->draw_clip_planes) || 
	       (!gui->draw_high_lists && gui->interactive_clipping && 
		gui->clipping_mode && gui->draw_clip_planes && gui->fast_rotation_type == 3)){ 
	     was_on = 1; 
	     unset_clipping(gui);
	   }else was_on = 0;
	   
	   /*printf("drawing list %d\n",CONTOUR_LISTS + i);*/
	   glCallList(CONTOUR_LISTS + i);
	 

	   if (was_on) set_clipping(gui);	
	 } 
       
	 if (gui->view_style != VIEW_STYLE_POLYGONAL) glDisable(GL_LIGHTING);
       }
       
     }

     unset_clipping(gui);
     glPopMatrix();

     glPopMatrix();

  if (gui->show_frame_rate && main_win){
    t2 = clock();
    num_frames = (float)(t2-t1)/CLOCKS_PER_SEC;
    if (num_frames != 0.0){
      num_frames = 1.0/num_frames;
      sprintf(frame_string,"%.2f fps",num_frames);
      /*display_string(frame_string, -225,-250,0);*/
      display_non_projected_string(gui,frame_string, -45,-45);
    }
  }

  if (gui->display_center_of_mass_strings){
    for (i=0;i<gui->num_bodies;i++)
      display_non_projected_string(gui,gui->bod[i].center_string,-48,50 - i*2);
    gui->display_center_of_mass_strings = 0;
  }

  if (gui->apeture_on && gui->beam_line_view){
    glDisable(GL_STENCIL_TEST);
    if (gui->cm){ 
      sprintf(ring_apeture_string,"Ring Diameter : %.2f cm",
	      (float)gui->ring_apeture_val/10.0);
      sprintf(apeture_string,
	      "View Diameter : %.2f cm",(float)gui->apeture_val/10.0);
    }else{ 
      sprintf(ring_apeture_string,"Ring Diameter : %d mm",gui->ring_apeture_val);
      sprintf(apeture_string,"View Diameter : %d mm",gui->apeture_val);
    }
    display_non_projected_string(gui,apeture_string, -48,48);
    display_non_projected_string(gui,ring_apeture_string, -48,46);
    glEnable(GL_STENCIL_TEST);
  }

  if (gui->doubleBuffer){
    if (!gui->single_buffer_on)
     glXSwapBuffers(gui->display, XtWindow(w));
   }
  else glFlush();
   
   /* Avoid indirect rendering latency from queuing. */
   if (!glXIsDirect(gui->display, gui->glxcontext)) glFinish();

   switch( glGetError() )
   {
       case GL_NO_ERROR:
       {
           break;
       }
       case GL_INVALID_ENUM:
       {
           printf("Warning OpenGL Error: GLenum argument out of range\n");
           break;
       }
       case GL_INVALID_VALUE:
       {
           printf("Warning OpenGL Error: numeric argument out of range\n");
           break;
       }
       case GL_INVALID_OPERATION:
       {
           printf("Warning OpenGL Error: operation illegal in current state\n");
           break;
       }
       case GL_STACK_OVERFLOW:
       {
           printf("Warning OpenGL Error: command would cause a stack overflow\n");
           break;
       }
       case GL_STACK_UNDERFLOW:
       {
           printf("Warning OpenGL Error: command would cause a stack underflow\n");
           break;
       }
       case GL_OUT_OF_MEMORY:
       {
           printf("Warning OpenGL Error: not enough memory left to execute command\n");
           break;
       }
       default:
       {
           printf("Unknown error found\n");
           break;
       }
   }

  DEBUG_TRACE_OUT printf("Done with draw\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : draw_all
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: calls draw on the main drawing area, and if 
%%%           multi-view is enabled it calls draw for the other
%%%           viewing windows also.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw_all(main_gui_t *gui)
{
  /*******************************************/
  /**  If the multi-view is on draw to all 
  /**  glwidgets
  /*******************************************/
  DEBUG_TRACE_IN printf("Entered draw_all\n");

  PostMessage(gui,"Redrawing . . .");
  
  draw(gui,gui->glxarea);
  if(XtIsManaged(gui->multiview_panel.form)){
    draw(gui,gui->multiview_panel.gl_lview_area);
    draw(gui,gui->multiview_panel.gl_rview_area);
    draw(gui,gui->multiview_panel.gl_tview_area);
  }
  RemoveMessage(gui,".");    

  DEBUG_TRACE_OUT printf("Done with draw_all\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : calculate_angle
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: x,y
%%%
%%%  Purpose: determines the angle from the x axis to the x,y point
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int calculate_angle(int x, int y)
{
  double precise_angle;
  int angle;

  DEBUG_TRACE_IN printf("Entered calculate_angle\n");

  if ((x > 0) && (y > 0)){
      /*********************************************/
      /*  First Quadrant
      /*********************************************/
    precise_angle = atan2((double)y,(double)x);

    angle = (int)((precise_angle)*(180.0/3.1415));
  } else if ((x < 0) && (y > 0)) {
    /*********************************************/
    /*  Second Quadrant
	/*********************************************/
    precise_angle = atan2((double)-x,(double)y);
    
    angle = (int)((precise_angle)*(180.0/3.1415));
    angle += 90;
  } else if ((x < 0) && (y < 0)) {
    /*********************************************/
    /*  Third Quadrant
	/*********************************************/
    precise_angle = atan2((double)-y,(double)-x);      
    
    angle = (int)((precise_angle)*(180.0/3.1415));
    angle += 180;
  } else if ((x > 0) && (y < 0)) {
    /*********************************************/
    /*  Fourth Quadrant
	/*********************************************/
    precise_angle = atan2((double)x,(double)-y);
    
    angle = (int)((precise_angle)*(180.0/3.1415));
    angle += 270;
  } else if (x == 0) {
    /*********************************************/
    /*  on y axis
	/*********************************************/
    if (y<0) angle = 270;
    else angle = 90;   
  } else if (y == 0) {
    /*********************************************/
    /*  on x axis
	/*********************************************/
    if (x < 0) angle = 180;
    else angle = 0;
  }

  DEBUG_TRACE_OUT printf("Done with calculate_angle, returning : %d\n",angle);
  return angle;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Draw_particle_paths
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: loops through the list of particle paths and
%%%           displays them based on their boldness, and type
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Draw_particle_paths(main_gui_t *gui)
{
  int i;
  int type;
  int was_on = 0;

  DEBUG_TRACE_IN printf("Entered Draw_particle_paths\n");

  unset_clipping(gui);

  if (glIsEnabled(GL_LIGHTING)){ 
    glDisable(GL_LIGHTING);
    was_on =1 ;
  }

  if (gui->antialiasing && !gui->rotating_messages)
    glEnable(GL_LINE_SMOOTH);
  
  for (i = 0;i<gui->num_particle_paths; i++){
    if (!glIsEnabled(GL_LINE_STIPPLE))
	  glEnable(GL_LINE_STIPPLE);

      switch ((int)gui->particle_path[i].type)
	{
	case 1:
	  /** gamma **/
	  glColor3f((float)(gui->line_types[GAMMA].color.red)/65535.0,
		    (float)(gui->line_types[GAMMA].color.green)/65535.0,
		    (float)(gui->line_types[GAMMA].color.blue)/65535.0);
	  type = GAMMA; break;
	  break;
	case 0:
	  /** NEUTRON **/
	        switch((int)gui->particle_path[i].energy)
		  {
		  case 3:
	            glColor3f((float)(gui->line_types[NEUTRON_LOW].color.red)/65535.0,
			      (float)(gui->line_types[NEUTRON_LOW].color.green)/65535.0,
			      (float)(gui->line_types[NEUTRON_LOW].color.blue)/65535.0);
	            type = NEUTRON_LOW; break;
		  case 2:
	            glColor3f((float)(gui->line_types[NEUTRON_MED].color.red)/65535.0,
			      (float)(gui->line_types[NEUTRON_MED].color.green)/65535.0,
			      (float)(gui->line_types[NEUTRON_MED].color.blue)/65535.0);
	            type = NEUTRON_MED; break;
		  case 1:
	            glColor3f((float)(gui->line_types[NEUTRON_HIGH].color.red)/65535.0,
			      (float)(gui->line_types[NEUTRON_HIGH].color.green)/65535.0,
			      (float)(gui->line_types[NEUTRON_HIGH].color.blue)/65535.0);
		    type = NEUTRON_HIGH; break;
		  }
		break;
	case 2:
	  /** BEAM **/
	        glColor3f((float)(gui->line_types[BEAM].color.red)/65535.0,
			  (float)(gui->line_types[BEAM].color.green)/65535.0,
			  (float)(gui->line_types[BEAM].color.blue)/65535.0);
		type = BEAM; break;
	case 3:
	  /** LOST **/
	        glColor3f((float)(gui->line_types[LOST].color.red)/65535.0,
			  (float)(gui->line_types[LOST].color.green)/65535.0,
			  (float)(gui->line_types[LOST].color.blue)/65535.0);
	        type = LOST; break;
	}

      switch (gui->line_types[type].type)
	{
	case 1:
	       glDisable(GL_LINE_STIPPLE);break;
	case 2:
	       glLineStipple(1,0x00FF);break;
	case 3:
	       glLineStipple(3,0x00FF);break;
	case 4:
	       glLineStipple(1,0xF279);break;
	}
      switch(gui->line_types[type].boldness)
	{
	case 1:
	        glLineWidth(1.0);break;
	case 2:
	        glLineWidth(2.0);break;
	case 3:
	        glLineWidth(3.0);break; 
	}      
      if (gui->ray_toggles[type])
	{
	  if (type == BEAM){ 
	    /*  printf("drawing the beam at : %.2f, %.2f, %.2f to %.2f, %.2f, %.2f\n",
		particle_paths[i][0],
		particle_paths[i][2],
		particle_paths[i][1],
		particle_paths[i][3],
		particle_paths[i][5],
		particle_paths[i][4]);*/
	    gui->beam_num = i;
	    /*  printf("beam_num is : %d\n",i);*/
	  }
	  

	  glBegin(GL_LINES);  
	  glVertex3f(gui->particle_path[i].start_x,
		     gui->particle_path[i].start_z,
		     gui->particle_path[i].start_y);
	  glVertex3f(gui->particle_path[i].end_x,
		     gui->particle_path[i].end_z,
		     gui->particle_path[i].end_y);
	  glEnd();
	}
    }

  if (glIsEnabled(GL_LINE_STIPPLE))
    glDisable(GL_LINE_STIPPLE);
  
  if (was_on)
    glEnable(GL_LIGHTING);
  
  if (gui->antialiasing && !gui->rotating_messages)
    glDisable(GL_LINE_SMOOTH);
  
  glLineWidth(1.0);
  
  set_clipping(gui);

  DEBUG_TRACE_OUT printf("Done with Draw_particle_paths\n");
}

void draw_crosshairs(main_gui_t *gui)
{
  int i,j;
  int z = 128;
  int width = 3;
  int height = gui->mainwindowsize;

  DEBUG_TRACE_IN printf("Entered draw_crosshairs\n");

  glMatrixMode (GL_PROJECTION);	/*  prepare for and then  */ 
  glPushMatrix();
  glLoadIdentity();
  glOrtho(-300,300,-300,300,gui->front_z,gui->back_z);
  glMatrixMode (GL_MODELVIEW);	/*  back to modelview matrix	*/

  /*if (!glIsEnabled(GL_LIGHTING))*/ glColor3f(1,1,1);
  /*else  set_full_material(1,1,1,1);*/
  /*glDisable(GL_DEPTH_TEST);*/

  glPushMatrix();
  glLoadIdentity();
  gluLookAt(0,0,200,0,0,0,0,1,0);

  unset_clipping(gui);
  /*
    for (i=0;i<6;i++)
      if (glIsEnabled(GL_CLIP_PLANE0+i))
	printf("clip plane %d is enabled\n",i);
	*/
  if (gui->crosshair_type <3){
    glBegin(GL_LINES);
    glVertex3f(0,gui->mainwindowsize/2,0);
    glVertex3f(0,-gui->mainwindowsize/2,0);
    
    glVertex3f(-gui->mainwindowsize/2,0,0);
    glVertex3f(gui->mainwindowsize/2,0,0); 
    
    glEnd();
    
    if (gui->crosshair_type == 2){
      glLineWidth(3.0);  
      glBegin(GL_LINES);
      glVertex3f(0,15,z);  glVertex3f(0,gui->mainwindowsize/2,z);
      glVertex3f(0,-15,z); glVertex3f(0,-gui->mainwindowsize/2,z);      
      glVertex3f(15,0,z);  glVertex3f(gui->mainwindowsize/2,0,z);      
      glVertex3f(-15,0,z); glVertex3f(-gui->mainwindowsize/2,0,z);
      glEnd();
      glLineWidth(1.0);
    }
  }else {
    glLineWidth(3.0);  
    glBegin(GL_LINES);
    glVertex3f(0,15,z);  glVertex3f(0,gui->mainwindowsize/2,z);
    glVertex3f(0,-15,z); glVertex3f(0,-gui->mainwindowsize/2,z);      
    glVertex3f(15,0,z);  glVertex3f(gui->mainwindowsize/2,0,z);      
    glVertex3f(-15,0,z); glVertex3f(-gui->mainwindowsize/2,0,z);
    glEnd();
    
    glLineWidth(1.0);  
    glBegin(GL_LINES);
    glVertex3f(0,15,z);   glVertex3f(15,0,z);  
    glVertex3f(15,0,z);   glVertex3f(0,-15,z);  
    glVertex3f(0,-15,z);  glVertex3f(-15,0,z);  
    glVertex3f(-15,0,z);  glVertex3f(0,15,z);  
    glEnd();
  }

  glPopMatrix();

  glMatrixMode (GL_PROJECTION);	/*  prepare for and then  */ 
  glPopMatrix();
  glMatrixMode (GL_MODELVIEW);	/*  back to modelview matrix	*/

  /*glEnable(GL_DEPTH_TEST);*/
  

  set_clipping(gui);

  DEBUG_TRACE_OUT printf("Done with draw_crosshairs\n");
}

void set_global_operations(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered set_global_operations\n");

   /*************************************************/
   /** Global Rotations
   /*************************************************/  
  /*if (gui->mouse_control_method == 1) 
     rotate_around_picked_axis();
     else{*/
      glRotatef(gui->rotation_x,1,0,0);
      glRotatef(gui->rotation_y,0,1,0);
      glRotatef(gui->rotation_z,0,0,1);   
      /*}*/

   /*************************************************/
   /** Global Scaling
   /*************************************************/  
   /*glScalef(scaling, scaling, scaling);*/

  DEBUG_TRACE_OUT printf("Done with set_global_operations\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Draw_IBeam(main_gui_t *gui)
{
  int was_on = 0;

  DEBUG_TRACE_IN printf("Entered Draw_IBeam\n");

  if (glIsEnabled(GL_LIGHTING)){ glDisable(GL_LIGHTING); was_on = 1;}

  glColor3f(0,1.0,0);
  glLineWidth(3.0);    
  glBegin(GL_LINES);
    glVertex3f(gui->ibeam_sx,gui->ibeam_sy,gui->ibeam_sz);
    glVertex3f(gui->ibeam_ex,gui->ibeam_ey,gui->ibeam_ez);
  glEnd();
  glLineWidth(1.0);  

  if (was_on) glEnable(GL_LIGHTING);

  DEBUG_TRACE_OUT printf("Done with Draw_IBeam\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_needed_locks(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered set_needed_locks\n");

  if (gui->bodies_locked){ 
    glPushMatrix();
    set_global_operations(gui);
  }
  DEBUG_TRACE_OUT printf("Done with set_needed_locks\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void unset_needed_locks(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered unset_needed_locks\n");
  if (gui->bodies_locked) glPopMatrix();
  DEBUG_TRACE_OUT printf("Done with unset_needed_locks\n");
}

void draw_test_sphere(main_gui_t *gui)
{
  static int first = 1;
  static GLUquadricObj *sphere;
  
  if (first){
    sphere = gluNewQuadric();
    first = 0;
  }
  glColor4f(0,1,0,.5);
  gluCylinder(sphere,100,10,125,25,25);
}

void draw_test_eight_cell(main_gui_t *gui)
{
  Cell_Triangle_t tris[10];
  int num_tris;
  char p1,p2,p3,p4,p5,p6,p7,p8;
  static GLUquadricObj *sphere;
  static int first = 1;
  int i,j;
  int d = 75;
  int o = 20;
  unsigned char val,cell;

  int x,y,z;
  
  if (first){
    sphere = gluNewQuadric();
    first = 0;
    gui->current_8cell = 0;
  }  

  
  p1 = gui->current_8cell & 1;
  p2 = gui->current_8cell & 2;
  p3 = gui->current_8cell & 4;
  p4 = gui->current_8cell & 8;
  p5 = gui->current_8cell & 16;
  p6 = gui->current_8cell & 32;
  p7 = gui->current_8cell & 64;
  p8 = gui->current_8cell & 128;

  cell = 0;
  if (p1) cell = cell | BIT_1;
  if (p2) cell = cell | BIT_2;
  if (p3) cell = cell | BIT_3;
  if (p4) cell = cell | BIT_4;
  if (p5) cell = cell | BIT_5;
  if (p6) cell = cell | BIT_6;
  if (p7) cell = cell | BIT_7;
  if (p8) cell = cell | BIT_8;
  
  determine_8_cell_vertex_ordering(gui,tris,&num_tris,cell);

  
  
  /** draw the endpoints **/
  if (!p1) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(-d,-d, d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("0", -(d+o), -(d+o), d+o);


  if (!p2) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(d,-d, d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("1", (d+o), -(d+o), d+o);

  if (!p3) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(d,-d,-d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("2", (d+o), -(d+o), -(d+o));


  if (!p4) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(-d,-d,-d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("3", -(d+o), -(d+o), -(d+o));  

  if (!p5) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(-d, d,-d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("4", -(d+o), (d+o), -(d+o));

  if (!p6) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(-d, d, d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("5", -(d+o), (d+o), d+o);  

  if (!p7) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(d, d, d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("6", (d+o), (d+o), d+o);

  if (!p8) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(d, d,-d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("7", (d+o), (d+o), -(d+o));



  glColor3f(1,0,0);
  glBegin(GL_TRIANGLES);
  for (i=0;i<num_tris;i++){
    for(j=0;j<3;j++){
      if (j == 0) val = tris[i].a;
      else if (j == 1) val = tris[i].b;
      else if (j == 2) val = tris[i].c;

      switch(val){
      case 0: x = -d;  y = -d;  z =  d; break;
      case 1: x =  d;  y = -d;  z =  d; break;
      case 2: x =  d;  y = -d;  z = -d; break;
      case 3: x = -d;  y = -d;  z = -d; break;
      case 4: x = -d;  y =  d;  z = -d; break;
      case 5: x = -d;  y =  d;  z =  d; break;
      case 6: x =  d;  y =  d;  z =  d; break;
      case 7: x =  d;  y =  d;  z = -d; break;
      }
      glNormal3f(tris[i].nx,tris[i].ny,tris[i].nz);
      glVertex3f(x,y,z);
    }

  }
  glEnd();
}



void draw_test_marching_cube_eight_cell(main_gui_t *gui)
{
  Cell_Triangle_t tris[10];
  int num_tris;
  char p0,p1,p2,p3,p4,p5,p6,p7;
  static GLUquadricObj *sphere;
  static int first = 1;
  int i,j;
  int d = 75;
  int o = 20;
  unsigned char val;
  unsigned char cell;
  float nx,ny,nz;
  float x,y,z;
  float v1x,v1y,v1z;
  float v2x,v2y,v2z;
  float v3x,v3y,v3z;

  if (first){
    sphere = gluNewQuadric();
    first = 0;
    gui->current_8cell = 0;
  }  
  
  p0 = gui->current_8cell & 1;
  p1 = gui->current_8cell & 2;
  p2 = gui->current_8cell & 4;
  p3 = gui->current_8cell & 8;
  p4 = gui->current_8cell & 16;
  p5 = gui->current_8cell & 32;
  p6 = gui->current_8cell & 64;
  p7 = gui->current_8cell & 128;
  cell = gui->current_8cell;

  /*get_marching_cube_polygons_for_8_cell(tris,&num_tris,cell);*/
  
  /** draw the endpoints **/
  if (!p3) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(-d,-d, d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("3", -(d+o), -(d+o), d+o);


  if (!p2) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(d,-d, d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("2", (d+o), -(d+o), d+o);

  if (!p1) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(d,-d,-d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("1", (d+o), -(d+o), -(d+o));


  if (!p0) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(-d,-d,-d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("0", -(d+o), -(d+o), -(d+o));  

  if (!p4) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(-d, d,-d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("4", -(d+o), (d+o), -(d+o));

  if (!p7) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(-d, d, d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("7", -(d+o), (d+o), d+o);  

  if (!p6) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(d, d, d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("6", (d+o), (d+o), d+o);

  if (!p5) glColor3f(.4,.4,.4);
  else glColor3f(0,0,1);
  glPushMatrix();
  glTranslatef(d, d,-d);
  gluSphere(sphere,10,25,25);
  glPopMatrix();
  display_string("5", (d+o), (d+o), -(d+o));



  glColor3f(1,0,0);
  glBegin(GL_TRIANGLES);
  for (i=0;i<num_tris;i++){

      switch(tris[i].a){
      case 0:  x = 0.5;   y = 1.0;   z = 0.0; break;
      case 1:  x = 1.0;   y = 0.5;   z = 0.0; break;
      case 2:  x = 0.5;   y = 0.0;   z = 0.0; break;
      case 3:  x = 0.0;   y = 0.5;   z = 0.0; break;
      case 4:  x = 0.5;   y = 1.0;   z = 1.0; break;
      case 5:  x = 1.0;   y = 0.5;   z = 1.0; break;
      case 6:  x = 0.5;   y = 0.0;   z = 1.0; break;
      case 7:  x = 0.0;   y = 0.5;   z = 1.0; break;
      case 8:  x = 0.0;   y = 1.0;   z = 0.5; break;
      case 9:  x = 1.0;   y = 1.0;   z = 0.5; break;
      case 10: x = 1.0;   y = 0.0;   z = 0.5; break;
      case 11: x = 0.0;   y = 0.0;   z = 0.5; break;
      }
      v1x = x * 2.0 * (float)d - (float)d;
      v1y = z * 2.0 * (float)d - (float)d;
      v1z = (1.0-y) * 2.0 * (float)d - (float)d;

      switch(tris[i].b){
      case 0:  x = 0.5;   y = 1.0;   z = 0.0; break;
      case 1:  x = 1.0;   y = 0.5;   z = 0.0; break;
      case 2:  x = 0.5;   y = 0.0;   z = 0.0; break;
      case 3:  x = 0.0;   y = 0.5;   z = 0.0; break;
      case 4:  x = 0.5;   y = 1.0;   z = 1.0; break;
      case 5:  x = 1.0;   y = 0.5;   z = 1.0; break;
      case 6:  x = 0.5;   y = 0.0;   z = 1.0; break;
      case 7:  x = 0.0;   y = 0.5;   z = 1.0; break;
      case 8:  x = 0.0;   y = 1.0;   z = 0.5; break;
      case 9:  x = 1.0;   y = 1.0;   z = 0.5; break;
      case 10: x = 1.0;   y = 0.0;   z = 0.5; break;
      case 11: x = 0.0;   y = 0.0;   z = 0.5; break;
      }
      v2x = x * 2.0 * (float)d - (float)d;
      v2y = z * 2.0 * (float)d - (float)d;
      v2z = (1.0-y) * 2.0 * (float)d - (float)d;

      switch(tris[i].c){
      case 0:  x = 0.5;   y = 1.0;   z = 0.0; break;
      case 1:  x = 1.0;   y = 0.5;   z = 0.0; break;
      case 2:  x = 0.5;   y = 0.0;   z = 0.0; break;
      case 3:  x = 0.0;   y = 0.5;   z = 0.0; break;
      case 4:  x = 0.5;   y = 1.0;   z = 1.0; break;
      case 5:  x = 1.0;   y = 0.5;   z = 1.0; break;
      case 6:  x = 0.5;   y = 0.0;   z = 1.0; break;
      case 7:  x = 0.0;   y = 0.5;   z = 1.0; break;
      case 8:  x = 0.0;   y = 1.0;   z = 0.5; break;
      case 9:  x = 1.0;   y = 1.0;   z = 0.5; break;
      case 10: x = 1.0;   y = 0.0;   z = 0.5; break;
      case 11: x = 0.0;   y = 0.0;   z = 0.5; break;
      }
      v3x = x * 2.0 * (float)d - (float)d;
      v3y = z * 2.0 * (float)d - (float)d;
      v3z = (1.0-y) * 2.0 * (float)d - (float)d;

      
      calculate_normal(v1x,v1y,v1z, 
		       v2x,v2y,v2z, 
		       v3x,v3y,v3z, 
		       &nx,&ny,&nz);
      glNormal3f(nx,ny,nz);
      

      /*printf("vertices : %f,%f,%f  %f,%f,%f  %f,%f,%f\n",v1x,v1y,v1z,v2x,v2y,v2z,v3x,v3y,v3z);*/
      glVertex3f(v1x,v1y,v1z);
      glVertex3f(v2x,v2y,v2z);
      glVertex3f(v3x,v3y,v3z);
  }
  glEnd();
}


