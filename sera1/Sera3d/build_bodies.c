#include "sera3d.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_complete_body_from_data_block
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: the type of body wanted (wireframe,solid)
%%%
%%%  Purpose: runs through the points stored with the body,
%%%           and build a glDisplayList based on the type of 
%%%           body wanted.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_complete_body_from_data_block(main_gui_t *gui,int body_style,int display_list_starting_index)
{
  int h,i,j,k,m,x,y,count=0,draw_on,found ;
  int zero_count = 0;
  int move_x, move_y;
  float z;
  float r,g,b;
  float step = .05;
  char is_inside;
  char message_string[256];
  char test = 0;
  int body_num;
  int point_count;
  int q;
  float wx,wy,wz;
  clock_t t1,t2;
  

  DEBUG_TRACE_IN printf("Entered build_complete_body_from_data_block\n");

  t1 = clock();


  /*glPointSize(gui->solid_point_size);*/

  for (body_num=1;body_num<gui->num_bodies;body_num++){

    fill_data_block_from_uv_using_bbox_for_body(gui,body_num);

    gui->solid_point_size = gui->z_spacing;
    for(q = 0; q < gui->rendering_quality-1; q++){
      reduce_resolution_of_data_block(gui);    
      gui->solid_point_size *= 2.0;
    }

    if (body_style == BODY_STYLE_SOLID){
      convert_data_block_to_binary_for_body(gui,body_num);
    }else{
      apply_body_containments_to_data_block(gui,body_num);
    }

    point_count = 0;


    glNewList((body_num-1)+display_list_starting_index,GL_COMPILE);

    glBegin(GL_POINTS);
    
    for (i=0;i<gui->block.z_size;i++){  
      for (j=0;j<gui->block.y_size;j++){
	for (k=0;k<gui->block.x_size;k++){

	  if (gui->block.data[i * gui->block.y_size*gui->block.x_size +  j*gui->block.x_size +  k] != 1) continue;

	  /** attempt to provide fake lighting **/
	  if (gui->view_style != VIEW_STYLE_WIREFRAME){
	    r = (float)gui->bod[body_num].r/65535.0;
	    g = (float)gui->bod[body_num].g/65535.0;
	    b = (float)gui->bod[body_num].b/65535.0;
	  }
	  is_inside = 1;
	    
	    
	  /** note if we just want the outline, don't bother to look above and below **/
	  if (body_style == BODY_STYLE_SOLID){
	    if (i < gui->block.z_size - 1){
	      /** above **/
	      if (gui->block.data[(i+1) * gui->block.y_size*gui->block.x_size +  j*gui->block.x_size +  k] != 1){
		if (gui->view_style != VIEW_STYLE_WIREFRAME){ r += step; g += step; b += step;} 
		is_inside = 0;
	      }
	    }
	    if (i!=0){
	      /** below **/
	      if (gui->block.data[(i-1) * gui->block.y_size*gui->block.x_size +  j*gui->block.x_size +  k] != 1){
		if (gui->view_style != VIEW_STYLE_WIREFRAME){ r -= step; g -= step; b -= step;} 
		is_inside = 0;	      
	      }
	    }	
	  }
	  
	  if (k < gui->block.x_size-1){
	    /** right **/
	    if (gui->block.data[i * gui->block.y_size*gui->block.x_size +  j*gui->block.x_size +  (k+1)] != 1){
	      if (gui->view_style != VIEW_STYLE_WIREFRAME){ r += step; g += step; b += step;} 
	      is_inside = 0;
	    }
	  }
	  
	  if (j < gui->block.y_size-1){
	    /** front **/
	    if (gui->block.data[i * gui->block.y_size*gui->block.x_size +  (j+1)*gui->block.x_size +  k] != 1){
	      if (gui->view_style != VIEW_STYLE_WIREFRAME){ r += step; g += step; b += step;} 
		is_inside = 0;
	      }
	    }
	    
	  if (k != 0){
	    /** left **/
	    if (gui->block.data[i * gui->block.y_size*gui->block.x_size +  j*gui->block.x_size +  (k-1)] != 1){
	      if (gui->view_style != VIEW_STYLE_WIREFRAME){ r -= step; g -= step; b -= step;} 
	      is_inside = 0;
	    }
	  }
	    
	  if (j != 0){
	    /** back **/
	    if (gui->block.data[i * gui->block.y_size*gui->block.x_size +  (j-1)*gui->block.x_size +  k] != 1){
	      if (gui->view_style != VIEW_STYLE_WIREFRAME){ r -= step; g -= step; b -= step;} 
	      is_inside = 0;
	    }
	  }
	  
	  if (gui->view_style != VIEW_STYLE_WIREFRAME){	    
	    if (r > 1.0) r = 1.0;
	    if (g > 1.0) g = 1.0;
	    if (b > 1.0) b = 1.0;
	    
	    if (r < 0.0) r = 0.0;
	    if (g < 0.0) g = 0.0;
	    if (b < 0.0) b = 0.0;
	  }	      
	    
	  if (body_style == BODY_STYLE_OUTLINE){
	    if (!is_inside){	      
	      if (gui->view_style != VIEW_STYLE_WIREFRAME) glColor3f(r,g,b);
	      point_count++;
	      convert_data_block_xyz_to_world_xyz(gui, k, j, i, &wx, &wy, &wz);
	      glVertex3f(wx,wy,wz);
	    }
	  }else{
	    if (gui->view_style != VIEW_STYLE_WIREFRAME) glColor3f(r,g,b);
	    point_count++;
	    convert_data_block_xyz_to_world_xyz(gui, k, j, i, &wx, &wy, &wz);
	    glVertex3f(wx,wy,wz);
	  }
	  

	}
      }
    }
    glEnd();
    glEndList();

    
    if (body_style == BODY_STYLE_OUTLINE)
      sprintf(message_string,"Outline Body: %s built . . . %d points",gui->bod[body_num].name,point_count);
    else
      sprintf(message_string,"Solid Body: %s built . . . %d points",gui->bod[body_num].name,point_count);
    PostMessage(gui,message_string);

    free_data_block(&gui->block);
  }

  /*free_data_block(&gui->block_b);*/
  /*free_data_block(&gui->block);*/


  t2 = clock();
  /*printf("Time it took to build the bodies : %.2f seconds\n",(float)(t2-t1)/CLOCKS_PER_SEC);*/
  
  DEBUG_TRACE_OUT printf("Done with build_complete_body_from_data_block\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_a_axial_square
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds a square parallel to the axial direction
%%%           used for displaying the axial slices
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_a_axial_square(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered build_a_axial_square\n");

  glNewList(AXIAL_SQUARE, GL_COMPILE);
  glBegin(GL_QUADS);
   glNormal3f(0.0,-1.0,0.0);
   glVertex3f(-gui->x_size, 0, -gui->y_size);
   glVertex3f( gui->x_size, 0, -gui->y_size);
   glVertex3f( gui->x_size, 0,  gui->y_size);
   glVertex3f(-gui->x_size, 0,  gui->y_size);
 glEnd();
 glEndList();

 DEBUG_TRACE_OUT printf("Done with build_a_axial_square\n");
}
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_a_coronal_square
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds a square parallel to the coronal direction
%%%           used for displaying the coronal slices
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_a_coronal_square(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered build_a_coronal_square\n");

  glNewList(CORONAL_SQUARE, GL_COMPILE);
  glBegin(GL_QUADS);
   glNormal3f(0.0,0.0,-1.0);
   glVertex3f(-gui->x_size , -(gui->z_spacing/2.0), 0);
   glVertex3f( gui->x_size , -(gui->z_spacing/2.0), 0);
   glVertex3f( gui->x_size ,   gui->z_spacing/2.0 , 0);
   glVertex3f(-gui->x_size ,   gui->z_spacing/2.0 , 0);
 glEnd();
 glEndList();

 DEBUG_TRACE_OUT printf("Done with build_a_coronal_square\n");
}
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_a_sagittal_square
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: builds a square parallel to the sagittal direction
%%%           used for displaying the sagittal slices
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_a_sagittal_square(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered build_a_sagittal_square\n");

  glNewList(SAGITTAL_SQUARE, GL_COMPILE);
  glBegin(GL_QUADS);
   glNormal3f(-1.0,0.0,0.0);
   glVertex3f(0, -(gui->z_spacing/2.0),  gui->y_size);
   glVertex3f(0, -(gui->z_spacing/2.0), -gui->y_size);
   glVertex3f(0,   gui->z_spacing/2.0 , -gui->y_size);
   glVertex3f(0,   gui->z_spacing/2.0 ,  gui->y_size);
   glEnd();
 glEndList();

 DEBUG_TRACE_OUT printf("Done with build_a_sagittal_square\n");
}

