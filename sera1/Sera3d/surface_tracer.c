#include "sera3d.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_polygonal_bodies_from_data_block_using_marching_cubes
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_polygonal_bodies(main_gui_t *gui)
{
  int body_num;
  char message_string[256];
  int q;
  clock_t t1,t2;
  unsigned char cell;

  DEBUG_TRACE_IN printf("Entered build_polygonal_bodies\n");


  for (body_num=1;body_num<gui->num_bodies;body_num++){
    
    t1 = clock();

    /** set up the data block to build from **/
    fill_data_block_from_uv_using_bbox_for_body(gui,body_num);    
    for(q = 0; q < gui->polygonal_rendering_quality-1; q++){
      reduce_resolution_of_data_block(gui);
    }
    apply_body_containments_to_data_block(gui,body_num);

    gui->polygon_count = 0;

    /** ok build the display list **/
    glNewList(body_num,GL_COMPILE); 
     extract_polygonal_surface_from_data_block(gui);
    glEndList();
    
    /** no longer need the data block **/
    free_data_block(&gui->block);
    

    t2 = clock();
    /** post the message to the message bar that we have finished **/       
    sprintf(message_string,"Polygonal Body: %s built . . . %d polygons (%.1f seconds)",
	    gui->bod[body_num].name,gui->polygon_count,(float)(t2-t1)/CLOCKS_PER_SEC);
    /*printf("%s\n",message_string);*/
    PostMessage(gui,message_string);

    DEBUG_DATA printf("%s\n",message_string);
  }
  
  /*glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);*/  
  DEBUG_TRACE_OUT printf("Done with build_polygonal_bodies\n");  
}





/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_polygonal_bodies_from_data_block_using_marching_cubes
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void build_polygonal_bodies_from_data_block_using_marching_cubes(main_gui_t *gui)
{
  int body_num,i,j,k, l,m;
  char message_string[256];
  char temp_count;
  unsigned char *t;

  char p0,p1,p2,p3,p4,p5,p6,p7;
  Cell_Triangle_t tris[10];
  int num_tris;
  unsigned char val;
  float x,y,z;
  float fx,fy,fz;
  float nx,ny,nz;
  float tx,ty,tz;
  int q;
  clock_t t1,t2;
  unsigned char cell;

  DEBUG_TRACE_IN printf("Entered build_polygonal_bodies\n");


  for (body_num=1;body_num<gui->num_bodies;body_num++){
    
    t1 = clock();
    
    fill_data_block_from_uv_using_bbox_for_body(gui,body_num);
    
    for(q = 0; q < gui->polygonal_rendering_quality-1; q++){
      reduce_resolution_of_data_block(gui);
    }
    apply_body_containments_to_data_block(gui,body_num);
    
    gui->polygon_count = 0;
    
    
    glNewList(body_num,GL_COMPILE);      
    
    for (i=0;i<gui->block.z_size-1;i++){ 
      glBegin(GL_TRIANGLES);	  
      for (j=0;j<gui->block.y_size-1;j++){
	for (k=0;k<gui->block.x_size-1;k++){
	  
	  p0 = gui->block.data[(i) * gui->block.x_size*gui->block.y_size + (j) * gui->block.x_size + (k)]; 
	  p1 = gui->block.data[(i) * gui->block.x_size*gui->block.y_size + (j) * gui->block.x_size + (k+1)]; 
	  p2 = gui->block.data[(i) * gui->block.x_size*gui->block.y_size + (j+1)*gui->block.x_size + (k+1)]; 
	  p3 = gui->block.data[(i) * gui->block.x_size*gui->block.y_size + (j+1)*gui->block.x_size + (k)]; 
	  p4 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j+1)*gui->block.x_size + (k)]; 
	  p5 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j) * gui->block.x_size + (k)]; 
	  p6 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j) * gui->block.x_size + (k+1)]; 
	  p7 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j+1)*gui->block.x_size + (k+1)]; 
	    
	  cell = 0;

	  if (p3) cell = cell | BIT_1;
	  if (p2) cell = cell | BIT_2;
	  if (p1) cell = cell | BIT_3;
	  if (p0) cell = cell | BIT_4;
	  if (p4) cell = cell | BIT_5;
	  if (p7) cell = cell | BIT_6;
	  if (p6) cell = cell | BIT_7;
	  if (p5) cell = cell | BIT_8;
	  
	  get_marching_cube_polygons_for_8_cell(gui,tris,&num_tris, cell);
	  
	  for (l=0;l<num_tris;l++){
	    for(m=0;m<3;m++){
	      if (m == 0) val = tris[l].a;
	      else if (m == 1) val = tris[l].b;
	      else if (m == 2) val = tris[l].c;
	      
	      switch(val){
	      case 0:  x = (float)k+.5;    y = (float)j+1;    z = (float)i;    break;
	      case 1:  x = (float)k+1;     y = (float)j+.5;   z = (float)i;    break;
	      case 2:  x = (float)k+.5;    y = (float)j;      z = (float)i;    break;
	      case 3:  x = (float)k;       y = (float)j+.5;   z = (float)i;    break;
	      case 4:  x = (float)k+.5;    y = (float)j+1;    z = (float)i+1;  break;
	      case 5:  x = (float)k+1;     y = (float)j+.5;   z = (float)i+1;  break;
	      case 6:  x = (float)k+.5;    y = (float)j;      z = (float)i+1;  break;
	      case 7:  x = (float)k;       y = (float)j+.5;   z = (float)i+1;  break;
	      case 8:  x = (float)k;       y = (float)j+1;    z = (float)i+.5; break;
	      case 9:  x = (float)k+1;     y = (float)j+1;    z = (float)i+.5; break;
	      case 10: x = (float)k+1;     y = (float)j;      z = (float)i+.5; break;
	      case 11: x = (float)k;       y = (float)j;      z = (float)i+.5; break;
	      }
	      
	      convert_data_block_floating_xyz_to_world_xyz(gui,
							   x,y,z,
							   &fx,&fy,&fz);
	      
	      convert_world_coord_to_texture_coord(gui,fx,fy,fz,&tx,&ty,&tz);
	      glTexCoord3f(tx,ty,tz);
	      
	      
	      find_normal_for_midpoint(gui->block.data, val, cell,
				       k, j, i,
				       gui->block.x_size, gui->block.y_size, gui->block.z_size,
				       &nx, &ny, &nz);
	      glNormal3f(nx,ny,nz);
	      glVertex3f(fx,fy,fz);
	    }
	    gui->polygon_count++;
	  }
	}
      }
      glEnd();	    
    }
    
    glEndList();
    
    free_data_block(&gui->block);
    
    t2 = clock();
    
    
    sprintf(message_string,"Polygonal Body: %s built . . . %d polygons (%.1f seconds)",
	    gui->bod[body_num].name,gui->polygon_count,(float)(t2-t1)/CLOCKS_PER_SEC);
    PostMessage(gui,message_string);
  }
  DEBUG_TRACE_OUT printf("Done with build_polygonal_bodies_\n");
}
*/


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_polygonal_bodies_from_data_block_using_8_cell_with_normal_mapping
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void build_polygonal_bodies_from_data_block_using_8_cell_with_normal_mapping(main_gui_t *gui)
{
  int body_num,i,j,k, l,m;
  char message_string[256];
  char temp_count;
  unsigned char *t;

  char p1,p2,p3,p4,p5,p6,p7,p8;
  Cell_Triangle_t tris[10];
  int num_tris;
  unsigned char val;
  int x,y,z;
  float fx,fy,fz;
  float nx,ny,nz;
  float tx,ty,tz;
  int q;
  clock_t t1,t2;
  Vertexf_t *Norms;

  DEBUG_TRACE_IN printf("Entered build_polygonal_bodies\n");


  for (body_num=1;body_num<gui->num_bodies;body_num++){

    t1 = clock();

    fill_data_block_from_uv_using_bbox_for_body(gui,body_num);
    
    for(q = 0; q < gui->polygonal_rendering_quality-1; q++){
      reduce_resolution_of_data_block(gui);
    }
    apply_body_containments_to_data_block(gui,body_num);

    gui->polygon_count = 0;

    /*** first run the 8cell algorithm to fill the normal map 
    if (!(Norms = (Vertexf_t *)malloc(sizeof(Vertexf_t) * gui->block.z_size * gui->block.x_size * gui->block.y_size))){
      printf("could not allocate enough mem for the normal map\n");
      break;
    }
      
    for (i=0;i<gui->block.z_size * gui->block.x_size * gui->block.y_size;i++){
      Norms[i].x = 0.0;
      Norms[i].y = 0.0;
      Norms[i].z = 0.0;
    }
    
    for (i=0;i<gui->block.z_size-1;i++){ 
      for (j=0;j<gui->block.y_size-1;j++){
	for (k=0;k<gui->block.x_size-1;k++){
	  
	  p1 = gui->block.data[(i)*gui->block.x_size*gui->block.y_size + (j)*gui->block.x_size + (k)]; 
	  p2 = gui->block.data[(i)*gui->block.x_size*gui->block.y_size + (j)*gui->block.x_size + (k+1)]; 
	  p3 = gui->block.data[(i)*gui->block.x_size*gui->block.y_size + (j+1)*gui->block.x_size + (k+1)]; 
	  p4 = gui->block.data[(i)*gui->block.x_size*gui->block.y_size + (j+1)*gui->block.x_size + (k)]; 
	  p5 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j+1)*gui->block.x_size + (k)]; 
	  p6 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j)*gui->block.x_size + (k)]; 
	  p7 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j)*gui->block.x_size + (k+1)]; 
	  p8 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j+1)*gui->block.x_size + (k+1)]; 
	  
	  determine_8_cell_vertex_ordering(gui,tris,&num_tris,p1,p2,p3,p4,p5,p6,p7,p8);
	  
	  for (l=0;l<num_tris;l++){
	    for(m=0;m<3;m++){
	      if (m == 0) val = tris[l].a;
	      else if (m == 1) val = tris[l].b;
	      else if (m == 2) val = tris[l].c;
	      
	      switch(val){
	      case 0: x = k;    z = i;    y = j;   break;
	      case 1: x = k+1;  z = i;    y = j;   break;
	      case 2: x = k+1;  z = i;    y = j+1; break;
	      case 3: x = k;    z = i;    y = j+1; break;
	      case 4: x = k;    z = i+1;  y = j+1; break;
	      case 5: x = k;    z = i+1;  y = j;   break;
	      case 6: x = k+1;  z = i+1;  y = j;   break;
	      case 7: x = k+1;  z = i+1;  y = j+1; break;
	      }    
	      
	      Norms[z*gui->block.x_size * gui->block.y_size + y*gui->block.x_size + x].x += tris[l].nx;
	      Norms[z*gui->block.x_size * gui->block.y_size + y*gui->block.x_size + x].y += tris[l].ny;
	      Norms[z*gui->block.x_size * gui->block.y_size + y*gui->block.x_size + x].z += tris[l].nz;
	    }
	  }
	  
	  
	}
      }
    }
    
    /* now normalize all the vertex normals 
    for (i=0;i<gui->block.z_size * gui->block.x_size * gui->block.y_size;i++)
      normalize(&Norms[i].x, &Norms[i].y, &Norms[i].z);
  
    
    /** ok  this time build the actual polygons 
    glNewList(body_num,GL_COMPILE);      
    
    for (i=0;i<gui->block.z_size-1;i++){ 
      glBegin(GL_TRIANGLES);	  
      for (j=0;j<gui->block.y_size-1;j++){
	for (k=0;k<gui->block.x_size-1;k++){
	  
	  p1 = gui->block.data[(i) * gui->block.x_size*gui->block.y_size + (j) * gui->block.x_size + (k)]; 
	  p2 = gui->block.data[(i) * gui->block.x_size*gui->block.y_size + (j) * gui->block.x_size + (k+1)]; 
	  p3 = gui->block.data[(i) * gui->block.x_size*gui->block.y_size + (j+1)*gui->block.x_size + (k+1)]; 
	  p4 = gui->block.data[(i) * gui->block.x_size*gui->block.y_size + (j+1)*gui->block.x_size + (k)]; 
	  p5 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j+1)*gui->block.x_size + (k)]; 
	  p6 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j) * gui->block.x_size + (k)]; 
	  p7 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j) * gui->block.x_size + (k+1)]; 
	  p8 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j+1)*gui->block.x_size + (k+1)]; 
	  
	  determine_8_cell_vertex_ordering(gui,tris,&num_tris,p1,p2,p3,p4,p5,p6,p7,p8);
	  
	  for (l=0;l<num_tris;l++){
	    for(m=0;m<3;m++){
	      if (m == 0) val = tris[l].a;
	      else if (m == 1) val = tris[l].b;
	      else if (m == 2) val = tris[l].c;
	      
	      switch(val){
	      case 0: x = k;    z = i;    y = j;   break;
	      case 1: x = k+1;  z = i;    y = j;   break;
	      case 2: x = k+1;  z = i;    y = j+1; break;
	      case 3: x = k;    z = i;    y = j+1; break;
	      case 4: x = k;    z = i+1;  y = j+1; break;
	      case 5: x = k;    z = i+1;  y = j;   break;
	      case 6: x = k+1;  z = i+1;  y = j;   break;
	      case 7: x = k+1;  z = i+1;  y = j+1; break;
	      }
	      
	      convert_data_block_xyz_to_world_xyz(gui,
						  x,y,z,
						  &fx,&fy,&fz);
	      
	      
	      /** fill in the texture coordinate 
	      convert_world_coord_to_texture_coord(gui,fx,fy,fz,&tx,&ty,&tz);
	      glTexCoord3f(tx,ty,tz);
	      
	      glNormal3f(Norms[z*gui->block.x_size * gui->block.y_size + y*gui->block.x_size + x].x,
			 Norms[z*gui->block.x_size * gui->block.y_size + y*gui->block.x_size + x].y,
			 Norms[z*gui->block.x_size * gui->block.y_size + y*gui->block.x_size + x].z);
	      
	      glVertex3f(fx,fy,fz);
	    }
	    gui->polygon_count++;
	  }
	}
      }
      glEnd();	    
    }
    
    glEndList();
    
    free_data_block(&gui->block);
    free(Norms);
    
    t2 = clock();
    
    
    sprintf(message_string,"Polygonal Body: %s built . . . %d polygons (%.1f seconds)",
	    gui->bod[body_num].name,gui->polygon_count,(float)(t2-t1)/CLOCKS_PER_SEC);
    PostMessage(gui,message_string);
  }
  
  DEBUG_TRACE_OUT printf("Done with build_polygonal_bodies_\n");
}
    */



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_polygonal_bodies_from_data_block_using_8_cell
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void build_polygonal_bodies_from_data_block_using_8_cell(main_gui_t *gui)
{
  int body_num,i,j,k, l,m;
  char message_string[256];
  char temp_count;
  unsigned char *t;

  char p1,p2,p3,p4,p5,p6,p7,p8;
  Cell_Triangle_t tris[10];
  int num_tris;
  unsigned char val;
  int x,y,z;
  float fx,fy,fz;
  float nx,ny,nz;
  float tx,ty,tz;
  int q;
  clock_t t1,t2;

  DEBUG_TRACE_IN printf("Entered build_polygonal_bodies_from_data_block_using_8_cell\n");



  for (body_num=1;body_num<gui->num_bodies;body_num++){

    t1 = clock();
    
    fill_data_block_from_uv_using_bbox_for_body(gui,body_num);
    
    for(q = 0; q < gui->polygonal_rendering_quality-1; q++){
      reduce_resolution_of_data_block(gui);
    }
    apply_body_containments_to_data_block(gui,body_num);
    gui->polygon_count = 0;

    glNewList(body_num,GL_COMPILE);      

    for (i=0;i<gui->block.z_size-1;i++){ 
      glBegin(GL_TRIANGLES);	  
      for (j=0;j<gui->block.y_size-1;j++){
	for (k=0;k<gui->block.x_size-1;k++){

	  p1 = gui->block.data[(i)*gui->block.x_size*gui->block.y_size + (j)*gui->block.y_size + (k)]; 
	  p2 = gui->block.data[(i)*gui->block.x_size*gui->block.y_size + (j)*gui->block.y_size + (k+1)]; 
	  p3 = gui->block.data[(i)*gui->block.x_size*gui->block.y_size + (j+1)*gui->block.y_size + (k+1)]; 
	  p4 = gui->block.data[(i)*gui->block.x_size*gui->block.y_size + (j+1)*gui->block.y_size + (k)]; 
	  p5 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j+1)*gui->block.y_size + (k)]; 
	  p6 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j)*gui->block.y_size + (k)]; 
	  p7 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j)*gui->block.y_size + (k+1)]; 
	  p8 = gui->block.data[(i+1)*gui->block.x_size*gui->block.y_size + (j+1)*gui->block.y_size + (k+1)]; 
	  
	  determine_8_cell_vertex_ordering(gui,tris,&num_tris,p1,p2,p3,p4,p5,p6,p7,p8);
	  
	  for (l=0;l<num_tris;l++){
	    for(m=0;m<3;m++){
	      if (m == 0) val = tris[l].a;
	      else if (m == 1) val = tris[l].b;
	      else if (m == 2) val = tris[l].c;
	      
	      switch(val){
	      case 0: x = k;    z = i;    y = j;   break;
	      case 1: x = k+1;  z = i;    y = j;   break;
	      case 2: x = k+1;  z = i;    y = j+1; break;
	      case 3: x = k;    z = i;    y = j+1; break;
	      case 4: x = k;    z = i+1;  y = j+1; break;
	      case 5: x = k;    z = i+1;  y = j;   break;
	      case 6: x = k+1;  z = i+1;  y = j;   break;
	      case 7: x = k+1;  z = i+1;  y = j+1; break;
	      }
	      
	      convert_data_block_xyz_to_world_xyz(gui,x,y,z,&fx,&fy,&fz);
	      
	      /** just put a normal for each triangle (first vertex) since we are flat shading* 
	      if (m == 0) glNormal3f(tris[l].nx,tris[l].ny,tris[l].nz);
	    
	      /** fill in the texture coordinate *
	      convert_world_coord_to_texture_coord(gui,fx,fy,fz,&tx,&ty,&tz);
	      glTexCoord3f(tx,ty,tz);
	      
	      glVertex3f(fx,fy,fz);
	    }
	    gui->polygon_count++;
	  }
	}
      }
      glEnd();	    
    }

    glEndList();
    
    free_data_block(&gui->block);
    
    t2 = clock();
    sprintf(message_string,"Polygonal Body: %s built . . . %d polygons (%.2f secs)",
	    gui->bod[body_num].name,gui->polygon_count, (float)(t2-t1)/CLOCKS_PER_SEC);
    PostMessage(gui,message_string);
  }
  
  DEBUG_TRACE_OUT printf("Entered build_polygonal_bodies_from_data_block_using_8_cell\n");
}
	      */

