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
void extract_polygonal_surface_from_data_block(main_gui_t *gui)
{
  
  DEBUG_TRACE_IN printf("Entered extract_polygonal_surface_from_data_block\n");

  switch(gui->polygonal_algorithm){
  case VERTEXCELL_WITH_SURFACE_NORMALS:
    extract_polygonal_surface_from_data_block_using_8_cell(gui);
    break;
  case VERTEXCELL_WITH_VERTEX_NORMALS:
    extract_polygonal_surface_from_data_block_using_8_cell_with_normal_mapping(gui);
    break;
  case MARCHING_CUBES:
    construct_midpoint_normal_array();
    extract_polygonal_surface_from_data_block_using_marching_cubes(gui);
    destroy_midpoint_normal_array();
    break;
  }
  DEBUG_TRACE_OUT printf("Done with extract_polygonal_surface_from_data_block\n");
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
void extract_polygonal_surface_from_data_block_using_8_cell(main_gui_t *gui)
{
  int i,j,k,l,m;
  char p1,p2,p3,p4,p5,p6,p7,p8;
  Cell_Triangle_t tris[10];
  int num_tris;
  int x,y,z;
  float fx,fy,fz;
  float tx,ty,tz;
  unsigned char val,cell;
  float nx,ny,nz;

  DEBUG_TRACE_IN printf("Entered build_polygonal_surface_from_data_block_using_8_cell\n");

  gui->polygon_count = 0;

  glBegin(GL_TRIANGLES);	  
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
	    /*fy = z * gui->z_spacing + gui->contour_shift;*/
	    
	    	      	
	    /** fill in the texture coordinate **/
	    convert_world_coord_to_texture_coord(gui,fx,fy,fz,&tx,&ty,&tz);
	    glTexCoord3f(tx,ty,tz);
	   	    
	    if (m==0) glNormal3f(tris[l].nx,tris[l].ny,tris[l].nz);
	    
	    glVertex3f(fx,fy,fz);
	  }
	  gui->polygon_count++;
	}
      }
    }
  }
  glEnd();	    

  DEBUG_TRACE_OUT printf("Done with build_polygonal_surface_from_data_block_using_8_cell\n");
}


void extract_polygonal_surface_from_data_block_using_8_cell_with_normal_mapping(main_gui_t *gui)
{
  int i,j,k,l,m;
  char p1,p2,p3,p4,p5,p6,p7,p8;
  Cell_Triangle_t tris[10];
  int num_tris;
  int x,y,z;
  float fx,fy,fz;
  float tx,ty,tz;
  unsigned char val,cell;
  float nx,ny,nz;
  Vertexf_t *Norms;

  gui->polygon_count = 0;

  DEBUG_TRACE_IN printf("Entered build_polygonal_surface_from_data_block_using_8_cell_with_normal_mapping\n");

  if (gui->polygonal_algorithm == VERTEXCELL_WITH_VERTEX_NORMALS){ 
    /*** first run the 8cell algorithm to fill the normal map */   
    if (!(Norms = (Vertexf_t *)MT_malloc(sizeof(Vertexf_t) * gui->block.z_size * gui->block.x_size * gui->block.y_size))){
      printf("could not allocate enough mem for the normal map\n");
      return;
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
    /* now normalize all the vertex normals */
    for (i=0;i<gui->block.z_size * gui->block.x_size * gui->block.y_size;i++)
      normalize(&Norms[i].x, &Norms[i].y, &Norms[i].z);
  }

  /* now pull the polygons from the algorithm */
  glBegin(GL_TRIANGLES);	  
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
	    /*fy = z * gui->z_spacing + gui->contour_shift;*/
	    
	    	      	
	    /** fill in the texture coordinate **/
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
  }
  glEnd();	    

  if (gui->polygonal_algorithm == VERTEXCELL_WITH_VERTEX_NORMALS)
    MT_free( (void *) Norms);

  DEBUG_TRACE_OUT printf("Done with build_polygonal_surface_from_data_block_using_8_cell_with_normal_mapping\n");
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
void extract_polygonal_surface_from_data_block_using_marching_cubes(main_gui_t *gui)
{
  int i,j,k, l,m;
  char message_string[256];
  char temp_count;
  unsigned char *t;

  char p0,p1,p2,p3,p4,p5,p6,p7;
  Cell_Triangle_t tris[20];
  int num_tris;
  unsigned char val;
  float x,y,z;
  float fx,fy,fz;
  float nx,ny,nz;
  float tx,ty,tz;
  int q;
  clock_t t1,t2;
  unsigned char cell;
  int special_case_val;

  DEBUG_TRACE_IN printf("Entered extract_polygonal_surface_from_data_block_using_marching_cubes\n");

  convert_data_block_to_cube_values ( gui );

  glBegin(GL_TRIANGLES);	  
    for (i=0;i<gui->block.z_size;i++){ 
      for (j=0;j<gui->block.y_size;j++){
	for (k=0;k<gui->block.x_size;k++){
	  
	  special_case_val = get_marching_cube_polygons_for_8_cell ( gui, tris, &num_tris, k, j, i );

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
	      
    
	      cell = gui->block.data[i*gui->block.x_size*gui->block.y_size + j*gui->block.x_size + k]; 

	      find_normal_for_midpoint ( gui->block.data, val, cell,
					 special_case_val, k, j, i,
					 gui->block.x_size, gui->block.y_size, gui->block.z_size,
					 &nx, &ny, &nz );
	      glNormal3f(nx,ny,nz);

	      glVertex3f(fx,fy,fz);
	    }
	    gui->polygon_count++;
	  }
	}
      }
    }
    glEnd();

    DEBUG_TRACE_OUT printf("Done with extract_polygonal_surface_from_data_block_using_marching_cubes\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: 
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int convert_data_block_to_cube_values ( main_gui_t *gui )
{
    unsigned char *cube_vals, *data;
    int xsize = gui->block.x_size - 1;
    int ysize = gui->block.y_size - 1;
    int zsize = gui->block.z_size - 1;
    int x, y, z;

    data = gui->block.data;

    cube_vals = ( unsigned char * ) MT_malloc ( xsize*ysize*zsize );

    memset ( cube_vals, 0, xsize*ysize*zsize );

    for ( z = 0; z < zsize; z++ )
      for ( y = 0; y < ysize; y++ )
	for ( x = 0; x < xsize; x++ )
	  {
	    if ( data[(z  )*gui->block.y_size*gui->block.x_size + (y  )*gui->block.x_size + (x  )] )  
	        cube_vals[z*ysize*xsize + y*xsize + x] |= BIT_4;
	    if ( data[(z  )*gui->block.y_size*gui->block.x_size + (y  )*gui->block.x_size + (x+1)] )  
	        cube_vals[z*ysize*xsize + y*xsize + x] |= BIT_3;
	    if ( data[(z  )*gui->block.y_size*gui->block.x_size + (y+1)*gui->block.x_size + (x+1)] )  
	        cube_vals[z*ysize*xsize + y*xsize + x] |= BIT_2;
	    if ( data[(z  )*gui->block.y_size*gui->block.x_size + (y+1)*gui->block.x_size + (x  )] )  
	        cube_vals[z*ysize*xsize + y*xsize + x] |= BIT_1;
	    if ( data[(z+1)*gui->block.y_size*gui->block.x_size + (y  )*gui->block.x_size + (x  )] )  
	        cube_vals[z*ysize*xsize + y*xsize + x] |= BIT_8;
	    if ( data[(z+1)*gui->block.y_size*gui->block.x_size + (y  )*gui->block.x_size + (x+1)] )  
	        cube_vals[z*ysize*xsize + y*xsize + x] |= BIT_7;
	    if ( data[(z+1)*gui->block.y_size*gui->block.x_size + (y+1)*gui->block.x_size + (x+1)] )  
	        cube_vals[z*ysize*xsize + y*xsize + x] |= BIT_6;
	    if ( data[(z+1)*gui->block.y_size*gui->block.x_size + (y+1)*gui->block.x_size + (x  )])  
	        cube_vals[z*ysize*xsize + y*xsize + x] |= BIT_5;
	  }

    gui->block.x_size--;
    gui->block.y_size--;
    gui->block.z_size--;

    MT_free ( (void *) gui->block.data );
    gui->block.data = cube_vals;

    return ( 1 );
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
void fill_data_block_from_uv_using_bbox_for_body(main_gui_t *gui,int body_num)
{
  int xsize,ysize,zsize;
  int x,y,z;
  int min_z,max_z,min_y,max_y,min_x,max_x;

  DEBUG_TRACE_IN printf("Entered fill_data_block_from_uv_using_bbox_for_body\n");
  /*printf("filling the data block from uv_file : %s\n",gui->uvfile);*/

  if(!read_uv_file(gui)){
    printf("Couldn't read the uv file : %s, aborting\n",gui->uvfile);
    return;
  }

  min_z = gui->bod[body_num].min_z-2;
  max_z = gui->bod[body_num].max_z+2 + gui->polygonal_rendering_quality*2;
  min_y = gui->bod[body_num].min_y-2;
  max_y = gui->bod[body_num].max_y+2 + gui->polygonal_rendering_quality*2;
  min_x = gui->bod[body_num].min_x-2;
  max_x = gui->bod[body_num].max_x+2 + gui->polygonal_rendering_quality*2;

  if (min_z < 0 ) min_z = 0;

  /* Warning:  MTC and MBR changed this because data was not being
     displayed from the uv/uvh files.  We're a littled worried about
     going out of array bounds... but we haven't found any problems
     yet.  May 25, 1999 */
  
  /*if (max_z > gui->num_slices-1 ) max_z = gui->num_slices-1;*/
  if (max_z > gui->num_slices ) max_z = gui->num_slices;

  if (min_x < 0)   min_x = 0;
  if (max_x > 255) max_x = 255;

  if (min_y < 0)   min_y = 0;
  if (max_y > 255) max_y = 255;

  /*
    printf("filling data_block_from_uv_for_body : %s -> min_z: %d, max_z: %d, min_y: %d, max_y: %d, min_x: %d, max_x: %d\n",
    gui->bod[body_num].name,min_z,max_z, min_y,max_y,min_x,max_x);
  */

  xsize = (max_x - min_x);
  ysize = (max_y - min_y);
  zsize = (max_z - min_z);

  /*
    printf(" xsize: %d  , ysize: %d,  zsize: %d\n", xsize, ysize, zsize);
  */  

  gui->block.data = (unsigned char *)MT_malloc(xsize*ysize*zsize);

  gui->block.x_size = xsize;
  gui->block.y_size = ysize;
  gui->block.z_size = zsize;
  gui->block.x_resolution = 1;
  gui->block.y_resolution = 1;
  gui->block.z_resolution = 1;

  gui->block.x_offset = min_x;
  gui->block.y_offset = min_y;
  gui->block.z_offset = min_z;


   for (z=0;z<zsize;z++)
    for (y=0;y<ysize;y++)
      for (x=0;x<xsize;x++){
	gui->block.data[z*xsize*ysize + y*xsize + x] = 
	  gui->volume[(z+min_z) * 256 * 256 + (y+min_y) * 256 + (x+min_x)];
      }

  MT_free( (void *) gui->volume);

  /*printf("done filling the data block with bbox\n");*/


 DEBUG_TRACE_OUT printf("Done with fill_data_block_from_uv_using_bbox_for_body\n");
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
void fill_data_block_from_uv(main_gui_t *gui){

  DEBUG_TRACE_IN printf("Entered fill_data_block_from_uv, for uvfile: %s\n",gui->uvfile);
  
  if(!read_uv_file(gui)){
    printf("Couldn't read the uv file : %s, aborting\n",gui->uvfile);
    return;
  }
  
  gui->block.data = (unsigned char *)MT_malloc(gui->num_slices * 256 * 256);

  gui->block.x_size = 256;
  gui->block.y_size = 256;
  gui->block.z_size = gui->num_slices;
  gui->block.x_resolution = 1;
  gui->block.y_resolution = 1;
  gui->block.z_resolution = 1;
  
  gui->block.x_offset = 0;
  gui->block.y_offset = 0;
  gui->block.z_offset = 0;

  memcpy(gui->block.data,gui->volume,gui->num_slices * 256 * 256);

  MT_free( (void *) gui->volume);

  DEBUG_TRACE_OUT printf("Done with fill_data_block_from_uv\n");
}

void backup_data_block(main_gui_t *gui)
{
  int full_size;
  
  gui->block_b.x_size = gui->block.x_size;
  gui->block_b.y_size = gui->block.y_size;
  gui->block_b.z_size = gui->block.z_size;
  gui->block_b.x_resolution = gui->block.x_resolution;
  gui->block_b.y_resolution = gui->block.y_resolution;
  gui->block_b.z_resolution = gui->block.z_resolution;

  full_size = gui->block_b.x_size * gui->block_b.y_size * gui->block_b.z_size;

  gui->block_b.data = (unsigned char *) MT_malloc (full_size);
  memcpy(gui->block_b.data,gui->block.data,full_size);
}

void fill_data_block_from_backup(main_gui_t *gui)
{
  int full_size;

  gui->block.x_size = gui->block_b.x_size;
  gui->block.y_size = gui->block_b.y_size;
  gui->block.z_size = gui->block_b.z_size;
  gui->block.x_resolution = gui->block_b.x_resolution;
  gui->block.y_resolution = gui->block_b.y_resolution;
  gui->block.z_resolution = gui->block_b.z_resolution;
  
  full_size = gui->block.x_size * gui->block.y_size * gui->block.z_size;

  gui->block.data = (unsigned char *) MT_malloc (full_size);
  memcpy(gui->block.data,gui->block_b.data,full_size);
}

void quick_copy_data_block_from_backup(main_gui_t *gui)
{
  memcpy(gui->block.data,gui->block_b.data, gui->block.x_size*gui->block.y_size*gui->block.z_size);
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
void reduce_resolution_of_data_block(main_gui_t *gui){
  unsigned char *old_data;
  int old_x_size, old_y_size, old_z_size;
  int i,j,k;

  DEBUG_TRACE_IN printf("Entered reduce_resolution_of_data_block\n");

  old_data = gui->block.data;
  old_x_size = gui->block.x_size;
  old_y_size = gui->block.y_size;
  old_z_size = gui->block.z_size;
  

  if (gui->block.z_size > 64){
    /** there are plenty of slices, cut the  number of slices in half also **/    
    gui->block.x_size /= 2;
    gui->block.y_size /= 2;
    gui->block.z_size /= 2;
    
    gui->block.x_resolution *= 2;
    gui->block.z_resolution *= 2;
    gui->block.y_resolution *= 2;
    
    gui->block.data = (unsigned char *)MT_malloc(gui->block.x_size * gui->block.y_size * gui->block.z_size);
    
    for (i=0; i<gui->block.z_size; i++)
      for (j=0; j<gui->block.y_size; j++)
	for (k=0; k<gui->block.x_size; k++)
	  gui->block.data[i*gui->block.x_size*gui->block.y_size + j*gui->block.x_size + k] = 
	    old_data[i*2*old_x_size*old_y_size + j*2*old_x_size + k*2];
    
  }else{
    /** this code will not reduce the z resolution only the x and y **/
    
    gui->block.x_size /= 2;
    gui->block.y_size /= 2;
    
    gui->block.x_resolution *= 2;
    gui->block.y_resolution *= 2;
    
    gui->block.data = (unsigned char *)MT_malloc(gui->block.x_size * gui->block.y_size * gui->block.z_size);
    
    for (i=0; i<gui->block.z_size; i++)
      for (j=0; j<gui->block.y_size; j++)
	for (k=0; k<gui->block.x_size; k++)
	  gui->block.data[i*gui->block.x_size*gui->block.y_size + j*gui->block.x_size + k] = 
	    old_data[i*old_x_size*old_y_size + j*2*old_x_size + k*2];
  } 
    
  MT_free( (void *) old_data);

  DEBUG_TRACE_OUT printf("Done with reduce_resolution_of_data_block\n");
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

void free_data_block(Data_block_t *block){
  DEBUG_TRACE_IN printf("Entered free_data_block\n");
  
  MT_free( (void *) block->data);
  block->x_size = 0;
  block->y_size = 0;
  block->z_size = 0;
  block->x_resolution = 0;
  block->y_resolution = 0;
  block->z_resolution = 0;

  DEBUG_TRACE_OUT printf("Done with free_data_block\n");
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
void make_test_data_block(main_gui_t *gui)
{
  int SIZE = 32;
  int i,j,k;
  float dist;

  gui->block.data = (unsigned char *)MT_malloc(SIZE * SIZE * SIZE);

  memset(gui->block.data,0,SIZE*SIZE*SIZE);

  for (i=1;i<SIZE-1;i++)
    for(j=1;j<SIZE-1;j++)
      for (k=1;k<SIZE-1;k++){
	dist = sqrt( (k-SIZE/2)*(k-SIZE/2) + (j-SIZE/2)*(j-SIZE/2) + (i-SIZE/2)*(i-SIZE/2) );
	if (dist < (float)(SIZE-4)/2.0){
	  gui->block.data[i*SIZE*SIZE+j*SIZE+k] = gui->bod[1].region_num;
	}
      }
  gui->block.x_size = SIZE;
  gui->block.y_size = SIZE;
  gui->block.z_size = SIZE;
  gui->block.x_resolution = 2;
  gui->block.y_resolution = 2;
  gui->block.z_resolution = 2;

  gui->num_slices = SIZE;
  gui->z_spacing = 1.0;
  gui->y_size = 1.0;
  gui->x_size = 1.0;
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
void apply_body_containments_to_data_block(main_gui_t *gui,int body_num)
{
  int i;
  int region_map[256];
  int current_bod;

  DEBUG_TRACE_IN printf("Entered apply_body_containments_to_data_block\n");


  for (i=0;i<256;i++) region_map[i] = 0;

  /** fill the current region and its contained regions with 1 **/
  region_map[gui->bod[body_num].region_num] = 1;

  for (i=0;i<gui->bod[body_num].num_contained_bods;i++){
    current_bod = gui->bod[body_num].contained_bods[i];
    region_map[ gui->bod[current_bod].region_num] = 1;
  }

  for (i=0;i<gui->block.x_size*gui->block.y_size*gui->block.z_size;i++) 
    gui->block.data[i] = region_map[gui->block.data[i]];  

  DEBUG_TRACE_OUT printf("Done with apply_body_containments_to_data_block\n");
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
void convert_data_block_to_binary_for_body(main_gui_t *gui,int body_num)
{
  int i;

  for (i=0;i<gui->block.x_size*gui->block.y_size*gui->block.z_size;i++) 
    if (gui->block.data[i] == gui->bod[body_num].region_num)
      gui->block.data[i] = 1;
    else
      gui->block.data[i] = 0;

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
void convert_data_block_floating_xyz_to_world_xyz(main_gui_t *gui, float orig_x, float orig_y, float orig_z, float *wx, float *wy, float *wz)
{
  float x,y,z;

  x = orig_x * gui->block.x_resolution + (float)gui->block.x_offset;
  y = orig_y * gui->block.y_resolution + (float)gui->block.y_offset;
  z = orig_z * gui->block.z_resolution + (float)gui->block.z_offset;


  *wx = ((x) - 128) * gui->x_size;

  *wy = (float)gui->num_slices/2.0 * gui->z_spacing - 
    (gui->num_slices - (z)) * gui->z_spacing   + (gui->z_spacing)/2.0;	      

  *wz = ( (255 - y)- 128) * gui->y_size;
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
void convert_data_block_xyz_to_world_xyz(main_gui_t *gui, int orig_x, int orig_y, int orig_z, float *wx, float *wy, float *wz)
{
  int x,y,z;

  x = orig_x * gui->block.x_resolution + gui->block.x_offset;
  y = orig_y * gui->block.y_resolution + gui->block.y_offset;
  z = orig_z * gui->block.z_resolution + gui->block.z_offset;


  *wx = ((x) - 128) * gui->x_size;

  *wy = (float)gui->num_slices/2.0 * gui->z_spacing - 
    (gui->num_slices - (z)) * gui->z_spacing   + (gui->z_spacing)/2.0;	      

  *wz = ( (255 - y)- 128) * gui->y_size;


  /*  
  *wx = ((x) - gui->block.x_size/2) * gui->block.x_resolution * gui->x_size;

  *wy = (float)gui->block.z_size/2.0*gui->z_spacing*gui->block.z_resolution - 
    (gui->block.z_size - (z)) * gui->z_spacing*gui->block.z_resolution 
    + (gui->z_spacing*gui->block.z_resolution)/2.0;	      

  *wz = (( (gui->block.y_size-1) -  (y)  )- gui->block.y_size/2) * gui->block.y_resolution * gui->y_size;	             
  */
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
void find_center_mass_for_bodies_in_data_block(main_gui_t *gui)
{
  int i,x,y,z;
  int num_body_pts[MAX_BODS];
  int avx[MAX_BODS];
  int avy[MAX_BODS];
  int avz[MAX_BODS];
  int val;

  DEBUG_TRACE_IN printf("Entered find_center_mass_for_bodies_in_data_block\n");

  PostMessage( gui, "Calculating center of mass for bodies . . ." );
  
  for(i=0;i<MAX_BODS;i++){
    num_body_pts[i] = 0;
    avx[i] = 0;
    avy[i] = 0;
    avz[i] = 0;
  }

  for(z=0;z<gui->block.z_size;z++)
    for(y=0;y<gui->block.y_size;y++)
      for(x=0;x<gui->block.x_size;x++){

	val = (int)gui->block.data[z*gui->block.x_size*gui->block.y_size + y*gui->block.x_size + x];
	if (val == 0) continue;
	
	for(i=1;i<gui->num_bodies;i++){
	  if (val == gui->bod[i].region_num){
	    avx[i] += x;
	    avy[i] += y;
	    avz[i] += z;
	    num_body_pts[i]++;
	    break;
	  }
	}
      }
  
  /** now average **/
  for(i=1;i<gui->num_bodies;i++){
    /*printf("averaging for body:%s\n",gui->bod[i].name);*/
    if (num_body_pts[i] == 0) continue;

    avx[i] = avx[i] / num_body_pts[i];
    avy[i] = avy[i] / num_body_pts[i];
    avz[i] = avz[i] / num_body_pts[i];


    gui->bod[i].center_uv_x = avx[i];
    gui->bod[i].center_uv_y = avy[i];
    gui->bod[i].center_uv_z = avz[i];
    /*
    convert_data_block_xyz_to_world_xyz(gui,avx[i],avy[i],avz[y], 
					&gui->bod[i].center_mass_x,
					&gui->bod[i].center_mass_y,
					&gui->bod[i].center_mass_z);
    */
    /** now convert to world coordinates **/    
    gui->bod[i].center_mass_x = ((avx[i]) - gui->block.x_size/2) * gui->block.x_resolution * gui->x_size;
    gui->bod[i].center_mass_y = (float)gui->block.z_size/2.0*gui->z_spacing*gui->block.z_resolution - 
      (gui->block.z_size - (avz[i])   )*gui->z_spacing*gui->block.z_resolution 
      + (gui->z_spacing*gui->block.z_resolution)/2.0;	      
    gui->bod[i].center_mass_z = (((gui->block.y_size-1) -  (avy[i])  )-gui->block.y_size/2) * gui->block.y_resolution * gui->y_size;	      
    /*
      printf("center of mass for %s is  %d,%d,%d  ->   %f,%f,%f\n",gui->bod[i].name,avx[i],avz[i],avy[i],
      gui->bod[i].center_mass_x,gui->bod[i].center_mass_y,gui->bod[i].center_mass_z);
    */  
    sprintf(gui->bod[i].center_string,"Center Univel for %10s : x: %3d,  y: %3d, z: %3d",
	    gui->bod[i].name,
	    gui->bod[i].center_uv_x, 
	    gui->bod[i].center_uv_y, 
	    gui->bod[i].center_uv_z);  
  }

  RemoveMessage( gui, "." );
  
  DEBUG_TRACE_OUT printf("Done with find_center_mass_for_bodies_in_data_block\n");
}

