#include "sera3d.h"

#define RGBA_TEXTURE 1

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Build_3d_texture_for_slice()
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: Through the use of GL_EXT_texture3D,
%%%           a 3-d texture map is built based on the raw
%%%           slice information.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_texture_planes(main_gui_t *gui)
{
  gui->splane[0] = 1.0/256.0;  gui->splane[1] = 0.0;         gui->splane[2] = 0.0;        gui->splane[3] = 0.0;
  gui->rplane[0] = 0.0      ;  gui->rplane[1] = 1.0/256.0;   gui->rplane[2] = 0.0;        gui->rplane[3] = 0.0;
  gui->tplane[0] = 0.0;        gui->tplane[1] = 0.0;         gui->tplane[2] = 1.0/256.0;  gui->tplane[3] = 0.0;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Build_3d_texture_from_qsh()
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: Through the use of GL_EXT_texture3D,
%%%           a 3-d texture map is built based on the raw
%%%           slice information.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_3d_texture_from_qsh(main_gui_t *gui,qsh_info_t *qsh_info)
{
    int x_dim,y_dim,z_dim;
    unsigned char *vp;
    char temp_string[256];
    int volume_size;
    static int first_time = 1;

    int s, r, c;        /* Indcies for slice, row, and column    */
    unsigned char val;  /* value at qsh_info->images[index]      */
    int sliceOffset;
    int rowOffset;

    DEBUG_TRACE_IN printf("Entered build_3d_texture_from_qsh\n");
  
    sprintf(temp_string,"Building the 3D texture map");
    PostMessage(gui,temp_string);
    wait_on_xserver(gui);
  
    gui->texture_z_size = get_factor_of_2_greater_than_value(qsh_info->size_of_dimension[0]);
    DEBUG_DATA printf("got the texture_z_size it is : %d\n",gui->texture_z_size);

    /* Allocate memory for the texture array */
    /* WARNING: Might cause some problems using fixed TEX_SIZE if images are not 256 x 256 (MBR 04.16.2000) */
    if (RGBA_TEXTURE)
        gui->texture = (unsigned char  *) MT_malloc((4*gui->texture_z_size*TEX_SIZE*TEX_SIZE));  
    else
        gui->texture = (unsigned char  *) MT_malloc((2*gui->texture_z_size*TEX_SIZE*TEX_SIZE));  

    /*
     * Start vp at the beginning of the gui->texture array.
     * This will iterate through the array, filling in values
     * that get computed in the following tripley nested for loops.
     */
    vp = gui->texture;

    /* Get sizes in the x, y, and z directions */
    z_dim = qsh_info->size_of_dimension[0];
    x_dim = qsh_info->size_of_dimension[1];
    y_dim = qsh_info->size_of_dimension[2];
    
    /** for now save the images in the texture_volume array, so if we
        need to rebuild the texture map, we have the images**/
    volume_size = z_dim * y_dim * x_dim;

    if (!first_time)
        MT_free( (void *) gui->texture_volume);

    first_time = 0;

    /* Backup the images in texture_volume */
    gui->texture_volume = (unsigned char *) MT_malloc(volume_size);
    memcpy(gui->texture_volume,qsh_info->images, volume_size);

    /*num_bpp = qsh_info->bytes_per_pixel;*/

    /*
     * Iterate through the gui->texture array and fill in values based
     * on the qsh_info->images array. Here we are assuming that the
     * qsh images are taken in an Axial or Transverse orientation,
     * and that the nose of the patient is towards the top of the images.
     * In the 3-D model, we want the nose of the patient facing out of the
     * screen. Therefore, the values in the gui->texture array will look like
     * the qsh_info->images, except that they will be flipped so that the nose
     * of the patient is at the bottom of the image. So, you will note, that
     * values are assigned into the gui->texture array in a top down fashion,
     * but the values are taken from the qsh_images in a bottom up fashion.
     * This accomplishes the "flip" that we need for things to look correct.
     *
     * WARNING:  As of this date (04.16.2000) the value of TEX_SIZE is defined
     *           to be 256. This is to say that we are assuming the size of the
     *           images are also 256 x 256. This is a bad assumption, and I (MBR)
     *           would change it, but I am afraid that that would wreak some major
     *           havoc with the rest of the code since it appears the 256 x 256
     *           dimensions are assumed in a lot of places. It seems to me that
     *           we should be looping through y_dim and x_dim here if we wanted
     *           to be totally correct. That would mean that we would also have
     *           to allocate our memory different above.
     *
     */
    for( s = 0; s < z_dim; s++ )    /* for each slice */
    {
        /* Calculate the slice offset for this slice into the qsh_info->images array */
        sliceOffset = s * TEX_SIZE * TEX_SIZE;

        /*
         * Loop through the rows of each slice, but start at the bottom and work
         * up, this will cause the image to be flipped which is the effect we want
         * to achieve, This will cause the anterior of the patient to be facing the
         * user sitting at the screen.
         */
        for( r = TEX_SIZE - 1; r >= 0; r-- ) /* for each row, in reverse */
        {
            /* Calculate the row offset */
            rowOffset = r * TEX_SIZE;

            /*
             * Loop through each column from left to right of the image.
             * (This is actually from the patient's right to left)
             */
            for( c = 0; c < TEX_SIZE; c++ )
            {
                /* Find the value at this position */
                val = qsh_info->images[ sliceOffset + rowOffset + c ];

                /*
                 * If using gamma_correction, index into the gamma_val array with val,
                 * otherwise we'll just use the raw value out of the qsh_indo->images array
                 */
                if( gui->texture_gamma_correction )
                    val = (GLubyte)( 255.0 * gui->gamma_val[ val ] );
                
                *vp = val; vp++;
                *vp = val; vp++;

                if( RGBA_TEXTURE )
                {
                    *vp = val; vp++;
                    *vp = val; vp++;
                }
            } /* end for each column */
        } /* end for each row, in reverse */
    } /* end for each slice */

    /* Free this qsh_info */
    Free_Qsh(qsh_info);

    DEBUG_DATA printf("filling the texture from : %d to %d with black\n",z_dim,gui->texture_z_size);      

    /* Fill remaining space with black */
    val = (GLubyte) 0;
    for ( s = z_dim; s < gui->texture_z_size; s++ )
    {
        for ( r = 0; r < TEX_SIZE; r++ )
        {
            for ( c = 0; c < TEX_SIZE; c++ )
            {
                *vp = val; vp++;
                *vp = val; vp++;

                if (RGBA_TEXTURE)
                {
                    *vp = val; vp++;
                    *vp = val; vp++;
                }
            }
        }
    }
    

    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    if (gui->texture_nearest){
        glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_NEAREST);  
        glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_NEAREST);   
    }else{
        glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_LINEAR);  
        glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_LINEAR);   
    }

    if (gui->texture_clamp){
        glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
        glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
        glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_REPEAT);
    }else{
        glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
        glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
        glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_REPEAT);
    }

    if (RGBA_TEXTURE){
        glTexImage3DEXT(GL_TEXTURE_3D_EXT, 0, GL_RGBA, 
                        TEX_SIZE,TEX_SIZE,gui->texture_z_size,
                        0,GL_RGBA, GL_UNSIGNED_BYTE, gui->texture);
    }else{
        glTexImage3DEXT(GL_TEXTURE_3D_EXT, 0, GL_LUMINANCE_ALPHA, 
                        TEX_SIZE,TEX_SIZE,gui->texture_z_size,
                        0,GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, gui->texture);
    }

    glAlphaFunc(GL_GREATER,1.0/5.0);   
  
  
    glMatrixMode(GL_TEXTURE);
    glLoadIdentity();
    glTranslatef(.5,.5,.5);
    glScalef(1.0/gui->x_size,1.0/gui->y_size,1.0);
    glTranslatef(-.5,-.5,-.5);
    glMatrixMode(GL_MODELVIEW);
   
    DEBUG_DATA printf("Built the 3-Dimensional Texture Map\n");


    MT_free( (void *) gui->texture);
    DEBUG_TRACE_OUT printf("Done with build_3d_texture_from_qsh\n");
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
void rebuild_3d_texture_from_texture_volume(main_gui_t *gui)
{
    int i,j,k,x_dim,y_dim,z_dim,num_bpp;
    int max3dtexdims;
    unsigned char *vp;
    char temp_string[256];
    int volume_size;
    static int first_time = 1;
    int contour_map_index;
    int slice_number_for_grid;
    Contour_grid_t *current_grid;

    int s, r, c;
    int sliceOffset;
    int rowOffset;
    unsigned char val;
  

    DEBUG_TRACE_IN printf("Entered rebuild_3d_texture_from_volume\n");

    DEBUG_DATA printf("Rebuilding texture map\n");
  
    sprintf(temp_string,"Building the 3D texture map");
    PostMessage(gui,temp_string);
    wait_on_xserver(gui);  

    gui->texture_z_size = get_factor_of_2_greater_than_value(gui->num_slices);
    /*printf("got the texture_z_size it is : %d\n",texture_z_size);*/

    gui->texture = (unsigned char  *) MT_malloc((4*gui->texture_z_size*TEX_SIZE*TEX_SIZE));  
    vp=gui->texture;

    z_dim = gui->num_slices;
    x_dim = 256;
    y_dim = 256;

    for( s = 0; s < z_dim; s++ )    /* for each slice */
    {
        /* Calculate the slice offset for this slice into the texture_volume array */
        sliceOffset = s * TEX_SIZE * TEX_SIZE;
          
        /*
         * Loop through the rows of each slice, but start at the bottom and work
         * up, this will cause the image to be flipped which is the effect we want
         * to achieve, This will cause the anterior of the patient to be facing the
         * user sitting at the screen.
         */
        for( r = TEX_SIZE - 1; r >= 0; r-- ) /* for each row, in reverse */
        {
            /* Calculate the row offset */
            rowOffset = r * TEX_SIZE;
              
            /*
             * Loop through each column from left to right of the image.
             * (This is actually from the patient's right to left)
             */
            for( c = 0; c < TEX_SIZE; c++ )
            {
                /* Find the value at this position */
                val = gui->texture_volume[ sliceOffset + rowOffset + c ];
                  
                /*
                 * If using gamma_correction, index into the gamma_val array with val,
                 * otherwise we'll just use the raw value out of the texture_volume array
                 */
                if( gui->texture_gamma_correction )
                    val = (GLubyte)( 255.0 * gui->gamma_val[ val ] );
                  
                *vp = val; vp++;
                *vp = val; vp++;
                *vp = val; vp++;
                *vp = val; vp++;
            } /* end for each column */
        } /* end for each row, in reverse */
    } /* end for each slice */

    
    /******************************************************************/
    /**  Now color over the slices that have contours loaded **/
    /******************************************************************/
    if (gui->colorwash_texture_with_dose){
        current_grid = gui->Grids.next;
        for(slice_number_for_grid=0;slice_number_for_grid<z_dim;slice_number_for_grid++){
            if(current_grid == NULL){
                printf("Error, grid exceeded in texture.c\n");
                break;
            }
            vp = &gui->texture[slice_number_for_grid*TEX_SIZE*TEX_SIZE*4]; 
            for (j=0;j<TEX_SIZE;j++){
                for (k=0;k<TEX_SIZE;k++){
                    contour_map_index = get_mapped_value_from_contour_grid(current_grid,j,k,256,256,gui->current_dose_component);   
                    if (contour_map_index >= MAX_CONTOUR_LEVEL)
                        contour_map_index = MAX_CONTOUR_LEVEL - 1;
                    /*The next line will occasionally cause contour_map_index to be -1
                      which causes an incorrect contour to be displayed */
                    /*contour_map_index --;*/

                    if (gui->Contour_Map[contour_map_index].r  != 0 || 
                        gui->Contour_Map[contour_map_index].g  != 0 || 
                        gui->Contour_Map[contour_map_index].b  != 0)
                    {
                        *vp++ = (gui->Contour_Map[contour_map_index].r + *vp)/2;
                        *vp++ = (gui->Contour_Map[contour_map_index].g + *vp)/2;
                        *vp++ = (gui->Contour_Map[contour_map_index].b + *vp)/2;
                    } else  {
                        vp++;
                        vp++;
                        vp++;
                    }

                    /* Increment past the alpha value */
                    vp++;

                }
            }
            current_grid = current_grid->next;
        }
/*      current_grid = gui->Grids.next; */
/*      printf("Adding the colorwashing\n");*/ 
    
/*      while(current_grid != NULL){ */
/*        slice_number_for_grid = convert_z_to_slice(gui, convert_slice_z_to_world_y(gui,current_grid->z_value) ); */
/*        printf("the slice number to insert it into is : %d\n",slice_number_for_grid);   */ 
      
/*        vp = &gui->texture[slice_number_for_grid*TEX_SIZE*TEX_SIZE*4]; */
      
/*        printf("entering the loops\n");*/ 
/*        printf("mapping into contour grid of %d x %d\n",current_grid->ncols,current_grid->nrows);*/ 
/*        for (j=0;j<TEX_SIZE;j++){ */
/*  	for (k=0;k<TEX_SIZE;k++){ */
/*  	  contour_map_index = get_mapped_value_from_contour_grid(current_grid,j,k,256,256,gui->current_dose_component); */
	  
/*  	  contour_map_index --; */
/*  	  if (contour_map_index >= MAX_CONTOUR_LEVEL) contour_map_index = MAX_CONTOUR_LEVEL; */

/*  	  if (gui->Contour_Map[contour_map_index].r == 0) { */
/*  	    if (gui->texture_gamma_correction) */
/*  	      *vp++=(GLubyte)(255.0*gui->gamma_val[ (int)gui->texture_volume[slice_number_for_grid * TEX_SIZE*TEX_SIZE + (TEX_SIZE -j) * TEX_SIZE + k]]);  */
/*  	    else *vp++=(GLubyte)((int)gui->texture_volume[slice_number_for_grid * TEX_SIZE*TEX_SIZE + (TEX_SIZE-j) * TEX_SIZE + k]); */
/*  	  }else */
/*  	    *vp++ = gui->Contour_Map[contour_map_index].r; */

/*  	  if (gui->Contour_Map[contour_map_index].g  == 0)  */
/*  	    if (gui->texture_gamma_correction) */
/*  	      *vp++=(GLubyte)(255.0*gui->gamma_val[ (int)gui->texture_volume[slice_number_for_grid*TEX_SIZE*TEX_SIZE + (TEX_SIZE -j) * TEX_SIZE + k]]);  */
/*  	    else*vp++=(GLubyte)((int)gui->texture_volume[slice_number_for_grid * TEX_SIZE*TEX_SIZE + (TEX_SIZE-j) * TEX_SIZE + k]); */
/*  	  else */
/*  	    *vp++ = gui->Contour_Map[contour_map_index].g; */

/*  	  if (gui->Contour_Map[contour_map_index].b == 0)  */
/*  	    if (gui->texture_gamma_correction) */
/*  	      *vp++=(GLubyte)(255.0*gui->gamma_val[ (int)gui->texture_volume[slice_number_for_grid*TEX_SIZE*TEX_SIZE + (TEX_SIZE -j) * TEX_SIZE + k]]);  */
/*  	    else*vp++=(GLubyte)((int)gui->texture_volume[slice_number_for_grid * TEX_SIZE*TEX_SIZE + (TEX_SIZE-j) * TEX_SIZE + k]); */
/*  	  else */
/*  	    *vp++ = gui->Contour_Map[contour_map_index].b; */

/*  	  if (contour_Map[contour_map].a == 0)*/  
/*  	    if (gui->texture_gamma_correction) */
/*  	      *vp++=(GLubyte)(255.0*gui->gamma_val[ (int)gui->texture_volume[slice_number_for_grid*TEX_SIZE*TEX_SIZE + (TEX_SIZE -j) * TEX_SIZE + k]]);  */
/*  	    else*vp++=(GLubyte)((int)gui->texture_volume[slice_number_for_grid * TEX_SIZE*TEX_SIZE + (TEX_SIZE-j) * TEX_SIZE + k]); */
/*  	    else */
/*  	     *vp++ = Contour_Map[contour_map_index].a;*/ 


/*  	   */
/*  	   *vp++ = Contour_Map[contour_map_index].r; */
/*  	   *vp++ = Contour_Map[contour_map_index].g; */
/*  	   *vp++ = Contour_Map[contour_map_index].b; */
/*  	   *vp++ *//*= Contour_Map[contour_map_index].a;*/ 
/*  	} */
/*        } */
/*        printf("Added the colorwashing\n");*/
/*        current_grid = current_grid->next; */
/*      } */
    }
    

    /* fill remaining space with black */
    val = (GLubyte) 0;
    vp = &gui->texture[z_dim*TEX_SIZE*TEX_SIZE*4];
    
    for ( s = z_dim; s < gui->texture_z_size; s++ )
    {
        for ( r = 0; r < TEX_SIZE; r++ )
        {
            for ( c = 0; c < TEX_SIZE; c++ )
            {
                *vp = val; vp++;
                *vp = val; vp++;
                *vp = val; vp++;
                *vp = val; vp++;
            }
        }
    }
    


    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    if (gui->texture_nearest){
        glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_NEAREST);  
        glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_NEAREST);   
    }else{
        glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_LINEAR);  
        glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_LINEAR);   
    }

    if (gui->texture_clamp){
        glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
        glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
        glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_REPEAT);
    }else{
        glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
        glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
        glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_REPEAT);
    }

    glTexImage3DEXT(GL_TEXTURE_3D_EXT, 0, GL_RGBA, 
                    TEX_SIZE,TEX_SIZE,gui->texture_z_size,
                    0,GL_RGBA, GL_UNSIGNED_BYTE, gui->texture);


    glAlphaFunc(GL_GREATER,1.0/5.0);   
  
  
    glMatrixMode(GL_TEXTURE);
    glLoadIdentity();
    glTranslatef(.5,.5,.5);
    glScalef(1.0/gui->x_size,1.0/gui->y_size,1.0);
    glTranslatef(-.5,-.5,-.5);
    glMatrixMode(GL_MODELVIEW);
   
    DEBUG_DATA printf("Built the 3-Dimensional Texture Map\n");

  
    MT_free( (void *) gui->texture);

    DEBUG_TRACE_OUT printf("Done with rebuild_3d_texture_from_texture_volume\n");
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
void build_3d_uv_texture_from_texture_volume(main_gui_t *gui)
{
  int i,j,k,x_dim,y_dim,z_dim,num_bpp;
  int max3dtexdims;
  unsigned char *vp;
  char temp_string[256];
  int volume_size;
  static int first_time = 1;
  int contour_map_index;
  int slice_number_for_grid;
  Contour_grid_t *current_grid;
  char temp_color[256][4];
  int index;

  DEBUG_TRACE_IN printf("Entered build_3d_texture_from_texture_volume\n");

  DEBUG_DATA printf("Rebuilding texture map\n");
  
  for (i=0;i<256;i++){
    temp_color[i][0] = rand()%255;
    temp_color[i][1] = rand()%255;
    temp_color[i][2] = rand()%255;
    temp_color[i][3] = (char)255;
  }
  temp_color[0][3] = 0;
  temp_color[1][3] = 0;
  /*temp_color[3][3] = 255;*/
  
printf("done with getting the random colors\n");

  gui->texture_z_size = get_factor_of_2_greater_than_value(gui->num_slices);
printf("got the texture_z_size it is : %d\n",gui->texture_z_size);

  gui->texture = (unsigned char  *) MT_malloc(( 4 * gui->texture_z_size*TEX_SIZE*TEX_SIZE));  
  vp=gui->texture;

  z_dim = gui->num_slices;
  x_dim = 256;
  y_dim = 256;
  
  for (i=0;i<z_dim;i++)
    for (j=1;j<=TEX_SIZE;j++)
      for (k=0;k<TEX_SIZE;k++){
	index = (int)gui->texture_volume[i*TEX_SIZE*TEX_SIZE + (TEX_SIZE-j) * TEX_SIZE + k];

	*vp++=(GLubyte)  temp_color[index][0];
	*vp++=(GLubyte)  temp_color[index][1];
	*vp++=(GLubyte)  temp_color[index][2];
	*vp++=(GLubyte)  index;
	/*printf("filled cell #%d\n",k*i*j);*/
      }
printf("filled in the colors\n");

  vp = &gui->texture[z_dim*TEX_SIZE*TEX_SIZE*4];
  for (i=z_dim;i<gui->texture_z_size;i++)
    for (j=0;j<TEX_SIZE;j++)
      for (k=0;k<TEX_SIZE;k++){
	*vp++=(GLubyte)0;
	*vp++=(GLubyte)0;
	*vp++=(GLubyte)0;
	*vp++=(GLubyte)0;
      }
printf("done filling in the black padding\n");


  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  if (gui->texture_nearest){
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_NEAREST);  
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_NEAREST);   
  }else{
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_LINEAR);  
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_LINEAR);   
  }

  if (gui->texture_clamp){
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_REPEAT);
  }else{
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_REPEAT);
  }

  glTexImage3DEXT(GL_TEXTURE_3D_EXT, 0, GL_RGBA, 
		  TEX_SIZE,TEX_SIZE,gui->texture_z_size,
		  0,GL_RGBA, GL_UNSIGNED_BYTE, gui->texture);

  glAlphaFunc(GL_GREATER,1.0/5.0);   
  
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glTranslatef(.5,.5,.5);
  glScalef(1.0/gui->x_size,1.0/gui->y_size,1.0);
  glTranslatef(-.5,-.5,-.5);
  glMatrixMode(GL_MODELVIEW);
   
  DEBUG_DATA printf("Built the 3-Dimensional Texture Map\n");


  MT_free( (void *) gui->texture);

  DEBUG_TRACE_OUT printf("Done with build_3d_texture_from_texture_volume\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Build_3d_texture_for_slice()
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: Through the use of GL_EXT_texture3D,
%%%           a 3-d texture map is built based on the raw
%%%           slice information.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void build_3d_texture_for_slice()
{
  int i,j,k;
  int max3dtexdims;
  unsigned char *vp;

  texture = (unsigned char  *) MT_malloc((4*64*TEX_SIZE*TEX_SIZE));  
  vp=texture;

  for (i=0;i<num_slices;i++){
    for (j=0;j<TEX_SIZE;j++){
      for (k=0;k<TEX_SIZE;k++){
	*vp++=(GLubyte)(255.0*gamma_val[(int)volume[i][j][k]]);
	*vp++=(GLubyte)(255.0*gamma_val[(int)volume[i][j][k]]);

	if (RGBA_TEXTURE){
	  *vp++=(GLubyte)(255.0*gamma_val[(int)volume[i][j][k]]);
	  *vp++=(GLubyte)(255.0*gamma_val[(int)volume[i][j][k]]);
	}

      }
    }
  }


  for (i=num_slices;i<64;i++){
    for (j=0;j<TEX_SIZE;j++){
      for (k=0;k<TEX_SIZE;k++){
	*vp++=(GLubyte)0;
	*vp++=(GLubyte)0;

	if (RGBA_TEXTURE){
	  *vp++=(GLubyte)0;
	  *vp++=(GLubyte)0;
	}

      }
    }
  }

  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  if (texture_nearest){
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_NEAREST);  
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_NEAREST);   
  }else{
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_LINEAR);  
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_LINEAR);   
  }

  if (texture_clamp){
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_REPEAT);
  }else{
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_REPEAT);
  }

  if (RGBA_TEXTURE){
    glTexImage3DEXT(GL_TEXTURE_3D_EXT, 0, GL_RGBA, 
		    TEX_SIZE,TEX_SIZE,64,
		    0,GL_RGBA, GL_UNSIGNED_BYTE, texture);
  }else{
    glTexImage3DEXT(GL_TEXTURE_3D_EXT, 0, GL_LUMINANCE_ALPHA, 
		    TEX_SIZE,TEX_SIZE,64,
		    0,GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, texture);
  }

  glAlphaFunc(GL_GREATER,1.0/5.0);   
  
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glTranslatef(.5,.5,.5);
  glScalef(1.0/x_size,1.0/y_size,1.0);
  glTranslatef(-.5,-.5,-.5);
  
  glMatrixMode(GL_MODELVIEW);
  
  printf("Built the 3-Dimensional Texture Map\n");
  MT_free( (void *) texture);
}
*/




/*
void insert_contour_grid_into_3d_texture_and_rebuild(Contour_grid_t *grid)
{
  int i,j,k,x_dim,y_dim,z_dim;
  int max3dtexdims;
  unsigned char *vp;
  int volume_size;
  static int first_time = 1;
  int contour_map_index;
  int slice_number_for_grid;
  Contour_grid_t *current_grid;
  

  DEBUG_TRACE_IN printf("Entered insert_contour_grid_into_3d_texture_and_rebuild\n");

  printf("Rebuilding texture map\n");
  
  texture_z_size = get_factor_of_2_greater_than_value(num_slices);

  if (!(texture = (unsigned char  *) MT_malloc((4*texture_z_size*TEX_SIZE*TEX_SIZE)))){
    printf("Sorry not enough room to make the texture map\n"); exit(0);
  }

  vp=texture;

 
  z_dim = num_slices;
  x_dim = 256;
  y_dim = 256;

  
  for (i=0;i<z_dim;i++)
    for (j=0;j<TEX_SIZE;j++)
      for (k=0;k<TEX_SIZE;k++){

	if (texture_gamma_correction){
	  *vp++=(GLubyte)(255.0*gamma_val[ (int)volume[i*TEX_SIZE*TEX_SIZE + 
						      (TEX_SIZE-j) * TEX_SIZE + k]]);
	  *vp++=(GLubyte)(255.0*gamma_val[ (int)volume[i*TEX_SIZE*TEX_SIZE + 
						      (TEX_SIZE -j) * TEX_SIZE + k]]);
	  *vp++=(GLubyte)(255.0*gamma_val[ (int)volume[i*TEX_SIZE*TEX_SIZE + 
						      (TEX_SIZE-j) * TEX_SIZE + k]]);
	  *vp++=(GLubyte)(255.0*gamma_val[ (int)volume[i*TEX_SIZE*TEX_SIZE + 
						      (TEX_SIZE -j) * TEX_SIZE + k]]);
	}else{
	  *vp++=(GLubyte)((int)volume[i*TEX_SIZE*TEX_SIZE + 
				     (TEX_SIZE-j) * TEX_SIZE + k]);
	  *vp++=(GLubyte)((int)volume[i*TEX_SIZE*TEX_SIZE + 
				     (TEX_SIZE -j) * TEX_SIZE + k]);
	  *vp++=(GLubyte)((int)volume[i*TEX_SIZE*TEX_SIZE + 
				     (TEX_SIZE-j) * TEX_SIZE + k]);
	  *vp++=(GLubyte)((int)volume[i*TEX_SIZE*TEX_SIZE + 
				     (TEX_SIZE -j) * TEX_SIZE + k]);	
	}      
      }

  current_grid = Grids.next;
  printf("Adding the colorwashing\n");

  while(current_grid != NULL){
    slice_number_for_grid = convert_z_to_slice( convert_slice_z_to_world_y(current_grid->z_value) );
    printf("the slice number to insert it into is : %d\n",slice_number_for_grid);    
    
    vp = &texture[slice_number_for_grid*TEX_SIZE*TEX_SIZE*4];
    
    printf("entering the loops\n");
    for (j=0;j<TEX_SIZE;j++){
      for (k=0;k<TEX_SIZE;k++){
	contour_map_index = get_mapped_value_from_contour_grid(current_grid,j,k,256,256,current_dose_component);
	
	contour_map_index --;
	if (contour_map_index >= 100) contour_map_index = 99;
	*vp++ = Contour_Map[contour_map_index].r;
	*vp++ ;
	*vp++ = Contour_Map[contour_map_index].b;
	*vp++ ;
      }
    }
    printf("Added the colorwashing\n");
    current_grid = current_grid->next;
  }

  printf("filling the rest of the texture map\n");
  vp = &texture[z_dim*TEX_SIZE*TEX_SIZE*4];
  for (i=z_dim;i<texture_z_size;i++)
    for (j=0;j<TEX_SIZE;j++)
      for (k=0;k<TEX_SIZE;k++){
	*vp++=(GLubyte)0;
	*vp++=(GLubyte)0;

	if (RGBA_TEXTURE){
	  *vp++=(GLubyte)0;
	  *vp++=(GLubyte)0;
	}
      }

  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  if (texture_nearest){
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_NEAREST);  
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_NEAREST);   
  }else{
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_LINEAR);  
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_LINEAR);   
  }

  if (texture_clamp){
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_REPEAT);
  }else{
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_REPEAT);
  }

  glTexImage3DEXT(GL_TEXTURE_3D_EXT, 0, GL_RGBA, 
		  TEX_SIZE,TEX_SIZE,texture_z_size,
		  0,GL_RGBA, GL_UNSIGNED_BYTE, texture);


  glAlphaFunc(GL_GREATER,1.0/5.0);   
  
  
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glTranslatef(.5,.5,.5);
  glScalef(1.0/x_size,1.0/y_size,1.0);
  glTranslatef(-.5,-.5,-.5);
  glMatrixMode(GL_MODELVIEW);
   
  printf("Built the 3-Dimensional Texture Map\n");


  MT_free( (void *) texture);

  DEBUG_TRACE_OUT printf("Done with insert_contour_grid_into_3d_texture_and_rebuild\n");  
}
*/


void build_cw_texture_map(main_gui_t *gui)
{
  DEBUG_TRACE_IN printf("Entered build_cw_texture_map\n");

  printf("Building the Colorwashed Texture\n");

  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  if (gui->texture_nearest){
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_NEAREST);  
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_NEAREST);   
  }else{
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_LINEAR);  
    glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_LINEAR);   
  }

  if (gui->texture_clamp){
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_REPEAT);
  }else{
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_REPEAT);
  }

  glTexImage3DEXT(GL_TEXTURE_3D_EXT, 0, GL_RGBA, 
		  TEX_SIZE,TEX_SIZE,gui->texture_z_size,
		  0,GL_RGBA, GL_UNSIGNED_BYTE, gui->colorwash_texture_data);

  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glTranslatef(.5,.5,.5);
  glScalef(1.0/gui->x_size,1.0/gui->y_size,1.0);
  glTranslatef(-.5,-.5,-.5);
  glMatrixMode(GL_MODELVIEW);

  printf("build the colorwashed 3D texture map\n");
  MT_free( (void *) gui->colorwash_texture_data);

  DEBUG_TRACE_OUT printf("Done with build_cw_texture_map\n");
}
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : draw_3d_textured_plane
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: the position of the slice
%%%
%%%  Purpose: draws a "slice" or plane through the 3d texture map
%%%           in the direction of the current slice
%%%           (axial, cornonal, or sagittal)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw_3d_textured_plane(main_gui_t *gui,float slice_val)
{
  int was_on = 0,blend_was_on = 0;
  float x_dist,y_dist;
  int c0=0,c1=0,c2=0,c3=0,c4=0,c5=0;
  float tx,ty,tz;

  DEBUG_TRACE_IN printf("Entered draw_3d_textured_plane\n");

  if (gui->alpha_culling_on) glEnable(GL_ALPHA_TEST); 
  
  glEnable(GL_TEXTURE_3D_EXT);  
  
  if (glIsEnabled(GL_LIGHTING)){
    was_on = 1;
    glDisable(GL_LIGHTING);
  }
  if (glIsEnabled(GL_BLEND)){
    blend_was_on = 1;
    glDisable(GL_BLEND);
  }

  glColor3f(1,1,1);

  x_dist = 128.0*gui->x_size;
  y_dist = 128.0*gui->y_size;

  if (gui->slice_dir == 1){

    if (glIsEnabled(GL_CLIP_PLANE0)) {glDisable(GL_CLIP_PLANE0); c0 = 1;}
    if (glIsEnabled(GL_CLIP_PLANE1)) {glDisable(GL_CLIP_PLANE1); c1 = 1; }

    glBegin(GL_QUADS);
     convert_world_coord_to_texture_coord(gui,-x_dist,slice_val,-y_dist,&tx,&ty,&tz);
     glTexCoord3f(tx,ty,tz);glVertex3f(-x_dist,slice_val,-y_dist);

     convert_world_coord_to_texture_coord(gui,-x_dist,slice_val, y_dist,&tx,&ty,&tz);   
     glTexCoord3f(tx,ty,tz);glVertex3f(-x_dist,slice_val,y_dist);

     convert_world_coord_to_texture_coord(gui,x_dist,slice_val,y_dist,&tx,&ty,&tz);
     glTexCoord3f(tx,ty,tz);glVertex3f(x_dist,slice_val,y_dist);

     convert_world_coord_to_texture_coord(gui,x_dist,slice_val,-y_dist, &tx,&ty,&tz);
     glTexCoord3f(tx,ty,tz);glVertex3f(x_dist,slice_val,-y_dist);
    glEnd();

   } else if (gui->slice_dir == 2){
     if (glIsEnabled(GL_CLIP_PLANE2)){glDisable(GL_CLIP_PLANE2); c2 = 1;}
     if (glIsEnabled(GL_CLIP_PLANE3)){glDisable(GL_CLIP_PLANE3); c3 = 1;}

     glBegin(GL_QUADS);
      convert_world_coord_to_texture_coord(gui,-x_dist,-y_dist,slice_val,&tx,&ty,&tz);
      glTexCoord3f(tx,ty,tz);glVertex3f(-x_dist,-y_dist,slice_val);
      
      convert_world_coord_to_texture_coord(gui,-x_dist,y_dist,slice_val,&tx,&ty,&tz);   
      glTexCoord3f(tx,ty,tz);glVertex3f(-x_dist,y_dist,slice_val);
      
      convert_world_coord_to_texture_coord(gui,x_dist,y_dist,slice_val,&tx,&ty,&tz);
      glTexCoord3f(tx,ty,tz);glVertex3f(x_dist,y_dist,slice_val);
      
      convert_world_coord_to_texture_coord(gui,x_dist,-y_dist,slice_val,&tx,&ty,&tz);
      glTexCoord3f(tx,ty,tz);glVertex3f(x_dist,-y_dist,slice_val);
     glEnd();

   }else{
     if (glIsEnabled(GL_CLIP_PLANE4)){glDisable(GL_CLIP_PLANE4); c4 = 1;}
     if (glIsEnabled(GL_CLIP_PLANE5)){glDisable(GL_CLIP_PLANE5); c5 = 1;}

    glBegin(GL_QUADS);
     convert_world_coord_to_texture_coord(gui,slice_val,-x_dist,-y_dist,&tx,&ty,&tz);
     glTexCoord3f(tx,ty,tz);glVertex3f(slice_val,-x_dist,-y_dist);

     convert_world_coord_to_texture_coord(gui,slice_val,-x_dist, y_dist,&tx,&ty,&tz);   
     glTexCoord3f(tx,ty,tz);glVertex3f(slice_val,-x_dist,y_dist);

     convert_world_coord_to_texture_coord(gui,slice_val,x_dist,y_dist,&tx,&ty,&tz);
     glTexCoord3f(tx,ty,tz);glVertex3f(slice_val,x_dist,y_dist);

     convert_world_coord_to_texture_coord(gui,slice_val,x_dist,-y_dist,&tx,&ty,&tz);
     glTexCoord3f(tx,ty,tz);glVertex3f(slice_val,x_dist,-y_dist);
    glEnd();

   }
  
  if (was_on) glEnable(GL_LIGHTING);
  if (blend_was_on) glEnable(GL_BLEND);

  glDisable(GL_ALPHA_TEST);
  glDisable(GL_TEXTURE_3D_EXT);
   
  if (c0) glEnable(GL_CLIP_PLANE0);
  if (c1) glEnable(GL_CLIP_PLANE1);
  if (c2) glEnable(GL_CLIP_PLANE2);
  if (c3) glEnable(GL_CLIP_PLANE3);
  if (c4) glEnable(GL_CLIP_PLANE4);
  if (c5) glEnable(GL_CLIP_PLANE5);

  DEBUG_TRACE_OUT printf("done with draw_3d_textured_plane\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : volume_render
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: draws 256 (numplanes) slices in the current 
%%%           slice direction, cutting throught the whole volume
%%%           texture, rendering it.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void volume_render(main_gui_t *gui)
{
  int was_on = 0, blend_was_on = 0;
  int slice,i;
  float x_dist,y_dist,pos,tx,ty,tz;
  int numplanes = 256;
  float a,b,c,mag_h, mag_v,x0,y0,z0;
  float h_x,h_y,h_z, v_x, v_y, v_z;
  float val = 128.0;

  DEBUG_TRACE_IN printf("Entered volume_render\n");

  if (gui->alpha_culling_on) glEnable(GL_ALPHA_TEST); 
   glEnable(GL_TEXTURE_3D_EXT);  
   
   if (glIsEnabled(GL_LIGHTING)){
     was_on = 1;
     glDisable(GL_LIGHTING);
   }
   if (glIsEnabled(GL_BLEND)){
     blend_was_on = 1;
     glDisable(GL_BLEND);
   }
   
   x_dist = 128.0 * gui->x_size; 
   y_dist = 128.0 * gui->y_size;
  
   glColor3f(1,1,1);
   
   glPushMatrix();

   glBegin(GL_QUADS);
    
   if (gui->beam_slice){
     a = gui->beam_slope_x; b = gui->beam_slope_y; c = gui->beam_slope_z;
     mag_h = sqrt(c*c+a*a);


     for (i= 0; i< numplanes; i++){

       x0 = gui->beam_slope_x * (numplanes/2-i);
       y0 = gui->beam_slope_y * (numplanes/2-i);
       z0 = gui->beam_slope_z * (numplanes/2-i);

       if (mag_h>0.0) {
	 mag_v = sqrt((a*b)*(a*b)+(a*a+c*c)*(a*a+c*c)+(b*c)*(b*c)); 
	 h_x = -c/mag_h;  h_y = 0;  h_z = a/mag_h;
	 v_x = (a*b)/mag_v;  v_y = -(a*a+c*c)/mag_v;  v_z = (b*c)/mag_v;
       } else { /* looking straight down y -- or thereabouts */
	 mag_h = 1.0;
	 mag_v = 1.0;
	 h_x = 1.0;  h_y = 0.0;  h_z = 0.0;
	 v_x = 0.0;  v_y = 0.0;  v_z = 1.0;
       }

       glNormal3f(0,1,0);
       convert_world_coord_to_texture_coord(gui, x0+h_x*val+v_x*val, y0+h_y*val+v_y*val, z0+h_z*val+v_z*val, &tx, &ty, &tz);				       
       glTexCoord3f(tx,ty,tz);
       glVertex3f(x0+h_x*val+v_x*val, y0+h_y*val+v_y*val, z0+h_z*val+v_z*val);
       
       convert_world_coord_to_texture_coord(gui,x0+h_x*val-v_x*val, y0+h_y*val-v_y*val, z0+h_z*val-v_z*val, &tx, &ty, &tz);				       
       glTexCoord3f(tx,ty,tz);
       glVertex3f(x0+h_x*val-v_x*val, y0+h_y*val-v_y*val, z0+h_z*val-v_z*val);

       convert_world_coord_to_texture_coord(gui, x0-h_x*val-v_x*val, y0-h_y*val-v_y*val, z0-h_z*val-v_z*val, &tx, &ty, &tz);				       
       glTexCoord3f(tx,ty,tz);
       glVertex3f(x0-h_x*val-v_x*val, y0-h_y*val-v_y*val, z0-h_z*val-v_z*val);

       convert_world_coord_to_texture_coord(gui,x0-h_x*val+v_x*val, y0-h_y*val+v_y*val, z0-h_z*val+v_z*val, &tx, &ty, &tz);				       
       glTexCoord3f(tx,ty,tz);
       glVertex3f(x0-h_x*val+v_x*val, y0-h_y*val+v_y*val, z0-h_z*val+v_z*val);
     }
   }else if (gui->slice_dir == 1){
     
     for (i= 0; i< numplanes; i++){
       
       pos = (float)(i) - 128.0;

       /*pos = (float)(i) * .25 - 128.0;*/

       glNormal3f(0,1,0);
       convert_world_coord_to_texture_coord(gui,-x_dist,pos,-y_dist, &tx,&ty,&tz);
       glTexCoord3f(tx,ty,tz);glVertex3f(-x_dist,pos,-y_dist);
       
       convert_world_coord_to_texture_coord(gui,-x_dist,pos, y_dist, &tx,&ty,&tz);   
       glTexCoord3f(tx,ty,tz);glVertex3f(-x_dist,pos,y_dist);
       
       convert_world_coord_to_texture_coord(gui,x_dist,pos,y_dist, &tx,&ty,&tz);
       glTexCoord3f(tx,ty,tz);glVertex3f(x_dist,pos,y_dist);
       
       convert_world_coord_to_texture_coord(gui, x_dist,pos,-y_dist, &tx,&ty,&tz);
       glTexCoord3f(tx,ty,tz);glVertex3f(x_dist,pos,-y_dist);

     }
   }else if (gui->slice_dir == 2){

     for (i= 0; i< numplanes; i++){
       pos = (float)i/(float)numplanes * (256.0 * gui->y_size) + -y_dist;

       glNormal3f(0,1,0);
       convert_world_coord_to_texture_coord(gui,-x_dist,-y_dist,pos, &tx,&ty,&tz);
       glTexCoord3f(tx,ty,tz);glVertex3f(-x_dist,-y_dist,pos);
       
       convert_world_coord_to_texture_coord(gui,-x_dist,y_dist,pos, &tx,&ty,&tz);   
       glTexCoord3f(tx,ty,tz);glVertex3f(-x_dist,y_dist,pos);
       
       convert_world_coord_to_texture_coord(gui,x_dist,y_dist,pos,&tx,&ty,&tz);
       glTexCoord3f(tx,ty,tz);glVertex3f(x_dist,y_dist,pos);
       
       convert_world_coord_to_texture_coord(gui,x_dist,-y_dist,pos, &tx,&ty,&tz);
       glTexCoord3f(tx,ty,tz);glVertex3f(x_dist,-y_dist,pos);
     }
   }else{
     for (i= 0; i< numplanes; i++){
       pos = (float)i/(float)numplanes * (256.0 * gui->x_size) + -x_dist;

       glNormal3f(0,1,0);
       convert_world_coord_to_texture_coord(gui,pos,-x_dist,-y_dist,&tx,&ty,&tz);
       glTexCoord3f(tx,ty,tz);glVertex3f(pos,-x_dist,-y_dist);
       
       convert_world_coord_to_texture_coord(gui,pos,-x_dist, y_dist, &tx,&ty,&tz);   
       glTexCoord3f(tx,ty,tz);glVertex3f(pos,-x_dist,y_dist);
       
       convert_world_coord_to_texture_coord(gui,pos,x_dist,y_dist, &tx,&ty,&tz);
       glTexCoord3f(tx,ty,tz);glVertex3f(pos,x_dist,y_dist);
       
       convert_world_coord_to_texture_coord(gui,pos,x_dist,-y_dist,	&tx,&ty,&tz);
       glTexCoord3f(tx,ty,tz);glVertex3f(pos,x_dist,-y_dist);
     }
   }
  glEnd();

  glPopMatrix();

  if (was_on) glEnable(GL_LIGHTING);
  if (blend_was_on) glEnable(GL_BLEND);
  glDisable(GL_ALPHA_TEST);
  glDisable(GL_TEXTURE_3D_EXT);

  DEBUG_TRACE_OUT printf("Done with volume_render\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Alpha_Culling_toggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the alpha_culling_on flag & redraws
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Alpha_Culling_toggledCB(Widget w, XtPointer clientdata, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;

  DEBUG_TRACE_IN printf("Entered Alpha_Culling_toggledCB\n");

  DisplayBusyCursor(gui->form);
  if (cbs->set) gui->alpha_culling_on = 1;
  else gui->alpha_culling_on = 0;
  
  draw_all(gui);
  RemoveBusyCursor(gui->form);

  DEBUG_TRACE_OUT printf("Done with Alpha_Culling_toggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Tex_type_toggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb) 
%%%
%%%  Purpose: toggles the texture mapping filtering between
%%%           nearest and linear interpolated.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Tex_type_toggledCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  int type;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)calldata;

  DEBUG_TRACE_IN printf("Entered Tex_type_toggledCB\n");



  if (cbs->set){
    if (strcmp(XtName(w),"LINEAR") == 0) type = 0;
    else type = 1;
    
    DisplayBusyCursor(gui->form);
    
    if (type == 1){
      glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_NEAREST);  
      glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      gui->texture_nearest = 1;
    }else{
      glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MAG_FILTER, GL_LINEAR);  
      glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      gui->texture_nearest = 0;
    }

    draw_all(gui);
    RemoveBusyCursor(gui->form);
  }
  
  DEBUG_TRACE_OUT printf("Done with Tex_type_toggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Tex_mod_toggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the texture wrapping between 
%%%           repeat and clamp.  
%%%           NOTE:  when clamping, the R component is not
%%%                  due to unexpecting results (Mesa problem?)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Tex_mod_toggledCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  int mod;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)calldata;
  
  DEBUG_TRACE_IN printf("Entered Tex_mod_toggledCB\n");

  if (cbs->set){

    if (strcmp(XtName(w),"REPEAT") == 0)
      mod = 1;
    else mod = 0;
    
    DisplayBusyCursor(gui->form);
    
    if (mod == 1){
      glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_REPEAT);
      gui->texture_clamp = 0;
    }else{
      glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
      glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
      glTexParameterf(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_CLAMP);
      gui->texture_clamp = 1;
    }
    draw_all(gui);
    RemoveBusyCursor(gui->form);
  }


  DEBUG_TRACE_OUT printf("Done with Tex_mod_toggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Alpha_value_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: changes the Alpha cutoff value
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Alpha_value_ChangedCB(Widget w, XtPointer clientdata, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;
  
  DEBUG_TRACE_IN printf("Entered Alpha_value_ChangedCB\n");

  glAlphaFunc(GL_GREATER,(float)(cbs->value)/256.0);    

  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Alpha_value_ChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Volume_Rendering_ToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: turns on the volume_render_on flag,
%%%           this flag gets turned off as soon as the volume_render
%%%           is done.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Volume_Rendering_ToggledCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  DEBUG_TRACE_IN printf("Entered Volume_Rendering_ToggledCB\n");

  DisplayBusyCursor(gui->form);
  gui->volume_render_on = 1;

  draw_all(gui);
  RemoveBusyCursor(gui->form);

  DEBUG_TRACE_OUT printf("Done with Volume_Rendering_ToggledCB\n");
}
