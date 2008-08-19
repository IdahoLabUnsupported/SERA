#include "sera3d.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : normalize
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: vector
%%%
%%%  Purpose: normalizes the vector
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void normalize(float *a,float *b, float *c)
{
  float mag, x,y,z;
  
  x = *a;  y = *b;  z = *c;
  
  mag = sqrt(x*x+y*y+z*z);

  *a = x/mag;  *b = y/mag;  *c = z/mag;
}

void cross_vectors(float a, float b, float c, float d, float e, float f, float *outa, float *outb, float *outc)
{
  /*
  | i   j   k |
  |           |
  | a   b   c |
  |           |
  | d   e   f |
  */
  
  *outa = b*f - e*c;
  *outb = a*f - d*c;
  *outc = a*e - d*b;
  	   
}

void vector_dot_product(float a, float b, float c, float d, float e, float f, float *out)
{
  *out = a*d + b*e + c*f;
}

void vector_sum(float a, float b, float c, float d, float e, float f, float *outa, float *outb, float *outc)
{
  *outa = a+d;
  *outb = b+e;
  *outc = c+f;
  normalize(outa,outb,outc);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : convert_world_coord_to_texture_coord
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: world coord, pointers to texture coords
%%%
%%%  Purpose: takes the world coordinate and maps it into the 
%%%           3d texture map, returning the texture coord
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void convert_world_coord_to_texture_coord(main_gui_t *gui,float x, float y, float z,
					  float *tx, float *ty, float *tz)
{
  DEBUG_TRACE_IN printf("Entered convert_world_coord_to_texture_coord\n");

  *tx = (x+128.0)/256.0;
  
  /** remember the texture map is on its side, so
      the z and y components have to be switched!**/
  *ty = (z+128.0)/256.0;
  
  /** **/
  *tz = (y + (float)gui->num_slices/2.0 * gui->z_spacing)/256.0 * 
    (float)gui->num_slices/(float)gui->texture_z_size * 
    256.0/((float)gui->num_slices*gui->z_spacing) ;

  DEBUG_TRACE_OUT printf("Done with convert_world_coord_to_texture_coord\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : convert_z_to_slice
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: z value
%%%
%%%  Purpose: takes the slices z_value and returns the integer
%%%           position of the slice (e.g. 3rd slice, 8th slice )
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int convert_z_to_slice(main_gui_t *gui,float z)
{
 int slice;
 float epsilon = 0.1;

 DEBUG_TRACE_IN printf("Entered convert_z_to_slice\n");
 /* 
    printf("converting z of : %.2f\n",z);
    printf("numslices : %d, z_spacing : %.2f\n",num_slices,z_spacing);
 */
 z = (float)gui->num_slices/2.0*gui->z_spacing + z + gui->z_spacing/2.0;
 slice = (int)(z/(float)gui->z_spacing + epsilon);

 /*printf ( "Found slice #%d for %f\n", slice, z );*/

 DEBUG_TRACE_OUT printf("Done with convert_z_to_slice\n");
 return slice;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : convert_slice_position_to_z
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: z value
%%%
%%%  Purpose: takes the slices z_value and returns the integer
%%%           position of the slice (e.g. 3rd slice, 8th slice )
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
float convert_slice_position_to_world_y(main_gui_t *gui,int pos)
{
 float temp;

 DEBUG_TRACE_IN printf("Entered convert_slice_position_to_world_y\n");

 temp = (float)gui->num_slices/2.0*gui->z_spacing - (gui->num_slices-pos)*gui->z_spacing /** added 10-13-98 ---> **/ + gui->z_spacing/2.0;

 DEBUG_TRACE_OUT printf("Done with convert_slice_position_to_world_y\n");
 return temp;
}




/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : convert_world_y_to_slice_z
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: world y value
%%%
%%%  Purpose: converts the world y value to the corresponding
%%%           slice z value
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
float convert_world_y_to_slice_z(main_gui_t *gui,float wy)
{
 DEBUG_TRACE_IN printf("Entered convert_world_y_to_slice_z\n");
 DEBUG_TRACE_OUT printf("Done with convert_world_y_to_slice_z\n");
  return ((wy - (-((float)gui->num_slices)/2.0*gui->z_spacing)) + gui->min_z);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : convert_slice_z_to_world_y
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: the slice z_value
%%%
%%%  Purpose: returns the y value in world space
%%%           that the slice's z_value corresponds to.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
float convert_slice_z_to_world_y(main_gui_t *gui,float sz)
{
 DEBUG_TRACE_IN printf("Entered convert_slice_z_to_world_y\n");
 DEBUG_TRACE_OUT printf("Done with convert_slice_z_to_world_y\n");
  return -(float)gui->num_slices/2.0*gui->z_spacing  +  (sz-gui->min_z);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : convert_world_coord_to_texture_coord
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: world coord, pointers to texture coords
%%%
%%%  Purpose: takes the world coordinate and maps it into the 
%%%           3d texture map, returning the texture coord
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
float convert_normalized_z_to_z(main_gui_t *gui,float fov, float normalized_z)
{
  float factor = 10.0;

  return normalized_z * factor /** fov*/;
}
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : calculate_camera_up
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (x,y,z) of camera position,
%%%              (x,y,z) of where camera is looking
%%%
%%%  Purpose: determine the upmost vector of the camera
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void calculate_camera_up(main_gui_t *gui,float from_a, float from_b, float from_c,
			 float to_a, float to_b, float to_c)
{
  float a,b,c;

 DEBUG_TRACE_IN printf("Entered calculate_camera_up\n");

  a = to_a - from_a; b = to_b - from_b; c = to_c - from_c;

  normalize(&a,&b,&c);
  
  gui->cam.up_y = sqrt(a*a + b*b);
  gui->cam.up_z = sqrt( (1-a*a-b*b)/(1+(a*a)/(b*b)));
  gui->cam.up_x = a*gui->cam.up_z/b;

 DEBUG_TRACE_OUT printf("Done with calculate_camera_up\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : init_gamma_table
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: simply fills in the gamma table
%%%           (used to brighten the slices)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_gamma_table(main_gui_t *gui)
{
  int i;
  float gamma = 2.0;
  double expo;
  
  DEBUG_TRACE_IN printf("Entered init_gamma_table\n");

  expo = (double)1.0/(double)gamma;

  for (i=0;i<256;i++)
      gui->gamma_val[i] = ((float)pow( (double)(i)/255.0,expo)); 

  DEBUG_TRACE_OUT printf("Done with init_gamma_table\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : get_resource_path_name
%%%
%%%  Written by: Mike Frandsen
%%%
%%%  Parameters: filename, basename
%%%
%%%  Purpose: returns the full filename with the SERA_RESOURCES
%%%           environment variable in from
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void get_resource_path_name(char *fname, char *basename) {
  char * ResourcePath;

  ResourcePath = (char *)getenv("SERA_RESOURCES");

  DEBUG_TRACE_IN printf("Entered get_resource_path_name\n");

  if (ResourcePath) {
    /* Use this path for the file */
    strcpy(fname, ResourcePath);
    if (fname[strlen(fname)-1]!='/')
      strcat(fname, "/");
    strcat(fname, basename);
  } else {
    /* Use the current directory */
    strcpy(fname, basename);
  }
  DEBUG_TRACE_OUT printf("Done with get_resource_path_name\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : test_speed
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: tests & reports the polygon speed of the system
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Test_SpeedCB(Widget w, XtPointer clientdata,XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  int i,was_on=0;
  clock_t t1,t2;
  float num_secs,num_tris_per_sec;
  char report1[256];
  int num_tris = 50000;
  int x,y,z;
  
  DEBUG_TRACE_IN printf("Entered Test_SpeedCB\n");

  DisplayBusyCursor(gui->form);

  if (glIsEnabled(GL_LIGHTING)){ was_on=1; glDisable(GL_LIGHTING);}

  srand(time(NULL));

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glPushMatrix(); 

     glColor3f((float)(rand()%100)/100.0,
	       (float)(rand()%100)/100.0,
	       (float)(rand()%100)/100.0);
     t1 = clock();
     glBegin(GL_POINTS);
     for(i=0;i<num_tris;i++){
       x=rand()%200-100, y =rand()%200-100; z=rand()%200-100;
       glVertex3f(x,y,z);
       /*
	 glVertex3f(-20,0,0);
	 */
     }
     glEnd();
     t2 = clock();
     
     num_secs = (float)(t2-t1)/CLOCKS_PER_SEC;
     num_tris_per_sec = (float)num_tris/num_secs;
     sprintf(report1,"Drew %d points in %f seconds, %.3f pts/sec",
	     num_tris,num_secs,num_tris_per_sec);
     display_non_projected_string(gui,report1, -45,-40);  
     
     
     glColor3f((float)(rand()%100)/100.0,
	       (float)(rand()%100)/100.0,
	       (float)(rand()%100)/100.0);
     t1 = clock();

     glBegin(GL_TRIANGLES);
     for(i=0;i<num_tris;i++){
       x=rand()%200-100, y =rand()%200-100; z=rand()%200-100;
       glVertex3f(x,y,z); glVertex3f(x,y+5,z); glVertex3f(x+5,y,z);
     }
     glEnd();
     t2 = clock();
     
     num_secs = (float)(t2-t1)/CLOCKS_PER_SEC;
     num_tris_per_sec = (float)num_tris/num_secs;
     sprintf(report1,"Drew %d triangles in %f seconds, %.3f tri/sec",
	     num_tris,num_secs,num_tris_per_sec);
     display_non_projected_string(gui,report1, -45,-43);  
     

     glEnable(GL_LIGHTING);
     
     /*set_full_material((float)(rand()%100)/100.0,
		       (float)(rand()%100)/100.0,
		       (float)(rand()%100)/100.0,1.0);*/
     glColor4f((float)(rand()%100)/100.0,
	       (float)(rand()%100)/100.0,
	       (float)(rand()%100)/100.0,1.0);

     t1 = clock();
     glBegin(GL_TRIANGLES);
     for(i=0;i<num_tris;i++){
       x=rand()%200-100, y =rand()%200-100; z=rand()%200-100;
       glVertex3f(x,y,z); glVertex3f(x,y+5,z); glVertex3f(x+5,y,z);
     }
     glEnd();
     t2 = clock();
     
     glDisable(GL_LIGHTING);
     
     num_secs = (float)(t2-t1)/CLOCKS_PER_SEC;
     num_tris_per_sec = (float)num_tris/num_secs;
     sprintf(report1,"Drew %d Lit triangles in %f seconds, %.3f tri/sec", num_tris,num_secs,num_tris_per_sec);
     display_non_projected_string(gui,report1, -45,-46);  
     
     glPopMatrix();
     
     if (was_on) glEnable(GL_LIGHTING);
     
     if (gui->doubleBuffer){
       if (!gui->single_buffer_on)
	 glXSwapBuffers(gui->display, XtWindow(gui->glxarea));
     }
     else glFlush();
     
     RemoveBusyCursor(gui->form);

     DEBUG_TRACE_OUT printf("Done with Test_SpeedCB\n");
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
char *GetTextOfLabel(Widget w)
{
  static char *text;
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered GetTextOfLabel\n");

  XtVaGetValues (w,XmNlabelString,&xmstr,NULL);

  if (XmStringGetLtoR (xmstr, XmFONTLIST_DEFAULT_TAG, &text)){
    DEBUG_TRACE_OUT printf("Done with GetTextOfLabel\n");
    return text;
  }
  else
  {
      DEBUG_TRACE_OUT printf("Done with GetTextOfLabel, couldn't get the text!\n");
      return( (char *) NULL );
  }
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
void debug(char *message, int type)
{
  switch(type){
  case PREFERENCES_DEBUG:
        #ifdef PREFERENCE_IO_PRINTS
           printf("%s\n",message);
        #
    break;
  case GUI_DEBUG:
    break;
  case DRAWING_DEBUG:
    break;
  case LOADING_REGION_DEBUG:
    break;
  }
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
int get_factor_of_2_greater_than_value(int value)
{

  if (value <= 32) return 32;
  if (value <= 64) return 64;
  if (value <= 128) return 128;
  if (value <= 256) return 256;
  
  printf("Sorry cannot handle that many slices\n");exit(0);

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
void bubble_sort_int_array(int *array, int num_elements)
{
  int i,j,temp;
  
  for(i=0;i<num_elements;i++)
    for (j=i+1;j<num_elements;j++)
      if (array[i] < array[j]){
	temp = array[i];
	array[i] = array[j];
	array[j] = temp;
      }
}


void calculate_normal(float x1, float y1, float z1,
		      float x2, float y2, float z2,
		      float x3, float y3, float z3,
		      float *normx, float *normy, GLfloat *normz)
{
   GLfloat ux, uy, uz, vx, vy, vz, wx, wy, wz, wnorm;
/*u=p1-p2   v=p3-p2    w=u x v     wnorm=||w||   */
   
   ux = x1-x2;
   uy = y1-y2;           /*p1-p2*/
   uz = z1-z2;
   
   vx = x3-x2;
   vy = y3-y2;          /*p3-p2*/
   vz = z3-z2;
   
   wx = uy*vz-uz*vy;
   wy = uz*vx-ux*vz;     /*u cross v*/
   wz = ux*vy-uy*vx;
   
   wnorm = sqrt((wx*wx) + (wy*wy) + (wz*wz));
  
   *normx = wx/wnorm;
   *normy = wy/wnorm;
   *normz = wz/wnorm;
 }

void changeLabels( main_gui_t * gui, int panel )
{
    DEBUG_TRACE_IN printf("Entering changeLabels\n");
    
    switch( panel )
    {
        case VIEW_PANEL:
        {
            char cam_name[6][16];
            XmString xmstr;
            int i;

            sprintf( cam_name[1], "%c", gui->axisLabels[ROW_AXIS].first );
            sprintf( cam_name[2], "%c", gui->axisLabels[COLUMN_AXIS].first );
            sprintf( cam_name[3], "%c", gui->axisLabels[COLUMN_AXIS].last );
            sprintf( cam_name[4], "%c", gui->axisLabels[ROW_AXIS].last );
            sprintf( cam_name[5], "%c", gui->axisLabels[SLICE_AXIS].last );

            for( i = 1; i < 6; i++ )
            {
                /* Now get the real names */
                switch( cam_name[i][0] )
                {
                    case 'A': strcpy( cam_name[i], "Anterior" ); break;
                    case 'P': strcpy( cam_name[i], "Posterior"); break;
                    case 'R': strcpy( cam_name[i], "Right"    ); break;
                    case 'L': strcpy( cam_name[i], "Left"     ); break;
                    case 'S': strcpy( cam_name[i], "Superior" ); break;
                    case 'I': strcpy( cam_name[i], "Inferior" ); break;
                }

                /* Relabel the button */
                xmstr = XmStringCreateLocalized( cam_name[i] );
                XtVaSetValues( gui->view_panel.camera[i],
                               XmNlabelString, xmstr,
                               NULL );
                XmStringFree( xmstr );
            }
        }
        break;
        
        case SLICE_PANEL:
        {
            char label[16];
            XmString xmstr;

            sprintf( label, "%s Slice", gui->axisLabels[SLICE_AXIS].name );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->slice_panel.ax_s_toggle,
                           XmNlabelString, xmstr,
                           NULL );
            XmStringFree( xmstr );

            sprintf( label, "%s Slice", gui->axisLabels[ROW_AXIS].name );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->slice_panel.cor_s_toggle,
                           XmNlabelString, xmstr,
                           NULL );
            XmStringFree( xmstr );

            sprintf( label, "%s Slice", gui->axisLabels[COLUMN_AXIS].name );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->slice_panel.sag_s_toggle,
                           XmNlabelString, xmstr,
                           NULL );
            XmStringFree( xmstr );
        }
        break;
        
        case SLIDER_PANEL:
        {
            char label[16];
            XmString xmstr;

            sprintf( label, "%s axis", gui->axisLabels[COLUMN_AXIS].name );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->slider_panel.slider_x_label,
                           XmNlabelString, xmstr,
                           NULL );
            XmStringFree( xmstr );
            

            sprintf( label, "%s axis", gui->axisLabels[ROW_AXIS].name );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->slider_panel.slider_y_label,
                           XmNlabelString, xmstr,
                           NULL );
            XmStringFree( xmstr );

            sprintf( label, "%s axis", gui->axisLabels[SLICE_AXIS].name );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->slider_panel.slider_z_label,
                           XmNlabelString, xmstr,
                           NULL );
            XmStringFree( xmstr );
        }
        break;
        
        case BEAM_PANEL:
        {
            char label[32];
            XmString xmstr;

            sprintf( label, "%s Position", gui->axisLabels[COLUMN_AXIS].name );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->ibeam_dialog.x_s,
                           XmNtitleString, xmstr,
                           NULL );
            XmStringFree( xmstr );

            sprintf( label, "%s Position", gui->axisLabels[ROW_AXIS].name );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->ibeam_dialog.y_s,
                           XmNtitleString, xmstr,
                           NULL );
            XmStringFree( xmstr );

            sprintf( label, "%s Position", gui->axisLabels[SLICE_AXIS].name );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->ibeam_dialog.z_s,
                           XmNtitleString, xmstr,
                           NULL );
            XmStringFree( xmstr );

            sprintf( label, "%c-%c",
                     gui->axisLabels[ROW_AXIS].last,
                     gui->axisLabels[ROW_AXIS].first );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->ibeam_dialog.pa_l,
                           XmNlabelString, xmstr,
                           NULL );
            XmStringFree( xmstr );

            sprintf( label, "%c-%c",
                     gui->axisLabels[COLUMN_AXIS].first,
                     gui->axisLabels[COLUMN_AXIS].last );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->ibeam_dialog.rl_l,
                           XmNlabelString, xmstr,
                           NULL );
            XmStringFree( xmstr );

            sprintf( label, "%c-%c  ",
                     gui->axisLabels[SLICE_AXIS].first,
                     gui->axisLabels[SLICE_AXIS].last );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->ibeam_dialog.is_l,
                           XmNlabelString, xmstr,
                           NULL );
            XmStringFree( xmstr );                        
            
        }
        break;

        case CLIPPING_PANEL:
        {
            char label[16];
            XmString xmstr;
            
            sprintf( label, "  %s  ", gui->axisLabels[SLICE_AXIS].name );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->clipping_dialog.ax_push,
                           XmNlabelString, xmstr,
                           NULL );
            XmStringFree( xmstr );

            sprintf( label, "  %s  ", gui->axisLabels[ROW_AXIS].name );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->clipping_dialog.cor_push,
                           XmNlabelString, xmstr,
                           NULL );
            XmStringFree( xmstr );

            sprintf( label, "  %s  ", gui->axisLabels[COLUMN_AXIS].name );
            xmstr = XmStringCreateLocalized( label );
            XtVaSetValues( gui->clipping_dialog.sag_push,
                           XmNlabelString, xmstr,
                           NULL );
            XmStringFree( xmstr );

            gui->reset_clipping = 1;
            Clip_Direction_ChangedCB( gui->clipping_dialog.ax_push, gui, NULL );
            gui->reset_clipping = 0;
        }    
    }
    
    DEBUG_TRACE_OUT printf("Leaving changeLabels\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : CheckExtension
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: extension name & string
%%%
%%%  Purpose: searches for the wanted extension in the extension
%%%           string (provided by gl or mesa)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
GLboolean CheckExtension(char *extName, const char *extString)
{
*/
  /*
  ** Search for extName in the extensions string.  Use of strstr()
  ** is not sufficient because extension names can be prefixes of
  ** other extension names.  Could use strtok() but the constant
  ** string returned by glGetString can be in read-only memory.
  */
/*
  char *p = (char *)extString;
  char *end;
  int extNameLen;
  

  extNameLen = strlen(extName);
  end = p + strlen(p);
  
printf("in the CheckExternsion, looking for : %s,  in : %s\n",
       extName,extString);
  while (p < end) {
    int n = strcspn(p, " ");
    if ((extNameLen == n) && (strncmp(extName, p, n) == 0)) {
      return GL_TRUE;
    }
    p += (n + 1);
  }
printf("done with the while\n");
  return GL_FALSE;
}
*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : check_for_extension
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void check_for_extension()
{
  int  new_ext_supported = GL_FALSE;

  printf("checking to see if extension is supported\n");
  if ( CheckExtension((char *)"GL_EXT_texture3D", (char *)glGetString(GL_EXTENSIONS)) )
    new_ext_supported = (int)GL_TRUE;
  
  if (new_ext_supported) printf("Yes it is supported\n");
  else printf("Sorry, its not supported\n");
}
*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : check_for_glx_extention
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void check_for_glx_extention() 
{
 int  new_ext_supported = GL_FALSE;
  int  major, minor, screen;
  
  if( !glXQueryVersion(dpy, &major, &minor) )
    exit(1);
  screen = DefaultScreen(dpy);
  
#ifdef GLX_VERSION_1_1
  
  printf("GLX_VERSION_1_1 is defined\n");
  if ( minor > 0 || major > 1 )
    if ( CheckExtension("GLX_EXT_texture3D", 
			glXQueryExtensionsString(dpy, screen)) )
      new_ext_supported = GL_TRUE;
#endif
  
#ifdef GLX_EXT_new_extension
  if (new_ext_supported)
    glNewExtensionEXT(...)
#endif
}
*/
