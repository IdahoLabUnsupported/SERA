#include "sera3d.h"
#include "keyval_tools.h"

void fill_default_preferences(main_gui_t *gui);
void clean_name(char *name);
int write_generic_preference_file(main_gui_t *gui);

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
void read_preferences(main_gui_t *gui)
{
    FILE *in;
    char junk[256];
    char buffer[256],*key,*value,*temp,*temp2,*temp3,*comment;
    char *val1,*val2,*val3,*val4,*val5;
    float temp_f;
    char *R_file = "Sera3d/Sera3d.rsc";
    char Resource_file[256];
    int temp_int1,temp_int2,temp_int3,temp_int4,temp_int5;
    int contour_level;
    int r,g,b;
    int status;

    DEBUG_TRACE_IN printf("Entered read_preferences\n");
  
    get_resource_path_name(Resource_file, R_file);
    KV_set_split_characters(":");

    /* Load defaults to begin with */
    fill_default_preferences( gui );
    gui->using_defaults = 1;
    
    /* Try to open up the resource file */
    in = fopen( Resource_file, "r" );
    if( ! in )
    {
        status = write_generic_preference_file( gui );
        if( status )
        {
            printf("Added a generic resource file, using that\n");

            /* Try to open up the new resource file */
            in = fopen( Resource_file, "r" );
            if( ! in )
            {
                printf("wrote the generic resource, but now can't open it, this shouldn't be happening\n");
                return;
            }
        }
        else
        {
            printf("No resource file and couldn't add a generic one, filling in defaults\n");
            return;
        }
    }

    key = NULL;
    value = NULL;
    
    /* We were able to open up the file, lets make sure it contains something usable */
    status = KV_read_next_key_and_value( in, buffer, 100, &key, &value );
    
    if( !status || value == NULL )
    {
        /* No keys and values were found, so lets write a generic preference file */
        fclose( in );

        status = write_generic_preference_file( gui );
        if( status )
        {
            printf( "Original resource file not valid!. Using generic resource file instead.\n" );

            in = fopen( Resource_file, "r" );
            if( ! in )
            {
                printf( "Couldn't even read generic file, so using defaults.\n" );
                return;
            }

            /* Get the first key and value out of the generic resource file */
            KV_read_next_key_and_value( in, buffer, 100, &key, &value );
        }
        else
        {
            printf( "Couldn't create a generic resource file. Must use defaults.\n" );
            return;
        }
    }

    /**** File opened so lets read from the file ****/
    gui->using_defaults = 0;
    gui->num_pref_bods = 1;
  
    DEBUG_IO printf("Opened the preferences, time to read\n");

    /* If here, guaranteed to have a value */
    KV_make_lower_string(value);
  
    while (1){
        DEBUG_IO printf("\nread : %s %s\n",key,value);
    
        if (strcmp(key,"main_window_size") == 0){
            sscanf(value,"%d",&gui->mainwindowsize);
            DEBUG_IO printf("mainwindowsize is : %d\n",gui->mainwindowsize);
      
        }else if (strcmp(key,"multi_window_size") == 0){
            sscanf(value,"%d",&gui->multiwindowsize);
            DEBUG_IO  printf("multiwindowsize is : %d\n",gui->multiwindowsize);
      
        }else if (strcmp(key,"multi-view") == 0){
            if (strstr(value,"off")) gui->multi_view = 0;
            else gui->multi_view = 1;
        }else if (strcmp(key,"background") == 0){
            if (strstr(value,"black")) gui->background = 0.0;
            else if (strstr(value,"25")) gui->background = 0.25;
            else if (strstr(value,"50")) gui->background = 0.5;
            else if (strstr(value,"75")) gui->background = 0.75;
            else gui->background = 1.0;
        }else if (strcmp(key,"axis_type") == 0){
            if      (strstr(value,"full"))  gui->axis_type = 1;  /* FULL */
            else if (strstr(value,"thin"))  gui->axis_type = 2;  /* THIN */
            else if (strstr(value,"thick")) gui->axis_type = 3;  /* THICK */
            else if (strstr(value,"ring"))  gui->axis_type = 4;  /* not being used */  
            else                            gui->axis_type = 0;  /* OFF */
        }else if (strcmp(key,"axis_labelling") == 0){
            if (strstr(value,"off")) gui->axis_labels_on = 0;
            else gui->axis_labels_on = 1;
        }else if (strcmp(key,"mouse_control") == 0){
            /*if (strstr(value,"pull")) gui->mouse_control_method = PICK;*/
            /*else*/ if (strstr(value,"slider")) gui->mouse_control_method = SLIDER;
            else if (strstr(value,"button")) gui->mouse_control_method = MOUSE;
      
        }else if (strstr(key,"gamma")){
            KV_break_into_lower_key_and_value(" ",value,&val1,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val2,&temp2);
            KV_break_into_lower_key_and_value(" ",temp2,&val3,&temp3);
            KV_break_into_lower_key_and_value(" ",temp3,&val4,&val5);
            sscanf(val1,"%d",&temp_int1);
            sscanf(val2,"%d",&temp_int2);
            sscanf(val3,"%d",&temp_int3);
            sscanf(val4,"%d",&temp_int4);
            sscanf(val5,"%d",&temp_int5);
            gui->line_types[GAMMA].color.red = (short)temp_int1;
            gui->line_types[GAMMA].color.green = (short)temp_int2;
            gui->line_types[GAMMA].color.blue = (short)temp_int3;
            gui->line_types[GAMMA].type = temp_int4;
            gui->line_types[GAMMA].boldness = temp_int5;
     
            DEBUG_IO printf("Low Energy Gamma is : %d, %d, %d, %d %d\n", 
                            gui->line_types[GAMMA].color.red,gui->line_types[GAMMA].color.green,
                            gui->line_types[GAMMA].color.blue,gui->line_types[GAMMA].type,
                            gui->line_types[GAMMA].boldness);
            /*
              }else if (strcmp(key,"medium_energy_gamma") == 0){
              KV_break_into_lower_key_and_value(" ",value,&val1,&temp);
              KV_break_into_lower_key_and_value(" ",temp,&val2,&temp2);
              KV_break_into_lower_key_and_value(" ",temp2,&val3,&temp3);
              KV_break_into_lower_key_and_value(" ",temp3,&val4,&val5);
              sscanf(val1,"%d",&temp_int1);
              sscanf(val2,"%d",&temp_int2);
              sscanf(val3,"%d",&temp_int3);
              sscanf(val4,"%d",&temp_int4);
              sscanf(val5,"%d",&temp_int5);
              gui->line_types[1].color.red = (short)temp_int1;
              gui->line_types[1].color.green = (short)temp_int2;
              gui->line_types[1].color.blue = (short)temp_int3;
              gui->line_types[1].type = temp_int4;
              gui->line_types[1].boldness = temp_int5;
      
              DEBUG_IO  printf("Medium Energy Gamma is : %d, %d, %d, %d %d\n", 
              gui->line_types[1].color.red,gui->line_types[1].color.green,
              gui->line_types[1].color.blue,gui->line_types[1].type,
              gui->line_types[1].boldness);
              }else if (strcmp(key,"high_energy_gamma") == 0){
              KV_break_into_lower_key_and_value(" ",value,&val1,&temp);
              KV_break_into_lower_key_and_value(" ",temp,&val2,&temp);
              KV_break_into_lower_key_and_value(" ",temp,&val3,&temp);
              KV_break_into_lower_key_and_value(" ",temp,&val4,&val5);
      
              sscanf(val1,"%d",&temp_int1);
              sscanf(val2,"%d",&temp_int2);
              sscanf(val3,"%d",&temp_int3);
              sscanf(val4,"%d",&temp_int4);
              sscanf(val5,"%d",&temp_int5);
              gui->line_types[2].color.red = (short)temp_int1;
              gui->line_types[2].color.green = (short)temp_int2;
              gui->line_types[2].color.blue = (short)temp_int3;
              gui->line_types[2].type = temp_int4;
              gui->line_types[2].boldness = temp_int5;
      
              DEBUG_IO   printf("High Energy Gamma is : %d, %d, %d, %d %d\n", 
              gui->line_types[2].color.red,gui->line_types[2].color.green,
              gui->line_types[2].color.blue,gui->line_types[2].type,
              gui->line_types[2].boldness);
            */
        }else if (strcmp(key,"low_energy_neutron") == 0){
            KV_break_into_lower_key_and_value(" ",value,&val1,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val2,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val3,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val4,&val5);
     
            sscanf(val1,"%d",&temp_int1);
            sscanf(val2,"%d",&temp_int2);
            sscanf(val3,"%d",&temp_int3);
            sscanf(val4,"%d",&temp_int4);
            sscanf(val5,"%d",&temp_int5);
            gui->line_types[NEUTRON_LOW].color.red = (short)temp_int1;
            gui->line_types[NEUTRON_LOW].color.green = (short)temp_int2;
            gui->line_types[NEUTRON_LOW].color.blue = (short)temp_int3;
            gui->line_types[NEUTRON_LOW].type = temp_int4;
            gui->line_types[NEUTRON_LOW].boldness = temp_int5;
     
            DEBUG_IO  printf("Low Energy Neutron is : %d, %d, %d, %d %d\n", 
                             gui->line_types[NEUTRON_LOW].color.red,gui->line_types[NEUTRON_LOW].color.green,
                             gui->line_types[NEUTRON_LOW].color.blue,gui->line_types[NEUTRON_LOW].type,
                             gui->line_types[NEUTRON_LOW].boldness);
        }else if (strcmp(key,"medium_energy_neutron") == 0){
            KV_break_into_lower_key_and_value(" ",value,&val1,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val2,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val3,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val4,&val5);
     
            sscanf(val1,"%d",&temp_int1);
            sscanf(val2,"%d",&temp_int2);
            sscanf(val3,"%d",&temp_int3);
            sscanf(val4,"%d",&temp_int4);
            sscanf(val5,"%d",&temp_int5);
            gui->line_types[NEUTRON_MED].color.red = (short)temp_int1;
            gui->line_types[NEUTRON_MED].color.green = (short)temp_int2;
            gui->line_types[NEUTRON_MED].color.blue = (short)temp_int3;
            gui->line_types[NEUTRON_MED].type = temp_int4;
            gui->line_types[NEUTRON_MED].boldness = temp_int5;
          
            DEBUG_IO printf("Medium Energy Neutron is : %d, %d, %d, %d %d\n", 
                            gui->line_types[NEUTRON_MED].color.red,gui->line_types[NEUTRON_MED].color.green,
                            gui->line_types[NEUTRON_MED].color.blue,gui->line_types[NEUTRON_MED].type,
                            gui->line_types[NEUTRON_MED].boldness);

        }else if (strcmp(key,"high_energy_neutron") == 0){
            KV_break_into_lower_key_and_value(" ",value,&val1,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val2,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val3,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val4,&val5);
     
            sscanf(val1,"%d",&temp_int1);
            sscanf(val2,"%d",&temp_int2);
            sscanf(val3,"%d",&temp_int3);
            sscanf(val4,"%d",&temp_int4);
            sscanf(val5,"%d",&temp_int5);
            gui->line_types[NEUTRON_HIGH].color.red = (short)temp_int1;
            gui->line_types[NEUTRON_HIGH].color.green = (short)temp_int2;
            gui->line_types[NEUTRON_HIGH].color.blue = (short)temp_int3;
            gui->line_types[NEUTRON_HIGH].type = temp_int4;
            gui->line_types[NEUTRON_HIGH].boldness = temp_int5;

            DEBUG_IO   printf("High Energy Neutron is : %d, %d, %d, %d %d\n", 
                              gui->line_types[NEUTRON_HIGH].color.red,gui->line_types[NEUTRON_HIGH].color.green,
                              gui->line_types[NEUTRON_HIGH].color.blue,gui->line_types[NEUTRON_HIGH].type,
                              gui->line_types[NEUTRON_HIGH].boldness);
        }else if (strcmp(key,"beam") == 0){
            KV_break_into_lower_key_and_value(" ",value,&val1,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val2,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val3,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val4,&val5);
     
            sscanf(val1,"%d",&temp_int1);
            sscanf(val2,"%d",&temp_int2);
            sscanf(val3,"%d",&temp_int3);
            sscanf(val4,"%d",&temp_int4);
            sscanf(val5,"%d",&temp_int5);
            gui->line_types[BEAM].color.red = (short)temp_int1;
            gui->line_types[BEAM].color.green = (short)temp_int2;
            gui->line_types[BEAM].color.blue = (short)temp_int3;
            gui->line_types[BEAM].type = temp_int4;
            gui->line_types[BEAM].boldness = temp_int5;
     
            DEBUG_IO  printf("Beam is : %d, %d, %d, %d %d\n", 
                             gui->line_types[BEAM].color.red,gui->line_types[BEAM].color.green,
                             gui->line_types[BEAM].color.blue,gui->line_types[BEAM].type,
                             gui->line_types[BEAM].boldness);
        }else if (strcmp(key,"lost") == 0){
            KV_break_into_lower_key_and_value(" ",value,&val1,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val2,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val3,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val4,&val5);
     
            sscanf(val1,"%d",&temp_int1);
            sscanf(val2,"%d",&temp_int2);
            sscanf(val3,"%d",&temp_int3);
            sscanf(val4,"%d",&temp_int4);
            sscanf(val5,"%d",&temp_int5);
            gui->line_types[LOST].color.red = (short)temp_int1;
            gui->line_types[LOST].color.green = (short)temp_int2;
            gui->line_types[LOST].color.blue = (short)temp_int3;
            gui->line_types[LOST].type = temp_int4;
            gui->line_types[LOST].boldness = temp_int5;

            DEBUG_IO   printf("Lost is : %d, %d, %d, %d %d\n", 
                              gui->line_types[LOST].color.red,gui->line_types[LOST].color.green,
                              gui->line_types[LOST].color.blue,gui->line_types[LOST].type,
                              gui->line_types[LOST].boldness);

            /*}else if (strcmp(key,"scaling") == 0){
              sscanf(value,"%f",&gui->scaling);
              DEBUG_IO printf("scaling is : %f\n",gui->scaling);*/

        }else if (strcmp(key,"rendering_quality") == 0){
            if (strstr(value,"low")) gui->rendering_quality = 4;
            else if (strstr(value,"medium")) gui->rendering_quality = 3;
            else if (strstr(value,"high")) gui->rendering_quality = 2;
            else if (strstr(value,"full")) gui->rendering_quality = 1;
            DEBUG_IO printf("rendering quality is : %d\n",gui->rendering_quality);

        }else if (strcmp(key,"polygon_quality") == 0){
            if (strstr(value,"low")) gui->polygonal_rendering_quality = 4;
            else if (strstr(value,"medium")) gui->polygonal_rendering_quality = 3;
            else if (strstr(value,"high")) gui->polygonal_rendering_quality = 2;
            else if (strstr(value,"full")) gui->polygonal_rendering_quality = 1;
            DEBUG_IO printf("polygon quality is : %d\n",gui->polygonal_rendering_quality);

            /*
              }else if (strcmp(key,"polygonal_algorithm") == 0){
              if (strstr(value,"surface")){
              gui->polygonal_normal_type = NORMAL_TYPE_SURFACE;
              glShadeModel(GL_FLAT);
              }else if (strstr(value,"vertex")){
              gui->polygonal_normal_type = NORMAL_TYPE_VERTEX;
              glShadeModel(GL_SMOOTH);
              }
            */

        }else if (strcmp(key,"messages") == 0){
            if (strstr(value,"on")) gui->messages_on = 1;
            else gui->messages_on = 0;
        }else if (strcmp(key,"fast_rotation") == 0){
            if (strstr(value,"wireframe")) gui->fast_rotation_type = 1;
            else if (strstr(value,"axis")) gui->fast_rotation_type = 2;      
            else gui->fast_rotation_type = 3;     
        }else if (strcmp(key,"measure_unit") == 0){
            if (strstr(value,"cm")) gui->cm = 1;
            else gui->cm = 0;
        }else if (strcmp(key,"texture_filtering") == 0){
            if (strstr(value,"nearest")) gui->texture_nearest = 1;
            else gui->texture_nearest = 0;
        }else if (strcmp(key,"texture_mode") == 0){
            if (strstr(value,"clamp")) gui->texture_clamp = 1;
            else gui->texture_clamp = 0;       
        }else if (strcmp(key,"alpha_culling") == 0){
            if (strstr(value,"on")) gui->alpha_culling_on = 1;
            else gui->alpha_culling_on = 0;
        }else if (strcmp(key,"use_uvh_colors") == 0){
            if (strstr(value,"rue")) gui->use_uvh_colors = 1;
            else gui->use_uvh_colors = 0;
        }else if (strstr(key,"color")){
            KV_break_into_lower_key_and_value(" ",value,&val1,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val2,&temp2);
            KV_break_into_lower_key_and_value(" ",temp2,&val3,&val4);
            strcpy(gui->pref_bod[gui->num_pref_bods].name,key);
            clean_name(gui->pref_bod[gui->num_pref_bods].name);
            sscanf(val1,"%d",&gui->pref_bod[gui->num_pref_bods].r);	 
            sscanf(val2,"%d",&gui->pref_bod[gui->num_pref_bods].g);	 
            sscanf(val3,"%d",&gui->pref_bod[gui->num_pref_bods].b);
            sscanf(val4,"%d",&gui->pref_bod[gui->num_pref_bods].t);	 	 
	
            DEBUG_IO printf("Filled Body #%d color: %s, %d %d %d %d\n",
                            gui->num_pref_bods,gui->pref_bod[gui->num_pref_bods].name,
                            gui->pref_bod[gui->num_pref_bods].r,gui->pref_bod[gui->num_pref_bods].g,
                            gui->pref_bod[gui->num_pref_bods].b,gui->pref_bod[gui->num_pref_bods].t);
            gui->num_pref_bods++;
        }else if (strstr(key,"contour_level")){
            /*printf("read a contour_level\n");*/
            KV_break_into_lower_key_and_value(" ",value,&val1,&temp);
            KV_break_into_lower_key_and_value(" ",temp,&val2,&temp2);
            KV_break_into_lower_key_and_value(" ",temp2,&val3,&val4);
     
            sscanf(val1,"%d",&contour_level);	 
            sscanf(val2,"%d",&r);	 
            sscanf(val3,"%d",&g);
            sscanf(val4,"%d",&b);
            /*printf("going to fill contour level %d, with r: %d, g: %d, b: %d\n",contour_level,r,g,b);*/

            gui->contour_pref[contour_level].color.red   = r;
            gui->contour_pref[contour_level].color.green = b;
            gui->contour_pref[contour_level].color.blue  = g;
            gui->contour_pref[contour_level].filled = 1;
     
            /*init_color(r,g,b,&contour_pref[contour_level].color);*/
            /*printf("filled contour level %d, with r: %d, g: %d, b: %d\n",contour_level,r,g,b);*/
        }
   
        if (!KV_read_next_key_and_value(in,buffer,100,&key,&value)){
            DEBUG_IO printf("Reached the end, closing the file\n");
            break;
        }else{
            KV_make_lower_string(value);
        }
   
    }
    gui->num_pref_bods--;
    fclose(in);
 
    DEBUG_TRACE_OUT printf("Done with read_preferences\n");
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
void clean_name(char *name)
{
  int done;
  char *ptr;

  DEBUG_TRACE_IN printf("Entered clean_name\n");

  ptr = name;
  while (*ptr != '_') ptr++;
  
  *ptr = '\0';

  DEBUG_TRACE_OUT printf("Done with clean_name\n");
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
void fill_default_preferences(main_gui_t *gui)
{
    int i;

    DEBUG_TRACE_IN printf("Entered fill_default_preferences\n");

    gui->mainwindowsize = 650; gui->multiwindowsize = 200;
    gui->multi_view = 0; 
    gui->background = 0.0;
  
    gui->axis_type = 0; gui->axis_labels_on = 0;

    gui->mouse_control_method = MOUSE;

    gui->line_types[GAMMA].color.red = 65535; 
    gui->line_types[GAMMA].color.green = 0; 
    gui->line_types[GAMMA].color.blue = 0;
    gui->line_types[GAMMA].type = 2; 
    gui->line_types[GAMMA].boldness = 2;

    gui->line_types[NEUTRON_LOW].color.red = 0;  
    gui->line_types[NEUTRON_LOW].color.green = 65535; 
    gui->line_types[NEUTRON_LOW].color.blue = 0;
    gui->line_types[NEUTRON_LOW].type = 2; 
    gui->line_types[NEUTRON_LOW].boldness = 2;

    gui->line_types[NEUTRON_MED].color.red = 0; 
    gui->line_types[NEUTRON_MED].color.green = 0; 
    gui->line_types[NEUTRON_MED].color.blue = 65535;
    gui->line_types[NEUTRON_MED].type = 2; 
    gui->line_types[NEUTRON_MED].boldness = 2;

    gui->line_types[NEUTRON_HIGH].color.red = 65535; 
    gui->line_types[NEUTRON_HIGH].color.green = 0; 
    gui->line_types[NEUTRON_HIGH].color.blue = 0;
    gui->line_types[NEUTRON_HIGH].type = 3; 
    gui->line_types[NEUTRON_HIGH].boldness = 2;

    gui->line_types[BEAM].color.red = 0; 
    gui->line_types[BEAM].color.green = 65535; 
    gui->line_types[BEAM].color.blue = 0;
    gui->line_types[BEAM].type = 3; 
    gui->line_types[BEAM].boldness = 2;

    gui->line_types[LOST].color.red = 0; 
    gui->line_types[LOST].color.green = 0; 
    gui->line_types[LOST].color.blue = 65535;
    gui->line_types[LOST].type = 3; 
    gui->line_types[LOST].boldness = 2;

    /**** Don't uncomment these, the line_types array  ****/
    /**** only has room for NUM_PARTICLES slots, which ****/
    /**** which right now is only set to 6             ****/
  
/*
  gui->line_types[6].color.red = 65535; 
  gui->line_types[6].color.green = 65535; 
  gui->line_types[6].color.blue = 65535;
  gui->line_types[6].type = 1; 
  gui->line_types[6].boldness = 3;

  gui->line_types[7].color.red = 65535; 
  gui->line_types[7].color.green = 65535; 
  gui->line_types[7].color.blue = 0;
  gui->line_types[7].type = 4; 
  gui->line_types[7].boldness = 2;
*/  

    gui->scaling = 1.0; 
    gui->messages_on = 1; 
    gui->fast_rotation_type = 1; gui->cm = 0;

    gui->alpha_culling_on = 1;
    gui->texture_clamp = 1;
    gui->texture_nearest = 1;

    DEBUG_TRACE_OUT printf("Done with fill_default_preferences\n");
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
int write_generic_preference_file(main_gui_t *gui)
{
  char filename[256];
  FILE *out;
  
  DEBUG_TRACE_IN printf("Entered write_generic_preference_file\n");

  strcpy(filename,getenv("SERA_RESOURCES"));
  strcat(filename,"/Sera3d/Sera3d.rsc");

  printf("writing the generic preference file : %s\n",filename);

  if (!(out = fopen(filename,"w"))){
    return 0;
  }

  fprintf(out,"# Sera3d resources\n");
  fprintf(out,"# ***************************************\n");
  fprintf(out,"Main_Window_Size: 650	# (550, 600, 650, 700, 750, 800)\n");
  fprintf(out,"Multi_Window_Size: 175	# (150, 175, 200, 225, 250)\n");
  fprintf(out,"Multi-View: Off 	# (On , Off)\n");
  fprintf(out,"Background: black 	# (black,25,50,75, white)\n");
  fprintf(out,"# ***************************************\n");
  fprintf(out,"Axis_Type: Off 		# (Thin, Thick, Full, Ring, off)\n");
  fprintf(out,"Axis_Labelling: Off 	# (On , Off)\n");
  fprintf(out,"# ***************************************\n");
  fprintf(out,"Mouse_Control: Buttons 	# (Pull, Sliders, Buttons)\n");
  fprintf(out,"# ***************************************\n");

 fprintf(out,"Gamma: 	                65535 0 0  1  2 	# (r g b  type (1-4)  boldness (1-3))\n");
 /*
  fprintf(out,"Low_Energy_Gamma: 	65535 0 0  1  2 	# (r g b  type (1-4)  boldness (1-3))\n");
  fprintf(out,"Medium_Energy_Gamma:	 0 65535 0  1  2 	# (r g b  type (1-4)  boldness (1-3))\n");
  fprintf(out,"High_Energy_Gamma:	 0 0 65535  1  2 	# (r g b  type (1-4)  boldness (1-3))\n");
 */
  fprintf(out,"Low_Energy_Neutron: 	65535 65535 0  1  2 	# (r g b  type (1-4)  boldness (1-3))\n");
  fprintf(out,"Medium_Energy_Neutron:	 0 65535 65535  1  2 	# (r g b  type (1-4)  boldness (1-3))\n");
  fprintf(out,"High_Energy_Neutron:	 65535 0 65535  1  2 	# (r g b  type (1-4)  boldness (1-3))\n");
  fprintf(out,"Beam: 			65535 65535 65535  1  3 # (r g b  type (1-4)  boldness (1-3))\n");
  fprintf(out,"Lost: 			65535 65535 0  4  2 	# (r g b  type (1-4)  boldness (1-3))\n");
  fprintf(out,"# ***************************************\n");
  /*fprintf(out,"Scaling: 1.00 			# (greater than .5 and less than 2.0)\n");*/
  fprintf(out,"rendering_quality: high          # (low,medium,high,full)\n");
  fprintf(out,"polygon_quality: high            # (low,medium,high,full)\n");
  fprintf(out,"polygonal_normal_type: vertex    # (surface,vertex)\n");
  fprintf(out,"Messages: On 			# (On , Off)\n");
  fprintf(out,"Fast_Rotation: Off 		# (Wireframe, Axis, Off)\n");
  fprintf(out,"Measure_Unit: mm		# (cm, mm)\n");
  fprintf(out,"# ***************************************\n");
  fprintf(out,"Texture_Filtering: nearest	# (nearest,linear)\n");
  fprintf(out,"Texture_Mode: repeat		# (clamp,repeat)\n");
  fprintf(out,"Alpha_Culling: on		# (on,off)\n");
  fprintf(out,"# ***************************************\n");
  fprintf(out,"contour_level: 95 65535 0 0  #(level r g b )\n");
  fprintf(out,"contour_level: 90 0 65535 0 #(level r g b)\n");
  fprintf(out,"contour_level: 80 0 0 65535 #(level r g b)\n");
  fprintf(out,"contour_level: 70 65535 65535 0 #(level r g b)\n");
  fprintf(out,"contour_level: 60 65535 0 65535 #(level r g b)\n");
  fprintf(out,"contour_level: 50 0 65535 65535 #(level r g b)\n");
  fprintf(out,"# ***************************************\n");
  
  fprintf(out,"Scalp_color: 48558 44714 36002 100	# (R G B Opacity(0-100))\n");
  fprintf(out,"Skull_color: 50928 53235 52978 100	# (R G B Opacity(0-100))\n");
  fprintf(out,"Brain_color: 14606 56310 33952 100	# (R G B Opacity(0-100))\n");
  fprintf(out,"Left-Eye_color: 65535 42665 0 100	# (R G B Opacity(0-100))\n");
  fprintf(out,"Right-Eye_color: 65535 22613 0 100	# (R G B Opacity(0-100))\n");
  fprintf(out,"Sinus_color: 65535 34465 33952 100	# (R G B Opacity(0-100))\n");
  fprintf(out,"L-Ventricle_color: 19026 15374 57078 100	# (R G B Opacity(0-100))\n");
  fprintf(out,"l-vent_color: 19026 15374 57078 100	# (R G B Opacity(0-100))\n");
  fprintf(out,"R-Ventricle_color: 49071 15374 57078 100	# (R G B Opacity(0-100))\n");
  fprintf(out,"r-vent_color: 49071 15374 57078 100	# (R G B Opacity(0-100))\n");
  fprintf(out,"Target_color: 46508 52210 0 100	# (R G B Opacity(0-100))\n");
  fprintf(out,"Tumor_color: 65535 0 0 100	# (R G B Opacity(0-100))\n");
  
  fprintf(out,"Default_color: 20000 39590 65535 100	# (R G B Opacity(0-100))\n");
  


  fclose(out);

  DEBUG_TRACE_OUT printf("Done with write_generic_preference_file\n");
  return 1;
}
