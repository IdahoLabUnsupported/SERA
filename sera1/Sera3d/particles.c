#include "sera3d.h"

void ParticleCancelCallback(Widget w, XtPointer clientData, XtPointer callData);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : AntialiasingToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles line antialiasing on/off (for particle paths)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void AntialiasingToggledCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;

  DEBUG_TRACE_IN printf("Entered AntialiasingToggledCB\n");
  
  DisplayBusyCursor(gui->mainwindow);

  if (cbs->set) gui->antialiasing  = 1;
  else gui->antialiasing = 0;
  
  draw_all(gui);
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with AntialiasingToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Rays_ToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: Toggles the various Particle Paths on/off
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Rays_ToggledCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;
  
  DEBUG_TRACE_IN printf("Entered Rays_ToggledCB\n");

  DisplayBusyCursor(gui->mainwindow);
  
  if (strcmp(XtName(w),"Gamma") == 0){
    if (cbs->set) gui->ray_toggles[GAMMA] = 1;
    else gui->ray_toggles[GAMMA] = 0;
    /*
      }else if (strcmp(XtName(w),"Gamma Med") == 0){
      if (cbs->set) gui->ray_toggles[1] = 1;
      else gui->ray_toggles[1] = 0;
      }else if (strcmp(XtName(w),"Gamma High") == 0){
      if (cbs->set) gui->ray_toggles[2] = 1;
      else gui->ray_toggles[2] = 0;
    */
  }else if (strcmp(XtName(w),"Neutron Low") == 0){
    if (cbs->set) gui->ray_toggles[NEUTRON_LOW] = 1;
    else gui->ray_toggles[NEUTRON_LOW] = 0;
  }else if (strcmp(XtName(w),"Neutron Med") == 0){
    if (cbs->set) gui->ray_toggles[NEUTRON_MED] = 1;
    else gui->ray_toggles[NEUTRON_MED] = 0;
  }else if (strcmp(XtName(w),"Neutron High") == 0){
    if (cbs->set) gui->ray_toggles[NEUTRON_HIGH] = 1;
    else gui->ray_toggles[NEUTRON_HIGH] = 0;
  }else if (strcmp(XtName(w),"Beam") == 0){
    if (cbs->set) gui->ray_toggles[BEAM] = 1;
    else gui->ray_toggles[BEAM] = 0;
  }else if (strcmp(XtName(w),"Lost") == 0){
    if (cbs->set) gui->ray_toggles[LOST] = 1;
    else gui->ray_toggles[LOST] = 0;
  }
  
  draw_all(gui);
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Rays_ToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Draw_ParticlesToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the drawing of all Paths
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Draw_ParticlesToggledCB(Widget w, XtPointer clientData,  XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;

  DEBUG_TRACE_IN printf("Entered Draw_ParticlesToggledCB\n");

  DisplayBusyCursor(gui->mainwindow);
  
  if (cbs->set) gui->draw_particle_paths = 1;
  else gui->draw_particle_paths = 0;

  draw_all(gui);
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Draw_ParticlesToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : SelectParticleFileCallback
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb), if clientdata is one, use the widgets name
%%%                    as the filename
%%%
%%%  Purpose: pulls up the selectfiledialog to open files
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void SelectParticleFileCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    main_gui_t *gui = (main_gui_t *)clientData;
    int is_direct;
    char fileName[256];
    int i;

    DEBUG_TRACE_IN printf("Entered SelectParticleFileCallback\n");

    is_direct = 0;
    for(i=0;i<gui->file_menu.load_paths.num_submenus;i++){
        if (w == gui->file_menu.load_paths.submenu[i+1]){
            is_direct = 1; break;
        }
    }

    DisplayBusyCursor(gui->form);
  
    if (!is_direct){
        if (!DT_select_file(gui->toplevel,gui->app,&fileName[0],NULL)){ 
            RemoveBusyCursor(gui->form);
            DEBUG_TRACE_OUT printf("Done with SelectParticleFileCallback\n");
            return;
        }
    }else{ 
        strcpy(fileName,GetTextOfLabel(w));
    }

    if ( FT_fileExists( fileName ) && FT_filenameEndsIn( fileName, ".pp" ) )  
    {
        process_particle_file(gui,fileName);
        fill_recent_file(gui,fileName,3);
    }
    else 
        DT_error(gui->toplevel,"Sorry that is not a valid file",NULL,NULL);
    
    draw_all(gui);

    RemoveBusyCursor(gui->form);

    DEBUG_TRACE_OUT printf("Done with SelectParticleFileCallback\n");
}
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : process_particle_file
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: filename to process
%%%
%%%  Purpose: reads in the particle file and fills the particle
%%%           paths into the structure
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void process_particle_file(main_gui_t *gui,char *filename)
{
  int i=0,j, done = 0;
  int ok = 1;
  FILE *in;
  float temp[8];
  

  DEBUG_TRACE_IN printf("Entered process_particle_file\n");

  /* initialize */
  /*for (i=0;i<gui->num_particle_paths;i++)
    for(j=0;j<8;j++)
    gui->particle_paths[i][j] = 0;*/

  /** now read in **/
  gui->num_particle_paths = 0;
  if (!(in = fopen(filename, "r"))){
    DT_error(gui->toplevel,"Couldn't open the Path file",NULL,NULL);
    return;
  }
  i=0;

  if (gui->num_particle_paths != 0){
    MT_free( (void *) gui->particle_path);
    gui->num_particle_paths = 0;
  }

  /*printf("checking to see how many tracks there are\n");*/
  while (1){
      int inchar, buffer[256];
      while((inchar=fgetc(in))!=EOF && inchar=='#'){
	fgets((char *)buffer,256,in);
      }
      ungetc(inchar, in);

      for (j=0;j<8;j++){
	if (fscanf(in,"%f",&temp[j]) == EOF){
	  ok = 0;break;
	}
      }
      if (!ok) break;
      gui->num_particle_paths ++;
      if (gui->num_particle_paths == MAX_PARTICLE_TRACKS){
	DT_error(gui->toplevel,"Loaded the max number of particle tracks",NULL,NULL);
	break;
      }
  }

  /*printf("there are %d tracks in the file\n",gui->num_particle_paths);*/
  
  gui->particle_path = (particle_path_t *)MT_malloc(sizeof(particle_path_t) * gui->num_particle_paths);
  
  rewind(in);
  {
    int inchar, buffer[256];
    while((inchar=fgetc(in))!=EOF && inchar=='#'){
      fgets((char *)buffer,256,in);
    }
    ungetc(inchar, in);
  }

  for(i=0;i<gui->num_particle_paths;i++){

      fscanf(in,"%f",&gui->particle_path[i].start_x);
      fscanf(in,"%f",&gui->particle_path[i].start_y);
      fscanf(in,"%f",&gui->particle_path[i].start_z);
      fscanf(in,"%f",&gui->particle_path[i].end_x);
      fscanf(in,"%f",&gui->particle_path[i].end_y);
      fscanf(in,"%f",&gui->particle_path[i].end_z);
      fscanf(in,"%f",&gui->particle_path[i].type);
      fscanf(in,"%f",&gui->particle_path[i].energy);

      
      /*      printf("read : %f  %f  %f  %f  %f  %f  %f  %f\n",
	      gui->particle_path[i].start_x,
	      gui->particle_path[i].start_y,
	      gui->particle_path[i].start_z,
	      gui->particle_path[i].end_x,
	      gui->particle_path[i].end_y,
	      gui->particle_path[i].end_z,
	      gui->particle_path[i].type,
	      gui->particle_path[i].energy);*/

      /*** we need to get the world coordinates for the z value of the particle track **/
      /*printf("the start_z was : %f",gui->particle_path[i].start_z);*/
      gui->particle_path[i].start_z = convert_slice_z_to_world_y(gui,gui->particle_path[i].start_z);
      gui->particle_path[i].end_z = convert_slice_z_to_world_y(gui,gui->particle_path[i].end_z);
      /*printf("  it is now : %f\n",gui->particle_path[i].start_z);
	printf("converting it back gives us : %f\n\n",convert_world_y_to_slice_z(gui,gui->particle_path[i].start_z));      
      */


      if (gui->particle_path[i].type == 2 && gui->particle_path[i].energy == 0){
	calculate_beam_slope(gui,
			     gui->particle_path[i].start_x,gui->particle_path[i].start_z,gui->particle_path[i].start_y,
			     gui->particle_path[i].end_x,gui->particle_path[i].end_z,gui->particle_path[i].end_y);
	gui->beam_slice_x = gui->particle_path[i].start_x;
	gui->beam_slice_y = gui->particle_path[i].start_z;
	gui->beam_slice_z = gui->particle_path[i].start_y;
	gui->beam_num = i;
	/*printf("set the beam number to : %d\n",i);*/
      }
  }
  fclose(in);
  
  gui->draw_particle_paths = 1;


  DEBUG_TRACE_OUT printf("Done with process_particle_file\n");
}    
