/*#include <stdlib.h>
#include <stdio.h>
#include <string.h>*/
#include "sera3d.h"
#include "gz_tools.h"
#include "libuv.h"

#define IS 0
#define PA 1
#define RL 2

/********************************************************/
/*  Procedures used only in this file
/********************************************************/
void jump_to_next_line (FILE *fptr);
void strip_bodyname(char *name);

/** Added 04.19.2000 **/
static void getAxisInfo( char * axis, axisInfo_t * info, main_gui_t * gui );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : read_uv_file
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: the # of slices to read from the file
%%%
%%%  Purpose: opens the uv file and reads its contents into the
%%%           3d array, volume.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int read_uv_file(main_gui_t *gui)
{
  int i,j,k,l;
  gz_FILE *uv_ptr;
  unsigned char * line;
  unsigned char tmp;

  DEBUG_TRACE_IN printf("Entered read_uv_file, going to read %d slices\n",gui->num_slices);

  /********************************************************/
  /*  Open the UV file.
  /********************************************************/
  if((uv_ptr = gz_fopen (gui->uvfile,"r")) == NULL)
    {    
      DT_error(gui->toplevel,"Sorry the uv file could not be opened",NULL,NULL);
      DEBUG_TRACE_OUT printf("Done with read_uv_file, failed\n");
      return 0;
    } 
  else{
    /********************************************************/
    /* File opened, now read in all the info into volume 
    /********************************************************/
    gui->volume = (unsigned char *)MT_malloc(gui->num_slices*256*256);
    
    gz_fread(gui->volume,1,gui->num_slices*256*256,uv_ptr);


    gz_fclose(uv_ptr);
  }  

  DEBUG_TRACE_OUT printf("Done loading the file, leaving read_uv_file\n");
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : read_uvh_file
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: opens the uvh file and reads in the needed info.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int read_uvh_file(main_gui_t *gui)
{
  geom_info_t Info;
  int i=0,j=0,k;
  int dim = 1;
  int found_pref_body_match;
  char orientation[256];
  
  DEBUG_TRACE_IN printf("Entered read_uvh_file\n");
  
  read_uvh(&Info,gui->uvhfile);
  DEBUG_LOADING   printf("Read UVH file ok\n");

  if (strstr(Info.dimensionality,"cm")) dim = 10;

  gui->num_slices = Info.imageslices;
  gui->x_size = Info.pixelsizecolumns * (float)dim;
  gui->y_size = Info.pixelsizerows  * (float)dim;
  gui->z_spacing = Info.pixelsizeslices * (float)dim;
  gui->min_z = Info.isaxismin * (float)dim;

  getAxisInfo( Info.imagecolumnaxis, &(gui->axisLabels[COLUMN_AXIS]), gui );
  getAxisInfo( Info.imagerowaxis,    &(gui->axisLabels[ROW_AXIS   ]), gui );
  getAxisInfo( Info.imagesliceaxis,  &(gui->axisLabels[SLICE_AXIS ]), gui );

  /* Change labels that are dependent on the slice orientation */
  changeLabels( gui, VIEW_PANEL );
  changeLabels( gui, SLICE_PANEL );
  changeLabels( gui, SLIDER_PANEL );
  changeLabels( gui, CLIPPING_PANEL );
  
  
  DEBUG_LOADING {
    printf("the num_slices is : %d\n",gui->num_slices);
    printf("the x_size is : %f\n",gui->x_size);
    printf("the y_size is : %f\n",gui->y_size);
    printf("the z_spacing is : %f\n",gui->z_spacing);
    printf("the min_z is : %f\n",gui->min_z);
  }

  i=0;
  for (j=0;j<256;j++){
    if (Info.valid.bodyname[j] && !strstr(Info.bodyname[j],"ABSENT")){

      /*printf("found a valid uvval: %d,  name : %s, putting into sera3d body #%d\n",j,Info.bodyname[j],i);*/
      gui->bod[i].region_num = j/*Info.uvval[j]*/;
      strcpy(gui->bod[i].name,Info.bodyname[j]);
      strip_bodyname(gui->bod[i].name);

      /*
      gui->bod[i].max_x = 256;
      gui->bod[i].max_y = 256;
      gui->bod[i].max_z = gui->num_slices;

      gui->bod[i].min_x = 0;
      gui->bod[i].min_y = 0;
      gui->bod[i].min_z = 0;

      if (Info.valid.bboxrlaxismax[j])
	gui->bod[i].max_x = (Info.bboxrlaxismax[j] / gui->x_size * dim) + 128;
      if (Info.valid.bboxrlaxismin[j])
      gui->bod[i].min_x = (Info.bboxrlaxismin[j] / gui->x_size * dim) + 128;

      if (Info.valid.bboxpaaxismax[j])
	gui->bod[i].max_y = (Info.bboxpaaxismax[j] / gui->y_size * dim) + 128;
      if (Info.valid.bboxpaaxismin[j])
	gui->bod[i].min_y = (Info.bboxpaaxismin[j] / gui->y_size * dim) + 128;


      if (Info.valid.bboxisaxismax[j])
	gui->bod[i].max_z = convert_z_to_slice(gui,Info.bboxrlaxismax[j]*dim);
      if (Info.valid.bboxisaxismin[j])
	gui->bod[i].min_z = convert_z_to_slice(gui,Info.bboxrlaxismin[j]*dim);

      if (gui->bod[i].max_z > gui->num_slices) gui->bod[i].max_z = gui->num_slices;
      if (gui->bod[i].min_z < 0) gui->bod[i].min_z = 0;
      */
      

    

      /*printf("for body: %s, libuv read a uvval of : %d\n",gui->bod[i].name,j);*/

      gui->bod[i].center_mass_x = 0;
      gui->bod[i].center_mass_y = 0;
      gui->bod[i].center_mass_z = 0;

      gui->bod[i].center_uv_x = 0;
      gui->bod[i].center_uv_y = 0;
      gui->bod[i].center_uv_z = 0;      
      

      DEBUG_LOADING printf("filled body #%d, name : %s, reg num : %d\n",i,
			   gui->bod[i].name,gui->bod[i].region_num);

      if (gui->use_uvh_colors){
	/** fill in the body colors with the values fromthe uvh **/
	if (Info.valid.color_red[j]) gui->bod[i].r = (int)( ((float)(Info.color_red[j])/255.0) * 65535.0  );
	else gui->bod[i].r = rand()%65535;
	if (Info.valid.color_green[j]) gui->bod[i].g = (int)( ((float)(Info.color_green[j])/255.0) * 65535.0  );
	else gui->bod[i].g = rand()%65535;
	if (Info.valid.color_blue[j]) gui->bod[i].b = (int)( ((float)(Info.color_blue[j])/255.0) * 65535.0  );
	else gui->bod[i].b = rand()%65535;
	gui->bod[i].t = 100;
      }else{
	/** fill in the body colors from the preference bodies if available, otherwise random **/
	found_pref_body_match = 0;
	for (k=0;k<gui->num_pref_bods;k++){
	  if (strcmp(gui->bod[i].name, gui->pref_bod[k].name) ==0){
	    gui->bod[i].r = gui->pref_bod[i].r;
	    gui->bod[i].g = gui->pref_bod[i].g;
	    gui->bod[i].b = gui->pref_bod[i].b;
	    gui->bod[i].t = 100;
	    found_pref_body_match = 1;
	    break;
	  }
	}
	/** didn't find a preference, randomly pick a color **/
	if (!found_pref_body_match){
	  gui->bod[i].r = rand()%65535;
	  gui->bod[i].g = rand()%65535;
	  gui->bod[i].b = rand()%65535;
	  gui->bod[i].t = 100;
	}
      }

      gui->bod[i].enabled = 1;
      gui->bod[i].clipped = 1;
      gui->num_regions++;
      gui->num_bodies++;
      i++;
    }
  }

  DEBUG_LOADING   printf("done, num_regions is : %d, num_bodies is : %d\n",
			 gui->num_regions,gui->num_bodies);

  build_body_containments(gui);
  determine_body_drawing_order_from_containments(gui);

  DEBUG_TRACE_OUT printf("Done with read_uvh_file\n");
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : jump_to_next_line
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: file ptr
%%%
%%%  Purpose: takes the fileptr and advances it to the next line.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void jump_to_next_line(FILE *fptr)
{
  char c;

  DEBUG_TRACE_IN printf("Entered jump_to_next_line\n");

  fscanf(fptr,"%c",&c);
  while (c != '\n') fscanf(fptr,"%c",&c);

  DEBUG_TRACE_OUT printf("Done with jump_to_next_line\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : strip_bodyname
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: the name to strip
%%%
%%%  Purpose: takes the body name and strips off the 'name' to name.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void strip_bodyname(char *name)
{  
  int i = 0,j=0;
  char c = '}';
  char tempname[256];

  DEBUG_TRACE_IN printf("Entered jump_to_next_line\n");

  while (c != '\0'){
    c = name[i];
    if (c != 39){
      tempname[j] = name[i]; j++;
      }
    i++;
  }
  tempname[i] = '\0';
  strcpy(name,tempname);

  DEBUG_TRACE_OUT printf("Done with jump_to_next_line\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: the name to strip
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void fill_recent_file(main_gui_t *gui,char *filename,int type)
{
  char *resource = getenv("SERA_RESOURCES");
  char res[256],error_string[256],buf[10][256];
  FILE *out;
  int already_in_there = 0,ok = 1;
  int count = 0,i;

  DEBUG_TRACE_IN printf("Entered fill_recent_file\n");

  strcpy(res,resource);
  strcat(res,"/Sera3d/");
  switch(type){
  case 1: strcat(res,"Recent_uv.rsc"); break;
  case 2: strcat(res,"Recent_qim.rsc"); break;
  case 3: strcat(res,"Recent_pp.rsc"); break;
  case 4: strcat(res,"Recent_single_contour.rsc"); break;
  case 5: strcat(res,"Recent_full_contour.rsc"); break;
  }


  if (!(out = fopen(res,"r"))){
    sprintf(error_string,"Couldn't open : %s, making it",res);

    printf("%s\n",error_string);
    printf("going to make it\n");
    if (!(out = fopen(res,"w"))) printf("couldn't make it\n");
  }   

  if ((fscanf(out,"%s",buf[count]) == EOF)) ok = 0;
  while(ok){
    /*printf("comparing : %s\nwith : %s\n",filename,buf[count]);*/
    if (strcmp(filename,buf[count]) == 0){
      /*printf("this file is alread in the list of recents\n");*/
      already_in_there = 1; break;
    }
    count++;
    if ((fscanf(out,"%s",buf[count]) == EOF)) ok = 0;
  }

  fclose(out);
  if (count >= (MAX_RECENT_FILES)) count = MAX_RECENT_FILES-1;
  if (!already_in_there){
    if (!(out = fopen (res,"w"))) printf("couldn't open %s to write\n",res); 
    else{
      fprintf(out,"%s\n",filename);
      for (i=0;i<count;i++)
	fprintf(out,"%s\n",buf[i]);
    }
    fclose(out);
  }

  DEBUG_TRACE_OUT printf("Done with fill_recent_file\n");
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
void get_recent_files_and_build_widgets(main_gui_t *gui, Widget parent, int type)
{
    int ok = 1;
    FILE *in;
    char buf[256],error_string[256];
    char *resource = getenv("SERA_RESOURCES");
    char res[256];
    XmString xmstr;
    Widget temp;

    char validSuffixes[5][32]; /* list of valid suffixes */
    int numSuffixes;
    int validFile;
    int i;
    
    
    DEBUG_TRACE_IN printf("Entered get_recent_files_and_build_widgets\n");
    
    strcpy(res,resource);
    /*printf("getting the recent names, res is : %s\n",res);*/
    strcat(res,"/Sera3d/");
    switch(type)
    {
        case 1:
            strcat(res,"Recent_uv.rsc");
            strcpy( validSuffixes[0], ".uv" );
            strcpy( validSuffixes[1], ".uvh" );
            strcpy( validSuffixes[2], ".uv.gz" );
            numSuffixes = 3;
            break;
            
        case 2:
            strcat(res,"Recent_qim.rsc");
            strcpy( validSuffixes[0], ".qhd" );
            strcpy( validSuffixes[1], ".qim" );
            numSuffixes = 2;
            break;

        case 3:
            strcat(res,"Recent_pp.rsc");
            strcpy( validSuffixes[0], ".pp" );
            numSuffixes = 1;
            break;
            
        case 4:
            strcat(res,"Recent_single_contour.rsc");
            strcpy( validSuffixes[0], ",contour" );
            numSuffixes = 1;
            break;

        case 5:
            strcat(res,"Recent_full_contour.rsc");
            strcpy( validSuffixes[0], ".cdf.sz" );
            numSuffixes = 1;
            break;
    }
    
    if (!(in = fopen(res,"r")))
    {
        sprintf(error_string,"Couldn't open : %s",res);
        
        printf("%s\n",error_string);            
        if (!(in = fopen(res,"w"))) printf("couldn't make it\n");
        else
        {
            printf("built it, it will be there next time\n");
            fclose(in);
        }
    }
    else
    {
        if ((fscanf(in,"%s",buf) == EOF)) ok = 0;
        while(ok)
        {
            if( FT_fileExists( buf ) )
            {
                /* Check for the correct type of file before adding it */
                validFile = 0;
                i = 0;
                while( i < numSuffixes && !validFile )
                {
                    if( FT_filenameEndsIn( buf, validSuffixes[i] ) )
                        validFile = 1;
                    i++;
                }

                if( validFile )  /* Make a widget if valid */
                {
                    
                    xmstr = XmStringCreateLocalized(buf);
                    temp = XtVaCreateManagedWidget("name",
                                                   xmPushButtonWidgetClass, parent,
                                                   XmNlabelString, xmstr,
                                                   NULL);
                    switch(type)
                    {
                        case 1: 
                            XtAddCallback(temp,XmNactivateCallback, LoadUnivelFileCallback,(XtPointer)gui);       
                            gui->file_menu.load_regions.num_submenus++;
                            gui->file_menu.load_regions.submenu[gui->file_menu.load_regions.num_submenus] = temp;
                            break;
                        case 2: 
                            XtAddCallback(temp,XmNactivateCallback, Load_RawsCB,(XtPointer)gui); 
                            gui->file_menu.load_images.num_submenus++;
                            gui->file_menu.load_images.submenu[gui->file_menu.load_images.num_submenus] = temp;
                            break;
                        case 3: 
                            XtAddCallback(temp,XmNactivateCallback, SelectParticleFileCallback,(XtPointer)gui); 
                            gui->file_menu.load_paths.num_submenus++;
                            gui->file_menu.load_paths.submenu[gui->file_menu.load_paths.num_submenus] = temp;
                            break;
                        case 4: 
                            XtAddCallback(temp,XmNactivateCallback, Load_Single_Contour_FileCB,(XtPointer)gui); 
                            gui->file_menu.load_single_contour.num_submenus++;
                            gui->file_menu.load_single_contour.submenu[gui->file_menu.load_single_contour.num_submenus] = temp;
                            break;
                        case 5: 
                            XtAddCallback(temp,XmNactivateCallback, Load_Full_Contour_FileCB,(XtPointer)gui); 
                            gui->file_menu.load_full_contour.num_submenus++;
                            gui->file_menu.load_full_contour.submenu[gui->file_menu.load_full_contour.num_submenus] = temp;
                            break;
                    }
                    XmStringFree(xmstr);
                }
            }
            
            if ((fscanf(in,"%s",buf) == EOF)) ok = 0;
        }
        fclose(in);
    }
    DEBUG_TRACE_OUT printf("Done with get_recent_files_and_build_widgets\n");
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
void build_body_containments(main_gui_t *gui)
{
  FILE *info_file;
  char *key,*value;
  int i,j,bod_name_found = 0;
  char bod_name[256],contained_bod_name[256];
  char buffer[100];
  char *R_file = "Sera3d/body_containment.info";
  char body_containment_filename[256];

  DEBUG_TRACE_IN printf("Entered build_body_containments\n");

  get_resource_path_name(body_containment_filename, R_file);

  if (!(info_file = fopen(body_containment_filename,"r"))){ 
    DT_error(gui->toplevel,"Couldn't open the containment file\n",NULL,NULL);
    return;
  }

  
  KV_set_split_characters(">");
  KV_read_next_key_and_value(info_file,buffer,100,&key,&value);
  KV_make_lower_string(value);
  
    while (1){
      

      /*printf("got    key : %s, value : %s\n",key,value);*/
      bod_name_found = 0;

      for (i=1;i<gui->num_bodies;i++){
	strcpy(bod_name,gui->bod[i].name);
	KV_make_lower_string(bod_name);

	if (strcmp(key,bod_name) == 0){
	  bod_name_found = 1;

	  for (j=1;j<gui->num_bodies;j++){
	    strcpy(contained_bod_name,gui->bod[j].name);
	    KV_make_lower_string(contained_bod_name);

	    if (strcmp(value,contained_bod_name) == 0){

                 DEBUG_DATA
	            printf("found a containment: %s contains : %s\n",
			   gui->bod[i].name,gui->bod[j].name);
                 

	      gui->bod[i].contained_bods[gui->bod[i].num_contained_bods] = j;
	      gui->bod[i].num_contained_bods++;
	      add_contained_bod_to_higher_bodies(gui,i,j);
	      break;
	    }
	  }
	}
	if (bod_name_found == 1) break;
      }

      if (!KV_read_next_key_and_value(info_file,buffer,100,&key,&value)){
	 break;
       }else{
	 KV_make_lower_string(value);
       }
    }

  DEBUG_TRACE_OUT printf("Done with build_body_containments\n");
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
void add_contained_bod_to_higher_bodies(main_gui_t *gui,int current_bod, int contained_bod)
{
  int i,j,num_contained;
  int does_contain_sub_body;
  int does_contain_current_body;

  DEBUG_TRACE_IN printf("Entered add_contained_bod_to_higher_bodies\n");

  for (i = 1; i<gui->num_bodies;i++){

    num_contained = gui->bod[i].num_contained_bods;
    
    if (num_contained != 0){
      does_contain_current_body = 0;
      does_contain_sub_body = 0;
      for (j = 0;j<num_contained; j++){

	if (gui->bod[i].contained_bods[j] == contained_bod)
	  does_contain_sub_body = 1;
	
	if (gui->bod[i].contained_bods[j] == current_bod){ 
	  does_contain_current_body = 1;

	  DEBUG_DATA
	       printf("found that %s contains %s, so adding %s\n",
		 gui->bod[i].name, gui->bod[current_bod].name, gui->bod[contained_bod].name);
	
	}
	  /** it has the current body, see if it already
	      contains the sub body passed in **/
      }

      if (does_contain_current_body && !does_contain_sub_body){

	gui->bod[i].contained_bods[gui->bod[i].num_contained_bods] = contained_bod;
	gui->bod[i].num_contained_bods ++;
      }
    }

  }

  DEBUG_TRACE_OUT printf("Done with add_contained_bod_to_higher_bodies\n");
}


void determine_body_drawing_order_from_containments(main_gui_t *gui)
{
  int i,j;
  char body_used[MAX_BODS];
  int body_containment_count[MAX_BODS];
  int temp[MAX_BODS];

  for (i=0;i<MAX_BODS;i++){
    body_used[i] = 0;
  }
  
  for (i=1;i<gui->num_bodies;i++)
    body_containment_count[i] = gui->bod[i].num_contained_bods;

  /*printf("These are the bodies we have... We have %d of them.\n", gui->num_bodies);
    for( i = 0; i < gui->num_bodies; i++ )
    printf("Body[%d] = %s\n", i, gui->bod[i].name);*/
  
  
  /*printf("the containment counts are : \n");
    for (i=1;i<gui->num_bodies;i++){
    printf("%s  : %d\n",gui->bod[i].name, body_containment_count[i]);
    }*/
  
  /*
   * Because buffer isn't included in the body_containment_count array
   * we are only going to sort gui->num_bodies-1 bodies, so pass that as
   * the number of elements to sort to bubble_sort.
   */
  bubble_sort_int_array(&body_containment_count[1],gui->num_bodies-1);

  /*
    printf("after the sort they are : \n");  
    for (i=1;i<gui->num_bodies;i++){
    printf("%d  %s\n", body_containment_count[i]);
    }*/
  

  for (i=1;i<gui->num_bodies;i++)
    for(j=1;j<gui->num_bodies;j++)
      if (gui->bod[j].num_contained_bods == body_containment_count[i] && !body_used[j]){
	body_used[j] = 1;
	/*printf("assigning %d to order position %d\n",j,gui->num_bodies-i);*/
	gui->body_order[gui->num_bodies-i] = j;
	break;
      }

  /*for (i=0;i<=gui->num_bodies;i++)*/

  DEBUG_DATA{
    printf("the body order is :\n");
    for (i=0;i<gui->num_bodies;i++){
      printf("%d:  %s\n",i,gui->bod[gui->body_order[i]].name);
    }
  }  
}


static void getAxisInfo( char * axis, axisInfo_t * info, main_gui_t * gui )
{
    int index = IS_AXIS;
    char copy[16];

    /* Make a local copy of axis, SHOULD only be a max of 4 bytes */
    strcpy( copy, axis );

    /* Convert to lower case */
    KV_trim_string( copy );
    KV_make_lower_string( copy );

    /* Now determine the type of orientation */
    if     ( strstr( copy, "is" ) ) index = IS_AXIS;
    else if( strstr( copy, "pa" ) ) index = PA_AXIS;
    else if( strstr( copy, "rl" ) ) index = RL_AXIS;

    /* Use index to determine the string name of this axis */
    strcpy( info->name, gui->axisLabelMap[index] );

    /*
     * Now get the "first" and "last" characters. This
     * will depend on if there is a '+' or '-' in axis
     */
    if( strrchr( copy, '+' ) )
    {
        /* First and last are in the correct order */
        info->first = info->name[0];
        info->last  = info->name[1];
    }
    else /* Need to reverse first and last */
    {
        info->first = info->name[1];
        info->last  = info->name[0];
    }
}
