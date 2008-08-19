#include "sera3d.h"

void LoadContoursCB(Widget w, XtPointer clientData, XtPointer callData);
void init_contour_data_structs();
void add_contours_to_bodylist();
void fill_forms_with_contour_names(main_gui_t *gui);
void read_colorwash_data(char *filename);
void build_test_cw_fileCB(Widget w,XtPointer clientdata, XtPointer calldata);

int read_header_file_values ( main_gui_t *gui, char *filename );
int read_contour_file_values ( main_gui_t *gui, char *filename );


void remove_contour_surfaces ( main_gui_t *gui )
{
  if (gui->num_contour_surfaces != 0)
  {
      glDeleteLists(CONTOUR_LISTS,gui->num_contour_surfaces);
      remove_contour_names_from_forms(gui);
      remove_contours_from_bodylist(gui);    
      remove_contours_from_clippinglist(gui);
  }
  gui->num_contour_surfaces = 0;
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
void build_polygonal_contour_surfacesCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = ( main_gui_t *)clientdata;
  int i;
  char string[256];
  char message[256];
  XColor color;

  DisplayBusyCursor(gui->mainwindow);

  remove_contour_surfaces ( gui );
  
  for (i=0;i<gui->legend.num_levels;i++){
    build_data_block_from_contour_data(gui, gui->legend.levels[i]);

    XtVaGetValues(gui->legend.color_box[i], XmNbackground, &color.pixel, NULL);

    get_rgb_from_pixel(gui,&color);

    /** set the color and characteristics**/
    gui->bod[MAX_BODS + i].r = color.red;
    gui->bod[MAX_BODS + i].g = color.green;
    gui->bod[MAX_BODS + i].b = color.blue;
    gui->bod[MAX_BODS + i].t = 100.0;
    gui->bod[MAX_BODS + i].enabled = 1;
    gui->bod[MAX_BODS + i].clipped = 1;
    sprintf(string,"Level %d",gui->legend.levels[i]);
    strcpy(gui->bod[MAX_BODS + i].name,string);

    glNewList(CONTOUR_LISTS + gui->num_contour_surfaces,GL_COMPILE);
      extract_polygonal_surface_from_data_block(gui);
    glEndList();
    
    gui->num_contour_surfaces++;

    /** don't need the data block anymore **/
    free_data_block(&gui->block);
    
    sprintf(message,"Contour surface : %s built . . . %d polygons",string,gui->polygon_count);
    PostMessage(gui,message);
    DEBUG_DATA printf("%s\n",message);
  }

  add_contours_to_bodylist(gui);
  fill_forms_with_contour_names(gui);
  add_contours_to_clippinglist(gui);

  draw_all(gui);

  RemoveBusyCursor(gui->mainwindow);
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
void build_data_block_from_contour_data(main_gui_t *gui, int wanted_contour_level)
{
  int contour_map_index;
  Contour_grid_t *current_grid;
  int num_grids = 0;
  int xsize,ysize,zsize;
  int x,y,z;
  unsigned char *block;
  int value;


  current_grid = gui->Grids.next;
  while(current_grid != NULL){ 
    num_grids++;
    current_grid = current_grid->next;
  }
  xsize = 256;
  ysize = 256;
  zsize = num_grids;

  
  gui->block.data = (unsigned char *)MT_malloc(xsize*ysize*zsize);
  gui->block.x_size = xsize;
  gui->block.y_size = ysize;
  gui->block.z_size = zsize;
  
  gui->block.x_resolution = 1;
  gui->block.y_resolution = 1;
  gui->block.z_resolution = 1;

  gui->block.x_offset = 0;
  gui->block.y_offset = 0;
  
  gui->block.z_offset = convert_z_to_slice(gui,gui->contour_shift);


  z=-1;
  current_grid = gui->Grids.next;
  while(current_grid != NULL){
    z++;

    /*printf("building slice #%d of the data block\n",z);*/


    for (y=0;y<ysize;y++){
      for (x=0;x<xsize;x++){
	/*printf("getting the value : %d, %d\n", y,x);*/
	value = get_mapped_value_from_contour_grid(current_grid,ysize-y,x,xsize,ysize,gui->current_dose_component);


	if (value < wanted_contour_level)
	  gui->block.data[z*xsize*ysize + y*xsize + x] = 0;
	else
	  gui->block.data[z*xsize*ysize + y*xsize + x] = 1;
	
      }
    }
    current_grid = current_grid->next;
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
void init_contour_structures(main_gui_t *gui)
{
  gui->Grids.numgrids = 0;
  gui->Grids.next = NULL;
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
void fill_contour_colormap(main_gui_t *gui)
{
  int i,j;
  int set;
  DEBUG_TRACE_IN printf("entered fill_contour_colormap\n");

  /*
  for (i=0;i<MAX_CONTOUR_LEVEL;i++){
    Contour_Map[i].r = (GLubyte)((float)i/100.0 * 255.0);
    Contour_Map[i].g = (GLubyte)0;
    Contour_Map[i].b = (GLubyte)((float)(100-i) * 255.0);
    Contour_Map[i].a = (GLubyte)255;
  }
  */
  /*
  for (i=0;i<MAX_CONTOUR_LEVEL;i++){
    Contour_Map[i].r = (GLubyte)((float)i/100.0 * 255.0);
    Contour_Map[i].g = (GLubyte)0;
    Contour_Map[i].b = (GLubyte)((float)(100-i) * 255.0);
    Contour_Map[i].a = (GLubyte)255;
  }
  */
  for(i=0;i<MAX_CONTOUR_LEVEL;i++){

    if (gui->contour_pref[i].filled){
      for (j=i;j<MAX_CONTOUR_LEVEL;j++){
	gui->Contour_Map[j].r = (GLubyte)((float)gui->contour_pref[i].color.red/65535.0 * 255.0);
	gui->Contour_Map[j].g = (GLubyte)((float)gui->contour_pref[i].color.green/65535.0 * 255.0);
	gui->Contour_Map[j].b = (GLubyte)((float)gui->contour_pref[i].color.blue/65535.0 * 255.0);
	gui->Contour_Map[j].a = (GLubyte)255;
	/*break;*/
      }
    }

  }

  DEBUG_TRACE_OUT printf("Done with fill_contour_colormap\n");
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
void rebuild_contour_colormap(main_gui_t *gui)
{
  int i,j;
  XColor color;


  DEBUG_TRACE_IN printf("entered fill_contour_colormap\n");

  for (i=0;i<MAX_CONTOUR_LEVEL;i++){
    gui->Contour_Map[i].r = (GLubyte)0;/*((float)i/100.0 * 255.0);*/
    gui->Contour_Map[i].g = (GLubyte)0;
    gui->Contour_Map[i].b = (GLubyte)0;/*((float)(100-i) * 255.0);*/
    gui->Contour_Map[i].a = (GLubyte)255;
  }

  if (gui->colorwash_outlines){
    for(i=0;i<MAX_CONTOUR_LEVEL;i++){
      for (j=0;j<gui->legend.num_levels;j++){
	if (i == gui->legend.levels[j]){
	  XtVaGetValues(gui->legend.color_box[j],XmNbackground, &color.pixel,NULL);
	  get_rgb_from_pixel(gui,&color);
	  gui->Contour_Map[i].r = (GLubyte)((float)color.red/65535.0 * 255.0);
	  gui->Contour_Map[i].g = (GLubyte)((float)color.green/65535.0 * 255.0);
	  gui->Contour_Map[i].b = (GLubyte)((float)color.blue/65535.0 * 255.0);
	  gui->Contour_Map[i].a = (GLubyte)255;
	  break;
	}
      }
    }
  }else{
    for(i=MAX_CONTOUR_LEVEL -1;i>-1;i--){
      for (j=0;j<gui->legend.num_levels;j++){
	if (i > gui->legend.levels[j]){
	  XtVaGetValues(gui->legend.color_box[j],XmNbackground, &color.pixel,NULL);
	  get_rgb_from_pixel(gui,&color);
	  gui->Contour_Map[i].r = (GLubyte)((float)color.red/65535.0 * 255.0);
	  gui->Contour_Map[i].g = (GLubyte)((float)color.green/65535.0 * 255.0);
	  gui->Contour_Map[i].b = (GLubyte)((float)color.blue/65535.0 * 255.0);
	  gui->Contour_Map[i].a = (GLubyte)255;
	  break;
	}
      }
    }
  }

  DEBUG_TRACE_OUT printf("Done with fill_contour_colormap\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Dose_Component_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Dose_Component_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
    main_gui_t *gui = (main_gui_t *)clientdata;
    int i;

    DisplayBusyCursor(gui->mainwindow);

    for (i=0;i<=8;i++){
        if (w == gui->contour_panel.dose_component[i]){
            gui->current_dose_component = i;
            break;
        }
    }

    if (gui->colorwash_texture_with_dose)
    {
        remove_contour_surfaces ( gui );
        rebuild_3d_texture_from_texture_volume ( gui );
        /*build_polygonal_contour_surfacesCB(w, clientdata, calldata);*/
        draw_all(gui);
    }

    RemoveBusyCursor(gui->mainwindow);
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
void Dose_Display_ToggledCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)calldata;

  DEBUG_TRACE_IN printf("Entered Dose_Display_ToggledCB\n");
  
  DisplayBusyCursor(gui->mainwindow);
  if (cbs->set)
  {
      gui->colorwash_texture_with_dose = 1;
      XtSetSensitive( gui->contour_panel.dose_outline_toggle, True );
  }
  else
  {
      gui->colorwash_texture_with_dose = 0;
      XmToggleButtonSetState( gui->contour_panel.dose_outline_toggle, False, False );
      XtSetSensitive( gui->contour_panel.dose_outline_toggle, False );
      gui->colorwash_outlines = 0;
  }
  
  rebuild_contour_colormap( gui );
  rebuild_3d_texture_from_texture_volume(gui);
  draw_all(gui);
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Dose_Display_ToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Dose_Outlines_ToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: pulls up the selectfiledialog to open files
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Dose_Outlines_ToggledCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;  

  DEBUG_TRACE_IN printf("Entered Dose_Outlines_ToggledCB\n");

  DisplayBusyCursor(gui->mainwindow);
  if (cbs->set) gui->colorwash_outlines = 1;
  else gui->colorwash_outlines = 0;
  
  rebuild_contour_colormap(gui);
  rebuild_3d_texture_from_texture_volume(gui);
  draw_all(gui);

  RemoveBusyCursor(gui->mainwindow);
  DEBUG_TRACE_OUT printf("Done with Dose_Outlines_ToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Load_Full_Contour_FileCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Load_Full_Contour_FileCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
    main_gui_t *gui = (main_gui_t *)clientdata;
    XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) calldata;
    char contourFilename[256], headerFilename[256], *temp;
    int i, is_direct = 0;

    DEBUG_TRACE_IN printf("Entered Load_Full_Contour_FileCB\n");

    for ( i = 0; i < gui->file_menu.load_full_contour.num_submenus; i++ )
    {
        if ( w == gui->file_menu.load_full_contour.submenu[i+1] )
        {
            is_direct = 1;
            break;
        }
    }

    /*************************************************/
    /** Get the string (filename)
    /*************************************************/
    if ( is_direct )
        strcpy ( contourFilename, GetTextOfLabel ( w ) );
    else
        DT_select_file ( gui->toplevel, gui->app, &contourFilename[0], NULL );
    
    strcpy ( headerFilename, contourFilename );
    
    if ( ! ( temp = strstr ( headerFilename, ".cdf.sz" ) ) )
    {
        DT_error ( gui->toplevel, "File is not a concatenated contour file.", NULL, NULL );
        return;    
    }

    strcpy ( temp, ".chd" );

    PostMessage ( gui, "Loading Header File . . ." );
    
    DEBUG_LOADING printf ( "Loading %s\n", headerFilename );
    
    if ( ! ( read_header_file_values ( gui, headerFilename ) ) )
    {
        DT_error ( gui->toplevel, "Could not read header file.", NULL, NULL );
        return;
    }

    PostMessage ( gui, "Loading Concatenated Contour File . . ." );
    
    DEBUG_LOADING printf ( "Loading %s\n", contourFilename );    
    
    if ( ! ( read_contour_file_values ( gui, contourFilename ) ) )
    {
        DT_error ( gui->toplevel, "Could not read contour file.", NULL, NULL );
        return;        
    }

    fill_recent_file ( gui, contourFilename, 5 );

    /* Set the Colorwash With Doses button, and call its callback */
    XtSetSensitive( gui->contour_panel.dose_display_toggle, True );
    XmToggleButtonSetState( gui->contour_panel.dose_display_toggle, True, True );
}


/*=====================================================================
  Function:    read_header_file_values

  Purpose:     Read and store values from a concatenated contour/mask
               header file.

  Parameters:  main_gui_t *gui  -  pointer to main program structure
               char *filename   -  header file name

  Returned:    1 upon success, 0 otherwise.

  Written by:  Matt Cohen 5/17/99
  ====================================================================*/
int read_header_file_values ( main_gui_t *gui, char *filename )
{
    FILE *infile;
    char line[256];
    char value[256];
    char z_val_str[256];
    static int first_call = 1;
    int i;
    
    DEBUG_TRACE_IN printf ( "Entering read_header_file_values\n" );
    
    KV_set_split_characters ( ":" );

    if ( ! ( infile = fopen ( filename, "r" ) ) )
    {
        printf ( "Could not load %s\n", filename );
        DEBUG_TRACE_OUT printf ( "Leaving read_header_file_values\n" );
        return ( 0 );
    }

    fgets ( line, 256, infile );
    
    /*
     *  As of now, expect only version 1.07 for concatenated files
     */
    
    if ( strstr ( line, "seraMC_1A0" ) || strstr(line, "seraPlan_1B0"))
    {
        DEBUG_LOADING printf(" ---- Version seraMC_1A0 Contour Data ----\n");
    }    
    else
    {
      char temp[512];
      snprintf(temp,512,"The file has a different version than expected.\nIt uses:\n%s\nDo you want to load it anyway?",line);
      if(DT_decide(gui->toplevel, gui->app, temp, "Different Version",
		   "Load","Cancel load" )){       
	DEBUG_LOADING printf(" ---- Version seraMC_1A0 Contour Data ----\n");
      } else {
        printf ( "This concatenated contour data file does not appear to be version seraMC_1A0.\n" );
        printf ( "sera3d does not have the ability to load this file.\n" );
        DEBUG_TRACE_OUT printf ( "Leaving read_header_file_values\n" );
        return ( 0 );
      }
    }

    /* Read in Field of View */
    if ( KV_read_string_value_for_key ( infile, "fov", value, 256 ) )
        sscanf ( value, "%e", &gui->FOV );
    else printf ( "Could not locate FOV\n" );

    /* Read in xmin, xmax, ymin, and ymax */
    if ( KV_read_string_value_for_key ( infile, "xmin", value, 256 ) )
        sscanf ( value, "%e", &gui->xmin );
    else printf ( "Could not locate xmin\n" );

    if ( KV_read_string_value_for_key ( infile, "xmax", value, 256 ) )
        sscanf ( value, "%e", &gui->xmax );
    else printf ( "Could not locate xmax\n" );

    if ( KV_read_string_value_for_key ( infile, "ymin", value, 256 ) )
        sscanf ( value, "%e", &gui->ymin );
    else printf ( "Could not locate ymin\n" );

    if ( KV_read_string_value_for_key ( infile, "ymax", value, 256 ) )
        sscanf ( value, "%e", &gui->ymax );
    else printf ( "Could not locate ymax\n" );

    /* Read in x_in_field, y_in_field, z_in_filed */
    if ( KV_read_string_value_for_key ( infile, "x_in_field", value, 256 ) )
        sscanf ( value, "%d", &gui->x_in_field );
    else printf ( "Could not locate x_in_field\n" );
    
    if ( KV_read_string_value_for_key ( infile, "y_in_field", value, 256 ) )
        sscanf ( value, "%d", &gui->y_in_field );
    else printf ( "Could not locate y_in_field\n" );
    
    if ( KV_read_string_value_for_key ( infile, "z_in_field", value, 256 ) )
        sscanf ( value, "%d", &gui->z_in_field );
    else printf ( "Could not locate z_in_field\n" );

    /* Read in num_cols and num_rows */
    if ( KV_read_string_value_for_key ( infile, "num_cols", value, 256 ) )
        sscanf ( value, "%d", &gui->num_cols );
    else printf ( "Could not locate num_cols\n" );

    if ( KV_read_string_value_for_key ( infile, "num_rows", value, 256 ) )
        sscanf ( value, "%d", &gui->num_rows );
    else printf ( "Could not locate num_rows\n" );

    /* Compute total number of points in dose image */
    gui->num_points = gui->num_cols * gui->num_rows;
    
    /* Read the number of planes */
    if ( KV_read_string_value_for_key ( infile, "num_planes", value, 256 ) )
    {
        sscanf ( value, "%d", &gui->num_planes );

        /* Make sure that the number of slices match */
        if( gui->num_slices != gui->num_planes )
        {
            char message[256];
            sprintf( message,
                     "The number of planes in your .chd file, %d,\ndoes not match the number of image slices, %d.\n",
                     gui->num_planes, gui->num_slices );
            DT_error( gui->toplevel,  message, "File Error", "OK" );
            return( 0 );
        }
    }
    else
    {
        DT_error( gui->toplevel,
                  "The number of planes could not be found in your\n.chd file.  Cannot continue!",
                  "File Error", "OK" );
        return( 0 );
    }

    /* Read the dose concentrations */
    if ( KV_read_string_value_for_key ( infile, "conc_1", value, 256 ) )
        sscanf ( value, "%e", &gui->BoronConc );
    else printf ( "Could not locate boron concentration\n" );
    
    if ( KV_read_string_value_for_key ( infile, "conc_2", value, 256 ) )
        sscanf ( value, "%e", &gui->GammaConc );
    else printf ( "Could not locate gamma concentration\n" );

    if ( KV_read_string_value_for_key ( infile, "conc_3", value, 256 ) )
        sscanf ( value, "%e", &gui->NitrogenConc );
    else printf ( "Could not locate nitrogen concentration\n" );    

    if ( KV_read_string_value_for_key ( infile, "conc_4", value, 256 ) )
        sscanf ( value, "%e", &gui->FastConc );
    else printf ( "Could not locate fast concentration\n" );

    if ( KV_read_string_value_for_key ( infile, "conc_8", value, 256 ) )
        sscanf ( value, "%e", &gui->OtherConc );
    else printf ( "Could not locate other concentration\n" );
    
    gui->CurConcs[0] = gui->BoronConc;
    gui->CurConcs[1] = gui->GammaConc;
    gui->CurConcs[2] = gui->NitrogenConc;
    gui->CurConcs[3] = gui->FastConc;
    
    /* Read the dose rbe values */
    if ( KV_read_string_value_for_key ( infile, "rbe_val_1", value, 256 ) )
        sscanf ( value, "%e", &gui->BoronFactor );
    else printf ( "Could not locate boron rbe value\n" );
    
    if ( KV_read_string_value_for_key ( infile, "rbe_val_2", value, 256 ) )
        sscanf ( value, "%e", &gui->GammaFactor );
    else printf ( "Could not locate gamma rbe value\n" );

    if ( KV_read_string_value_for_key ( infile, "rbe_val_3", value, 256 ) )
        sscanf ( value, "%e", &gui->NitrogenFactor );
    else printf ( "Could not locate nitrogen rbe value\n" );    

    if ( KV_read_string_value_for_key ( infile, "rbe_val_4", value, 256 ) )
        sscanf ( value, "%e", &gui->FastFactor);
    else printf ( "Could not locate fast rbe value\n" );

    if ( KV_read_string_value_for_key ( infile, "rbe_val_8", value, 256 ) )
        sscanf ( value, "%e", &gui->OtherFactor );
    else printf ( "Could not locate other reference rbe valuen" );    

    gui->CurFactors[0] = gui->BoronFactor;
    gui->CurFactors[1] = gui->GammaFactor;
    gui->CurFactors[2] = gui->NitrogenFactor;
    gui->CurFactors[3] = gui->FastFactor;   

    /* Read the reference dose values */
    if ( KV_read_string_value_for_key ( infile, "ref_dose_0", value, 256 ) )
        sscanf ( value, "%e", &gui->TotalRef );
    else printf ( "Could not locate total reference dose\n" );

    if ( KV_read_string_value_for_key ( infile, "ref_dose_1", value, 256 ) )
        sscanf ( value, "%e", &gui->BoronRef );
    else printf ( "Could not locate boron reference dose\n" );
    
    if ( KV_read_string_value_for_key ( infile, "ref_dose_2", value, 256 ) )
        sscanf ( value, "%e", &gui->GammaRef );
    else printf ( "Could not locate gamma reference dose\n" );

    if ( KV_read_string_value_for_key ( infile, "ref_dose_3", value, 256 ) )
        sscanf ( value, "%e", &gui->NitrogenRef );
    else printf ( "Could not locate nitrogen reference dose\n" );    

    if ( KV_read_string_value_for_key ( infile, "ref_dose_4", value, 256 ) )
        sscanf ( value, "%e", &gui->FastRef );
    else printf ( "Could not locate fast reference dose\n" );

    if ( KV_read_string_value_for_key ( infile, "ref_dose_5", value, 256 ) )
        sscanf ( value, "%e", &gui->FastFluenceRef );
    else printf ( "Could not locate fast fluence reference dose\n" );

    if ( KV_read_string_value_for_key ( infile, "ref_dose_6", value, 256 ) )
        sscanf ( value, "%e", &gui->EpithermalFluenceRef );
    else printf ( "Could not locate epithermal fluence reference dose\n" );

    if ( KV_read_string_value_for_key ( infile, "ref_dose_7", value, 256 ) )
        sscanf ( value, "%e", &gui->ThermalFluenceRef );
    else printf ( "Could not locate thermal fluence reference dose\n" );

    gui->CurRefs[0] = gui->BoronRef;
    gui->CurRefs[1] = gui->GammaRef;
    gui->CurRefs[2] = gui->NitrogenRef;
    gui->CurRefs[3] = gui->FastRef;
    gui->CurRefs[4] = gui->FastFluenceRef;
    gui->CurRefs[5] = gui->EpithermalFluenceRef;
    gui->CurRefs[6] = gui->ThermalFluenceRef;

    if ( first_call )
    {
        gui->z_val = NULL;
        first_call = 0;
    }
    
    if ( gui->z_val )
        MT_free ( ( void * ) gui->z_val );
    
    /* Get memory for z values */
    gui->z_val = ( float * ) MT_malloc ( sizeof ( float ) * gui->num_planes );
    
    /* Read in the z_values */
    for ( i = 0; i < gui->num_planes; i++ )
    {
        /* Create the key for the file */
        sprintf ( z_val_str, "z_value_%02d", i );

        if ( KV_read_string_value_for_key ( infile, z_val_str, value, 256 ) )
        {
            sscanf ( value, "%e", &gui->z_val[i] );
            
        }
        else
        {
            printf ( "Could not locate z_value for slice #%d\n", i );
            continue;
        }
    }

    /* Concatenated Header file read successfully */
    DEBUG_TRACE_OUT printf ( "Leaving read_header_file_values\n" );
    return ( 1 );
}


/*=====================================================================
  Function:    read_contour_file_values

  Purpose:     Read and store values from a concatenated contour file.

  Parameters:  main_gui_t *gui  -  pointer to main program structure
               char *filename   -  contour file name

  Returned:    1 upon success, 0 otherwise.

  Written by:  Matt Cohen 5/17/99
  ====================================================================*/
int read_contour_file_values ( main_gui_t *gui, char *filename )
{
    char *originalFileArray, *fileArray, line[256];
    Contour_grid_t *new_contour;
    Floyd_data_t *data,*original_data;
    int abscissa, ordinate;
    int NUMPOINTS;
    float  totalRef, boronRef, gammaRef, nitrogenRef;
    float  fastRef, group1Ref, group2Ref, thermalFluenceRef;    
    float container[3];
    int i, count, skipInt, arraySize;

    DEBUG_TRACE_IN printf ( "Entering read_contour_file_values\n" );

    DisplayBusyCursor ( gui->mainwindow );
    
    if ( ! ( SZ_UnzipFileIntoArray ( filename, &fileArray, &arraySize ) ) )
    {
        printf ( "Couldn't open %s\n", filename );
        DEBUG_TRACE_OUT printf ( "Leaving read_contour_file_values\n" );
        return ( 0 );
    }

    originalFileArray = fileArray;
    
    for ( i = 0; i < gui->num_planes; i++ )
    {
        /* Skip line " new_v107" */
        /* No need to skip this any longer since Chuck is now
           writting the file without this line - MTC 5/26/99
        */
        /*KV_SZ_readln ( &fileArray, line, 256 );*/
        
        new_contour = allocate_new_contour_grid ( gui );

        new_contour->FOV = gui->FOV;
        new_contour->z_value = gui->z_val[i];
        new_contour->xmin = gui->xmin;
        new_contour->xmax = gui->xmax;
        new_contour->ymin = gui->ymin;
        new_contour->ymax = gui->ymax;
        new_contour->Xcolumn= gui->x_in_field;
        new_contour->Ycolumn = gui->y_in_field;
        new_contour->ncols = gui->num_cols;
        new_contour->nrows = gui->num_rows;

        abscissa = new_contour->Xcolumn - 1;
        ordinate = new_contour->Ycolumn - 1;
        
        NUMPOINTS = new_contour->ncols * new_contour->nrows;
        new_contour->grid = (Floyd_data_t *)MT_malloc(sizeof(Floyd_data_t) * NUMPOINTS);
        original_data = (Floyd_data_t *)MT_malloc(sizeof(Floyd_data_t) * NUMPOINTS);
        data = new_contour->grid;

        totalRef          = 100.0 / gui->TotalRef;
        boronRef          = 100.0 / gui->BoronRef;
        gammaRef          = 100.0 / gui->GammaRef;
        nitrogenRef       = 100.0 / gui->NitrogenRef;
        fastRef           = 100.0 / gui->FastRef;
        group1Ref         = 100.0 / gui->FastFluenceRef;
        group2Ref         = 100.0 / gui->EpithermalFluenceRef;
        thermalFluenceRef = 100.0 / gui->ThermalFluenceRef;

        for ( count = 0; count < NUMPOINTS; count ++)
        {
            KV_SZ_readln ( &fileArray, line, 256 );
            
            sscanf ( line, "%f %f %f %d %f %f %f %f %f %f %f %f %f",
                     &container[0], &container[1], &container[2],
                     &skipInt,
                     &(original_data[count].totalDose), 
                     &(original_data[count].boronDose),
                     &(original_data[count].gammaDose),
                     &(original_data[count].nitrogenDose), 
                     &(original_data[count].fastDose),
                     &(original_data[count].group1Fluence), 
                     &(original_data[count].group2Fluence),
                     &(original_data[count].thermalFluence), 
                     &(original_data[count].otherDose));
     
            data[count].x = container[abscissa];
            data[count].y = container[ordinate];
            data[count].totalDose      = original_data[count].totalDose * totalRef;
            data[count].boronDose      = original_data[count].boronDose * boronRef;
            data[count].gammaDose      = original_data[count].gammaDose * gammaRef;
            data[count].nitrogenDose   = original_data[count].nitrogenDose*nitrogenRef;
            data[count].fastDose       = original_data[count].fastDose * fastRef;
            data[count].group1Fluence  = original_data[count].group1Fluence*group1Ref;
            data[count].group2Fluence  = original_data[count].group2Fluence*group2Ref;
            data[count].thermalFluence = original_data[count].thermalFluence * thermalFluenceRef;
        }     

        MT_free ( (void *) original_data );

        new_contour->z_value = new_contour->z_value *10.0;

        DEBUG_LOADING printf ( "Loading slice %d at %f\n", i, new_contour->z_value );
        
        if ( new_contour == gui->Grids.next )
        {
            gui->contour_shift = convert_slice_z_to_world_y ( gui, new_contour->z_value );
        }    
    }

    SZ_FreeArray ( &originalFileArray );
    
    gui->contour_grids_loaded = 1;

    draw_all ( gui );
  
    RemoveBusyCursor ( gui->mainwindow );
  
    DEBUG_TRACE_OUT printf ( "Leaving read_contour_file_values\n" );
    return ( 1 );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : LoadSingle_Contour_FileCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Load_Single_Contour_FileCB(Widget w, XtPointer clientdata,	XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmFileSelectionBoxCallbackStruct *cbs =  (XmFileSelectionBoxCallbackStruct *) calldata;
  char fileName[256],*temp;
  char line[256],string[35];
  int i = 0,valid_file = 1,count;
  int skipped_lines,skipInt;
  float container[3];
  FILE *in_file;
  Contour_grid_t *new_contour;
  Floyd_data_t *data,*original_data;
  int abscissa, ordinate;
  float  totalRef, boronRef, gammaRef, nitrogenRef;
  float  fastRef, group1Ref, group2Ref, thermalFluenceRef;
  int NUMPOINTS;
  int versionFlag;
  int LINE_SIZE = 256;
  int is_direct = 0;
  
  DEBUG_TRACE_IN printf("Entered Load_Single_Contour_FileCB\n");


  for(i=0;i<gui->file_menu.load_single_contour.num_submenus;i++){
    if (w == gui->file_menu.load_single_contour.submenu[i+1]){
      is_direct = 1; break;
    }
  }

  DisplayBusyCursor(gui->mainwindow);

  /*************************************************/
  /** Get the string (filename)
  /*************************************************/
  if (is_direct)
    strcpy(fileName,GetTextOfLabel(w));
  else DT_select_file(gui->toplevel,gui->app,&fileName[0],NULL);

  if ( ! strstr ( fileName, ",contour" ) )
  {
      DT_error(gui->toplevel,"File does not appear to be a single contour file.",NULL,NULL); return;      
  }
  else if (!(in_file = fopen(fileName,"r"))){
      DT_error(gui->toplevel,"Couldn't open the file\n",NULL,NULL); return;
  }
    
    fill_recent_file(gui,fileName,4);

    new_contour = allocate_new_contour_grid(gui);

    fgets(line, 256, in_file); /* skip past new */
    versionFlag = 0;
    if( strstr(line, " new_v107") ) {
      printf(" ---- Version 1.07 Contour Data ----\n");
      versionFlag = 107;
    }

    /*if (!(strstr(line,"new"))){
      break;
      }*/

    fscanf(in_file,"%e %e %e %e %e %e %d %d %d %d %d", 
	   &new_contour->FOV, &new_contour->z_value,
	   &new_contour->xmin, &new_contour->xmax, 
	   &new_contour->ymin, &new_contour->ymax, 
	   &new_contour->Xcolumn, &new_contour->Ycolumn,
	   &new_contour->ncols, &new_contour->nrows, 
	   &skipped_lines);
    abscissa = new_contour->Xcolumn - 1;
    ordinate = new_contour->Ycolumn - 1;

    NUMPOINTS = new_contour->ncols * new_contour->nrows;
    /*printf("there are %d points in the contour\n",NUMPOINTS);*/
    new_contour->grid = (Floyd_data_t *)MT_malloc(sizeof(Floyd_data_t) * NUMPOINTS);
    original_data = (Floyd_data_t *)MT_malloc(sizeof(Floyd_data_t) * NUMPOINTS);
    data = new_contour->grid;

    /** skip the header  **/
   for (i=0; i<skipped_lines + 1; i++)
     fgets(line, LINE_SIZE, in_file);



  /* read the concentrations */
   if(versionFlag == 107)
     fscanf(in_file,"%35c %e %e %e %e %e %e %e %e %e", string,
	    &container[0], &gui->BoronConc, &gui->GammaConc, &gui->NitrogenConc,
	    &gui->FastConc, &container[0], &container[0], &container[0], &gui->OtherConc);
   else
     fscanf(in_file,"%35c %e %e %e %e %e %e %e %e", string,
	    &container[0], &gui->BoronConc, &gui->GammaConc, &gui->NitrogenConc,
	    &gui->FastConc, &container[0], &container[0], &container[0]);
   /*
     printf("here are the concentrations\n");
     printf("%35c %e %e %e %e %e %e %e %e %e\n", string,
     container[0], gui->BoronConc, gui->GammaConc, gui->NitrogenConc,
     gui->FastConc, container[0], container[0], container[0], gui->OtherConc);
   */
   gui->CurConcs[0] = gui->BoronConc;
   gui->CurConcs[1] = gui->GammaConc;
   gui->CurConcs[2] = gui->NitrogenConc;
   gui->CurConcs[3] = gui->FastConc;



   /* read the dose factors */
   if(versionFlag == 107)
     fscanf(in_file,"%35c %e %e %e %e %e %e %e %e %e", string,
	    &container[0], &gui->BoronFactor, &gui->GammaFactor, &gui->NitrogenFactor,
	    &gui->FastFactor, &container[0], &container[0], &container[0], &gui->OtherFactor);
   else
     fscanf(in_file,"%35c %e %e %e %e %e %e %e %e", string,
	    &container[0], &gui->BoronFactor, &gui->GammaFactor, &gui->NitrogenFactor,
	    &gui->FastFactor, &container[0], &container[0], &container[0]);
   /*
     printf("here are the dose factors\n");
     printf("%35c %e %e %e %e %e %e %e %e %e\n", string,
     container[0], gui->BoronFactor, gui->GammaFactor, gui->NitrogenFactor,
     gui->FastFactor, container[0], container[0], container[0], gui->OtherFactor);
   */
   gui->CurFactors[0] = gui->BoronFactor;
   gui->CurFactors[1] = gui->GammaFactor;
   gui->CurFactors[2] = gui->NitrogenFactor;
   gui->CurFactors[3] = gui->FastFactor;


 /* read the ref values at thermal peak */
   if(versionFlag == 107)
     fscanf(in_file,"%35c %e %e %e %e %e %e %e %e %e", string,
	    &totalRef, &gui->BoronRef, &gui->GammaRef, &gui->NitrogenRef,
	    &gui->FastRef, &group1Ref, &group2Ref, &thermalFluenceRef, &gui->OtherRef);
   else
     fscanf(in_file,"%35c %e %e %e %e %e %e %e %e", string,
	    &totalRef, &gui->BoronRef, &gui->GammaRef, &gui->NitrogenRef,
	    &gui->FastRef, &group1Ref, &group2Ref, &thermalFluenceRef);
   /*
     printf("here are the ref values at thermal peak\n");
     printf("%s %e %e %e %e %e %e %e %e %e\n", string,
     totalRef, gui->BoronRef, gui->GammaRef, gui->NitrogenRef,
     gui->FastRef, group1Ref, group2Ref, thermalFluenceRef, gui->OtherRef);
   */


   gui->CurRefs[0] = gui->BoronRef;
   gui->CurRefs[1] = gui->GammaRef;
   gui->CurRefs[2] = gui->NitrogenRef;
   gui->CurRefs[3] = gui->FastRef;
   gui->CurRefs[4] = group1Ref;
   gui->CurRefs[5] = group2Ref;
   gui->CurRefs[6] = thermalFluenceRef;

   totalRef          = 100.0 / totalRef;
   boronRef          = 100.0 / gui->BoronRef;
   gammaRef          = 100.0 / gui->GammaRef;
   nitrogenRef       = 100.0 / gui->NitrogenRef;
   fastRef           = 100.0 / gui->FastRef;
   group1Ref         = 100.0 / group1Ref;
   group2Ref         = 100.0 / group2Ref;
   thermalFluenceRef = 100.0 / thermalFluenceRef;

   for( count = 0; count < NUMPOINTS; count ++){
    if(versionFlag == 107){
       fscanf(in_file,"%f %f %f %d %f %f %f %f %f %f %f %f %f",
	      &container[0], &container[1], &container[2],
	      &skipInt,
	      &(original_data[count].totalDose), 
	      &(original_data[count].boronDose),
	      &(original_data[count].gammaDose),
	      &(original_data[count].nitrogenDose), 
	      &(original_data[count].fastDose),
	      &(original_data[count].group1Fluence), 
	      &(original_data[count].group2Fluence),
	      &(original_data[count].thermalFluence), 
	      &(original_data[count].otherDose));
     }else{
       fscanf(in_file,"%f %f %f %d %f %f %f %f %f %f %f %f",
	      &container[0], &container[1], &container[2],
	      &skipInt,
	      &(original_data[count].totalDose), 
	      &(original_data[count].boronDose),
	      &(original_data[count].gammaDose),
	      &(original_data[count].nitrogenDose), 
	      &(original_data[count].fastDose),
	      &(original_data[count].group1Fluence), 
	      &(original_data[count].group2Fluence),
	      &(original_data[count].thermalFluence));
     }
     
     data[count].x = container[abscissa];
     data[count].y = container[ordinate];
     data[count].totalDose      = original_data[count].totalDose * totalRef;
     data[count].boronDose      = original_data[count].boronDose * boronRef;
     data[count].gammaDose      = original_data[count].gammaDose * gammaRef;
     data[count].nitrogenDose   = original_data[count].nitrogenDose*nitrogenRef;
     data[count].fastDose       = original_data[count].fastDose * fastRef;
     data[count].group1Fluence  = original_data[count].group1Fluence*group1Ref;
     data[count].group2Fluence  = original_data[count].group2Fluence*group2Ref;
     data[count].thermalFluence = original_data[count].thermalFluence * thermalFluenceRef;
   }     

   fclose(in_file);
   
   /*
     printf("here are the grid values:\n");   
     for (count=0;count<NUMPOINTS;count++){
     
     printf("%f %f %f %d %f %f %f %f %f %f %f %f \n",
     container[0], container[1], container[2],
     skipInt,
     data[count].totalDose, 
     data[count].boronDose,
     data[count].gammaDose,
     data[count].nitrogenDose, 
     data[count].fastDose,
     data[count].group1Fluence, 
     data[count].group2Fluence,
     data[count].thermalFluence);
     }
     printf("Z_value is : %f\n",new_contour->z_value);
   */

   /*new_contour->z_value = convert_normalized_z_to_z(new_contour->FOV,new_contour->z_value);*/
   new_contour->z_value = new_contour->z_value*10.0;

   printf("Z_value converted is : %f\n",new_contour->z_value);

   MT_free( (void *) original_data );

   gui->contour_grids_loaded = 1;

   /* Set the Colorwash With Doses button, and call its callback */
   XtSetSensitive( gui->contour_panel.dose_display_toggle, True );
   XmToggleButtonSetState( gui->contour_panel.dose_display_toggle, True, True );

   RemoveBusyCursor(gui->mainwindow);

   DEBUG_TRACE_OUT printf("Done with Load_Single_Contour_FileCB\n");
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
Contour_grid_t *allocate_new_contour_grid(main_gui_t *gui)
{
  Contour_grid_t *temp;
  
  DEBUG_TRACE_IN printf("Entered allocate_new_contour_grid\n");
  
  if (gui->Grids.next != NULL){
    /*printf("Grids.next != NULL\n");*/
    temp = gui->Grids.next;
    /*printf("calling the while\n");*/
    while(temp->next != NULL) temp = temp->next;
    /*printf("past the while\n"); */
   temp->next = (Contour_grid_t *)MT_malloc(sizeof(Contour_grid_t));
   /*printf("past the malloc\n");*/
    temp = temp->next;
    temp->next = NULL; 
 }else{
   /*printf("entered the else\n");*/
    gui->Grids.next = (Contour_grid_t *)MT_malloc(sizeof(Contour_grid_t));
    temp = gui->Grids.next;
    temp->next = NULL;
  }
    
  DEBUG_TRACE_OUT printf("Done with allocate_new_contour_grid\n");
  return temp;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: get_rgb_from_contour_data_for_point
%%
%%  Written by: Cory Albright
%%
%%  Parameters:(cb)
%%
%%  Purpose: 
%%         
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_rgb_from_contour_data_for_point(main_gui_t *gui, int j, int k, int l, float *r, float *g, float *b)
{
  Contour_grid_t *grid;
  int found_the_contour_grid_for_slice = 0;
  float mapped_value;

  grid = gui->Grids.next;
  while(grid != NULL){
    if (convert_z_to_slice(gui,grid->z_value) == j){
      /** we found a matching contour grid for the slice **/
      found_the_contour_grid_for_slice = 1;
      break;
    }
    grid = grid->next;
  }
  
  if (!found_the_contour_grid_for_slice) return 0;
  
  mapped_value = get_mapped_value_from_contour_grid(grid,k,l,256,256,gui->current_dose_component);

  *r = (float)(gui->Contour_Map[(int)mapped_value].r)/255.0;
  *g = (float)(gui->Contour_Map[(int)mapped_value].g)/255.0;
  *b = (float)(gui->Contour_Map[(int)mapped_value].b)/255.0;

  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: get_mapped_value_from_contour_grid
%%
%%  Written by: Cory Albright
%%
%%  Parameters:(cb)
%%
%%  Purpose: maps the x,y mouse position intot the dose grid
%%           does a double linear interpolation and returns the dose value
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
float get_mapped_value_from_contour_grid(Contour_grid_t *grid, int orig_x, int orig_y,  
					 int image_size_x, int image_size_y,
					 int doseFlag)
{
  double value;
  double x,y;
  int x1,x2;
  int y1,y2;
  int i1,i2,i3,i4;
  float dose1,dose2,dose3,dose4;
  double x_avg1,x_avg2,x_avg_factor;
 
  
  y =  ( ((double)orig_x/(double)image_size_x) * (double)(grid->ncols-1) );
  x = /*(double)(grid->nrows-1) -*/ ( ((double)orig_y/(double)image_size_y) * (double)(grid->nrows-1) );
  
  x1 = (int)x;      
  y1 = (int)(y);
  x2 = (int)(x+1.0);  
  y2 = (int)(y+1.0);
  i1 = y1 * grid->ncols + x1;
  i2 = y1 * grid->ncols + x2;
  i3 = y2 * grid->ncols + x1;
  i4 = y2 * grid->ncols + x2;


  switch(doseFlag){
  case 0:
    /*printf("looking in the boronDose grid\n");*/
    dose1 = grid->grid[i1].boronDose;
    dose2 = grid->grid[i2].boronDose;
    dose3 = grid->grid[i3].boronDose;
    dose4 = grid->grid[i4].boronDose;
    break;
  case 1:
    /*printf("looking in the gammaDose grid\n");*/
    dose1 = grid->grid[i1].gammaDose;
    dose2 = grid->grid[i2].gammaDose;
    dose3 = grid->grid[i3].gammaDose;
    dose4 = grid->grid[i4].gammaDose;
    break;
  case 2:
    /*printf("looking in the nitrogenDose grid\n");*/
    dose1 = grid->grid[i1].nitrogenDose;
    dose2 = grid->grid[i2].nitrogenDose;
    dose3 = grid->grid[i3].nitrogenDose;
    dose4 = grid->grid[i4].nitrogenDose;
    break;
  case 3:
    /*printf("looking in the fastDose grid\n");*/
    dose1 = grid->grid[i1].fastDose;
    dose2 = grid->grid[i2].fastDose;
    dose3 = grid->grid[i3].fastDose;
    dose4 = grid->grid[i4].fastDose;
    break;
  case 4:
    dose1 = grid->grid[i1].group1Fluence;
    dose2 = grid->grid[i2].group1Fluence;
    dose3 = grid->grid[i3].group1Fluence;
    dose4 = grid->grid[i4].group1Fluence;
    break;
  case 5:
    dose1 = grid->grid[i1].group2Fluence;
    dose2 = grid->grid[i2].group2Fluence;
    dose3 = grid->grid[i3].group2Fluence;
    dose4 = grid->grid[i4].group2Fluence;
    break;
  case 6:
    dose1 = grid->grid[i1].thermalFluence;
    dose2 = grid->grid[i2].thermalFluence;
    dose3 = grid->grid[i3].thermalFluence;
    dose4 = grid->grid[i4].thermalFluence;
    break;
  case 7:
    dose1 = grid->grid[i1].otherDose;
    dose2 = grid->grid[i2].otherDose;
    dose3 = grid->grid[i3].otherDose;
    dose4 = grid->grid [i4].otherDose;
    break;
  case 8:
    dose1 = grid->grid[i1].totalDose;
    dose2 = grid->grid[i2].totalDose;
    dose3 = grid->grid[i3].totalDose;
    dose4 = grid->grid[i4].totalDose;
    break;
  }

#ifdef NOT_DEFINED
  x_avg_factor = (x-x1)/(x2-x1);
  x_avg1 = x_avg_factor * (dose2-dose1) + dose1;
  x_avg2 = x_avg_factor * (dose3-dose4) + dose4;
  
  /*
    printf("\n\n\n\nthe dose values i'm using are : \n");
    printf("\t%.2f\t\t%.2f\n\n",dose1,dose2);
    printf("\n\n");
    printf("\t%.2f\t\t%.2f\n\n",dose4,dose3);
  */

  /*** going to look in dose_data grid to find the grid ****/
  if (x > (double)(grid->ncols-1)){


    if (y > (double)(grid->nrows-1)){
      /** tough case, for now just take the corner pixel **/
      /*printf("taking the corner pixel!\n");*/
      value = grid->grid[grid->ncols * grid->nrows -1].totalDose;
    }else{
      /** right side, take average of pixel above and below **/
      value = ((y-y1)/(y4-y1)) * (dose4 - dose1) + dose1;
      /*printf("on the right, mapping from above and below\n");*/
    }


  } else if (y > (double)(grid->ncols-1)){
      /** bottom side, take average of pixel above and below **/
      value = x_avg1;
      /*printf("on the bottom , mapping from left and right\n");*/


  }else{
    value = ((y-y1)/(y4-y1)) * (x_avg2-x_avg1) + x_avg1;
  } 
#endif

    value = (dose1*(x2-x)+dose2*(x-x1))*(y2-y)+(dose3*(x2-x)+dose4*(x-x1))*(y-y1);


  return value;
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
void fill_forms_with_contour_names(main_gui_t *gui){
  int i;
  XColor color;

  for(i=0;i<gui->num_contour_surfaces;i++){
    gui->color_panel.bodies[i+MAX_BODS] = 
      XtCreateManagedWidget(gui->bod[i+MAX_BODS].name,xmPushButtonWidgetClass,
			    gui->color_panel.body_pane, NULL, 0);
    
    gui->transparency_panel.transparencies[i+MAX_BODS] = 
      XtCreateManagedWidget(gui->bod[i+MAX_BODS].name,
			    xmPushButtonWidgetClass,
			    gui->transparency_panel.transparency_pane, NULL, 0);
    
    XtAddCallback(gui->color_panel.bodies[i+MAX_BODS], XmNactivateCallback,
		  Selected_body_changedCB, (XtPointer)gui);
    
    XtAddCallback(gui->transparency_panel.transparencies[i+MAX_BODS], XmNactivateCallback,
		  Selected_body_changedCB, (XtPointer)gui);
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
void remove_contour_names_from_forms(main_gui_t *gui){
  int i;
  XColor color;

  for(i=0;i<gui->num_contour_surfaces;i++){
    XtDestroyWidget(gui->color_panel.bodies[i+MAX_BODS]);
    XtDestroyWidget(gui->transparency_panel.transparencies[i+MAX_BODS]);
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
void add_contours_to_bodylist(main_gui_t *gui)
{
  int i;
  for (i=0;i<gui->num_contour_surfaces;i++){
    XmListAddItem(gui->bodylist, XmStringCreateLocalized(gui->bod[i+MAX_BODS].name), gui->num_bodies+1+i);
    XmListSelectItem(gui->bodylist, XmStringCreateLocalized(gui->bod[MAX_BODS+i].name),FALSE);
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
void remove_contours_from_bodylist(main_gui_t *gui)
{
  int i;
  for (i=0;i<gui->num_contour_surfaces;i++){
    XmListDeleteItem(gui->bodylist, XmStringCreateLocalized(gui->bod[i+MAX_BODS].name));
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
void add_contours_to_clippinglist(main_gui_t *gui)
{
  int i;

  
  for (i=0;i<gui->num_contour_surfaces;i++){
    gui->clipping_panel.clip_bods[i+gui->num_bodies] = XtVaCreateManagedWidget(gui->bod[i+MAX_BODS].name,
									     xmToggleButtonWidgetClass,
									     gui->clipping_panel.s_win_form,
									     XmNtopAttachment, XmATTACH_WIDGET,
									     XmNtopWidget, gui->clipping_panel.clip_bods[(i+gui->num_bodies)-1],
									     XmNleftAttachment, XmATTACH_FORM,
									     XmNtopOffset, 5,
									     XmNleftAttachment, 5,
									     XmNset, TRUE,
									     NULL);
    
    XtAddCallback(gui->clipping_panel.clip_bods[i+gui->num_bodies], XmNvalueChangedCallback, 
		  (XtCallbackProc)Body_Clipping_ChangedCB, (XtPointer)gui);
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
void remove_contours_from_clippinglist(main_gui_t *gui)
{
  int i;

  
  for (i=0;i<gui->num_contour_surfaces;i++){
    XtRemoveCallback(gui->clipping_panel.clip_bods[i+gui->num_bodies], XmNvalueChangedCallback,
		     (XtCallbackProc)Body_Clipping_ChangedCB, (XtPointer)gui);
    XtDestroyWidget(gui->clipping_panel.clip_bods[i+gui->num_bodies]);
  }
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     freeContourGrids()
%%%
%%%  Purpose:      Free the linked list of contour grids.
%%%
%%%  Parameters:   grid -> A Contour_grid_t *.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:        Called in CloseCallback in glmf.c
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void freeContourGrids( Contour_grid_t * currentGrid )
{
    DEBUG_TRACE_IN printf("Entering freeContourGrids\n");

    if( currentGrid != NULL )
    {
        /* Recurse on next node in list */ 
        freeContourGrids( currentGrid->next );

        /* Free the memory for this node */ 
        MT_free( (void *) currentGrid->grid );
        MT_free( (void *) currentGrid );
    }

    DEBUG_TRACE_OUT printf("Leaving freeContourGrids\n");
}
