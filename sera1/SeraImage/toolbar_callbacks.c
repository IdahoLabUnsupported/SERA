#include "toqsh.h"


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
void Add_Blank_ImageCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  unsigned char *new_images,*old_images,*ptr;
  int size_of_image,i,num_original_images;
  int * which_are_marked = NULL;
  float * temp_ils = NULL;
  int spacing_valid;
  float spacing;
  int num_images;
  
  DEBUG_TRACE_IN printf("Entered Add_Blank_ImageCB\n");

  if( gui->images_loaded )
  {
    DisplayBusyCursor(gui->mainwindow);
  
    size_of_image = gui->qsh_gui->qsh_info->size_of_dimension[1] * gui->qsh_gui->qsh_info->size_of_dimension[2];
    old_images = gui->qsh_gui->qsh_info->images;
    num_original_images = gui->qsh_gui->qsh_info->size_of_dimension[0];

    new_images = (unsigned char *)MT_malloc((num_original_images+1)*size_of_image);

    ptr = new_images;
    memset(ptr,0,size_of_image);
    ptr += size_of_image;

    /* create a list to keep track of which images are marked for delete */
    which_are_marked = (int *) MT_calloc( num_original_images+1, sizeof(int) );

    /* create a list to keep track of the original image locations */
    temp_ils = (float *) MT_calloc( num_original_images+1, sizeof( float ) );

    for (i=0;i<num_original_images;i++)
    {
      which_are_marked[i+1] = gui->image_block.image[i].marked_for_delete;
      temp_ils[i+1] = gui->qsh_gui->qsh_info->image_location[i];
      memcpy(ptr,&old_images[i * size_of_image],size_of_image);
      ptr += size_of_image;
    }

    MT_free((void *)old_images);
    gui->qsh_gui->qsh_info->images = new_images;
    gui->qsh_gui->qsh_info->size_of_dimension[0] += 1;

    /*
     * Find the new z-value for the first image
     */
    num_images = gui->qsh_gui->qsh_info->size_of_dimension[0];
    spacing_valid = verify_ils_internally( gui->qsh_gui->qsh_info->image_location,
                                           num_images - 1,
                                           &spacing, 1.00 );
    if( spacing_valid == -1 )
        temp_ils[0] = gui->qsh_gui->qsh_info->image_location[0] - spacing;
    else
        temp_ils[0] = 0.0;
    
    /*
     * Now copy over the new set of image locations to the qsh structure.
     */
    for( i = 0; i < num_images; i++ )
        gui->qsh_gui->qsh_info->image_location[i] = temp_ils[i];
    
    destroy_and_rebuild_images(gui);

    /*
     * Now go through and mark those images that were before
     */
    for( i = 0; i < num_original_images+1; i++ )
    {
      if( which_are_marked[i] == 1 )
      {
	gui->image_block.image[i].marked_for_delete = 1;
	draw_x_on_image( gui, &gui->image_block.image[i] );
      }
    }
	
    adjust_manip_window_to_program_changes ( gui );
    gui->move_images_button.images_were_added = 1;

    MT_free( (void *) which_are_marked );
    which_are_marked = NULL;
    MT_free( (void *) temp_ils );
    temp_ils = NULL;

    RemoveBusyCursor(gui->mainwindow);
  }
  else
  {
    DT_error( w, "Sorry, no images are currently loaded", NULL, NULL );
  }

  DEBUG_TRACE_OUT printf("Done with Add_Blank_ImageCB\n");
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
void Remove_Marked_ImagesCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  int i;
  int num_original_images;
  int size_of_image;
  int num_images_left = 0;
  unsigned char *new_images,*ptr;
  unsigned char *old_images;
  float temp_image_location[MAX_QSH_SLICES];
  int il_count = 0;
  int found = 0;

  DEBUG_TRACE_IN   printf("Entered Remove_Marked_ImagesCB\n");

  if (!gui->images_loaded){
    DEBUG_TRACE_OUT   printf("Done with Remove_Marked_ImagesCB\n");    
    return;
  }

  /*
   * Before doing anything, check to see if there are
   * any images actually marked for delete. If not just
   * return without any further processing.
   */
  i = 0;
  while( i < gui->image_block.num_images && !found )
  {
      if( gui->image_block.image[i].marked_for_delete ) found = 1;
      else i++;
  }
  
  if( !found )
  {
      DEBUG_TRACE_OUT printf("Leaving Remove_Marked_Image, no images were marked\n");
      return;
  }
  
  
  if ( !DT_decide(gui->toplevel,gui->app,"Are you sure you want to remove all marked images?",NULL,NULL,NULL))
  {
    DEBUG_TRACE_OUT  printf("Done with Remove_Marked_ImagesCB\n");    
    return;
  }

  if ( XtIsManaged ( gui->manip_gui.shell ) && gui->manip_gui.undo_list )
  {
      if ( !DT_decide ( gui->toplevel,gui->app, "You will not be able to undo your changes. Continue anyway?", NULL, NULL, NULL ) )
      {
	  DEBUG_TRACE_OUT  printf("Done with Remove_Marked_ImagesCB\n");    
	  return;
      }
  }

  DisplayBusyCursor(gui->mainwindow);
  num_original_images = gui->qsh_gui->qsh_info->size_of_dimension[0];
  size_of_image = gui->qsh_gui->qsh_info->size_of_dimension[1] * gui->qsh_gui->qsh_info->size_of_dimension[2];
  old_images = gui->qsh_gui->qsh_info->images;

  for (i=0;i<num_original_images;i++){
    if (!gui->image_block.image[i].marked_for_delete)
      num_images_left++;
  }

  /*printf("\nnum_original images = %d, num_images_left = %d\n",num_original_images,num_images_left);*/

  /*
   * Unload all the images if they are all marked for delete 
   */
  if( num_images_left == 0 ) {
    Unload_ImagesCB( w, (XtPointer) gui, NULL );
    DEBUG_TRACE_OUT   printf("Done with Remove_Marked_ImagesCB\n");    
    return;
  }

  /*
   * None marked for delete, just return
   */
  if (num_images_left == num_original_images){
    DEBUG_TRACE_OUT   printf("Done with Remove_Marked_ImagesCB\n");    
    return;
  }

  new_images = (unsigned char *)MT_malloc(num_images_left * size_of_image);

  ptr = new_images;
  for (i=0;i<num_original_images;i++){
    if (!gui->image_block.image[i].marked_for_delete){

      /* first copy over the image */
      memcpy(ptr,&old_images[i * size_of_image],size_of_image);
      ptr += size_of_image;

      /* then copy over the image location key */
      temp_image_location[il_count] = gui->qsh_gui->qsh_info->image_location[i];
      il_count++;
    }    
  }

  /* make sure the images left are not marked for delete */
  for (i=0;i<num_images_left;i++)
    gui->image_block.image[i].marked_for_delete = 0;

  
  /* copy over the image location keys left back into the qsh_structure */
  for (i=0;i<num_images_left;i++){
    gui->qsh_gui->qsh_info->image_location[i] = temp_image_location[i];
    gui->qsh_gui->qsh_info->valid.image_location[i] = 1;
  }
  for (i=num_images_left; i<MAX_QSH_SLICES; i++)
    gui->qsh_gui->qsh_info->valid.image_location[i] = 0;

  MT_free((void *)old_images);
  gui->qsh_gui->qsh_info->images = new_images;
  gui->qsh_gui->qsh_info->size_of_dimension[0] = num_images_left;
  gui->image_block.num_images = num_images_left;

  destroy_and_rebuild_images(gui);
  adjust_manip_window_to_program_changes ( gui );
  gui->move_images_button.images_were_removed = 1;

  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT   printf("Done with Remove_Marked_ImagesCB\n");
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
void Color_Normalization_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;

  DEBUG_TRACE_IN printf("Entered Color_Normalization_ChangedCB\n");

  if (!gui->images_loaded){
    DEBUG_TRACE_IN printf("Done with Color_Normalization_ChangedCB\n");
    return;
  }

  DisplayBusyCursor(gui->mainwindow);
  if (w == gui->color_norm[0])      gui->color_normalization = COLOR_OPT_NONE;
  else if (w == gui->color_norm[1]) gui->color_normalization = COLOR_OPT_LOCAL;
  else if (w == gui->color_norm[2]) gui->color_normalization = COLOR_OPT_GLOBAL;
  
  destroy_and_rebuild_images(gui);
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_IN printf("Done with Color_Normalization_ChangedCB\n");
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
void color_normalize_images(main_gui_t *gui)
{
    int max, min;
    float scale;
    int i,j,k;
    int temp_count;
    unsigned char val;
    int num_images;
    int image_size;
    
    if (!gui->images_loaded) return;

    num_images = gui->image_block.num_images;
    image_size = gui->image_block.width * gui->image_block.height;
    
    switch (gui->color_normalization){
        case COLOR_OPT_NONE: return;
        case COLOR_OPT_LOCAL:
            for(i = 0; i < num_images; i++){
                max = -5;
                min = 500;
                for (j = 0; j < image_size; j++){
                    val = gui->image_block.image[i].ximage->data[j];
                    if ((int)val > max) max = (int)val;
                    else if ((int)val < min) min = (int)val;
                }

                scale = (float)(MAX_GRAY - MIN_GRAY + 1)/(float)(max-min+1);
                /*printf("Image : %d,   min is : %d, max is : %d\n",i,min,max);
                  printf("         MIN_GRAY: %d, MAX_GRAY: %d, scale : %f\n",MIN_GRAY,MAX_GRAY,scale);*/
                for (j = 0; j < image_size; j++){
                    val = gui->image_block.image[i].ximage->data[j];
                    gui->image_block.image[i].ximage->data[j] =
                        (unsigned char)((float)(val-min)*scale) + (unsigned char)(MIN_GRAY);
                }
            }
            break;

            /* temporary check to make sure that they are all centered from MIN_GRAY to MAX_GRAY */
            /*
              for(i=0;i<gui->image_block.num_images;i++){
              temp_count = 0;
              max = -5;
              min = 500;
              for (j=0;j<gui->image_block.width*gui->image_block.height;j++){
              val = gui->image_block.image[i].ximage->data[j];
              if ((int)val > max) max = (int)val;
              else if ((int)val < min) min = (int)val;
              }
 
              for (j=0;j<gui->image_block.width*gui->image_block.height;j++){
              val = gui->image_block.image[i].ximage->data[j];
              if (val == min) temp_count++;
              }

              printf("Image %d, min now : %d, max : %d, with %d pixels colored with the min\n",i,min,max,temp_count);
      
              if (max != MAX_GRAY) printf("THE MAX DIDN't get filled in in image : %d, max now of :%d\n",i,max);
              if (min != MIN_GRAY) printf("THE MIN DIDN't get filled in in image : %d, min now of :%d\n",i,min);    
              }
            */


        case COLOR_OPT_GLOBAL:
            max = -5;
            min = 500;
            for(i = 0; i < num_images; i++){
                for (j = 0; j < image_size; j++){
                    val = gui->image_block.image[i].ximage->data[j];
                    if ((int)val > max) max = (int)val;
                    else if ((int)val < min) min = (int)val;
                }
            }

            scale = (float)(MAX_GRAY - MIN_GRAY + 1)/(float)(max-min+1);
            for(i = 0; i < num_images; i++){
                for (j = 0; j < image_size; j++){
                    val = gui->image_block.image[i].ximage->data[j];
                    gui->image_block.image[i].ximage->data[j] =
                        (unsigned char)((float)(val-min)*scale) + (unsigned char)(MIN_GRAY);
                }
            }
            break;
    }
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
/*void Image_Remove_ToggledCB (Widget w,XtPointer clientData, XtPointer calldata)
{*/
void image_remove_toggled ( main_gui_t *gui, image_t *the_image )
{
  /*static XExposeEvent *event;*/
  /*static int i;*/
  /*main_gui_t *gui = (main_gui_t *)clientData;*/
  /*image_t *the_image = NULL;*/
  /*image_t *the_image = (image_t *)clientData;*/
  /*XmDrawingAreaCallbackStruct *cbs = (XmDrawingAreaCallbackStruct *)calldata;*/

  DEBUG_TRACE_IN  printf("Entered image_remove_toggled for image\n");

  /*for (i=0;i<gui->image_block.num_images;i++){
    if (w == gui->image_block.image[i].drawing_area){
      the_image = &gui->image_block.image[i]; break;
    }
  }
  if (the_image == NULL){
    printf("didn't get the image in Image_Remove_ToggledCB\n");return;
  }*/

  /*&the_image = gui->image_block.image[image_num];*/

  if (the_image->marked_for_delete){
    XtVaSetValues(the_image->frame,
		  XmNshadowType, XmSHADOW_OUT,
		  XmNshadowThickness, 2,
		  XmNbackground, gui->hl.pixel,
		  NULL);
    the_image->marked_for_delete = 0;
    /*printf("\n\ncalling myPutImage with the real image\n");*/
    myPutImage(gui, 
	       XtWindow(the_image->drawing_area), 
	       gui->gc, the_image->ximage, 
	       0, 0, 0,0,
	       gui->image_block.width,gui->image_block.height);
  }else{
    XtVaSetValues(the_image->frame,
		  XmNshadowType, XmSHADOW_IN,
		  XmNshadowThickness, 2,
		  XmNbackground, gui->hl.pixel,
		  NULL);
    the_image->marked_for_delete = 1;
    /*printf("\n\ncalling myPutImage with the bad image\n");*/
    myPutImage(gui, 
	       XtWindow(the_image->drawing_area), 
	       gui->gc, the_image->ximage, 
	       0, 0, 0,0,
	       gui->image_block.width,gui->image_block.height);

    draw_x_on_image ( gui, the_image );
  }

  /*printf("the image marked for delete is : %d\n",the_image->marked_for_delete);*/

  DEBUG_TRACE_OUT  printf("Done with image_remove_toggled\n");
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
void Image_Size_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  int i;
  int size_x,size_y;
  int old_size,new_size;
  int stepping  = 32;
  unsigned char *ptr;
  main_gui_t *gui = (main_gui_t *)clientdata;
  DEBUG_TRACE_IN printf("Entered Image_Size_ChangedCB\n");

  
  if (!gui->images_loaded){
    DT_error( w, "Sorry, no images are loaded", NULL, NULL ); 
    return;
  }

  DisplayBusyCursor(gui->mainwindow);

  old_size = gui->image_block.width;
  if ( w == gui->zoom_in_button ){
    if (old_size + stepping > 512){
      RemoveBusyCursor(gui->mainwindow);
      DT_inform( w, "Cannot go any larger!", NULL, NULL );
      return;
    }else{
      new_size = old_size + stepping;
    }
  }else{
    if (old_size - stepping < 32){
      RemoveBusyCursor(gui->mainwindow);
      DT_inform( w, "Cannot go any smaller!", NULL, NULL );
      return;
    }else{
      new_size = old_size - stepping;
    }
  }

  gui->image_block.width = new_size;
  gui->image_block.height = new_size;
  /*printf("set the size to : %d\n",new_size);*/

  destroy_and_rebuild_images(gui);
  adjust_manip_window_to_program_changes ( gui );

  RemoveBusyCursor(gui->mainwindow);

  /*XInstallColormap(gui->display, gui->color_info.cmap);*/
  
  DEBUG_TRACE_OUT printf("Done with Image_Size_ChangedCB\n");
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
void Number_of_Columns_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  int i;
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) calldata;

  DEBUG_TRACE_IN printf("Entered Number_of_Columns_ChangedCB\n");

  DisplayBusyCursor(gui->mainwindow);

  gui->image_block.num_columns = cbs->value;

  if( gui->images_loaded )
  {
      XtUnmanageChild(gui->image_block.rc);
      set_image_block_rc_columns(gui);
      XtManageChild(gui->image_block.rc);
  }

  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Number_of_Columns_ChangedCB\n");
}
