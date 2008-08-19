/******************************************************************************
 * arrange_images.c                                                           *
 *                                                                            *
 * INEEL BNCT Research Project                                                *
 * Montana State University - Bozeman                                         *
 *                                                                            *
 * The functions in this file are used when re-arranging the images in        *
 * toqsh.                                                                     *
 *                                                                            *
 * Matt Cohen 8/19/98                                                         *
 *****************************************************************************/
#include "toqsh.h"

/*=========================================================================
  Function:    highlight_image_EH

  Purpose:     Highlights an image selected to move, or possible
               places to place an image which has been selected to move.

  Parameters:  Normal Event Handler parameters.
               Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure is passed through clientData.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void highlight_image_EH ( Widget w,XtPointer clientData,
			  XEvent *event, Boolean *flag)
{
  main_gui_t *gui  = (main_gui_t *)clientData;
  image_t *the_image = NULL;
  int this_image;
  int i;
  
  DEBUG_TRACE_IN printf("Entered Highlight_image_EH\n");

  /* Just return if an image hasn't been selected to move */
  if ( gui->image_block.image_move_started )
  {

    /* Find which image has been entered */
    for ( i = 0; i < gui->image_block.num_images; i++ )
    {
       if ( w == gui->image_block.image[i].drawing_area )
       {
	 the_image = &gui->image_block.image[i];
	 this_image = i;
	 break;
       }
    }
  
    /* Return if the image wasn't located for some reason */
    if (the_image == NULL)
    {
      printf("didn't get the image in imageSelectedCallback\n");
      DEBUG_TRACE_OUT printf("Leaving Highlight_image_EH\n");
      return;
    }

    /* If the image is the one that will be moved highlight it in red */
    if ( this_image == gui->image_block.image_to_move )
    {
     XtVaSetValues ( the_image->frame, 
		     XmNtopShadowColor, gui->red_pixel, 
		     XmNbottomShadowColor, gui->red_pixel,
		     NULL );
    }
    /* otherwise highlight it in green */
    else
    {
     XtVaSetValues ( the_image->frame, 
		     XmNtopShadowColor, gui->green_pixel, 
		     XmNbottomShadowColor, gui->green_pixel,
		     NULL );
    }
  } 
  DEBUG_TRACE_OUT printf("Leaving Highlight_image_EH\n");
}


/*=========================================================================
  Function:    dehighlight_image_EH

  Purpose:     Removes the highlight of an image when the mouse leaves
               the image 

  Parameters:  Normal Event Handler parameters.
               Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure is passed through clientData.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void dehighlight_image_EH (Widget w,XtPointer clientData, 
			   XEvent *event, Boolean *flag)
{
  main_gui_t *gui  = (main_gui_t *)clientData;
  image_t *the_image = NULL;
  int this_image;
  int i;
  
  DEBUG_TRACE_IN printf("Entered Dehighlight_image_EH\n");

  /* If an image hasn't been selected to move just return */
  if ( gui->image_block.image_move_started )
  {

    /* Find the image which is being left */
    for ( i = 0; i < gui->image_block.num_images; i++ )
    {
      if ( w == gui->image_block.image[i].drawing_area )
      {
	  the_image = &gui->image_block.image[i];
	  this_image = i;
	  break;
      }
    }
    
    /* Return if for some reason the image wasn't found */
    if (the_image == NULL)
    {
      printf("didn't get the image in imageSelectedCallback\n");
      DEBUG_TRACE_OUT printf("Leaving Deighlight_image_EH\n");
      return;
    }
  
    /* If this isn't the image to move, then return it to it's original 
     * shadow colors */
    if ( this_image != gui->image_block.image_to_move )
      XtVaSetValues ( the_image->frame, 
		      XmNtopShadowColor, gui->ts.pixel, 
		      XmNbottomShadowColor, gui->bs.pixel,
		      NULL );
  }

  DEBUG_TRACE_OUT printf("Leaving Dehighlight_image_EH\n");
}


/*=========================================================================
  Function:    image_selected_EH

  Purpose:     When the mouse in on an image and the middle mouse
               button is pressed, the image is selected to move or
               the location to place the selected image is selected

  Parameters:  Normal Event Handler parameters.
               Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure is passed through clientData.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void image_selected_EH (Widget w,XtPointer clientData, 
			XEvent *event, Boolean *flag)
{
  main_gui_t *gui  = (main_gui_t *)clientData;
  image_t *the_image;
  int image_num = -1;
  int i;

  DEBUG_TRACE_IN printf("Entered image_selected_EH\n");

  for (i=0;i<gui->image_block.num_images;i++){
    if (w == gui->image_block.image[i].drawing_area){
      the_image = &gui->image_block.image[i]; 
      image_num = i;
      break;
    }
  }

  if ( image_num == -1 )
  {
      DEBUG_TRACE_OUT printf("Leaving image_selected_EH\n");
      return;
  }

  if ( (event->xmotion.state & Button2Mask) ) /*&&
        gui->move_images_button.state == END_IMAGE_MOVE )*/
  {
      move_image_toggled ( gui, the_image, image_num );
  }
  else if ( (event->xmotion.state & Button3Mask) ) /* &&
	     gui->move_images_button.state == START_IMAGE_MOVE )*/
  {
      image_remove_toggled ( gui, the_image );
  }

  DEBUG_TRACE_OUT printf("Leaving image_selected_EH\n");
}


/*=========================================================================
  Function:    move_image_toggled

  Purpose:     Called when an image is clicked on to be moved, whether it
               is first being selected to move or to be placed in a new
	       location.

  Parameters:  Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure.
	       A pointer to the image being clicked.
	       The number of the image being clicked.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void move_image_toggled ( main_gui_t *gui, image_t *the_image, int image_num )
{

  DEBUG_TRACE_IN printf("Entering move_image_toggled\n");

  if ( gui->image_block.image_move_started )
  {

      XtVaSetValues ( gui->image_block.
		      image[gui->image_block.image_to_move].frame,
		      XmNtopShadowColor, gui->ts.pixel,
		      XmNbottomShadowColor, gui->bs.pixel,
		      NULL );
      XtVaSetValues ( the_image->frame,
		      XmNtopShadowColor, gui->ts.pixel,
		      XmNbottomShadowColor, gui->bs.pixel,
		      NULL );

      move_image ( gui, gui->image_block.image_to_move, image_num );
      gui->image_block.image_move_started = 0;
  }
  else
  {
    /*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     * Don't think that this is necessary, and this will cause a crash if
     * image_to_move no longer exists because of it being marked for
     * delete etc.
     *
     * Get the color values of the image shadows 
     * XtVaGetValues ( gui->image_block.
     *                 image[gui->image_block.image_to_move].frame,
     *                 XmNtopShadowColor, &gui->ts.pixel,
     *	               XmNbottomShadowColor, &gui->bs.pixel,
     *	               NULL );
     *
     *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

    XtVaSetValues ( the_image->frame, 
		    XmNtopShadowColor, gui->red_pixel, 
		    XmNbottomShadowColor, gui->red_pixel,
		    NULL );

    gui->image_block.image_to_move = image_num;
    gui->image_block.image_move_started = 1;  
  }

  DEBUG_TRACE_OUT printf("Leaving move_image_toggled\n");
}


/*=========================================================================
  Function:    move_image

  Purpose:     This procedure copies all the raw image data into a temp
               array in the new order depending on which direction the move
               occured.  The old array is then freed and the new array takes
               the place of the old raw data. 

  Parameters:  Pointer to the main_gui_t structure which contains the
               rotate_gui_t structure.
	       The number of the image to move.
               The number of the location to move the image.

  Returned:    None.

  Author:      Matt Cohen

  Date:        9/3/98
=========================================================================*/
void move_image ( main_gui_t *gui, int from_num, int to_num )
{
    unsigned char *all_images, *the_image, *temp_image, *ptr_to_old,
                  *ptr_to_new;
    int           size_of_all, size_of_one, i;

    DEBUG_TRACE_IN printf ( "Entering move_image\n" );

    /* Only do if the image "has" to be moved */
    if( from_num != to_num )  
    {
      /* Get memory for single image */
      size_of_one = gui->qsh_gui->qsh_info->size_of_dimension[1] *
	            gui->qsh_gui->qsh_info->size_of_dimension[2];

      the_image = (unsigned char *)MT_calloc(size_of_one, 
					     sizeof(unsigned char));
 
      /* Get memory for copy of reordered images */
      size_of_all = size_of_one * gui->qsh_gui->qsh_info->size_of_dimension[0];

      all_images = (unsigned char *)MT_calloc(size_of_all,
					      sizeof(unsigned char));

      if ( from_num < to_num )
      {
        /* Copy all images up to the one to be moved */ 
        memcpy ( all_images, gui->qsh_gui->qsh_info->images, 
		 size_of_one*from_num );

	/* Point to the image to move and copy it to a temp */
        ptr_to_old = &gui->qsh_gui->qsh_info->images[size_of_one*from_num];
	memcpy ( the_image, ptr_to_old, size_of_one );
	
        /* Copy all the images in between the image to move an its
         * New location */
        ptr_to_new = &all_images[size_of_one*from_num];
	ptr_to_old = &gui->qsh_gui->qsh_info->images[size_of_one*(from_num+1)];
        memcpy (ptr_to_new, ptr_to_old, size_of_one*(to_num-from_num));
	
        /* Copy in the temp image */
        ptr_to_new = &all_images[size_of_one*to_num];
        memcpy (ptr_to_new, the_image, size_of_one);
	
        /* Copy all the rest of the images */ 
        ptr_to_old = &gui->qsh_gui->qsh_info->images[size_of_one*(to_num+1)];
        ptr_to_new = &all_images[size_of_one*(to_num+1)];
        memcpy (ptr_to_new, ptr_to_old, size_of_one * 
		(gui->qsh_gui->qsh_info->size_of_dimension[0]-to_num-1) );
	
        /* Free old images and re-assign pointer */
        MT_free ( (void *) gui->qsh_gui->qsh_info->images );
        gui->qsh_gui->qsh_info->images = all_images;

	/* Free temp image */
	MT_free( (void *) the_image );

        destroy_and_rebuild_images ( gui ); 
      }
      else if ( from_num > to_num )
      {
        /* Copy all images up to the insert spot */ 
        memcpy ( all_images, gui->qsh_gui->qsh_info->images, 
		 size_of_one*to_num );

	/* Point to the image to move and copy it to new array */
        ptr_to_old = &gui->qsh_gui->qsh_info->images[size_of_one*from_num];
        ptr_to_new = &all_images[size_of_one*to_num];
	memcpy ( ptr_to_new, ptr_to_old, size_of_one );
	
        /* Copy all the images in between the image to move an its
         * New location */
        ptr_to_new = &all_images[size_of_one*(to_num+1)];
	ptr_to_old = &gui->qsh_gui->qsh_info->images[size_of_one*to_num];
        memcpy (ptr_to_new, ptr_to_old, size_of_one*(from_num-to_num));
	
        /* Copy all the rest of the images */ 
        ptr_to_old = &gui->qsh_gui->qsh_info->images[size_of_one*(from_num+1)];
        ptr_to_new = &all_images[size_of_one*(from_num+1)];
        memcpy (ptr_to_new, ptr_to_old, size_of_one * 
		(gui->qsh_gui->qsh_info->size_of_dimension[0]-from_num-1) );
	
        /* Free old images and re-assign pointer */
        MT_free ( (void *) gui->qsh_gui->qsh_info->images );
        gui->qsh_gui->qsh_info->images = all_images;

	/* Free temp image */
	MT_free( (void *) the_image );

        destroy_and_rebuild_images ( gui );

      }

    }

    /*else printf ( "You're not moving anywhere!\n" );*/ 
    
    DEBUG_TRACE_OUT printf ( "Leaving move_image\n" );
}
