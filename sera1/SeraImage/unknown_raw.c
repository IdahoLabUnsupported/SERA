#include "toqsh.h"
 
int read_raw_normalizing_two_bytes(unsigned char *out, int num_bytes_in_image, 
				   int header_skip, int big_endian, 
				   int split_bytes, FILE *in);

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_unknown_raw_popup_shell
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_unknown_raw_popup_shell(main_gui_t *gui)
{
  /*XmString xmstr;*/

  DEBUG_TRACE_IN printf("Entered build_unknown_raw_popup_shell\n");

  gui->ur.shell = XtCreatePopupShell("Unknown Raw",
				     topLevelShellWidgetClass,gui->toplevel,
				     NULL,0);
  XtPopdown(gui->ur.shell);
  /*printf("done\n");*/
  gui->ur.form = XtVaCreateManagedWidget("ctform",
					 xmFormWidgetClass, gui->ur.shell,
					 NULL);

  gui->ur.title = XtVaCreateManagedWidget("Set Raw Image File Properties",
					  xmLabelWidgetClass, gui->ur.form,
					  XmNtopAttachment, XmATTACH_FORM,
					  XmNtopOffset, 10,
					  XmNleftAttachment, XmATTACH_FORM,
					  XmNleftOffset, 25,
					  NULL);
  gui->ur.sep1 = XtVaCreateManagedWidget("sep1",
					 xmSeparatorWidgetClass, gui->ur.form,
					 XmNleftAttachment, XmATTACH_FORM,
					 XmNleftOffset, 10,
					 XmNrightAttachment, XmATTACH_FORM,
					 XmNrightOffset, 10,
					 XmNtopAttachment, XmATTACH_WIDGET,
					 XmNtopWidget, gui->ur.title,
					 XmNtopOffset, 10,
					 NULL);

  gui->ur.header_toggle = XtVaCreateManagedWidget("Header Size:",
						 xmToggleButtonWidgetClass,gui->ur.form,
						 XmNtopAttachment, XmATTACH_WIDGET,
						 XmNtopWidget, gui->ur.sep1,
						 XmNtopOffset, 20,
						 XmNleftAttachment, XmATTACH_FORM,
						 XmNleftOffset, 10,
						 NULL);

  /*
   * The callback update_user_file_sizeCB is registered with the
   * header toggle, header_tb, xsize_tb, ysize_tb, bpp_menu, and
   * footer toggle. If any of these values are changed, the 
   * user_file_size_text widget will be updated. Also, the editable
   * text fields have a callback, check_for_int_input, which will
   * only allow numbers to be entered.
   */
  XtAddCallback( gui->ur.header_toggle, XmNvalueChangedCallback,
		 update_user_file_sizeCB, (XtPointer) gui );

  gui->ur.header_tb = XtVaCreateManagedWidget("Header TB",
					      xmTextFieldWidgetClass,gui->ur.form,
					      XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
					      XmNtopWidget, gui->ur.header_toggle,
					      XmNleftAttachment, XmATTACH_FORM,
					      XmNleftOffset, 210,
					      XmNrightAttachment, XmATTACH_FORM,
					      XmNrightOffset, 10,
					      NULL);

  XtAddCallback( gui->ur.header_tb, XmNmodifyVerifyCallback,
		 check_for_int_input, NULL );
  XtAddCallback( gui->ur.header_tb, XmNvalueChangedCallback,
		 update_user_file_sizeCB, (XtPointer) gui );

  gui->ur.xsize_label = XtVaCreateManagedWidget("Image Width (pixels)",
						xmLabelWidgetClass,gui->ur.form,
						XmNtopAttachment, XmATTACH_WIDGET,
						XmNtopWidget, gui->ur.header_toggle,
						XmNtopOffset, 20,
						XmNleftAttachment, XmATTACH_FORM,
						XmNleftOffset, 10,
						NULL);
  gui->ur.xsize_tb = XtVaCreateManagedWidget("xsize tb",
					     xmTextFieldWidgetClass,gui->ur.form,
					     XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
					     XmNtopWidget, gui->ur.xsize_label,
					     XmNleftAttachment, XmATTACH_FORM,
					     XmNleftOffset, 210,
					     XmNrightAttachment, XmATTACH_FORM,
					     XmNrightOffset, 10,
					     NULL);

  XtAddCallback( gui->ur.xsize_tb, XmNmodifyVerifyCallback,
		 check_for_int_input, NULL );
  XtAddCallback( gui->ur.xsize_tb, XmNvalueChangedCallback,
		 update_user_file_sizeCB, (XtPointer) gui );

  gui->ur.ysize_label = XtVaCreateManagedWidget("Image Height (pixels)",
						xmLabelWidgetClass,gui->ur.form,
						XmNtopAttachment, XmATTACH_WIDGET,
						XmNtopWidget, gui->ur.xsize_label,
						XmNtopOffset, 20,
						XmNleftAttachment, XmATTACH_FORM,
						XmNleftOffset, 10,
						NULL);

  gui->ur.ysize_tb = XtVaCreateManagedWidget("ysize_tb",
					     xmTextFieldWidgetClass,gui->ur.form,
					     XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
					     XmNtopWidget, gui->ur.ysize_label,
					     XmNleftAttachment, XmATTACH_FORM,
					     XmNleftOffset, 210,
					     XmNrightAttachment, XmATTACH_FORM,
					     XmNrightOffset, 10,
					     NULL);

  XtAddCallback( gui->ur.ysize_tb, XmNmodifyVerifyCallback,
		 check_for_int_input, NULL );
  XtAddCallback( gui->ur.ysize_tb, XmNvalueChangedCallback,
		 update_user_file_sizeCB, (XtPointer) gui );

  gui->ur.footer_toggle = XtVaCreateManagedWidget("Footer Size:",
						 xmToggleButtonWidgetClass,gui->ur.form,
						 XmNtopAttachment, XmATTACH_WIDGET,
						 XmNtopWidget, gui->ur.ysize_label,
						 XmNtopOffset, 20,
						 XmNleftAttachment, XmATTACH_FORM,
						 XmNleftOffset, 10,
						 NULL);

  XtAddCallback( gui->ur.footer_toggle, XmNvalueChangedCallback,
		 update_user_file_sizeCB, (XtPointer) gui );


  gui->ur.footer_tb = XtVaCreateManagedWidget("Footer TB",
					      xmTextFieldWidgetClass,gui->ur.form,
					      XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
					      XmNtopWidget, gui->ur.footer_toggle,
					      XmNleftAttachment, XmATTACH_FORM,
					      XmNleftOffset, 210,
					      XmNrightAttachment, XmATTACH_FORM,
					      XmNrightOffset, 10,
					      NULL);

  XtAddCallback( gui->ur.footer_tb, XmNmodifyVerifyCallback,
		 check_for_int_input, NULL );
  XtAddCallback( gui->ur.footer_tb, XmNvalueChangedCallback,
		 update_user_file_sizeCB, (XtPointer) gui );


  gui->ur.bpp_label = XtVaCreateManagedWidget("Bytes Per Pixel",
						xmLabelWidgetClass,gui->ur.form,
						XmNtopAttachment, XmATTACH_WIDGET,
						XmNtopWidget, gui->ur.footer_toggle,
						XmNtopOffset, 20,
						XmNleftAttachment, XmATTACH_FORM,
						XmNleftOffset, 10,
						NULL);

  gui->ur.bpp_pane = (Widget)XmCreatePulldownMenu(gui->ur.form,"bytes_per_pixel_pane",NULL, 0);
  gui->ur.bpp_menu = (Widget)XtVaCreateManagedWidget("bytes_per_pixel_menu",xmRowColumnWidgetClass,gui->ur.form,
                                                     XmNmarginHeight,       0,
                                                     XmNmarginWidth,        0,
                                                     XmNpacking,            XmPACK_TIGHT,
                                                     XmNpopupEnabled,       TRUE,
                                                     XmNrowColumnType,      XmMENU_OPTION,
                                                     XmNspacing,            0,
		                                     XmNsubMenuId,  gui->ur.bpp_pane, 
		                                     XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		                                     XmNtopWidget, gui->ur.bpp_label,
		                                     XmNleftAttachment, XmATTACH_FORM,
		                                     XmNleftOffset, 210,
		                                     NULL);
  gui->ur.bpp[0] = XtVaCreateManagedWidget("1",xmPushButtonWidgetClass,gui->ur.bpp_pane,NULL);
  gui->ur.bpp[1] = XtVaCreateManagedWidget("2",xmPushButtonWidgetClass,gui->ur.bpp_pane,NULL);

  XtAddCallback( gui->ur.bpp[0], XmNactivateCallback, 
		 update_user_file_sizeCB, (XtPointer) gui );
  XtAddCallback( gui->ur.bpp[1], XmNactivateCallback, 
		 update_user_file_sizeCB, (XtPointer) gui );

  gui->ur.byte_order_label = XtVaCreateManagedWidget("Byte Order: ",
						xmLabelWidgetClass,gui->ur.form,
						XmNtopAttachment, XmATTACH_WIDGET,
						XmNtopWidget, gui->ur.bpp_menu,
						XmNtopOffset, 20,
						XmNleftAttachment, XmATTACH_FORM,
						XmNleftOffset, 10,
						NULL);

  gui->ur.byte_order_pane = (Widget)XmCreatePulldownMenu(gui->ur.form,"bytes_per_pixel_pane",NULL, 0);
  gui->ur.byte_order_menu = (Widget)XtVaCreateManagedWidget("bytes_per_pixel_menu",xmRowColumnWidgetClass,gui->ur.form,
                                                     XmNmarginHeight,       0,
                                                     XmNmarginWidth,        0,
                                                     XmNpacking,            XmPACK_TIGHT,
                                                     XmNpopupEnabled,       TRUE,
                                                     XmNrowColumnType,      XmMENU_OPTION,
                                                     XmNspacing,            0,
		                                     XmNsubMenuId,  gui->ur.byte_order_pane, 
		                                     XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		                                     XmNtopWidget, gui->ur.byte_order_label,
		                                     XmNleftAttachment, XmATTACH_FORM,
		                                     XmNleftOffset, 210,
		                                     NULL);
  gui->ur.byte_order[0] = XtVaCreateManagedWidget("Big Endian",xmPushButtonWidgetClass,gui->ur.byte_order_pane,NULL);
  gui->ur.byte_order[1] = XtVaCreateManagedWidget("Little Endian",xmPushButtonWidgetClass,gui->ur.byte_order_pane,NULL);

  gui->ur.split_bytes_toggle = XtVaCreateManagedWidget("Split Bytes",		       
						       xmToggleButtonWidgetClass, gui->ur.form,
						       XmNtopAttachment, XmATTACH_WIDGET,
						       XmNtopWidget, gui->ur.byte_order_menu,
						       XmNleftOffset, 10,
						       XmNleftAttachment, XmATTACH_FORM,
						       NULL);

  gui->ur.actual_file_size_label = XtVaCreateManagedWidget( "Actual File Size: ", 
							    xmLabelWidgetClass, gui->ur.form,
							    XmNtopAttachment, XmATTACH_WIDGET,
							    XmNtopWidget,     gui->ur.split_bytes_toggle,
							    XmNtopOffset,     10,
							    XmNleftAttachment, XmATTACH_FORM,
							    XmNleftOffset,    10,
							    NULL );
  
  gui->ur.actual_file_size_text = XtVaCreateManagedWidget( "actual_file_size_text", 
							   xmTextFieldWidgetClass, gui->ur.form,
							   XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
							   XmNtopWidget,     gui->ur.actual_file_size_label,
							   XmNrightAttachment, XmATTACH_FORM,
							   XmNrightOffset,   10,
							   XmNleftAttachment, XmATTACH_FORM,
							   XmNleftOffset,    210,
							   XmNeditable,      False,
							   XmNcursorPositionVisible, False,
							   NULL );
  
  gui->ur.user_file_size_label = XtVaCreateManagedWidget( "Your File Size:  ", 
							  xmLabelWidgetClass, gui->ur.form,
							  XmNtopAttachment, XmATTACH_WIDGET,
							  XmNtopWidget,     gui->ur.actual_file_size_label,
							  XmNtopOffset,     10,
							  XmNleftAttachment, XmATTACH_FORM,
							  XmNleftOffset,    10,
							  NULL );
  
  gui->ur.user_file_size_text = XtVaCreateManagedWidget( "user_file_size_text", 
							 xmTextFieldWidgetClass, gui->ur.form,
							 XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
							 XmNtopWidget,     gui->ur.user_file_size_label,
							 XmNrightAttachment, XmATTACH_FORM,
							 XmNrightOffset,   10,
							 XmNleftAttachment, XmATTACH_FORM,
							 XmNleftOffset,    210,
							 XmNeditable,      False,
							 XmNcursorPositionVisible, False,
							 NULL );
  
  gui->ur.sep2 = XtVaCreateManagedWidget("sep2",
					 xmSeparatorWidgetClass, gui->ur.form,
					 XmNleftAttachment, XmATTACH_FORM,
					 XmNleftOffset, 10,
					 XmNrightAttachment, XmATTACH_FORM,
					 XmNrightOffset, 10,
					 XmNtopAttachment, XmATTACH_WIDGET,
					 XmNtopWidget, gui->ur.user_file_size_label,
					 XmNtopOffset, 20,
					 NULL);  


  gui->ur.apply = XtVaCreateManagedWidget("Try This",
					  xmPushButtonWidgetClass, gui->ur.form,
					  XmNtopAttachment, XmATTACH_WIDGET,
					  XmNtopWidget, gui->ur.sep2,
					  XmNtopOffset, 5,
					  XmNrightAttachment, XmATTACH_FORM,
					  XmNrightOffset, 5,
					  XmNbottomAttachment, XmATTACH_FORM,
					  XmNbottomOffset, 5,
					  NULL);
  XtAddCallback(gui->ur.apply, XmNactivateCallback,
		Unknown_Raw_ApplyCB,(XtPointer) &gui->ur);

  gui->ur.cancel = XtVaCreateManagedWidget("Cancel",
					   xmPushButtonWidgetClass, gui->ur.form,
					   XmNtopAttachment, XmATTACH_WIDGET,
					   XmNtopWidget, gui->ur.sep2,
					   XmNtopOffset, 5,
					   XmNrightAttachment, XmATTACH_WIDGET,
					   XmNrightWidget, gui->ur.apply,
					   XmNrightOffset, 5,
					   XmNbottomAttachment, XmATTACH_FORM,
					   XmNbottomOffset, 5,
					   NULL);
  XtAddCallback(gui->ur.cancel, XmNactivateCallback,
		Unknown_Raw_CancelCB,(XtPointer)&gui->ur);

  gui->ur.finished = 0;
  
  DEBUG_TRACE_OUT printf("Done with build_unknown_raw_popup_shell\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Unknown_Raw_CancelCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Unknown_Raw_CancelCB(Widget w,XtPointer clientdata, XtPointer calldata)
{
  unknown_raw_popup_t *ur = (unknown_raw_popup_t *)clientdata;

  DEBUG_TRACE_IN printf("Entered Unknown_Raw_CancelCB\n");

  XtPopdown(ur->shell);
  ur->cancelled = 1;
  ur->finished = 1;

  DEBUG_TRACE_OUT printf("Done with Unknown_Raw_CancelCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Unknown_Raw_ApplyCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Unknown_Raw_ApplyCB(Widget w,XtPointer clientdata, XtPointer calldata)
{
  unknown_raw_popup_t *ur = (unknown_raw_popup_t *)clientdata;
  int actual_size;
  int user_size;
  char *ptr;

  DEBUG_TRACE_IN printf("Entered Unknown_Raw_ApplyCB\n");

  /*
   * Before closing the widget, make sure that the actual file
   * size, and the user file size match. Otherwise, make the 
   * user change their values until they match.
   */

  ptr = XmTextGetString( ur->actual_file_size_text );
  actual_size = atoi( ptr );
  XtFree( ptr );

  ptr = XmTextGetString( ur->user_file_size_text );
  user_size = atoi( ptr );
  XtFree( ptr );

  if( actual_size == user_size ) {
    XtPopdown(ur->shell);
    ur->cancelled = 0;
    ur->finished = 1;
  } else {
    DT_error( w, "The actual size of the file, and the\nsize you've specified do not match!\nPlease change your values.", 
	      NULL, NULL );
    ur->cancelled = 0;
    ur->finished = 0;
  }

  DEBUG_TRACE_OUT printf("Done with Unknown_Raw_ApplyCB\n");
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
int get_user_values_for_unknown_raw_image(main_gui_t *gui)
{
  /*XtPopup(gui->ur.shell,XtGrabNone);*/ /* popped up already */

  DEBUG_TRACE_IN printf("Entered get_user_values_for_unknown_raw_image\n");

  
  while (!gui->ur.finished)
    XtAppProcessEvent(gui->app, XtIMAll);
  
  gui->ur.finished = 0;

  DEBUG_TRACE_OUT printf("Done with get_user_values_for_unknown_raw_image\n");
  return 1;
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : read_unknown_raw_file
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int read_unknown_raw_file(main_gui_t *gui, char *filename)
{
  int i;
  int header_skip;
  int footer_skip;
  int xsize;
  int ysize;
  int bpp;
  int bytes_read;
  int big_endian;
  int split_bytes;
  int size_of_qsh_image;
  int old_size_of_qsh_images;
  int new_size_of_qsh_images;
  int number_of_bytes_to_read;
  int user_guessed_file_size;
  int num_bytes_in_ready_image;
  int actual_file_size;
  Widget temp_widget;
  FILE *in;
  unsigned char *image,*resized_image;
  unsigned char *ready_image,*ptr;
  unsigned char *new_images;
  Boolean test;

  DEBUG_TRACE_IN printf("Entered read_unknown_raw_file\n");

  
  if (!(in = fopen(filename,"r"))){
    DEBUG_TRACE_OUT printf("Done with read_unknown_raw_file\n");
    return 0;
  }


  XtVaGetValues(gui->ur.header_toggle,XmNset,&test,NULL);
  if (!test)
    header_skip = 0;
  else
    header_skip = atoi((char *)XmTextGetString(gui->ur.header_tb));

  XtVaGetValues(gui->ur.footer_toggle,XmNset,&test,NULL);
  if (!test)
    footer_skip = 0;
  else
    footer_skip = atoi((char *)XmTextGetString(gui->ur.footer_tb));

  xsize       = atoi((char *)XmTextGetString(gui->ur.xsize_tb));
  ysize       = atoi((char *)XmTextGetString(gui->ur.ysize_tb));
  XtVaGetValues(gui->ur.bpp_menu,XmNmenuHistory,&temp_widget,NULL);
  bpp         = atoi((char *)XtName(temp_widget));

  XtVaGetValues(gui->ur.byte_order_menu,XmNmenuHistory,&temp_widget,NULL);
  big_endian = strncmp(XtName(temp_widget),"Big",3)==0 ? 1 : 0;

  XtVaGetValues(gui->ur.split_bytes_toggle,XmNset,&test,NULL);
  split_bytes = test;
    
  /*printf("got the bpp, it is : %d\n",bpp);*/

  /*
  printf("got the values : header_skip : %d, xsize : %d, y_size : %d, bpp  : %d\n",
	 header_skip,xsize,ysize,bpp);
  */

  /*****************************************************************/
  /** If it is the first image loaded, we have to set up a new **/
  /** qsh_info structure **/
  /*****************************************************************/
  if (!gui->images_loaded){
    /*printf("there were NOT already images loaded, so constructing the qsh_info structure\n"); */
    gui->qsh_gui->qsh_info = (qsh_info_t *)MT_malloc(sizeof(qsh_info_t));
    gui->qsh_gui->qsh_info->images = (unsigned char *)MT_malloc(xsize * ysize);
    gui->qsh_gui->qsh_info->size_of_dimension[0] = 1;
    
    init_qsh_info_validity(gui->qsh_gui->qsh_info);

    gui->qsh_gui->qsh_info->valid.number_of_dimensions = 1;
    gui->qsh_gui->qsh_info->number_of_dimensions = 3;
    
    gui->qsh_gui->qsh_info->size_of_dimension[0] = 1;
    gui->qsh_gui->qsh_info->valid.size_of_dimension[0] = 1;
    gui->qsh_gui->qsh_info->size_of_dimension[1] = xsize;
    gui->qsh_gui->qsh_info->valid.size_of_dimension[1] = 1;
    gui->qsh_gui->qsh_info->size_of_dimension[2] = ysize;
    gui->qsh_gui->qsh_info->valid.size_of_dimension[2] = 1;
    gui->qsh_gui->current_qsh_image_referencing = 2;

    /* initialize the image locations to all zero for raw images */
    for( i = 0; i < MAX_QSH_SLICES; i++ )
        gui->qsh_gui->qsh_info->image_location[i] = 0.00;
    
    gui->qsh_gui->qsh_info->bytes_per_pixel = 1;
    gui->qsh_gui->qsh_info->valid.bytes_per_pixel = 1;

    /** make sure we tell libqsh that there are no key alias mappings, otherwise it will try to free them **/
    gui->qsh_gui->num_mappings = 0;

    /* next is currently removed 8-27-98 CLA */
    /*check_and_report_qhd_values(gui->qsh_gui); */

  }

  user_guessed_file_size = header_skip + xsize * ysize * bpp + footer_skip;
  number_of_bytes_to_read = xsize*ysize * bpp;
  fseek(in,0,SEEK_END);
  actual_file_size = (int)ftell(in);
  /*printf("the actual size of the file is : %d\n",actual_file_size);*/
  rewind(in);

  if (user_guessed_file_size != actual_file_size){
    printf("Sorry, your specifications do not match the size of the file, your : %d,  actual: %d\n",
	   user_guessed_file_size, actual_file_size);
    DEBUG_TRACE_OUT printf("Done with read_unknown_raw_file\n");
    return 0;
  }

  image = (unsigned char *)MT_malloc(number_of_bytes_to_read);


  if (bpp == 1){
    /*****************************************************************/
    /** skip the header **/
    /*****************************************************************/
    fread(image,1,header_skip,in);

    /*****************************************************************/
    /** read the image **/
    /*****************************************************************/
    bytes_read = fread(image,1,number_of_bytes_to_read,in);


    /*****************************************************************/
    /** check to make sure all were read **/
    /*****************************************************************/
    if (bytes_read  < number_of_bytes_to_read){
      printf("Couldn't read enough bytes out of the file\n");
      DEBUG_TRACE_OUT printf("Done with read_unknown_raw_file\n");
      return 0;
    }
    num_bytes_in_ready_image = number_of_bytes_to_read;

  }else{
    /*printf("calling read_raw_norm header_skip is : %d\n",header_skip);*/
    if (!read_raw_normalizing_two_bytes(image,number_of_bytes_to_read,
					header_skip,big_endian,
					split_bytes, in)){
      /*printf("read_raw_normalizing_two_bytes returned 0\n");*/
      return 0;
    }
    /*
      gui->qsh_gui->qsh_info->valid.bytes_per_pixel = 1;
      gui->qsh_gui->qsh_info->bytes_per_pixel = 1;
    */
    num_bytes_in_ready_image = (number_of_bytes_to_read)/2;
  }

  ready_image = image;

  /***************************************************************/
  /**  Now we have the image, add it to the qsh_info structure**/
  /***************************************************************/
  if (gui->images_loaded){
    /*printf("there were already images, so we will just add it to the structure\n");*/
    /***************************************************************/
    /**  There were already images in the qsh_structure, so we need to append this one **/
    /***************************************************************/
    /*printf("ok, there were %d images, adding another\n",gui->qsh_gui->qsh_info->size_of_dimension[0]);*/

    size_of_qsh_image = gui->qsh_gui->qsh_info->size_of_dimension[1] * gui->qsh_gui->qsh_info->size_of_dimension[2];
    /*printf("the size of a qsh_image is : %d\n",size_of_qsh_image);*/
    old_size_of_qsh_images = gui->qsh_gui->qsh_info->size_of_dimension[0] * size_of_qsh_image;
    /*printf("the old size of the qsh_images was : %d\n",old_size_of_qsh_images);*/
    new_size_of_qsh_images = (gui->qsh_gui->qsh_info->size_of_dimension[0] + 1) * size_of_qsh_image;
    /*printf("the new size of the qsh_images was : %d\n",new_size_of_qsh_images);*/
    
    /***************************************************************/
    /**  check to see if this image is the same size as the ones loaded**/
    /**  if not, resize it before adding it **/
    /***************************************************************/
    if (xsize != gui->qsh_gui->qsh_info->size_of_dimension[1] ||
	ysize != gui->qsh_gui->qsh_info->size_of_dimension[2]){

      resized_image = (unsigned char *)MT_malloc(size_of_qsh_image);

      /*printf("resizing the image because it is not the same as in the current qsh_info struct\n");*/
      generic_resize_image(image,resized_image,
			   xsize,ysize,
			   gui->qsh_gui->qsh_info->size_of_dimension[1],
			   gui->qsh_gui->qsh_info->size_of_dimension[2],
			   1);

      /***************************************************************/
      /**  set the ready image to the resized one, get rid of the **/
      /** original, since it is not needed **/
      /***************************************************************/
      ready_image = resized_image;
      MT_free((void *)image);
    }

    /***************************************************************/
    /**  make room for all the images including the new one **/
    /***************************************************************/    
    new_images = (unsigned char *) MT_malloc ( new_size_of_qsh_images );
  
    /***************************************************************/
    /** copy all the images over, then copy the new one over too**/
    /***************************************************************/
    memcpy(new_images,gui->qsh_gui->qsh_info->images,old_size_of_qsh_images);
    ptr = &new_images[old_size_of_qsh_images];
    memcpy(ptr,ready_image,size_of_qsh_image);
    
    /***************************************************************/
    /** free the old images and set the old image pointer to the new **/
    /** images **/
    /***************************************************************/
    MT_free((void *)gui->qsh_gui->qsh_info->images);
    gui->qsh_gui->qsh_info->images = new_images;

    /***************************************************************/
    /**  We now have another image */
    /***************************************************************/
    gui->qsh_gui->qsh_info->image_location[gui->qsh_gui->qsh_info->size_of_dimension[0]] = 0.0;
    gui->qsh_gui->qsh_info->size_of_dimension[0]++;
 
    /***************************************************************/
    /** update the image block with the new image **/
    /***************************************************************/
    /*destroy_and_rebuild_images(gui); removed, no need to do this every time*/

  }else {
    /*********************************************************************/
    /**  This is the first image in the set **/
    /*********************************************************************/
    gui->image_block.width = 128;
    gui->image_block.height = 128;
    
    /***************************************************************/
    /** copy the new image into the new qsh_info structure **/
    /***************************************************************/
    memcpy(gui->qsh_gui->qsh_info->images,ready_image,num_bytes_in_ready_image);
  }

  /* we now have images */
  gui->images_loaded = 1;
  gui->qsh_gui->qsh_info->valid.images = 1;
  
  /* no need for this image any more */
  MT_free((void *)ready_image);
  
  /* no need for the file any more */
  fclose(in);

  DEBUG_TRACE_OUT printf("Done with read_unknown_raw_file\n");
  return 1;
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
int read_raw_normalizing_two_bytes(unsigned char *out, int num_bytes_in_image, 
				   int header_skip, int big_endian, 
				   int split_bytes, FILE *in)
{
  int i;
  int num_out_bytes;
  int max,min,range;
  unsigned int holding_value;
  unsigned char byte1,byte2;
  /*int big_endian = 1;*/

  DEBUG_TRACE_IN printf("Entered read_raw_normalizing_two_bytes\n");

  /******************************************************************/
  /** first scan through and find the max value so we know  **/
  /** what to normalize then from **/
  /******************************************************************/
  max = 0;
  min = 100000;
  num_out_bytes = num_bytes_in_image/2;

  /*****************************************************************/
  /** skip the header **/
  /*****************************************************************/
  rewind(in);
  /*printf("skipping %d bytes for the header\n",header_skip);*/
  fread(out,1,header_skip,in);

  /*printf("going to loop %d times\n",num_out_bytes);*/
  for (i=0;i<num_out_bytes;i++){
    if (!fread(&byte1,1,1,in)){
      printf("returning 0 in loop #%d, checking the max/min\n",i);
      return 0;
    }
    if (!fread(&byte2,1,1,in)){
      printf("returning 0 in loop #%d, reading second byte, checking the max/min\n",i);
      return 0;
    }
    
    if (big_endian){
      holding_value = (int)byte1*256;
      if(!split_bytes)
	holding_value += (int)byte2;
    }else{
      holding_value = (int)byte2*256;
      if(!split_bytes)
	holding_value += (int)byte1;
    }

    if (holding_value > max) max = holding_value;
    if (holding_value < min) min = holding_value;
  }

/*  max = 65535; */
  DEBUG_DATA printf("found the max value it is : %d\n",max);
  DEBUG_DATA printf("found the min value it is : %d\n",min);  
  range = max - min;

  rewind(in); 
  /*****************************************************************/
  /** skip the header **/
  /*****************************************************************/
  fread(out,1,header_skip,in);

  for (i=0;i<num_out_bytes;i++){
    if (!fread(&byte1,1,1,in)){
      printf("returning 0 in loop #%d\n",i);
      return 0;
    }
    if (!fread(&byte2,1,1,in)){
      printf("returning 0 in loop #%d, reading second byte\n",i);
      return 0;
    }

    if (big_endian){
      holding_value = (int)byte1*256;
      if(!split_bytes)
	holding_value += (int)byte2;
    }else{
      holding_value = (int)byte2*256;
      if(!split_bytes)
	holding_value += (int)byte1;
    }


    out[i] = (unsigned char)((float)(holding_value-min) * 255.0/(float)range);
    /*if (q->qsh_info->images[i] != 0) printf("found a value of %d\n",q->qsh_info->images[i]); */
  }

  DEBUG_TRACE_OUT printf("Done with read_raw_normalizing_two_bytes");

  return 1;
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
void smart_set_unknown_raw_popup(main_gui_t *gui)
{
  FILE *in;
  int file_size, this_file_size;
  int xsize,ysize;
  /*int found = 0;*/
  int exact_match = 0;
  int min_distance = 5000000;
  int last_min_distance = 5000000;
  int best_xsize,best_ysize;
  int final_xsize,final_ysize,final_bpp;
  char temp_string[50];
  int header = 0;
  int STEP = 64;
  int file_sizes_match = 1;
  int i = 0;

  DEBUG_TRACE_IN printf("Entered smart_set_unknown_raw_popup\n");

  /*
   * Make sure that all of the files have the same size
   * or else warn the user.
   */
  if (!(in = fopen(gui->mfb.files[0],"r"))){
    printf("Coudn't open the file to guess at the size\n");
    DEBUG_TRACE_OUT printf("Leaving smart_set_unknown_raw_popup\n");
    return;
  }
  
  fseek(in,0,SEEK_END);
  file_size = (int)ftell(in);  
  fclose(in);

  i = 1;
  while( i < gui->mfb.num_files && file_sizes_match )
  {
    if ( (in = fopen(gui->mfb.files[i],"r")) != NULL )
    {
      fseek(in,0,SEEK_END);
      this_file_size = (int)ftell(in);
      fclose(in);

      if( file_size == this_file_size )
	i++;
      else
      {
	DT_warn(gui->toplevel,
		"It appears that some of the files you\nhave selected have different sizes.\nThis will cause serious problems if\nyou continue.\
 You would be advised to\nload these files separately.", NULL, NULL);
	file_sizes_match = 0;
      }
    }
    else
      file_sizes_match = 0;
  }    


  /*printf("the file size is : %d\n",file_size);*/
  /************************************************/
  /*  ok now figure the file sizes */
  /************************************************/
  /*printf("going to check for  the best 1 byte fit\n");*/
  xsize = 1024;
  ysize = 1024;
  exact_match = 0;
  while (!exact_match){
    /*printf("\n\ntrying : %d x %d  (%d)\n", xsize,ysize,xsize*ysize);*/
    if (xsize * ysize  == file_size){
      exact_match = 1;
      final_xsize = xsize;
      final_ysize = ysize;
      final_bpp = 1;
      header = 0;
      /*printf("FOUND AN EXACT MATCH : %d x %d\n",xsize,ysize);*/
      break;
    }else if (xsize * ysize > file_size){
      xsize -=STEP;
      ysize -=STEP;
      continue;
    }else{
      last_min_distance = min_distance;
      min_distance = file_size - (xsize *ysize);
      /*printf("set the min_distance to : %d\n",min_distance);*/
      if (min_distance > last_min_distance){
	/** we're getting further away from the right guess, stop and return the last guess **/
	header = last_min_distance;
	final_xsize = best_xsize;
	final_ysize = best_ysize; 
	final_bpp = 1;
	/*printf("found the best header size of : %d\n",header);*/
	break;
      }
      best_xsize = xsize;
      best_ysize = ysize;
      xsize -= STEP;
      ysize -= STEP;
    }
  }
  
  /*printf("done checking the 1 byte possiblity\n");*/
  
  if (!exact_match){
    /*************************************************/
    /**  now check to see if the set happened to be **/
    /** a 2 byte image set, if that gets any closer **/
    /*************************************************/
    xsize = 1024;
    ysize = 1024;
    last_min_distance = 5000000;
    min_distance = 5000000;
    
    /*printf("just entering the 2 byte while loop, filesize of : %d\n",file_size);*/
    while (!exact_match){
      /*printf("testing : %d, %d   (%d)\n",xsize,ysize,xsize*ysize*2);*/
      if (xsize * ysize * 2  == file_size){
	exact_match = 1;
	final_xsize = xsize;
	final_ysize = ysize;
	final_bpp = 2;
	header = 0;
	/*printf("FOUND AN EXACT 2 BYTE MATCH : %d, %d\n",xsize,ysize);*/
	break;
    }else if (xsize * ysize * 2 > file_size){
      xsize -=STEP;
      ysize -=STEP;
      continue;
    }else{
      last_min_distance = min_distance;
      min_distance = file_size - (xsize *ysize*2);
      /*printf("min_distance : %d, last_min_distance : %d\n",min_distance, last_min_distance);*/
      if (min_distance > last_min_distance){
	/** we're getting further away from the right guess, stop and return the last guess **/
	if (last_min_distance < header){
	  /** we found a smaller header go with this **/
	header = last_min_distance;
	final_xsize = best_xsize;
	final_ysize = best_ysize;
	final_bpp = 2;
	/*printf("Found a better match with the 2 byte try\n");*/
	 }/*else
	  printf("Couldn't find a better match\n");*/

	break;
      }
      best_xsize = xsize;
      best_ysize = ysize;
      xsize -= STEP;
      ysize -= STEP;
    }
    }
  }
    /*
      printf("done with the 2 byte possiblity\n");
      printf("the header is : %d\n",header);
      printf("the xsize is : %d\n",final_xsize); 
      printf("the ysize is : %d\n",final_ysize);
      printf("the bpp is : %d\n",final_bpp);
    */

  if (header != 0){
    /*printf("printing to the temp_string , %d\n",header);*/
    sprintf(temp_string,"%d",header);
    /*printf("setting the tempstring for the header size to : %s\n",temp_string);*/
    XmTextSetString(gui->ur.header_tb,temp_string);
    XtVaSetValues(gui->ur.header_toggle,XmNset,TRUE,NULL);
  }else{		  
    XtVaSetValues(gui->ur.header_toggle,XmNset,FALSE,NULL);
  }
  
  sprintf(temp_string,"%d",final_xsize);
  /*printf("setting the tempstring for the final_xsize to : %s\n",temp_string); */
  XmTextSetString(gui->ur.xsize_tb,temp_string);

  sprintf(temp_string,"%d",final_ysize);
  /*printf("setting the tempstring for the final_ysize to : %s\n",temp_string);*/
  XmTextSetString(gui->ur.ysize_tb,temp_string);

  if (final_bpp == 1) XtVaSetValues(gui->ur.bpp_menu,XmNmenuHistory,gui->ur.bpp[0],NULL);
  else XtVaSetValues(gui->ur.bpp_menu,XmNmenuHistory,gui->ur.bpp[1],NULL);

  /* set the actual file size */
  sprintf(temp_string, "%d", file_size);
  XmTextSetString(gui->ur.actual_file_size_text, temp_string);

  /* set the user file size */
  sprintf(temp_string, "%d", header+(final_xsize*final_ysize*final_bpp));
  XmTextSetString(gui->ur.user_file_size_text, temp_string);

  XtVaSetValues(gui->ur.footer_toggle,XmNset,FALSE,NULL);

  DEBUG_TRACE_OUT printf("Done with smart_set_unknown_raw_popup\n");

}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     update_user_file_sizeCB
%% 
%% Purpose:      Recalculate the size of the user guessed file size as they
%%               make changes to the unknown_raw_popup widget. The new size
%%               is displayed in the user_file_size_text widget.
%% 
%% Parameters:   Callback parameters. main_gui_t * passed through clientData.
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void update_user_file_sizeCB( Widget w, XtPointer clientData, XtPointer callData )
{
  main_gui_t * gui = (main_gui_t *) clientData;

  int header;
  int footer;
  int xsize;
  int ysize;
  int bpp;
  char temp_string[128];
  char * ptr;
  int new_size;
  Widget temp_widget;

  DEBUG_TRACE_IN printf("Entering update_user_file_sizeCB\n");

  /* Check for header bytes */
  if( XmToggleButtonGetState( gui->ur.header_toggle ) ) {
    ptr = XmTextGetString(gui->ur.header_tb);
    header = atoi( ptr );
    XtFree( ptr );
  }
  else
    header = 0;

  /* Check for footer bytes */
  if( XmToggleButtonGetState( gui->ur.footer_toggle ) ) {
    ptr = XmTextGetString(gui->ur.footer_tb);
    footer = atoi( ptr );
    XtFree( ptr );
  }
  else
    footer = 0;

  /* Check bytes_per_pixel */
  XtVaGetValues( gui->ur.bpp_menu, XmNmenuHistory, &temp_widget, NULL );
  bpp = atoi((char *)XtName( temp_widget ) );

  /* Get x and y sizes */
  ptr = XmTextGetString( gui->ur.xsize_tb );
  xsize = atoi( ptr );
  XtFree( ptr );
  ptr = XmTextGetString( gui->ur.ysize_tb );
  ysize = atoi( ptr );
  XtFree( ptr );

  /* Calculate the new file size, and put that value in the
     user_file_size_text widget */
  new_size = header + (xsize*ysize*bpp) + footer;
  sprintf( temp_string, "%d", new_size );
  XmTextSetString( gui->ur.user_file_size_text, temp_string );

  DEBUG_TRACE_OUT printf("Leaving update_user_file_sizeCB\n");
}
