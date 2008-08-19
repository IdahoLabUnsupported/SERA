#include "sera3d.h"

void draw_axial_slice(int slice);
void draw_coronal_slice(int slice);
void draw_sagittal_slice(int slice);
int external_slice_beam = 0;
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Slice_value_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: updates the slice position label (passed in)
%%%           and redraws the scene when the slice slider moves
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Slice_value_ChangedCB(Widget w, XtPointer clientdata, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;
  XmString xmstr;
  char string[256],meas[5];
  float denom;

  DEBUG_TRACE_IN printf("Entered Slice_value_ChangedCB\n");

  if (gui->cm){ 
    denom = 10.0; 
    strcpy(meas,"(cm)");
  }else{ 
    denom = 1.0; 
    strcpy(meas,"(mm)");
  }

  gui->cur_slice = cbs->value;
  
  if (gui->slice_dir == 1){
    sprintf(string,"Slice %s= %.2f %s",
            gui->axisLabels[SLICE_AXIS].name,
            convert_world_y_to_slice_z(gui,(float)gui->cur_slice)/denom, meas);
    xmstr = XmStringCreateLocalized(string);
    XtVaSetValues(gui->slice_panel.loc_label, XmNlabelString, xmstr, NULL);
  }else if (gui->slice_dir == 2){
    sprintf(string,"Slice %s = %.2f %s",
            gui->axisLabels[ROW_AXIS].name,
            (float)gui->cur_slice/denom,meas);
    xmstr = XmStringCreateLocalized(string);
    XtVaSetValues(gui->slice_panel.loc_label, XmNlabelString, xmstr, NULL);
  }else{
    sprintf(string,"Slice %s = %.2f %s",
            gui->axisLabels[COLUMN_AXIS].name,
            (float)gui->cur_slice/denom,meas);
    xmstr = XmStringCreateLocalized(string);
    XtVaSetValues(gui->slice_panel.loc_label, XmNlabelString, xmstr, NULL);
  }
  XmStringFree(xmstr);

  if (gui->inlay_slice){
    if (gui->fast_rotation_type != 3) gui->draw_high_lists = 1;
    draw_all(gui);
    if (gui->fast_rotation_type != 3) gui->draw_high_lists = 0;
  }

  DEBUG_TRACE_OUT printf("Done with Slice_value_ChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Slice_Dir_ToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the current direction of the slice between
%%%           axial,coronal,sagittal
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Slice_Dir_ToggledCB(Widget w, XtPointer clientdata,XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;

  DEBUG_TRACE_IN printf("Entered Slice_Dir_ToggledCB\n");

  DisplayBusyCursor(gui->mainwindow);

  if      ( w == gui->slice_panel.ax_s_toggle  && cbs->set)  gui->slice_dir = 1;
  else if ( w == gui->slice_panel.cor_s_toggle && cbs->set)  gui->slice_dir = 2;
  else if ( w == gui->slice_panel.sag_s_toggle && cbs->set)  gui->slice_dir = 3;

  
  if (gui->inlay_slice) draw_all(gui);
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Slice_Dir_ToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Inlay_sliceToggleCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the inlay_slice flag and redraws
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Inlay_sliceToggledCB(Widget w, XtPointer clientdata,XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmToggleButtonCallbackStruct *cbs =  (XmToggleButtonCallbackStruct *) callData;
  XmString xmstr;
  char string[256],meas[5];
  float denom;
  
  DEBUG_TRACE_IN printf("Entered Inlay_sliceToggledCB\n");

  DisplayBusyCursor(gui->mainwindow);
  if (cbs->set) gui->inlay_slice = 1;
  else gui->inlay_slice = 0;

  if (gui->cm)
  { 
    denom = 10.0; 
    strcpy(meas,"(cm)");
  }
  else
  { 
    denom = 1.0; 
    strcpy(meas,"(mm)");
  }

  /* Get the current slice */
  XtVaGetValues( gui->slice_panel.s_slider, XmNvalue, &gui->cur_slice, NULL );
  
  if (gui->slice_dir == 1)
  {
    sprintf(string,"Slice IS = %.2f %s", convert_world_y_to_slice_z(gui,(float)gui->cur_slice)/denom, meas);
    xmstr = XmStringCreateLocalized(string);
    XtVaSetValues(gui->slice_panel.loc_label, XmNlabelString, xmstr, NULL);
  }
  else if (gui->slice_dir == 2)
  {
    sprintf(string,"Slice PA = %.2f %s", (float)gui->cur_slice/denom,meas);
    xmstr = XmStringCreateLocalized(string);
    XtVaSetValues(gui->slice_panel.loc_label, XmNlabelString, xmstr, NULL);
  }
  else
  {
    sprintf(string,"Slice RL = %.2f %s", (float)gui->cur_slice/denom,meas);
    xmstr = XmStringCreateLocalized(string);
    XtVaSetValues(gui->slice_panel.loc_label, XmNlabelString, xmstr, NULL);
  }
  XmStringFree(xmstr);

  if (gui->inlay_slice)
  {
    if (gui->fast_rotation_type != 3) gui->draw_high_lists = 1;
    draw_all(gui);
    if (gui->fast_rotation_type != 3) gui->draw_high_lists = 0;
  }  
  else
  {
      draw_all( gui );
  }

  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Inlay_sliceToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Load_RawsCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: gets the wanted file name (FSB), then reads
%%%           in the raw slices (replaces the univels since
%%%           they are not needed).
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Load_RawsCB(Widget w, XtPointer clientData, XtPointer calldata)
{
    main_gui_t *gui = (main_gui_t *)clientData;
    int is_direct;
    char fileName[256];
    int i = 0;
    qsh_info_t *my_qsh;

    DEBUG_TRACE_IN printf("Entered Load_RawsCB\n");

    is_direct = 0;
    for(i=0;i<gui->file_menu.load_images.num_submenus;i++){
        if (w == gui->file_menu.load_images.submenu[i+1]){
            is_direct = 1; break;
        }
    }

    DisplayBusyCursor(gui->form);

    /*************************************************/
    /** Get the string (filename)                    */
    /*************************************************/
    if (!is_direct){
        if(!DT_select_file(gui->toplevel,gui->app,&fileName[0],NULL)){
            RemoveBusyCursor(gui->form);
            DEBUG_TRACE_OUT printf("Done with Load_RawsCB\n");
            return;
        }
    }else{
        strcpy(fileName,GetTextOfLabel(w));
    }

  
    if( FT_fileExists( fileName ) && is_a_valid_qsh_file( fileName ) )
    {
        my_qsh = (qsh_info_t *)MT_malloc(sizeof(qsh_info_t));
    
        if (!read_qsh_pair(my_qsh,fileName,gui->toplevel,gui->app))
        {
            DT_error(gui->toplevel, "Sorry the qsh pair could not be read",NULL,NULL);
            RemoveBusyCursor(gui->form);
            DEBUG_TRACE_OUT printf("Done with Load_RawsCB\n");
            return;
        }

        fill_recent_file(gui,fileName,2);


        XtVaSetValues(gui->file_menu.load_single_contour.pane,XmNsensitive,TRUE,NULL);
        XtVaSetValues(gui->file_menu.load_full_contour.pane,XmNsensitive,TRUE,NULL);
        XtVaSetValues(gui->clipping_dialog.neg_cap, XmNsensitive, True, NULL );
        XtVaSetValues(gui->clipping_dialog.pos_cap, XmNsensitive, True, NULL );

        if (my_qsh->size_of_dimension[0] == gui->num_slices) {
            build_3d_texture_from_qsh(gui,my_qsh);
	    gui->images_loaded = 1;
        /** NOTE:  the qsh structure my_qsh is being freed in the above function **/
        } else
            DT_error(gui->toplevel,
                     "Sorry i couldn't build the texture map,\n the number of slices in the uv_file does not\n match the number of images in the qim file\n",
                     NULL,NULL);
    
    }
    else
    {
        DT_error(gui->toplevel,"Sorry that is not a valid Image file",NULL,NULL);
    }

    draw_all(gui);
    RemoveBusyCursor(gui->form);

    DEBUG_TRACE_OUT printf("Done with Load_RawsCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : draw_slice_in_slice_win
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: when the external slice window is up,
%%%           this procedure changes the projection (in a separate
%%%           glxcontext) to an orthographic, then renders the 
%%%           inlaid slice.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw_slice_in_slice_win(main_gui_t *gui)
{
  static int first_time = 1;
  Bool worked = 0;
 
  DEBUG_TRACE_IN printf("Entered draw_slice_in_slice_win\n");
  
  if (first_time){
    gui->glx_slice_context = glXCreateContext(XtDisplay(gui->external_slice_dialog.gl_slice_area), 
					      gui->visinfo,
					      gui->glxcontext,
					      True);
    if (gui->glx_slice_context == NULL)
      printf("Problem, the glx_slice_context is NULL\n");
 
    first_time = 0;
  }
   

  /*************************************************/
  /** Make w the current drawing widget.           */
  /*************************************************/
  worked = glXMakeCurrent(XtDisplay(gui->external_slice_dialog.gl_slice_area), 
			  XtWindow(gui->external_slice_dialog.gl_slice_area), 
			  gui->glx_slice_context);

  if (!worked) printf("WARNING, problem switching to the glx_slice_context\n");
 

  /*************************************************/
  /** Set the viewport size to the widget size     */
  /*************************************************/
  
  if (first_time){
    glViewport(0, 0, 256, 256);
    glEnable(GL_DEPTH_TEST);  
    glDrawBuffer(GL_FRONT); 
    glClearDepth(1.0);
    glClearColor(0.0, 0.0, 0.0, 0.0);
  }
 

   
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  
  glMatrixMode (GL_PROJECTION);       
  glPushMatrix();
  glLoadIdentity();
  glOrtho(-128,128,-128,128,gui->front_z,gui->back_z);
  glMatrixMode (GL_MODELVIEW);      
  
  glColor3f(1,1,1);
  glEnable(GL_DEPTH_TEST);
  glPushMatrix();
  glLoadIdentity();
  
 
  if (!gui->beam_slice){
    if (gui->slice_dir == 1) gluLookAt(0,-500,0,0,0,0,0,0,1);  
    else if (gui->slice_dir == 2) gluLookAt(0,0,200,0,0,0,0,1,0);  
    else gluLookAt(200,0,0,0,0,0,0,1,0);  
    draw_3d_textured_plane(gui,(float)gui->cur_slice);    
  }else{
    if (!gui->ibeam){
      gui->cam.x = (float)gui->particle_path[gui->beam_num].start_x;
      gui->cam.y = (float)gui->particle_path[gui->beam_num].start_z;
      gui->cam.z = (float)gui->particle_path[gui->beam_num].start_y;
      
      gui->cam.at_x = (float)gui->particle_path[gui->beam_num].end_x;
      gui->cam.at_y = (float)gui->particle_path[gui->beam_num].end_z;
      gui->cam.at_z = (float)gui->particle_path[gui->beam_num].end_y;
    }else{
      gui->cam.x = gui->ibeam_sx;
      gui->cam.y = gui->ibeam_sy;
      gui->cam.z = gui->ibeam_sz;
      
      gui->cam.at_x = gui->ibeam_ex;
      gui->cam.at_y = gui->ibeam_ey;
      gui->cam.at_z = gui->ibeam_ez;
    }
    gluLookAt(gui->cam.x,     gui->cam.y,    gui->cam.z,
	      gui->cam.at_x,  gui->cam.at_y, gui->cam.at_z,
	      0,              1,             0);
    draw_beam_slice(gui,gui->beam_slice_x, gui->beam_slice_y, gui->beam_slice_z);
  }      
  if (gui->external_slice_beam) Draw_particle_paths(gui);/*draw_beam_lines();*/  

  glPopMatrix();
  
  glMatrixMode (GL_PROJECTION);      
  glPopMatrix();
  glMatrixMode (GL_MODELVIEW);      
  
  glXSwapBuffers(gui->display, XtWindow(gui->external_slice_dialog.gl_slice_area));

  glXMakeCurrent(gui->display, XtWindow(gui->glxarea), gui->glxcontext);

  DEBUG_TRACE_OUT printf("Done with draw_slice_in_slice_win\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_slice_win_dialog
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent widget
%%%
%%%  Purpose: builds the external slice window
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_slice_win_dialog(main_gui_t *gui)
{
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered build_slice_win_dialog\n");

  gui->external_slice_dialog.dialog = (Widget)XmCreateMessageDialog(gui->toplevel, "Slice Win",NULL,0);

  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->external_slice_dialog.dialog,XmDIALOG_SYMBOL_LABEL));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->external_slice_dialog.dialog,XmDIALOG_MESSAGE_LABEL));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->external_slice_dialog.dialog,XmDIALOG_CANCEL_BUTTON));
  XtUnmanageChild((Widget)XmMessageBoxGetChild(gui->external_slice_dialog.dialog,XmDIALOG_HELP_BUTTON));
  
  xmstr = XmStringCreateLocalized("Close");
  XtVaSetValues((Widget)XmMessageBoxGetChild(gui->external_slice_dialog.dialog,XmDIALOG_OK_BUTTON),
		XmNlabelString, xmstr, NULL);
  
  XtAddCallback(gui->external_slice_dialog.dialog, XmNokCallback,SliceWinDialogOKCB,(XtPointer)gui);

 
  gui->external_slice_dialog.main_form = XtVaCreateManagedWidget ("main_form", 
				       xmFormWidgetClass, gui->external_slice_dialog.dialog, 
				       NULL);
  
  gui->external_slice_dialog.gl_slice_area = XtVaCreateManagedWidget("gl_slice_area",
								     DRAWING_AREA_TYPE, gui->external_slice_dialog.main_form,
								     GLwNvisualInfo, gui->visinfo,
								     GLwNallocateOtherColors, FALSE,
								     GLwNinstallColormap, FALSE,
								     XmNheight, 256,
								     XmNwidth, 256,
								     NULL);
  gui->external_slice_dialog.x2_button = XtVaCreateManagedWidget("Double Size",
								 xmPushButtonWidgetClass, gui->external_slice_dialog.main_form,
								 XmNleftAttachment, XmATTACH_FORM,
								 XmNrightAttachment, XmATTACH_NONE,
								 XmNtopAttachment, XmATTACH_WIDGET,
								 XmNtopWidget, gui->external_slice_dialog.gl_slice_area,
								 XmNtopOffset, 5,
								 NULL);
  gui->external_slice_dialog.beam_button = XtVaCreateManagedWidget("Display Beam Lines",
								   xmPushButtonWidgetClass, gui->external_slice_dialog.main_form,
								   XmNleftAttachment, XmATTACH_WIDGET,
								   XmNleftWidget, gui->external_slice_dialog.x2_button,
								   XmNrightAttachment, XmATTACH_FORM,
								   XmNtopAttachment, XmATTACH_WIDGET,
								   XmNtopWidget, gui->external_slice_dialog.gl_slice_area,
								   XmNtopOffset, 5,
								   NULL);
  XtAddCallback(gui->external_slice_dialog.x2_button, XmNactivateCallback, Double_Slice_WinCB, (XtPointer)gui);
  XtAddCallback(gui->external_slice_dialog.beam_button, XmNactivateCallback, External_Slice_Beam_ToggledCB, (XtPointer)gui);

  DEBUG_TRACE_OUT printf("Done with build_slice_win_dialog\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Double_Slice_WinCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the size of the external slice window
%%%           between 256 & 512
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Double_Slice_WinCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  static int size = 1;
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered Double_Slice_WinCB\n");

  if (size == 1){
    XtVaSetValues (gui->external_slice_dialog.gl_slice_area, 
		   XmNwidth, 512,
		   XmNheight, 512,NULL);
    xmstr = XmStringCreateLocalized("Normal Size");
    XtVaSetValues(w,XmNlabelString,xmstr,NULL);
    XmStringFree(xmstr);

    glXMakeCurrent(XtDisplay(gui->external_slice_dialog.gl_slice_area), 
		   XtWindow(gui->external_slice_dialog.gl_slice_area), 
		   gui->glx_slice_context);
    glViewport(0, 0, 512, 512);
    glXMakeCurrent(XtDisplay(gui->glxarea), XtWindow(gui->glxarea), gui->glxcontext);
    size = 2;
  }else{
    XtVaSetValues (gui->external_slice_dialog.gl_slice_area, 
		   XmNwidth, 256,
		   XmNheight, 256,NULL);
    xmstr = XmStringCreateLocalized("Double Size");
    XtVaSetValues(w,XmNlabelString,xmstr,NULL);
    XmStringFree(xmstr);

    glXMakeCurrent(XtDisplay(gui->external_slice_dialog.gl_slice_area), 
		   XtWindow(gui->external_slice_dialog.gl_slice_area), 
		   gui->glx_slice_context);
    glViewport(0, 0, 256, 256);
    glXMakeCurrent(XtDisplay(gui->glxarea), XtWindow(gui->glxarea), gui->glxcontext);
    size = 1;
  }  
  draw_slice_in_slice_win(gui);

  DEBUG_TRACE_OUT printf("Done with Double_Slice_WinCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : External_Slice_Beam_ToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void External_Slice_Beam_ToggledCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmString xmstr;

  DEBUG_TRACE_IN printf("Entered External_Slice_Beam_ToggledCB\n");

  if (!gui->external_slice_beam){
    gui->external_slice_beam = 1;
    xmstr = XmStringCreateLocalized("Display Beam Lines");
    XtVaSetValues(w,XmNlabelString,xmstr,NULL);
  }else{
    gui->external_slice_beam = 0;
    xmstr = XmStringCreateLocalized("Remove Beam Lines");
    XtVaSetValues(w,XmNlabelString,xmstr,NULL);
  }
  XmStringFree(xmstr);
  /*printf("external_slice_beam is : %d\n",external_slice_beam);*/
  draw_slice_in_slice_win(gui);

  DEBUG_TRACE_OUT printf("Done with External_Slice_Beam_ToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Show_Slice_Win_DialogCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: simply brings up the external slice window if it
%%%            is not managed, if it is the first time, it builds
%%%            the slice window first.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Show_Slice_Win_DialogCB(Widget w, XtPointer clientData,  XtPointer callData)
{
    main_gui_t *gui = (main_gui_t *)clientData;
    static int first_time = 1;
    Position main_x,main_y;
    Dimension width;
    Dimension dialogWidth;

    DEBUG_TRACE_IN printf("Entered Show_Slice_Win_DialogCB\n");

    if( gui->images_loaded ) /* make sure images are loaded first */
    {
        if( XmToggleButtonGetState( gui->slice_panel.s_toggle ) )
        {
            if(first_time) {
                build_slice_win_dialog(gui);
                first_time = 0;
            }
            /*printf("showing the slice win\n");*/

            if (!XtIsManaged(gui->external_slice_dialog.dialog)){
                /*XtVaSetValues(slice_win_dialog,XmNx,50,XmNy,50,NULL);*/
                XtManageChild(gui->external_slice_dialog.dialog);

                XtVaGetValues( gui->external_slice_dialog.dialog,
                               XmNwidth, &dialogWidth,
                               NULL );
                
                XtVaGetValues(gui->toplevel,XmNwidth,&width,NULL);
                XtVaGetValues(gui->toplevel,
                              XmNx,&main_x,
                              XmNy,&main_y,
                              NULL);
                
                /* make sure we don't pop it up off the screen */
                if( (int)main_x + (int)width + (int)dialogWidth < gui->Screenwidth )
                {
                    XtVaSetValues(gui->external_slice_dialog.dialog,
                                  XmNx,main_x + (int)width,
                                  XmNy,main_y,
                                  NULL);
                }
                
                /*XtVaGetValues(gui->external_slice_dialog.dialog,XmNx,&x,XmNy,&y,NULL);*/
                /*printf("the slice win is at : %d,%d\n",(int)x,(int)y);*/

                gui->slice_win = 1;
            }else{
                XtUnmanageChild(gui->external_slice_dialog.dialog);
                gui->slice_win = 0;
            }

            draw_all(gui);
        }
        else
        {
            if( !first_time && XtIsManaged( gui->external_slice_dialog.dialog ) )
            {
                XtUnmanageChild( gui->external_slice_dialog.dialog );
                gui->slice_win = 0;
            }
        }
    }
  
    DEBUG_TRACE_OUT printf("Done with Show_Slice_Win_DialogCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : SliceWindDialogOKCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: simply closes the external slice window
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void SliceWinDialogOKCB (Widget w, XtPointer clientData, XtPointer callData)
{ 
  main_gui_t *gui = (main_gui_t *)clientData;
  DEBUG_TRACE_IN printf("Entered SliceWinDialogOKCB\n");

  DisplayBusyCursor(gui->form);
  
  gui->slice_win = 0;
  RemoveBusyCursor(gui->form);

  DEBUG_TRACE_IN printf("Entered SliceWinDialogOKCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : draw_clipped_slice
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: slice position
%%%
%%%  Purpose: calls the corresponding procedure to the 
%%%           slice direction.  (This is the old way of 
%%%           drawing the slices, the 3d texture is not used)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw_clipped_slice(main_gui_t *gui,int slice)
{
  DEBUG_TRACE_IN printf("Entered draw_clipped_slice\n");

  if (slice > -129 && slice < 129)
  switch(gui->slice_dir)
    {
      /*
    case 1: draw_axial_slice(slice);break;
    case 2: draw_coronal_slice(slice);break;
    case 3: draw_sagittal_slice(slice);break;
      */
    }
  DEBUG_TRACE_OUT printf("Done with draw_clipped_slice\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : draw_unclipped_slice
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: slice position
%%%
%%%  Purpose: calls the corresponding procedure to the 
%%%           slice direction.  (This is the old way of 
%%%           drawing the slices, the 3d texture is not used)
%%%           Clipping is disabled.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw_unclipped_slice(main_gui_t *gui,int slice)
{
  DEBUG_TRACE_IN printf("Entered draw_unclipped_slice\n");
  /*printf("slice is : %d\n",slice);*/
  if (slice > -129 && slice < 129)
    {
      unset_clipping(gui);
      switch(gui->slice_dir)
	{
	  /*
	case 1: draw_axial_slice(slice);break;
	case 2: draw_coronal_slice(slice);break;
	case 3: draw_sagittal_slice(slice);break;
	  */
	}
      set_clipping(gui);
    }
  
  DEBUG_TRACE_OUT printf("Done with draw_unclipped_slice\n");
}
