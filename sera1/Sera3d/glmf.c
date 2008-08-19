/*#define Local_vars*/
#include "sera3d.h"
#include "environment_tools.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Variables used only in this file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
static int snglBuf[] = {GLX_RGBA, 
			GLX_DEPTH_SIZE, 12,
			GLX_STENCIL_SIZE,4,
			GLX_RED_SIZE, 1, 
			None};
static int dblBuf[] = {GLX_RGBA, 
		       GLX_DEPTH_SIZE, 12,
		       GLX_DOUBLEBUFFER, 
		       GLX_RED_SIZE,4, 
		       GLX_STENCIL_SIZE, 4, None};
static String fallbackResources[] = {"*glxarea*width: 500", 
				     "*glxarea*height: 500",
				     "*gl_frame*shadowType: SHADOW_IN", 
				     NULL};

int loading_images_with_fsb = 0;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Local Prototypes (only used in this file)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

void init_globals(main_gui_t *gui);

/* Added 04.20.2000 MBR */
static void initAxisInfo( main_gui_t * gui );

void reset_variables(main_gui_t *gui);
void build_gui(main_gui_t *gui);

void graphicsInit(Widget w, XtPointer clientData, XtPointer call);
void expose(Widget w, XtPointer clientData, XtPointer call);
void resize(Widget w, XtPointer clientData, XtPointer call);
void input(Widget w, XtPointer clientData, XtPointer callData);

void CreateOptionsPane(main_gui_t *gui);
void CreateFilePane(main_gui_t *gui);
void CreatePreferencesPane(main_gui_t *gui);
void CreateHelpPane(main_gui_t *gui);
void CreateMenuBar(main_gui_t *gui);

void OpenCallback (Widget w, XtPointer clientData, XtPointer callData);
void CloseCallback (Widget w, XtPointer clientData, XtPointer callData);
void DisablebodyCB (Widget w, XtPointer clientData, XtPointer callData);
void CheckVersionCB(Widget w, XtPointer clientData, XtPointer callData);
void ExitCallback (Widget w, XtPointer clientData, XtPointer callData);

void CancelCallback(Widget w, XtPointer clientData, XtPointer callData);
void Process_files(Widget w, XtPointer clientData, XtPointer callData);
void Multi_viewCB(Widget w, XtPointer clientData,XtPointer callData);


void CreateCascadingAxisPane(Widget parent);
void CreateCascadingRotationPane(Widget parent);
void CreateCascadingBackgroundPane(Widget parent);
void fix_filename_and_open(Widget w, XtPointer clientData, XtPointer callData);
void Messages_toggleCB(Widget w, XtPointer clientData,XtPointer callData);
void FastRotation_changedCB(Widget w, XtPointer clientData,XtPointer callData);
void Background_typeCB(Widget w, XtPointer clientData,XtPointer callData);
void CreateCascadingMainWindowSizePane(Widget parent);
void MainWindowSizeChangedCB(Widget w, XtPointer clientData, XtPointer callData);
void CreateCascadingMultiWindowSizePane(Widget parent);
void MultiWindowSizeChangedCB(Widget w, XtPointer clientData, XtPointer callData);
void set_window_sizes(main_gui_t *gui,int main, int multi, int priority);

void dump_opengl_implementation_dependent_state_variables();

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Main
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: command line args
%%%
%%%  Purpose: calls all of the setup procedures and initializes
%%%           all of the needed variables for the program.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int main(int argc, char **argv)
{
  int i;
  Pixmap pix;
  main_gui_t gui;

  ET_checkEnvironment( ); /* check environment variables */


  /********************************************************/
  /*  Initialize the top level app
  /********************************************************/
  gui.toplevel = XtAppInitialize(&gui.app, "Sera3d", options, 
				 XtNumber(options), &argc, argv,
				 fallbackResources, NULL, 0);

  set_debug_values( argv[0], gui.toplevel );
  
  if (argc > 1) debug_syntax(argc,argv);

  /********************************************************/
  /*  Initialize the global variables
  /********************************************************/
  init_globals(&gui);
  read_preferences(&gui);

  /*XtVaSetValues(toplevel,XmNx, 0, XmNy, 0,NULL);*/

  /*XtVaSetValues(toplevel, XmNallowShellResize, TRUE, NULL);*/

  gui.Screenwidth = XWidthOfScreen(XtScreen(gui.toplevel));
  gui.Screenheight = XHeightOfScreen(XtScreen(gui.toplevel));

  printf("======================================================\n");
  printf("Screen size is :   %d x %d\n",gui.Screenwidth,gui.Screenheight);
    
  /********************************************************/
  /*  Setup the display and visual
  /********************************************************/
  gui.display = XtDisplay(gui.toplevel);
  gui.screen = DefaultScreen(gui.display);


  gui.visinfo = glXChooseVisual(gui.display, gui.screen, dblBuf);
  if (gui.visinfo == NULL){
    printf("couldn't get  double-buffering, trying for single\n");
    gui.visinfo = glXChooseVisual(gui.display, gui.screen, snglBuf);
    if (gui.visinfo == NULL)
      XtAppError(gui.app, "couldn't even find a single buffered visual!");
    gui.doubleBuffer = False;
  }
  printf("Visual Depth : %d\n",(int)gui.visinfo->depth);

  /********************************************************/
  /*  Build the GUI for all widgets and windows
  /********************************************************/
  build_gui(&gui);

  pix = XCreatePixmapFromBitmapData(gui.display,
				    RootWindowOfScreen(XtScreen(gui.form)),
				    (char *)icon2_bits, 
				    icon2_width, icon2_height,
				    gui.bg, gui.hl,
				    DefaultDepthOfScreen(XtScreen(gui.form)));
  XtVaSetValues(gui.toplevel, 
		XmNiconPixmap, pix,
		XmNiconic, TRUE,
		NULL);


  /*********************************************************/
  /* Add the callbacks and Event Handlers                  */
  /*********************************************************/
  
  /***************************************************/
  /** register the graphics area callbacks 
  /***************************************************/   
        
  XtAddCallback(gui.glxarea, GLwNginitCallback, graphicsInit, (XtPointer)&gui);
  XtAddCallback(gui.glxarea, GLwNexposeCallback, expose, (XtPointer)&gui);
  /*XtAddCallback(glxarea, GLwNresizeCallback, resize, NULL);*/
  
  if (gui.multi_view){
    XtAddCallback(gui.multiview_panel.gl_lview_area, GLwNginitCallback, graphicsInit, (XtPointer)&gui);
    XtAddCallback(gui.multiview_panel.gl_lview_area, GLwNexposeCallback, expose, (XtPointer)&gui);
    /*XtAddCallback(gl_lview_area, GLwNresizeCallback, resize, NULL);*/
    
    XtAddCallback(gui.multiview_panel.gl_rview_area, GLwNginitCallback, graphicsInit, (XtPointer)&gui);
    XtAddCallback(gui.multiview_panel.gl_rview_area, GLwNexposeCallback, expose, (XtPointer)&gui);
    /*XtAddCallback(gl_rview_area, GLwNresizeCallback, resize, NULL);*/
    
    XtAddCallback(gui.multiview_panel.gl_tview_area, GLwNginitCallback, graphicsInit, (XtPointer)&gui);
    XtAddCallback(gui.multiview_panel.gl_tview_area, GLwNexposeCallback, expose, (XtPointer)&gui);
    /*XtAddCallback(gl_tview_area, GLwNresizeCallback, resize, NULL);*/
  }

  /****************************************************/
  /*** register the callback for the list of bodies ***/
  /*** this callback enables/disables the drawing   ***/
  /*** of the bodies in the gl window               ***/
  /****************************************************/
  XtAddCallback (gui.bodylist, XmNmultipleSelectionCallback, DisablebodyCB, (XtPointer)&gui);

  switch(gui.mouse_control_method){
  case SLIDER:
    auto_set_window_sizes(&gui,1);
    add_slider_callbacks(&gui);
    break;
  case MOUSE:
    XtAddEventHandler(gui.glxarea, ButtonMotionMask, FALSE, Rotate_with_mouse, (XtPointer)&gui);
    XtAddEventHandler(gui.glxarea,ButtonPressMask, FALSE, Reset_positions, (XtPointer)&gui);
    if (gui.fast_rotation_type != 3){
      XtAddEventHandler(gui.glxarea,ButtonPressMask, FALSE, Set_High_Lists, (XtPointer)&gui);
      XtAddEventHandler(gui.glxarea,ButtonReleaseMask, FALSE, Unset_High_Lists, (XtPointer)&gui);
    }
    break;
  }
  
  
  /*****************************************************/
  /*** realize the top level and start the main loop ***/
  /*****************************************************/
  XtRealizeWidget(gui.toplevel);
  

  /** check the command line parameters **/

  for (i=1;i<argc;i++){
    if (strstr(argv[i],"-gl_vars") || strstr(argv[i],"-GL_VARS")){
      dump_opengl_implementation_dependent_state_variables();
      break;
    }
  }

  /*
    if (argc >= 2) {
    strcpy(gui.uvfile, argv[1]);
    fix_filename_and_open(NULL,(XtPointer)&gui,NULL);
    }
    
    if (argc >= 3){
    gui.images_loaded = 1;
    
    if (strstr(argv[2],".qim") || strstr(argv[2],".qhd")){
    my_qsh = (qsh_info_t *)malloc(sizeof(qsh_info_t));
    
    if (read_qsh_pair(my_qsh,argv[2],gui.toplevel,gui.app)){
    if (my_qsh->size_of_dimension[0] == gui.num_slices)
    
    build_3d_texture_from_qsh(&gui,my_qsh);
    else
    DT_error(gui.toplevel,
    "Couldn't build the texture map,\n the number of slices in the uv_file does not\n match the number of images in the qim file\n",
    NULL, NULL);
    free(my_qsh);
    }
    }
    else
    DT_error(gui.toplevel,"Sorry that is not a valid raw file",NULL,NULL);
    }
    
    
    if (argc >= 4){
    process_particle_file(&gui,argv[3]);
    }
    if (gui.using_defaults) {
    DT_error(gui.toplevel,"Couldn't open the Sera3d.rsc (preferences) file,Defaults installed",NULL,NULL);
    }
    */

  XtAppMainLoop(gui.app);
  
  return 0;             /* ANSI C requires main to return int. */
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : graphicsInit 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: sets up an OpenGL rendering context and state
%%%           for rendering OpenGL in to the main window.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void graphicsInit(Widget w, XtPointer clientData, XtPointer call)
{
  XVisualInfo *visinfo;
  main_gui_t *gui = (main_gui_t *)clientData;

  DEBUG_TRACE_IN printf("Entered graphicsInit\n");

  /*****************************************************/
  /*** Create OpenGL rendering context
  /*****************************************************/
  XtVaGetValues(w, GLwNvisualInfo, &visinfo, NULL);
  gui->glxcontext = glXCreateContext(gui->display, visinfo,
				     0,                  /* No sharing. */
				     True);              /* Direct rendering if possible. */
  

  /*****************************************************/
  /*** Create OpenGL state
  /*****************************************************/
  glXMakeCurrent(XtDisplay(w), XtWindow(w), gui->glxcontext);

  glClearDepth(1.0);
  glClearColor(gui->background, gui->background, gui->background, 0.0);  
  
 /*****************************************************/
 /*** Setup OpenGL viewing matrices
 /*****************************************************/
  glMatrixMode (GL_PROJECTION);	/*  prepare for and then  */ 
  glLoadIdentity ();	/*  define the projection  */
  glFrustum (-20, 20, -20, 20, gui->front_z, gui->back_z);      
  glMatrixMode (GL_MODELVIEW);	/*  back to modelview matrix	*/


  /*
  printf("mainwindowsize is : %d\n",mainwindowsize);
  glViewport(0, 0, mainwindowsize, mainwindowsize);
  */
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT |
	    GL_STENCIL_BUFFER_BIT);
    glLoadIdentity ();	/*  clear the matrix	*/
    gluLookAt(gui->cam.x,gui->cam.y,gui->cam.z,
	      gui->cam.at_x,gui->cam.at_y,gui->cam.at_z,
	      gui->cam.up_x,gui->cam.up_y,gui->cam.up_z);  
 
  /*****************************************************/
  /** Enable GL options
  /*****************************************************/
    
    glEnable(GL_AUTO_NORMAL);
    glEnable(GL_NORMALIZE);
  
    glEnable(GL_COLOR_MATERIAL);

    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
    glDepthFunc(GL_LEQUAL);
    glEnable(GL_DEPTH_TEST);

  /*****************************************************/
  /** Set up the lights and clipping planes.
  /*****************************************************/     
  init_lights(gui);  
  init_clipping_planes(gui);

  /*****************************************************/
  /** Build the x,y,z axis & labels.
  /*****************************************************/
  build_axis(gui);


  /*build_X_Y_Z();*/

  glShadeModel (GL_SMOOTH);
  /*glShadeModel (GL_FLAT);*/

  draw_apeture_into_stencil_buffer(gui);

  /*check_for_extension();*/
   /*check_for_glx_extention(); */

  /*dump_opengl_implementation_dependent_state_variables();*/

  DEBUG_TRACE_OUT printf("Done with  graphicsInit\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : resize 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: sets the glviewport to the new window size
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void resize(Widget w, XtPointer clientData, XtPointer call)
{
  GLwDrawingAreaCallbackStruct *callData;
  main_gui_t *gui = (main_gui_t *)clientData;

  DEBUG_TRACE_IN printf("Entered resize\n");

  callData = (GLwDrawingAreaCallbackStruct *) call;
  glXMakeCurrent(XtDisplay(w), XtWindow(w), gui->glxcontext);
  glXWaitX();

  glMatrixMode (GL_PROJECTION);	/*  prepare for and then  */ 
  glLoadIdentity ();	/*  define the projection  */
  glFrustum (-20, 20, -20, 20,gui->front_z, gui->back_z);	/*  transformation  */
  glMatrixMode (GL_MODELVIEW);	/*  back to modelview matrix	*/
  
  glViewport(0, 0, callData->width, callData->height);

  DEBUG_TRACE_OUT printf("Done with resize\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : expose
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: simply redraws the windows
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void expose(Widget w, XtPointer clientData, XtPointer call)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  static int first_time = 1;

  DEBUG_TRACE_IN printf("Entered expose\n");

  if (first_time && XtIsRealized(gui->form)){
    register_message_handlers_for_children(gui,gui->form);
    first_time = 0;
  }
  
  DisplayBusyCursor(gui->mainwindow);
  PostMessage (gui,"Redrawing . . .");  
  draw(gui,w);
  RemoveMessage(gui,".");

  /*
   * Added this flag, because the gui was being exposed when
   * loading image with the fsb and the wait cursor was being
   * removed, which makes it appear the program has frozen
   */
  if ( ! loading_images_with_fsb )
      RemoveBusyCursor(gui->mainwindow);
  
  DEBUG_TRACE_OUT printf("Done with expose\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : build_gui
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: builds all of the widgets for the user interface
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void build_gui(main_gui_t *gui)
{

  DEBUG_TRACE_IN printf("Entered build_gui\n");

  gui->mainwindow = XtVaCreateManagedWidget ("mainwindow",
					     xmMainWindowWidgetClass,
					     gui->toplevel, NULL,0);

  /*************************************************/
  /** Build the Menubar **/
  /*************************************************/
  CreateMenuBar(gui);

 /*************************************************/
 /** build the main form on which everything
 /** is built
 /*************************************************/
  gui->form = XmCreateForm(gui->mainwindow, "MainFormform", NULL, 0);

  XtVaSetValues ( gui->mainwindow, XmNworkWindow, gui->form, NULL );
  /*** fill in the global window decoration colors ***/
  XtVaGetValues(gui->form,XmNhighlightColor,&gui->hl,NULL);
  XtVaGetValues(gui->form,XmNforeground,&gui->fg,NULL);
  XtVaGetValues(gui->form,XmNbackground,&gui->bg,NULL);

  XtManageChild(gui->form);
  
  build_top_level_forms(gui);
  DEBUG_GUI printf("Built top_level_forms\n");

  build_message_pad(gui);
  DEBUG_GUI printf("Built message_pad\n");

  build_sliders(gui);
  DEBUG_GUI  printf("Built sliders\n"); 
  
  build_multi_view(gui);
  DEBUG_GUI  printf("Built multi_view\n");
  
  build_body_list(gui);
  DEBUG_GUI  printf("built body_list\n");
  
  build_controls(gui);
  DEBUG_GUI  printf("built controls\n");
  
  build_view_form(gui);
  DEBUG_GUI  printf("built view_form\n");
  
  build_color_form(gui);
  DEBUG_GUI  printf("built color_form\n");
  
  build_transparency_form(gui);
  DEBUG_GUI  printf("built transparency_form\n");
  
  build_lighting_form(gui);
  DEBUG_GUI  printf("built lighting_form\n");

  build_clipping_form(gui);
  DEBUG_GUI  printf("built clipping_form\n");
  
  build_polygon_form(gui);
  DEBUG_GUI  printf("built nurb_form\n");
  
  build_particles_form(gui);
  DEBUG_GUI  printf("built particles_form\n");
  
  build_slice_form(gui);
  DEBUG_GUI  printf("built slice_form\n");
  
  build_mouse_form(gui);
  DEBUG_GUI  printf("built mouse_form\n");
  
  build_axis_form(gui);
  DEBUG_GUI  printf("built axis_form\n");
  
  build_contours_form(gui);
  DEBUG_GUI  printf("built contours_form\n");
  
  build_view_params_form(gui);
  DEBUG_GUI  printf("built view_params_form\n");
  
  build_beam_form(gui);
  DEBUG_GUI  printf("built beam_form\n");
  
  build_ray_track_form(gui);
  DEBUG_GUI  printf("built ray_track_form\n");
  
  build_texture_form(gui);
  DEBUG_GUI  printf("built texture_form\n");
  
  build_options_selector(gui);
  DEBUG_GUI  printf("built options_selector\n");

  build_main_window(gui);
  DEBUG_GUI  printf("built main_window\n"); 
  
  CreatePopupMenu(gui);  
  DEBUG_GUI  printf("built the Popup Menu for the display form\n"); 

  build_colorwash_levels_popup(gui);
  DEBUG_GUI  printf("built the Popup Menu for the contour levels\n"); 

  build_clipping_dialog(gui);
  DEBUG_GUI  printf("built the clipping dialog\n"); 

  XtAddCallback (gui->clipping_panel.numeric_button, XmNactivateCallback, Show_clipping_dialogCB, (XtPointer)gui);

  /*printf("calling XtAddCallback for gui->clipping_dialog.numeric_button\n");
  XtAddCallback (gui->clipping_dialog.numeric_button, XmNactivateCallback, Show_clipping_dialogCB, (XtPointer)gui);
printf("done\n");
  */

  DEBUG_TRACE_OUT printf("Done with build_gui\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : ResetButtonCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (callback)
%%%
%%%  Purpose: resets the camera position, and rotation back
%%%           to the default.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void ResetButtonCB (Widget w, XtPointer clientData, XtPointer callData)
{
  XmString xmstr;
  main_gui_t *gui = (main_gui_t *)clientData;

  DEBUG_TRACE_IN printf("Entered ResetButtonCB\n");
  
  DisplayBusyCursor(gui->form);
  
  gui->beam_line_view = 0;
  /*************************************************/
  /** Reset the camera position.**/
  /*************************************************/
  gui->cam.x =  0.0; 
  gui->cam.y = 200.0; 
  gui->cam.z = 500.0;
  
  /*************************************************/
  /** Reset the camera position.**/
  /*************************************************/
  gui->cam.at_x = 0.0; 
  gui->cam.at_y = 0.0; 
  gui->cam.at_z = 0.0;

  /*************************************************/
  /** Reset the camera position.**/
  /*************************************************/
  gui->cam.up_x = 0.0; 
  gui->cam.up_y = 1.0; 
  gui->cam.up_z = 0.0;
  
  /*************************************************/
  /** Reset the object position.**/
  /*************************************************/
  gui->rotation_y = 0; 
  gui->rotation_x = 0; 
  gui->rotation_z = 0;
  
  if (gui->mouse_control_method == SLIDER) {
    xmstr = XmStringCreateLocalized("0");
    XtVaSetValues(gui->slider_panel.slider_x_numeric_label,
		  XmNlabelString, xmstr,
		  NULL);
    XtVaSetValues(gui->slider_panel.slider_y_numeric_label,
		  XmNlabelString, xmstr,
		  NULL);
    XtVaSetValues(gui->slider_panel.slider_z_numeric_label,
		  XmNlabelString, xmstr,
		  NULL);
    
    XmScaleSetValue(gui->slider_panel.slider_x, gui->rotation_x);
    XmScaleSetValue(gui->slider_panel.slider_y, gui->rotation_z);
    XmScaleSetValue(gui->slider_panel.slider_z, gui->rotation_y);
    
    XmStringFree(xmstr);
  }

  XmScaleSetValue(gui->view_panel.scaling_slider, 100);
  gui->scaling = 1.0;
  
  /*************************************************/
  /** Change the camera position**/
  /*************************************************/
  /* glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);*/
  
  glMatrixMode (GL_PROJECTION);	/*  prepare for and then  */ 
  glLoadIdentity ();	/*  define the projection  */
  glFrustum (-20, 20, -20, 20, gui->front_z, gui->back_z);      
  glMatrixMode (GL_MODELVIEW);	/*  back to modelview matrix	*/
  
  glLoadIdentity ();	/*  clear the matrix	*/
  /*printf("In Reset cam_pos is : %f, %f, %f\n",
    cam_pos_x,cam_pos_y,cam_pos_z);*/
  gluLookAt(gui->cam.x,gui->cam.y,gui->cam.z,
	    gui->cam.at_x,gui->cam.at_y,gui->cam.at_z,
	    gui->cam.up_x,gui->cam.up_y,gui->cam.up_z);  
  
  
  /*************************************************/
  /** Redraw with the reset view.**/
  /*************************************************/
  draw_all(gui);
  
  if (gui->current_camera != 0)
    XtVaSetValues(gui->view_panel.camera[(int)gui->current_camera], XmNset, FALSE, NULL);     
  gui->current_camera = 0;

  RemoveBusyCursor(gui->form);

  DEBUG_TRACE_OUT printf("Done with ResetButtonCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Camera_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: changes the camera position to the 
%%%           corresponding widget
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Camera_ChangedCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  int old_camera;
  main_gui_t *gui = (main_gui_t *)clientdata;
  int i;

  DEBUG_TRACE_IN printf("Entered Camera_ChangedCB\n");
  
  DisplayBusyCursor(gui->form);
  
  old_camera = gui->current_camera;
  
  for (i = 0;i<6;i++)
    if (gui->view_panel.camera[i] == w)
      gui->current_camera = i;

  if (old_camera == gui->current_camera) {
    XtVaSetValues(gui->view_panel.camera[old_camera], XmNset, TRUE, NULL);
    RemoveBusyCursor(gui->form);
    DEBUG_TRACE_OUT printf("Done with CameraChangedCB\n");
    return;
  }

  switch(gui->current_camera){
  case 1:
    /*************************************************/
    /** Move camera position to the front.
    /*************************************************/
    gui->cam.x =  0.0;   gui->cam.y = 100.0;  gui->cam.z = 500.0;
    gui->cam.at_x = 0.0; gui->cam.at_y = 0.0; gui->cam.at_z = 0.0;
    gui->cam.up_x = 0.0; gui->cam.up_y = 1.0; gui->cam.up_z = 0.0;
    break;
  case 2:
    /*************************************************/
    /** Move the camera position to the left side.
    /*************************************************/
    gui->cam.x = -450.0; gui->cam.y = 100.0;  gui->cam.z = 0.0;
    gui->cam.at_x = 0.0; gui->cam.at_y = 0.0; gui->cam.at_z = 0.0;
    gui->cam.up_x = 0.0; gui->cam.up_y = 1.0; gui->cam.up_z = 0.0;
    break;
  case 3:
    /*********************************************/
    /** Move the camera position to the right side.
    /*************************************************/
    gui->cam.x = 450.0;  gui->cam.y = 100.0;  gui->cam.z = 0.0;
    gui->cam.at_x = 0.0; gui->cam.at_y = 0.0; gui->cam.at_z = 0.0;
    gui->cam.up_x = 0.0, gui->cam.up_y = 1.0, gui->cam.up_z = 0.0;
    break;
  case 4:
    /*************************************************/
    /** Move the camera behind the objects.
      /*************************************************/
    gui->cam.x = 0.0;    gui->cam.y = 100.0;  gui->cam.z = -450.0;
    gui->cam.at_x = 0.0; gui->cam.at_y = 0.0; gui->cam.at_z = 0.0;
    gui->cam.up_x = 0.0, gui->cam.up_y = 1.0, gui->cam.up_z = 0.0;
    break;
  case 5:
    /*************************************************/
    /** Move the camera above the objects.
    /*************************************************/
    gui->cam.x = 0.0;    gui->cam.y = 500.0;  gui->cam.z = 0.0;
    gui->cam.at_x = 0.0; gui->cam.at_y = 5.0; gui->cam.at_z = 0.0;
    gui->cam.up_x = 0.0; gui->cam.up_y = 0.0; gui->cam.up_z = -1.0;
    break;
  }

  if (old_camera != 0)
    XtVaSetValues(gui->view_panel.camera[old_camera], XmNset, FALSE, NULL);
   
  /*************************************************/
  /** Reset the object position.
  /*************************************************/
  gui->rotation_y = 0; gui->rotation_x = 0; gui->rotation_z = 0;
  

  /*printf("In CameraChanged cam_pos is : %f, %f, %f\n",
       cam_pos_x, cam_pos_y, cam_pos_z);*/
  /*************************************************/
  /** Change the camera position
  /*************************************************/
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT |
	  GL_STENCIL_BUFFER_BIT);
  glLoadIdentity ();	/*  clear the matrix	*/
  gluLookAt(gui->cam.x,gui->cam.y,gui->cam.z,
	    gui->cam.at_x,gui->cam.at_y,gui->cam.at_z,
	    gui->cam.up_x,gui->cam.up_y,gui->cam.up_z);  
  
  /*************************************************/
  /** Redraw with the new view..
  /*************************************************/
  draw_all(gui);
  
  RemoveBusyCursor(gui->form);

  DEBUG_TRACE_OUT printf("Done with CameraChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : CreateMenuBar
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: takes the parent of the menubar
%%%
%%%  Purpose: builds a menubar
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void CreateMenuBar (main_gui_t *gui)
{  
  DEBUG_TRACE_IN printf("Entered CreateMenuBar\n");

  /*************************************************/
  /** Build the Menubar.
  /*************************************************/  
  gui->menubar = (Widget)XmCreateMenuBar(gui->mainwindow, "menu", NULL, 0);

  /*************************************************/
  /** Create the submenus.
  /*************************************************/
  CreateFilePane(gui);
  CreateOptionsPane(gui);
  CreatePreferencesPane(gui);
  CreateHelpPane(gui);
  
  XtManageChild(gui->menubar);

  DEBUG_TRACE_OUT printf("Done with CreateMenuBar\n");
  /*return (gui->menu);*/
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : CreateFilePane
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: takes the parent of the file pane
%%%
%%%  Purpose: builds the File pulldown menu
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void CreateFilePane(main_gui_t *gui)
{

  DEBUG_TRACE_IN printf("Entered CreateFilePane\n");



  /*************************************************/
  /** Create the File Pulldown .
  /*************************************************/
  gui->file_menu.main_menu = (Widget)XmCreatePulldownMenu(gui->menubar, "fileSubmenu", NULL,0);
  
  gui->file_menu.main_cascade = XtVaCreateManagedWidget("File", xmCascadeButtonWidgetClass,
							gui->menubar,
							XmNsubMenuId, gui->file_menu.main_menu,
							NULL);
  
  /*** BUILD the Load Regions menu ***/
  gui->file_menu.load_regions.num_submenus = 0;
  gui->file_menu.load_regions.menu = XmCreatePulldownMenu(gui->file_menu.main_menu,"ls",NULL,0);
  gui->file_menu.load_regions.pane = XtVaCreateManagedWidget("Load Regions(UV/UVH)", 
							     xmCascadeButtonWidgetClass,
							     gui->file_menu.main_menu,
							     XmNsubMenuId, gui->file_menu.load_regions.menu,
							     NULL);
  get_recent_files_and_build_widgets(gui,gui->file_menu.load_regions.menu,1);
  gui->file_menu.load_regions.submenu[0] = XtCreateManagedWidget ("Use FSB", xmPushButtonWidgetClass,
								  gui->file_menu.load_regions.menu, NULL, 0);
  XtAddCallback(gui->file_menu.load_regions.submenu[0], XmNactivateCallback, LoadUnivelFileCallback, (XtPointer)gui);




  /*** BUILD the Load Raw Images menu ***/
  gui->file_menu.load_images.num_submenus = 0;
  gui->file_menu.load_images.menu = XmCreatePulldownMenu(gui->file_menu.main_menu,"ls",NULL,0);
  gui->file_menu.load_images.pane = XtVaCreateManagedWidget("Load Images (QIM/QHD)", 
							     xmCascadeButtonWidgetClass,
							     gui->file_menu.main_menu,
							     XmNsubMenuId, gui->file_menu.load_images.menu,
							     NULL);
  get_recent_files_and_build_widgets(gui,gui->file_menu.load_images.menu,2);
  gui->file_menu.load_images.submenu[0] = XtCreateManagedWidget ("Use FSB", xmPushButtonWidgetClass,
								  gui->file_menu.load_images.menu, NULL, 0);
  XtAddCallback(gui->file_menu.load_images.submenu[0], XmNactivateCallback, Load_RawsCB, (XtPointer)gui);
  XtVaSetValues(gui->file_menu.load_images.pane,XmNsensitive,False,NULL);



  /*** BUILD the Load Paths menu ***/
  gui->file_menu.load_paths.num_submenus = 0;
  gui->file_menu.load_paths.menu = XmCreatePulldownMenu(gui->file_menu.main_menu,"ls",NULL,0);
  gui->file_menu.load_paths.pane = XtVaCreateManagedWidget("Load Paths (PP)", 
							     xmCascadeButtonWidgetClass,
							     gui->file_menu.main_menu,
							     XmNsubMenuId, gui->file_menu.load_paths.menu,
							     NULL);
  get_recent_files_and_build_widgets(gui,gui->file_menu.load_paths.menu,3);
  gui->file_menu.load_paths.submenu[0] = XtCreateManagedWidget ("Use FSB", xmPushButtonWidgetClass,
								  gui->file_menu.load_paths.menu, NULL, 0);
  XtAddCallback(gui->file_menu.load_paths.submenu[0], XmNactivateCallback, SelectParticleFileCallback, (XtPointer)gui);
  XtVaSetValues(gui->file_menu.load_paths.pane,XmNsensitive,False,NULL);


  /*** BUILD the Load Single Contour menu ***/
  gui->file_menu.load_single_contour.num_submenus = 0;
  gui->file_menu.load_single_contour.menu = XmCreatePulldownMenu(gui->file_menu.main_menu,"ls",NULL,0);
  gui->file_menu.load_single_contour.pane = XtVaCreateManagedWidget("Load Single Contour File", 
							     xmCascadeButtonWidgetClass,
							     gui->file_menu.main_menu,
							     XmNsubMenuId, gui->file_menu.load_single_contour.menu,
							     NULL);
  get_recent_files_and_build_widgets(gui,gui->file_menu.load_single_contour.menu,4);
  gui->file_menu.load_single_contour.submenu[0] = XtCreateManagedWidget ("Use FSB", xmPushButtonWidgetClass,
								  gui->file_menu.load_single_contour.menu, NULL, 0);
  XtAddCallback(gui->file_menu.load_single_contour.submenu[0], XmNactivateCallback, Load_Single_Contour_FileCB, (XtPointer)gui);
  XtVaSetValues(gui->file_menu.load_single_contour.pane,XmNsensitive,False,NULL);


  /*** BUILD the Load Full Contour menu ***/
  gui->file_menu.load_full_contour.num_submenus = 0;
  gui->file_menu.load_full_contour.menu = XmCreatePulldownMenu(gui->file_menu.main_menu,"ls",NULL,0);
  gui->file_menu.load_full_contour.pane = XtVaCreateManagedWidget("Load Full Contour File", 
							     xmCascadeButtonWidgetClass,
							     gui->file_menu.main_menu,
							     XmNsubMenuId, gui->file_menu.load_full_contour.menu,
							     NULL);
  get_recent_files_and_build_widgets(gui,gui->file_menu.load_full_contour.menu,5);
  gui->file_menu.load_full_contour.submenu[0] = XtCreateManagedWidget ("Use FSB", xmPushButtonWidgetClass,
								  gui->file_menu.load_full_contour.menu, NULL, 0);
  XtAddCallback(gui->file_menu.load_full_contour.submenu[0], XmNactivateCallback, Load_Full_Contour_FileCB, (XtPointer)gui);
  XtVaSetValues(gui->file_menu.load_full_contour.pane,XmNsensitive,False,NULL);


  /*** BUILD the Launch menu ***/
  /*
   * Added LT_make_launch_menu() 1-7-99 mbr 
   */

  LT_make_launch_menu( gui->file_menu.main_menu, "sera3d" );


  gui->file_menu.restart_b = XtCreateManagedWidget ("Reset Program", 
						    xmPushButtonWidgetClass,
						    gui->file_menu.main_menu, NULL, 0);
  XtAddCallback (gui->file_menu.restart_b, XmNactivateCallback, ResetProgramCB, (XtPointer)gui);
  XtSetSensitive( gui->file_menu.restart_b, False );

  gui->file_menu.version_b = XtVaCreateManagedWidget("Check Version",
                                                     xmPushButtonWidgetClass,
                                                     gui->file_menu.main_menu, NULL, 0);
  XtAddCallback (gui->file_menu.version_b, XmNactivateCallback, CheckVersionCB, (XtPointer)gui);
  

  gui->file_menu.exit_b = XtCreateManagedWidget ("Exit", 
						 xmPushButtonWidgetClass,
						 gui->file_menu.main_menu, NULL, 0);
  XtAddCallback (gui->file_menu.exit_b, XmNactivateCallback, ExitCallback, (XtPointer)gui);
  
  DEBUG_TRACE_OUT printf("Done with CreateFilePane\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : CreateOptionsPane
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent
%%%
%%%  Purpose: builds the Options pulldown menu
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void CreateOptionsPane(main_gui_t *gui)
{
  int i;

  DEBUG_TRACE_IN printf("Entered CreateOptionsPane\n");

  /*************************************************/
  /** Create the Options Pulldown with 2 buttons:
  /**   axis, multiview.
  /*************************************************/
  gui->options_menu.main_menu = (Widget)XmCreatePulldownMenu (gui->menubar, "optionsSubmenu", NULL, 0);

  gui->options_menu.main_cascade = XtVaCreateManagedWidget ("Options",
							    xmCascadeButtonWidgetClass,
							    gui->menubar,
							    XmNsubMenuId, gui->options_menu.main_menu,
							    NULL);
 	 

  /*CreateCascadingRotationPane(gui->options_menu.main_menu);
  CreateCascadingBackgroundPane(gui->options_menu.main_menu);
  CreateCascadingMainWindowSizePane(gui->options_menu.main_menu);*/

  /*** build the Rotation Submenu ****/
  gui->options_menu.rotation.menu = (Widget)XmCreatePulldownMenu(gui->options_menu.main_menu, "Fast Rotation",NULL,0);
  gui->options_menu.rotation.pane = XtVaCreateManagedWidget("Fast Rotation",
							   xmCascadeButtonWidgetClass, gui->options_menu.main_menu,
							   XmNsubMenuId, gui->options_menu.rotation.menu,
							   NULL);
  gui->options_menu.rotation.submenu[0]= XtCreateManagedWidget ("by Wireframe",
							       xmPushButtonWidgetClass,
							       gui->options_menu.rotation.menu, NULL, 0);
  XtAddCallback ( gui->options_menu.rotation.submenu[0], XmNactivateCallback, (XtCallbackProc)FastRotation_changedCB, (XtPointer)gui);
  gui->options_menu.rotation.submenu[1] = XtCreateManagedWidget ("by Axis",
								xmPushButtonWidgetClass,
								gui->options_menu.rotation.menu, NULL, 0);
  XtAddCallback ( gui->options_menu.rotation.submenu[1], XmNactivateCallback,	(XtCallbackProc)FastRotation_changedCB, (XtPointer)gui);
  gui->options_menu.rotation.submenu[2] = XtCreateManagedWidget ("off",
								xmPushButtonWidgetClass,
								gui->options_menu.rotation.menu, NULL, 0);
  XtAddCallback ( gui->options_menu.rotation.submenu[2], XmNactivateCallback, (XtCallbackProc)FastRotation_changedCB, (XtPointer)gui);	

  /*** build the Background Submenu ****/
  gui->options_menu.background.menu = (Widget)XmCreatePulldownMenu(gui->options_menu.main_menu, "Background",NULL,0);
  
  gui->options_menu.background.pane = XtVaCreateManagedWidget("Background",
							      xmCascadeButtonWidgetClass,gui->options_menu.main_menu,
							      XmNsubMenuId, gui->options_menu.background.menu,
							      NULL);

  gui->options_menu.background.submenu[0] = XtCreateManagedWidget ("Black",
								   xmPushButtonWidgetClass,
								   gui->options_menu.background.menu, NULL, 0);
  gui->options_menu.background.submenu[1] = XtCreateManagedWidget ("25% Gray",
								   xmPushButtonWidgetClass,
								   gui->options_menu.background.menu, NULL, 0);
  gui->options_menu.background.submenu[2] = XtCreateManagedWidget ("50% Gray",
								   xmPushButtonWidgetClass,
								   gui->options_menu.background.menu, NULL, 0);
  gui->options_menu.background.submenu[3] = XtCreateManagedWidget ("75% Gray",
								   xmPushButtonWidgetClass,
								   gui->options_menu.background.menu, NULL, 0);
  gui->options_menu.background.submenu[4] = XtCreateManagedWidget ("White",
								   xmPushButtonWidgetClass,
								   gui->options_menu.background.menu, NULL, 0);
  
  for (i=0;i<5;i++)
    XtAddCallback (gui->options_menu.background.submenu[i], XmNactivateCallback, Background_typeCB, (XtPointer)gui);


  /*** build the MainWindowsize Menu ****/
  gui->options_menu.main_win.menu = (Widget)XmCreatePulldownMenu(gui->options_menu.main_menu, "Background",NULL,0);
  
  gui->options_menu.main_win.pane = XtVaCreateManagedWidget("Main Window Size",
							    xmCascadeButtonWidgetClass,
							    gui->options_menu.main_menu,
							    XmNsubMenuId, gui->options_menu.main_win.menu,
							    NULL);

  gui->options_menu.main_win.submenu[0] = XtCreateManagedWidget ("550 X 550",
								 xmPushButtonWidgetClass,
								 gui->options_menu.main_win.menu, NULL, 0);
  XtAddCallback ( gui->options_menu.main_win.submenu[0], XmNactivateCallback, MainWindowSizeChangedCB, (XtPointer)gui);

  gui->options_menu.main_win.submenu[1] = XtCreateManagedWidget ("600 X 600",
								 xmPushButtonWidgetClass,
								 gui->options_menu.main_win.menu, NULL, 0);  
  XtAddCallback ( gui->options_menu.main_win.submenu[1], XmNactivateCallback,  MainWindowSizeChangedCB, (XtPointer)gui);

  gui->options_menu.main_win.submenu[2] = XtCreateManagedWidget ("650 X 650",
								 xmPushButtonWidgetClass,
								 gui->options_menu.main_win.menu, NULL, 0);  
  XtAddCallback ( gui->options_menu.main_win.submenu[2], XmNactivateCallback, 	MainWindowSizeChangedCB, (XtPointer)gui);

  gui->options_menu.main_win.submenu[3] = XtCreateManagedWidget ("700 X 700",
								 xmPushButtonWidgetClass,
								 gui->options_menu.main_win.menu, NULL, 0);  
  XtAddCallback (gui->options_menu.main_win.submenu[3], XmNactivateCallback, MainWindowSizeChangedCB, (XtPointer)gui);

  gui->options_menu.main_win.submenu[4] = XtCreateManagedWidget ("750 X 750",
								 xmPushButtonWidgetClass,
								 gui->options_menu.main_win.menu, NULL, 0);  
  XtAddCallback ( gui->options_menu.main_win.submenu[4], XmNactivateCallback, MainWindowSizeChangedCB, (XtPointer)gui);

  gui->options_menu.main_win.submenu[5] = XtCreateManagedWidget ("800 X 800",
								 xmPushButtonWidgetClass,
								 gui->options_menu.main_win.menu, NULL, 0);  
  XtAddCallback ( gui->options_menu.main_win.submenu[5], XmNactivateCallback, MainWindowSizeChangedCB, (XtPointer)gui);



  /*** build the MultiWindowsize Submenu ****/
  gui->options_menu.multi_win.menu = (Widget)XmCreatePulldownMenu(gui->options_menu.main_menu, "Background",NULL,0);
  
  gui->options_menu.multi_win.pane = XtVaCreateManagedWidget("Multi-View Window Size",
							     xmCascadeButtonWidgetClass,gui->options_menu.main_menu,
							     XmNsubMenuId, gui->options_menu.multi_win.menu,
							     NULL);

  gui->options_menu.multi_win.submenu[0]= XtCreateManagedWidget ("150 X 150",
								 xmPushButtonWidgetClass,
								 gui->options_menu.multi_win.menu, NULL, 0);
  XtAddCallback ( gui->options_menu.multi_win.submenu[0], XmNactivateCallback,	MultiWindowSizeChangedCB, (XtPointer)gui);
  
  gui->options_menu.multi_win.submenu[1] = XtCreateManagedWidget ("175 X 175",
								  xmPushButtonWidgetClass,
								  gui->options_menu.multi_win.menu, NULL, 0);  
  XtAddCallback ( gui->options_menu.multi_win.submenu[1], XmNactivateCallback, MultiWindowSizeChangedCB, (XtPointer)gui);
  
  gui->options_menu.multi_win.submenu[2] = XtCreateManagedWidget ("200 X 200",
								  xmPushButtonWidgetClass,
								  gui->options_menu.multi_win.menu, NULL, 0);  
  XtAddCallback ( gui->options_menu.multi_win.submenu[2], XmNactivateCallback,	MultiWindowSizeChangedCB, (XtPointer)gui);
  
  gui->options_menu.multi_win.submenu[3] = XtCreateManagedWidget ("225 X 225",
								  xmPushButtonWidgetClass,
								  gui->options_menu.multi_win.menu, NULL, 0);  
  XtAddCallback (gui->options_menu.multi_win.submenu[3] , XmNactivateCallback, MultiWindowSizeChangedCB, (XtPointer)gui);
  
  gui->options_menu.multi_win.submenu[4] = XtCreateManagedWidget ("250 X 250",
								  xmPushButtonWidgetClass,
								  gui->options_menu.multi_win.menu, NULL, 0);  
  XtAddCallback ( gui->options_menu.multi_win.submenu[4], XmNactivateCallback, MultiWindowSizeChangedCB, (XtPointer)gui);





  gui->options_menu.multi_view_toggle = XtCreateManagedWidget ("Multi-View", 
							       xmToggleButtonWidgetClass,
							       gui->options_menu.main_menu, NULL, 0);
  if (gui->multi_view) XtVaSetValues(gui->options_menu.multi_view_toggle, XmNset, TRUE, NULL);

  XtAddCallback (gui->options_menu.multi_view_toggle, XmNvalueChangedCallback, Multi_viewCB, (XtPointer)gui);
	 	 
  gui->options_menu.messages_toggle = XtVaCreateManagedWidget ("Messages", 
							       xmToggleButtonWidgetClass, gui->options_menu.main_menu,
							       NULL);
  if (gui->messages_on) XtVaSetValues(gui->options_menu.messages_toggle, XmNset, TRUE, NULL);  
  XtAddCallback (gui->options_menu.messages_toggle, XmNvalueChangedCallback, Messages_toggleCB, (XtPointer)gui);

  DEBUG_TRACE_OUT printf("Done with CreateOptionsPane\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : CreatePreferencesPane
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent
%%%
%%%  Purpose: builds the Options pulldown menu
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void CreatePreferencesPane(main_gui_t *gui)
{
  Widget cascade, submenu, button1;

  DEBUG_TRACE_IN printf("Entered CreatePreferencesPane\n");

  /*************************************************/
  /** Create the Preferences Pulldown with 1 button:
  /**   set preferences
  /*************************************************/
  submenu = (Widget)XmCreatePulldownMenu (gui->menubar, "preferencesSubmenu", 
					  NULL, 0);

  cascade = XtVaCreateManagedWidget ("Preferences",
				     xmCascadeButtonWidgetClass,
				     gui->menubar,
				     XmNsubMenuId, submenu,
				     NULL);
  
  button1 = XtVaCreateManagedWidget ("Preferences . . .", 
				     xmPushButtonWidgetClass,
				     submenu, NULL);

  build_preferences_dialog(gui);
  
  XtAddCallback (button1, XmNactivateCallback, 	Show_preferences_dialogCB, (XtPointer)gui);

  DEBUG_TRACE_OUT printf("Done with CreatePreferencesPane\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : CreateHelpPane 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent
%%%
%%%  Purpose: builds the Help pulldown menu
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void CreateHelpPane(main_gui_t *gui)
{
  Widget cascade, submenu, button1;

  DEBUG_TRACE_IN printf("Entered CreateHelpPane\n");

  /*************************************************/
  /** Create the Help Pulldown
  /*************************************************/
  submenu = (Widget)XmCreatePulldownMenu (gui->menubar, "helpSubmenu", NULL, 0);

  cascade = XtVaCreateManagedWidget ("Help",
				     xmCascadeButtonWidgetClass,
				     gui->menubar,
				     XmNsubMenuId, submenu,
				     NULL);

  XtVaSetValues(gui->menubar, XmNmenuHelpWidget, cascade, NULL);

  button1 = XtCreateManagedWidget("HelponContext", xmPushButtonWidgetClass,
				 submenu, NULL, 0);
  
  set_preference_directory((char *)"/Sera3d/");
  XtAddCallback (button1, XmNactivateCallback,
		 ContextHelpCallback, (XtPointer)gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with CreateHelpPane\n");
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : CreateCascadingMultiWindowSizePane
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: parent widget
%%%
%%%  Purpose: creates a cascading menu off of the multi window
%%%           size menu item
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void CreateCascadingMultiWindowSizePane(Widget parent)
{
  DEBUG_TRACE_IN printf("Entered CreateCascadingMultiWindowSizePane\n");
  


  DEBUG_TRACE_OUT printf("Done with CreateCascadingMultiWindowSizePane\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Background_typeCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: changes the color of the background
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Background_typeCB(Widget w, XtPointer clientData,XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int type = 0;
  int i;

  DEBUG_TRACE_IN printf("Entered Background_typeCB\n");

  for (i=0;i<5;i++)
    if (w == gui->options_menu.background.submenu[i])
      type = i;

  /*type = 1;*/
  DisplayBusyCursor(gui->mainwindow);
  
  switch(type){
    case 0:glClearColor(0.0,0.0,0.0,0.0); gui->background=0.0; break;
    case 1:glClearColor(0.25,0.25,0.25,0.0); gui->background=.25; break;
    case 2:glClearColor(0.5,0.5,0.5,0.0); gui->background=.5; break;
    case 3:glClearColor(0.75,0.75,0.75,0.0);gui->background=.75; break;
    case 4:glClearColor(1.0,1.0,1.0,0.0);gui->background = 1.0; break;
  }
  draw_all(gui);
  
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Background_typeCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : MainWindowSizeChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: changes the main window size
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void MainWindowSizeChangedCB(Widget w, XtPointer clientData,XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;

  DEBUG_TRACE_IN printf("Entered MainWindowSizeChangedCB\n");

  XtVaSetValues(gui->toplevel, XmNallowShellResize, TRUE, NULL);
  DisplayBusyCursor(gui->mainwindow);

  if (strcmp(XtName(w),"550 X 550") == 0)
    set_window_sizes(gui,550, gui->multiwindowsize, 0);
  else if (strcmp(XtName(w),"600 X 600") == 0)
    set_window_sizes(gui,600, gui->multiwindowsize, 0);
  else if (strcmp(XtName(w),"650 X 650") == 0)
    set_window_sizes(gui,650, gui->multiwindowsize, 0);
  else if (strcmp(XtName(w),"700 X 700") == 0)
    set_window_sizes(gui,700, gui->multiwindowsize, 0);
  else if (strcmp(XtName(w),"750 X 750") == 0)
    set_window_sizes(gui,750, gui->multiwindowsize, 0);
  else if (strcmp(XtName(w),"800 X 800") == 0)
    set_window_sizes(gui,800, gui->multiwindowsize, 0);

  glViewport(0, 0, gui->mainwindowsize, gui->mainwindowsize);

  draw_all(gui);

  XtVaSetValues(gui->toplevel, XmNallowShellResize, FALSE, NULL);
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with MainWindowSizeChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : auto_set_window_sizes
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: priority level
%%%
%%%  Purpose: window sizes are auto set to fit screen if the
%%%           sizes are changed
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void auto_set_window_sizes(main_gui_t *gui,int priority)
{
  int main_size, multi, ok = 0;

  DEBUG_TRACE_IN printf("Entered auto_set_window_sizes\n");

  main_size = gui->mainwindowsize;
  multi = gui->multiwindowsize;

  XtVaSetValues(gui->toplevel, XmNallowShellResize, TRUE, NULL);
  /*printf("auto_setting window sizes, main_size : %d, multi : %d\n",
	 main,multi);*/
  while (!ok)
    {
      if (gui->mouse_control_method == SLIDER && gui->Screenheight < main_size + 350)
	{}
      else if (main_size + 100 > gui->Screenheight)
	{}
      /** check the width **/
      else if (multi+main_size+350 > gui->Screenwidth && 
	       XtIsManaged(gui->multiview_panel.form))
	{
	  /*printf("multi+main is : %d, too much!\n",multi+main);*/
	}
      else if (main_size+300 > gui->Screenwidth &&
	       !XtIsManaged(gui->multiview_panel.form))
	{}
      else ok = 1;
      
      if (!ok){
	/*printf("not enough room, cutting the main by 50\n");*/
	main_size -= 50;
      }
    }
  
  gui->mainwindowsize = main_size;
  XtVaSetValues(gui->glxarea,XmNwidth, main_size, XmNheight, main_size, NULL);
  XtVaSetValues(gui->toplevel, XmNallowShellResize, FALSE, NULL);

  DEBUG_TRACE_OUT printf("done with auto_set_window_sizes\n");  
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : set_window_sizes
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: window sizes
%%%
%%%  Purpose: tries to set the window sizes, and checks to see
%%%           if the new size will fit the screen or not
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void set_window_sizes(main_gui_t *gui,int main, int multi, int priority)
{

  DEBUG_TRACE_IN printf("Entered set_window_sizes\n");
  
  XtVaSetValues(gui->toplevel, XmNallowShellResize, TRUE, NULL);
  if (priority == 0)
    {
      /*** main window priority ***/
      /** check the height **/
      if (gui->mouse_control_method == SLIDER && gui->Screenheight < main + 256)
	DT_error(gui->toplevel,"Sorry not enough room with the sliders active",NULL,NULL);
      else if (gui->Screenheight < main + 100)
	DT_error(gui->toplevel,"Sorry the screen is not tall enough",NULL,NULL);
      /** check the width **/
      else if (multi+main > gui->Screenwidth-300 &&  XtIsManaged(gui->multiview_panel.form))
	DT_error(gui->toplevel,"Sorry the screen is not wide enough",NULL,NULL);
      else if (main > gui->Screenwidth -300 && !XtIsManaged(gui->multiview_panel.form))
	DT_error(gui->toplevel,"Sorry the screen is not wide enough",NULL,NULL);
      else{
	gui->mainwindowsize = main;
	XtVaSetValues(gui->glxarea,XmNwidth, main,XmNheight, main, NULL);
      }
    } else {
      /*** multi window priority ***/
      /** check the height **/
      if (gui->mouse_control_method == SLIDER && gui->Screenheight < (multi+10)*3 + 256)
	DT_error(gui->toplevel,"Sorry not enough room with the sliders active",NULL,NULL);
      else if (gui->Screenheight < (multi+10)*3 + 100)
	DT_error(gui->toplevel,"Sorry the screen is not tall enough",NULL,NULL);
      /** check the width **/
      else if (multi+main > gui->Screenwidth-300)
	DT_error(gui->toplevel,"Sorry the screen is not wide enough",NULL,NULL);
      else{
	gui->multiwindowsize = multi;
	XtVaSetValues(gui->multiview_panel.gl_lview_area,XmNwidth,multi,XmNheight, multi, NULL);
	XtVaSetValues(gui->multiview_panel.gl_rview_area,XmNwidth,multi,XmNheight, multi, NULL);
	XtVaSetValues(gui->multiview_panel.gl_tview_area,XmNwidth,multi,XmNheight, multi, NULL);
      }
    }
  XtVaSetValues(gui->toplevel, XmNallowShellResize, FALSE, NULL);

  DEBUG_TRACE_OUT printf("Done with set_window_sizes\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : MultiWindowSizeChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: sets the multi window size 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void MultiWindowSizeChangedCB(Widget w, XtPointer clientData,
			      XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  DEBUG_TRACE_IN printf("Entered MultiWindowSizeChangedCB\n");

  XtVaSetValues(gui->toplevel, XmNallowShellResize, TRUE, NULL);
  DisplayBusyCursor(gui->mainwindow);

  if (strcmp(XtName(w),"150 X 150") == 0)
      set_window_sizes(gui,gui->mainwindowsize, 150, 1);
  else if (strcmp(XtName(w),"175 X 175") == 0)
      set_window_sizes(gui,gui->mainwindowsize, 175, 1);
  else if (strcmp(XtName(w),"200 X 200") == 0)
      set_window_sizes(gui,gui->mainwindowsize, 200, 1);
  else if (strcmp(XtName(w),"225 X 225") == 0)
      set_window_sizes(gui,gui->mainwindowsize, 225, 1);
  else if (strcmp(XtName(w),"250 X 250") == 0)
      set_window_sizes(gui,gui->mainwindowsize, 250, 1);

  RemoveBusyCursor(gui->mainwindow);
  XtVaSetValues(gui->toplevel, XmNallowShellResize, FALSE, NULL);

  DEBUG_TRACE_OUT printf("Done with MultiWindowSizeChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : FastRotation_changedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles between the 3 rotation speed settings
%%%            wireframe, axis, off
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void FastRotation_changedCB(Widget w, XtPointer clientData,XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  DEBUG_TRACE_IN printf("Entered FastRotation_changedCB\n");

  DisplayBusyCursor(gui->mainwindow);
  
  switch(gui->fast_rotation_type){
  case 1:
  case 2:
    XtRemoveEventHandler(gui->glxarea,ButtonPressMask, FALSE, Set_High_Lists, (XtPointer)gui); 
    XtRemoveEventHandler(gui->glxarea,ButtonReleaseMask, FALSE,	Unset_High_Lists, (XtPointer)gui); 
    break;
  case 3:
    XtRemoveEventHandler(gui->glxarea,ButtonPressMask, FALSE, Set_High_Lists, (XtPointer)gui); 
    XtRemoveEventHandler(gui->glxarea,ButtonReleaseMask, FALSE, Unset_High_Lists, (XtPointer)gui); 
    break;
  }
  
  if (strcmp(XtName(w),"by Wireframe") == 0){
    gui->fast_rotation_type = 1;
    XtAddEventHandler(gui->glxarea,ButtonPressMask, FALSE, Set_High_Lists, (XtPointer)gui); 
    XtAddEventHandler(gui->glxarea,ButtonReleaseMask, FALSE, Unset_High_Lists, (XtPointer)gui);
  }else if (strcmp(XtName(w),"by Axis") == 0){
    gui->fast_rotation_type = 2;
    XtAddEventHandler(gui->glxarea,ButtonPressMask, FALSE, Set_High_Lists, (XtPointer)gui); 
    XtAddEventHandler(gui->glxarea,ButtonReleaseMask, FALSE, Unset_High_Lists, (XtPointer)gui);
  }else if (strcmp(XtName(w),"off") == 0)  
    gui->fast_rotation_type = 3;
  
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with FastRotation_changedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Messages_toggleCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the message bar on/off
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Messages_toggleCB(Widget w, XtPointer clientData,XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  DEBUG_TRACE_IN printf("Entered Messages_ToggledCB\n");

  XtVaSetValues(gui->toplevel, XmNallowShellResize, TRUE, NULL);
  DisplayBusyCursor(gui->mainwindow);

  if (gui->messages_on) {
    XtUnmanageChild(gui->message_panel.frame);
    gui->messages_on = 0;
  }else {
    XtManageChild(gui->message_panel.frame);
    gui->messages_on = 1;
  }
  RemoveBusyCursor(gui->mainwindow);
  XtVaSetValues(gui->toplevel, XmNallowShellResize, FALSE, NULL);

  DEBUG_TRACE_OUT printf("Done with Messages_ToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Multi_viewCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (callback)
%%%
%%%  Purpose: Toggles the multi_view from
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Multi_viewCB(Widget w, XtPointer clientData,XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  DEBUG_TRACE_IN printf("Entered Multi_viewCB\n");

  DisplayBusyCursor(gui->mainwindow);

  /*************************************************/
  /** Toggle the multi_view form
  /*************************************************/

  XtVaSetValues(gui->toplevel, XmNallowShellResize, TRUE, NULL);

  if(XtIsManaged(gui->multiview_panel.form))
    {
      gui->multi_view = 0;
      XtUnmanageChild(gui->multiview_panel.divider);
      XtUnmanageChild(gui->multiview_panel.form);

      XtRemoveCallback(gui->multiview_panel.gl_lview_area, GLwNginitCallback, graphicsInit, (XtPointer)gui);
      XtRemoveCallback(gui->multiview_panel.gl_lview_area, GLwNexposeCallback, expose, (XtPointer)gui);
      /*XtRemoveCallback(gl_lview_area, GLwNresizeCallback, resize, NULL);*/
      
      XtRemoveCallback(gui->multiview_panel.gl_rview_area, GLwNginitCallback, graphicsInit, (XtPointer)gui);
      XtRemoveCallback(gui->multiview_panel.gl_rview_area, GLwNexposeCallback, expose, (XtPointer)gui);
      /*XtRemoveCallback(gl_rview_area, GLwNresizeCallback, resize, NULL);*/
      
      XtRemoveCallback(gui->multiview_panel.gl_tview_area, GLwNginitCallback, graphicsInit, (XtPointer)gui);
      XtRemoveCallback(gui->multiview_panel.gl_tview_area, GLwNexposeCallback, expose, (XtPointer)gui);
      /*XtRemoveCallback(gl_tview_area, GLwNresizeCallback, resize, NULL);*/
      glXMakeCurrent(gui->display,XtWindow(gui->glxarea),gui->glxcontext);
   }else{
     gui->multi_view = 1;
     XtManageChild(gui->multiview_panel.divider);

     XtManageChild(gui->multiview_panel.form);
     
     auto_set_window_sizes(gui,1);

     /*printf("setting the multi window sizes to : %d\n",multiwindowsize);*/
     XtVaSetValues(gui->multiview_panel.gl_lview_area,
		   XmNwidth,gui->multiwindowsize,
		   XmNheight,gui->multiwindowsize,NULL);
     XtVaSetValues(gui->multiview_panel.gl_rview_area,
		   XmNwidth,gui->multiwindowsize,
		   XmNheight,gui->multiwindowsize,NULL);
     XtVaSetValues(gui->multiview_panel.gl_tview_area,
		   XmNwidth,gui->multiwindowsize,
		   XmNheight,gui->multiwindowsize,NULL);
     
     XtAddCallback(gui->multiview_panel.gl_lview_area, GLwNginitCallback, graphicsInit, (XtPointer)gui);
     XtAddCallback(gui->multiview_panel.gl_lview_area, GLwNexposeCallback, expose, (XtPointer)gui);
     /*XtAddCallback(gl_lview_area, GLwNresizeCallback, resize, NULL);*/
     
     XtAddCallback(gui->multiview_panel.gl_rview_area, GLwNginitCallback, graphicsInit, (XtPointer)gui);
     XtAddCallback(gui->multiview_panel.gl_rview_area, GLwNexposeCallback, expose, (XtPointer)gui);
     /*XtAddCallback(gl_rview_area, GLwNresizeCallback, resize, NULL);*/
     
     XtAddCallback(gui->multiview_panel.gl_tview_area, GLwNginitCallback, graphicsInit, (XtPointer)gui);
     XtAddCallback(gui->multiview_panel.gl_tview_area, GLwNexposeCallback, expose, (XtPointer)gui);
     /*XtAddCallback(gl_tview_area, GLwNresizeCallback, resize, NULL);*/
   }
  RemoveBusyCursor(gui->mainwindow);

  XtVaSetValues(gui->toplevel, XmNallowShellResize, FALSE, NULL);

  DEBUG_TRACE_OUT printf("Done with Multi_viewCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Process_files
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (callback)
%%%
%%%  Purpose: call the file routines and add the info to the 
%%%           main widgets.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Process_files (Widget w, XtPointer clientData,XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmString xmstr;
  int i;

  DEBUG_TRACE_IN printf("Entered Process_files\n");

  /*************************************************/
  /** If a file is already loaded, close it.
  /*************************************************/
  if(gui->Loaded)
    CloseCallback(w,clientData,callData);
 
  /*************************************************/
  /** Read in the info from the uvh and uv files.
  /*************************************************/
  if(read_uvh_file(gui)){
    /*** passing zero builds the backup point lists ***/
    calculate_bboxes_for_bodies(gui);
    build_complete_body_from_data_block(gui,BODY_STYLE_OUTLINE,WIREFRAME_LISTS);

  }else
    DT_error(gui->toplevel,"Problem reading the uvh file",NULL,NULL);
  
  /*************************************************/
  /** Add the body names to the bodynamelist widget**/
  /*************************************************/
  for (i=0;i<gui->num_bodies;i++) {
   xmstr = XmStringCreateLocalized(gui->bod[i].name);
   XmListAddItem(gui->bodylist, xmstr,i+1);
   XmStringFree(xmstr);
  
    if (i != 0){
      XmListSelectItem(gui->bodylist, XmStringCreateLocalized(gui->bod[i].name),FALSE);
    }
  
  }
  /*
  if (gui->using_defaults)  set_default_body_colors_and_transparency(gui);
  else  smart_set_body_colors(gui);

  for (i=1;i<=gui->num_bodies;i++){
    gui->bod[i].r = rand()%65535;
    gui->bod[i].g = rand()%65535;
    gui->bod[i].b = rand()%65535;
    gui->bod[i].t = rand()%100;
  }
  */

  fill_forms_with_body_names(gui);
  fill_body_clipping_with_bodies(gui);

  /*************************************************/
  /** We now have the info loaded.
  /*************************************************/
  gui->Loaded = 1;

  /*****************************************************/
  /** set the point size **/
  /*****************************************************/
  /*
  gui->solid_point_size = 10.0;
printf("the gui->solid_point_size is : %d\n",gui->solid_point_size);
*/

  build_a_axial_square(gui);
  build_a_coronal_square(gui);
  build_a_sagittal_square(gui);

  /* calculate center of mass for bodies */
  fill_data_block_from_uv( gui );
  find_center_mass_for_bodies_in_data_block( gui );
  free_data_block( &gui->block );

  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Process_files\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : CloseCallback
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (callback)
%%%
%%%  Purpose: Removes all of the info from the main widgets,
%%%           Deletes all of the OpenGL Display Lists.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void CloseCallback(Widget w, XtPointer clientData, XtPointer callData)
{
  int i;
  main_gui_t *gui = (main_gui_t *)clientData;

  DEBUG_TRACE_IN printf("Entered CloseCallback\n");

  DisplayBusyCursor(gui->mainwindow);

  /*************************************************/
  /** Remove the names from the bodynamelist widget,
  /**   and disable the bodies from being drawn
  /*************************************************/
  for (i=0;i<gui->num_bodies;i++){      
    XmListDeleteItem(gui->bodylist, XmStringCreateLocalized(gui->bod[i].name));
    gui->bod[i].enabled = 0;
  }

  /*************************************************/
  /** Delete the GL Display Lists.
  /*************************************************/
  glDeleteLists(1,gui->num_bodies);
  glDeleteLists(HIGH_LISTS,gui->num_bodies);

  /*****************************************************/
  /** Take the names off of the color & transparency
  /** menu widgets.
  /*****************************************************/
  remove_names_from_selectors(gui);
  remove_bodies_from_body_clipping(gui);

  /*************************************************/
  /** No longer have info loaded.
  /*************************************************/
  gui->Loaded = 0;

  /*************************************************/
  /** Free the linked list of grids.              **/
  /*************************************************/
  freeContourGrids( gui->Grids.next );
  init_contour_structures( gui );

  /*************************************************/
  /** Free the images texture map if present      **/
  /*************************************************/
  if (gui->images_loaded) MT_free( (void *) gui->texture_volume );
  gui->texture_volume = NULL;
  gui->images_loaded = 0;
  
  /*************************************************/
  /** Re-initialize the globals, for the next info.
  /*************************************************/
  /*printf("resetting the variables\n");*/
  reset_variables(gui);
  /*printf("done\n");*/
  /*XtRemoveCallback (gui->clipping_dialog.numeric_button, XmNactivateCallback, Show_clipping_dialogCB, (XtPointer)gui); */ 
  /*XtUnrealizeWidget(gui->clipping_dialog.dialog);*/

  
  /*************************************************/
  /** Redraw (black the screen).
  /*************************************************/
  draw_all(gui);

  /*RemoveBusyCursor(gui->mainwindow);*/

  DEBUG_TRACE_OUT printf("Done with CloseCallback\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : DisablebodyCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (callback)
%%%
%%%  Purpose: toggles the body selected from the list widget
%%%           on/off (from being drawn)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void DisablebodyCB (Widget w ,XtPointer clientData, XtPointer callData)
{
  XmListCallbackStruct *cbs = (XmListCallbackStruct *) callData;
  main_gui_t *gui = (main_gui_t *)clientData;

  DEBUG_TRACE_IN printf("Entered DisablebodyCB\n");

  DisplayBusyCursor(gui->mainwindow);
  
  if (cbs->item_position <= gui->num_bodies){
    /*************************************************/
    /** Toggle the body from being drawn
      /*************************************************/
    if (gui->bod[cbs->item_position-1].enabled == 1)
      gui->bod[cbs->item_position-1].enabled = 0;
    else
      gui->bod[cbs->item_position-1].enabled = 1;
  }
  else{
    if (gui->bod[cbs->item_position-1-gui->num_bodies + MAX_BODS].enabled == 1)
      gui->bod[cbs->item_position-1-gui->num_bodies + MAX_BODS].enabled = 0;
    else
      gui->bod[cbs->item_position-1-gui->num_bodies+MAX_BODS].enabled = 1;
  }

  /*************************************************/
  /** Redraw to take effect.
    /*************************************************/
  draw_all(gui);
  
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with DisablebodyCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : ExitCallback
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: exits the program
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void ExitCallback(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;

  DEBUG_TRACE_IN printf("Entered ExitCallback\n");

  /*************************************************/
  /** Quitting time.
  /*************************************************/
  if(gui->Loaded) CloseCallback(w,clientData,callData);
  
  /*****************************************
   * Print unfreed memory if tracing memory
   *****************************************/
  MT_onexit();
  
  exit(0);

  DEBUG_TRACE_OUT printf("Done with ExitCallback\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : DisplayBusyCursor
%%%
%%%  Written by: Youngs book
%%%
%%%  Parameters: widget
%%%
%%%  Purpose: turns the cursor to a watch
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void DisplayBusyCursor(Widget w)
{
  static Cursor cursor = 0;

  DEBUG_TRACE_IN printf("Entered DisplayBusyCursor\n");

  if (!cursor) cursor = XCreateFontCursor(XtDisplay (w), XC_watch);
  XDefineCursor (XtDisplay(w), XtWindow (w), cursor);
  XFlush(XtDisplay (w));
  /*XtAppAddWorkProc (XtWidgetToApplicationContext (w),
    (XtWorkProc)RemoveBusyCursor, (XtPointer)w);*/

  DEBUG_TRACE_OUT printf("Done with DisplayBusyCursor\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : RemoveBusyCursor
%%%
%%%  Written by: Youngs book
%%%
%%%  Parameters: widget
%%%
%%%  Purpose: removes the watch cursor
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
Boolean RemoveBusyCursor (Widget w)
{
  DEBUG_TRACE_IN printf("Entered RemoveBusyCursor\n");

  XUndefineCursor(XtDisplay(w), XtWindow (w));

  DEBUG_TRACE_OUT printf("Done with RemoveBusyCursor\n");
  return (TRUE);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Init_globals
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: initializes all of the global variables.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_globals(main_gui_t *gui)
{
  int i;


  DEBUG_TRACE_IN printf("Entered init_globals\n");

  srand(time(NULL));
  
  gui->scaling = 1.00;
  gui->Loaded = 0;

  gui->use_uvh_colors = 1;

  gui->wireframe_point_size = 1.0;

  gui->current_camera = 0;

  gui->polygonal_algorithm = VERTEXCELL_WITH_VERTEX_NORMALS;

  gui->mouse_control_method = MOUSE;
  fill_contour_colormap(gui);
  init_contour_structures(gui);

  gui->num_contour_surfaces = 0;
  gui->draw_contour_surfaces = 1;

  init_texture_planes(gui);

  gui->solid_point_size = 1.0;
  gui->wireframe_point_size = 1.0;

  gui->num_particle_paths = 0;

  gui->lmodel_ambient[0] = .1;
  gui->lmodel_ambient[1] = .1;
  gui->lmodel_ambient[2] = .1;
  gui->lmodel_ambient[3] = 1;

  gui->light1_spot_direction[0]= 1.0;
  gui->light1_spot_direction[1]= -1.0;
  gui->light1_spot_direction[2]= -1.0;
  gui->light_diffuse[0] = 1.0;
  gui->light_diffuse[1] = 1.0;
  gui->light_diffuse[2] = 1.0;
  
  /*************************************************/
  /** reset all the global vars.
  /*************************************************/
  /*gui->workId = 0;*/
  gui->doubleBuffer = True;
  gui->cam.x =  0.0;      gui->cam.y =  200.0;    gui->cam.z = 500.0;
  gui->cam.at_x = 0.0;    gui->cam.at_y = 0.0;    gui->cam.at_z = 0.0;
  gui->cam.up_x = 0.0;    gui->cam.up_y = 1.0;    gui->cam.up_z = 0.0;
  
  gui->rotation_y = 0; gui->rotation_x = 0; 
  gui->pickedaxis = 2;
  gui->oldx = 0; 
  gui->oldy = 0;
  gui->want_voxels = 0;
  gui->num_bodies = 0; gui->num_slices = 0;
  gui->view_style = VIEW_STYLE_WIREFRAME;
  /*gui->coord_sys = 1;*/

      /*    for (j=0;j<MAX_SLICES;j++) gui->bod[i].numpts[j] = 0;
  */  

  for (i=0;i<8;i++) gui->ray_toggles[i] = 1;
  
  gui->front_z = 60.0; gui->back_z = 800.0;
  for (i=0;i<3;i++) {
    gui->clip[i].on = 0;
    gui->clip[i].f_capped = 0;
    gui->clip[i].b_capped = 0;
  }
  gui->clipping_dialog.current_dir = 1;

  
  gui->reverse_ax = 0;
  gui->reverse_cor = 0; 
  gui->reverse_sag = 0;

  gui->ambient_val = 0.4;
  gui->draw_high_lists = 0; 
  gui->rotating_messages = 0; gui->quick_messages_on = 1;
  gui->draw_particle_paths=0; 
  gui->antialiasing = 0;
  gui->cur_cp = 1;
  gui->inlay_slice = 0;
  gui->slice_dir = 1;
  gui->images_loaded = 0;
  init_gamma_table(gui);

  gui->interactive_clipping = 0; 
  gui->draw_clip_planes = 0;
  gui->clipping_mode = 0;
  gui->reset_clipping = 0;

  /*real_time = 0;*/
  
  gui->loaded_ray_tracking = 0;
  /*init_intersection_object();
    current_tracked_ray = 0;*/
  
  gui->contours_loaded = 0;
  gui->contour_grids_loaded = 0;
  gui->bodies_locked = 0;
  gui->clip_planes_locked = 0;
  gui->slice_locked = 0;

  gui->volume_render_on = 0;

  gui->single_buffer_on = 0;
  gui->crosshair_type = 0;

  gui->beam_slice_x = gui->beam_slice_y = gui->beam_slice_z = 0.0;
  gui->beam_position_val = 0.0;

  gui->slice_win = 0;
  gui->beam_slice = 0; gui->beam_clip = 0;
  gui->auto_rot_degs = 10;
  gui->ibeam = 0;
  gui->ibeam_sx = gui->ibeam_sy = gui->ibeam_sz = gui->ibeam_ex = gui->ibeam_ey = gui->ibeam_ez = 0.0;
  gui->beam_line_view = 0;
  gui->show_frame_rate = 1;

  gui->surface_texturing_on = 0;

  gui->apeture_val = 200;
  gui->ring_apeture_val = 20;
  gui->apeture_on = 0;

  gui->texture_gamma_correction = 1;
  gui->current_dose_component = 8;
  
  gui->colorwash_texture_with_dose = 0;
  gui->colorwash_outlines = 0;
      
  for (i=0;i<MAX_CONTOUR_LEVEL;i++)
    gui->contour_pref[i].filled = 0;


  gui->rendering_quality = 4;

  /* Initialize the names of the axes */
  initAxisInfo( gui );

  DEBUG_TRACE_OUT printf("Done with init_globals\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : reset_variables
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: resets some of the globals when a new uv file is loaded
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void reset_variables(main_gui_t *gui)
{
  int i;

  DEBUG_TRACE_IN printf("Entered reset_variables\n");

  /*************************************************/
  /** if a file is loaded, disable all the bodies,
  /**   remove their numbers, and names. 
  /*************************************************/
  if (gui->Loaded){
    for (i=1;i<=gui->num_bodies;i++){
      gui->bod[i].enabled = 0;
      gui->bod[i].clipped = 0;
      gui->bod[i].pos = i;
      gui->bod[i].region_num = 0;
      strcpy((char *)gui->bod[i].name,"\0");	  
    }
  }

  /*************************************************/
  /** reset all the global vars.
  /*************************************************/
  /*  gui->workId = 0;*/
  gui->doubleBuffer = True;
  gui->cam.x =  0.0;      gui->cam.y = 200.0;     gui->cam.z = 500.0;
  gui->cam.at_x = 0.0;    gui->cam.at_y = 0.0;    gui->cam.at_z = 0.0;
  gui->cam.up_x = 0.0;    gui->cam.up_y = 1.0;    gui->cam.up_z = 0.0;
  
  gui->rotation_y = 0;gui->rotation_x = 0; gui->rotation_z = 0; 
  gui->pickedaxis = 2;
  gui->oldx = 0; gui->oldy = 0;
  gui->num_bodies = 0; gui->num_slices = 0;

  gui->view_style = VIEW_STYLE_WIREFRAME;
  
  XtVaSetValues(gui->view_panel.view_s[0], XmNshadowType, XmSHADOW_IN, NULL);
  for (i=1;i<3;i++)
    XtVaSetValues(gui->view_panel.view_s[i], XmNshadowType, XmSHADOW_OUT, NULL);
  /*
    for (i=0;i<MAX_BODS;i++)
    for (j=0;j<MAX_SLICES;j++) gui->bod[i].numpts[j] = 0;
  */

  gui->front_z = 50.0; 
  gui->back_z = 800.0;
  gui->reverse_ax = 0; gui->reverse_cor = 0; gui->reverse_sag = 0;
  gui->draw_high_lists = 0; 
  gui->rotating_messages = 0; gui->quick_messages_on = 1;
  gui->images_loaded = 0;

  DEBUG_TRACE_OUT printf("Done with reset_variables\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : LoadUnivelFileCallback
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (callback)
%%%
%%%  Purpose: pulls up the selectfile dialog to open files.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void LoadUnivelFileCallback(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int is_direct;
  char fileName[256];
  int i = 0;
  
  DEBUG_TRACE_IN printf("Entered LoadUnivelFileCallback, gui is : %p\n",gui);

  /** needs fixing **/

  /*is_direct = (int)clientData;*/
  is_direct = 0;
 for(i=0;i<gui->file_menu.load_regions.num_submenus;i++){
    if (w == gui->file_menu.load_regions.submenu[i+1]){
      is_direct = 1; break;
    }
 }

  DisplayBusyCursor(gui->mainwindow);

  /*************************************************/
  /** Get the string (filename)
  /*************************************************/

  if ( ! is_direct )
  {
      /*
       * Local flag which indicates that when expose is called
       * we don't want to remove the busy cursor
       */
      loading_images_with_fsb = 1;

      if (!DT_select_file(gui->toplevel,gui->app,&fileName[0],NULL))
      {
          loading_images_with_fsb = 0;
          RemoveBusyCursor(gui->mainwindow);
          DEBUG_TRACE_OUT printf("Done with LoadUnivelFileCallback\n");
          return;
      }
  }
  else
  {
      strcpy(fileName,GetTextOfLabel(w)); 
  }
  
  
  if( FT_fileExists( fileName ) &&
      ( FT_filenameEndsIn(fileName,".uv") || FT_filenameEndsIn(fileName,".uvh") || FT_filenameEndsIn(fileName,".uv.gz") ) )
  {
      /* File exists, and is valid */
      strcpy(gui->uvfile,fileName);    
      fix_filename_and_open(w,clientData,callData);
      fill_recent_file(gui,fileName,1);

      XtVaSetValues(gui->file_menu.load_images.pane,XmNsensitive,TRUE,NULL);
      XtVaSetValues(gui->file_menu.load_paths.pane,XmNsensitive,TRUE,NULL);

      /** add the uv_filename to the message pad on top **/
      set_uv_name_in_message_bar(gui);
  }
  else
      DT_error( gui->toplevel, "That is not a valid UV file!", NULL, NULL );

  /* Unset flag in case it was set */
  loading_images_with_fsb = 0;
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with LoadUnivelFileCallback\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : fix_filename_and_open
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: determines which file was selected (uv or uvh) and
%%%           sets both names to the correct location to open and 
%%%           read.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void fix_filename_and_open(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  char fileName[256];
  int valid_file=1,i=0, ok = 1;
  FILE *temp;

  DEBUG_TRACE_IN printf("Entered fix_filename_and_open\n");

  strcpy(fileName,gui->uvfile);

  /*************************************************/
  /** If the user selected the uv file, append an
  /** 'h', for the uvh file,
  /** If they selected the uvh file, pull off the
  /**  'h' for the uv file.
  /*************************************************/
  if (strstr(fileName,".uvh"))
  {
    /*************************************************/
    /**  The uvh file was selected.
    /*************************************************/
    strcpy(gui->uvhfile,fileName);
    
    /*************************************************/
    /** Pull off the 'h' to get the uv filename.
    /*************************************************/
    do{        
      gui->uvfile[i] = fileName[i]; i++;
    }while (fileName[i-1] != '\0');

    gui->uvfile[i-2] = '\0';

    if (!(temp = fopen(gui->uvfile,"r"))) strcat(gui->uvfile,".gz");
    else fclose(temp);
  } else if (strstr(fileName,".uv")){
    /*************************************************/
    /**  The uv file was selected.
    /*************************************************/
    strcpy(gui->uvfile,fileName);
    strcpy(gui->uvhfile,fileName);
    
   if(strstr(fileName,".gz")){
       /********************************************/
       /* strip off the .gz and add an 'h'
       /*******************************************/
     do{
       gui->uvhfile[i] = fileName[i]; i++;
       if ((i>=3) && (fileName[i-2] == '.')&&
	   (fileName[i-1] == 'g') && (fileName[i] =='z'))
	 ok = 0;
     }while (ok);
     
     gui->uvhfile[i-2] = '\0';
     strcat(gui->uvhfile,"h");
   }else{
     /*************************************************/
     /**  The uv file was selected.
       /*************************************************/
     strcpy(gui->uvfile,fileName);
     
     /*************************************************/
     /**  Add an 'h' to get the uvh filename.
       /*************************************************/
     strcpy(gui->uvhfile,fileName);
     strcat(gui->uvhfile,"h");
   }
  }else {
    /*************************************************/
    /** A non- uvh or uv file was selected, not valid.
      /*************************************************/
    DT_warn(gui->toplevel,"You must select either a .uv or .uvh file",NULL,NULL);
    valid_file = 0;
  }
  
  /*************************************************/
  /** If we received a valid file, open them and 
  /** get their info.
  /*************************************************/
  if(valid_file)
    Process_files(w,clientData,callData);

  DEBUG_TRACE_OUT printf("Done with fix_filename_and_open\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Control_Panel_changedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: this callback manages the correct control panel,
%%%           based on which button was pressed.  Will switch
%%%	      between view,color,transparency, ...
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Control_Panel_changedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  static int old_cp = 0;
  int cp=0;
  int i;

  DEBUG_TRACE_IN printf("Entered Control_Panel_changedCB\n");

  for (i=0;i<15;i++)
    if (w == gui->options_panel.panel[i]){ 
      cp = i; break;
    }

  /*** check to see if the panel  should be available, if not, just return ***/
  /*if (cp == 1 && gui->view_style != VIEW_STYLE_POLYGONAL && gui->view_style != VIEW_STYLE_WIREFRAME) return;*/
  if (cp == 2 && gui->view_style != VIEW_STYLE_POLYGONAL && gui->num_contour_surfaces == 0) return;
  if (cp == 3 && gui->view_style != VIEW_STYLE_POLYGONAL) return;


  
  gui->cur_cp = cp;
  /*printf("the new cp is %d\n",gui->cur_cp);
    printf("the old cp is : %d\n",old_cp);*/

  

  
  if (old_cp != gui->cur_cp){
    XtVaSetValues(gui->options_panel.panel[gui->cur_cp],XmNshadowType, XmSHADOW_IN,NULL);
    XtVaSetValues(gui->options_panel.panel[old_cp],XmNshadowType, XmSHADOW_OUT,NULL);
    
    switch (old_cp)
      {
      case 0:  XtUnmanageChild(gui->view_panel.form); break;
      case 1:  XtUnmanageChild(gui->color_panel.form); break;
      case 2:  XtUnmanageChild(gui->transparency_panel.form); break;
      case 3:  XtUnmanageChild(gui->lighting_panel.form); break;
      case 4:  XtUnmanageChild(gui->axis_panel.form); break;
      case 5:  XtUnmanageChild(gui->clipping_panel.form); break;
      case 6:  XtUnmanageChild(gui->polygon_panel.form); break;
      case 7:  XtUnmanageChild(gui->particle_panel.form); break;
      case 8:  XtUnmanageChild(gui->slice_panel.form); break;
      case 9:  XtUnmanageChild(gui->contour_panel.form);break;
      case 10: XtUnmanageChild(gui->mouse_panel.form);break;
      case 11: XtUnmanageChild(gui->view_params_panel.form);break;
      case 12: XtUnmanageChild(gui->beam_panel.form);break;
      case 13: XtUnmanageChild(gui->ray_track_panel.form);break;
      case 14: XtUnmanageChild(gui->texture_panel.form); break;
      }
    
    switch (gui->cur_cp)
      {
      case 0:  XtManageChild(gui->view_panel.form); break;
      case 1:  XtManageChild(gui->color_panel.form); break;
      case 2:  XtManageChild(gui->transparency_panel.form); break;
      case 3:  XtManageChild(gui->lighting_panel.form); break;
      case 4:  XtManageChild(gui->axis_panel.form); break;
      case 5:  XtManageChild(gui->clipping_panel.form); break; 
      case 6:  XtManageChild(gui->polygon_panel.form); break;
      case 7:  XtManageChild(gui->particle_panel.form); break;
      case 8:  XtManageChild(gui->slice_panel.form); break;
      case 9:  XtManageChild(gui->contour_panel.form);break;
      case 10: XtManageChild(gui->mouse_panel.form);break;
      case 11: XtManageChild(gui->view_params_panel.form);break;
      case 12: XtManageChild(gui->beam_panel.form);break;
      case 13: XtManageChild(gui->ray_track_panel.form);break;
      case 14: XtManageChild(gui->texture_panel.form);break;
      }
    old_cp = gui->cur_cp;
  }

  DEBUG_TRACE_OUT printf("Done with Control_Panel_changedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : selected_body_changedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: sets the selected_body to which body was selected,
%%%           and sets the sliders (color & trans) to that body's
%%%	      position.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Selected_body_changedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int i;
  XColor color;

  DEBUG_TRACE_IN printf("Entered Selected_body_changedCB\n");

  for(i=1;i<MAX_BODS*2;i++){
    if (strcmp(XtName(w),gui->bod[i].name) == 0){
      gui->selected_body = i;
      
      XmScaleSetValue(gui->color_panel.r_slider, gui->bod[i].r);
      XmScaleSetValue(gui->color_panel.g_slider, gui->bod[i].g);
      XmScaleSetValue(gui->color_panel.b_slider, gui->bod[i].b);           
      XmScaleSetValue(gui->transparency_panel.t_slider, gui->bod[i].t);
      
      init_color(gui,gui->bod[i].r,
		 gui->bod[i].g,
		 gui->bod[i].b,
		 &color);
      
      XtVaSetValues(gui->color_panel.main_color_swatch,
		    XmNbackground, color.pixel,
		    NULL);
      XtVaSetValues(gui->color_panel.body_menu,
		    XmNmenuHistory,gui->color_panel.bodies[i],
		    NULL);        
      XtVaSetValues(gui->transparency_panel.transparency_menu,
		    XmNmenuHistory,gui->transparency_panel.transparencies[i],
		    NULL);        
      break;
    }
  }

  DEBUG_TRACE_OUT printf("Done with Selected_body_changedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : remove_names_from_selectors()
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: removes the body names from the color menu and
%%%           transparency menu.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void remove_names_from_selectors(main_gui_t *gui)
{
  int i;

  DEBUG_TRACE_IN printf("Entered remove_names_from_selectors\n");
  
  XtManageChild(gui->color_panel.bodies[0]);
  XtVaSetValues(gui->color_panel.body_menu,XmNmenuHistory,gui->color_panel.bodies[0],NULL);        
  XtManageChild(gui->transparency_panel.transparencies[0]);
  XtVaSetValues(gui->transparency_panel.transparency_menu,
		XmNmenuHistory,gui->transparency_panel.transparencies[0],NULL);

  for (i = 1;i<gui->num_bodies;i++){
    XtUnmanageChild(gui->color_panel.bodies[i]);
    XtDestroyWidget(gui->color_panel.bodies[i]);
    
    XtUnmanageChild(gui->transparency_panel.transparencies[i]);
    XtDestroyWidget(gui->transparency_panel.transparencies[i]);
  }

  DEBUG_TRACE_OUT printf("Done with remove_names_from_selectors\n");
} 

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : fill_forms_with_body_names()
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: puts the body names into the color and transparency
%%%           menus.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void fill_forms_with_body_names(main_gui_t *gui)
{
  int i;
  XColor color;

  DEBUG_TRACE_IN printf("Entered fill_forms_with_body_names\n");

  for(i=1;i<gui->num_bodies;i++){
    gui->color_panel.bodies[i] = XtCreateManagedWidget(gui->bod[i].name,xmPushButtonWidgetClass,
						       gui->color_panel.body_pane, NULL, 0);
    
    gui->transparency_panel.transparencies[i] = XtCreateManagedWidget(gui->bod[i].name,
								      xmPushButtonWidgetClass,
								      gui->transparency_panel.transparency_pane, NULL, 0);
    
    XtAddCallback(gui->color_panel.bodies[i], XmNactivateCallback,
		  Selected_body_changedCB, (XtPointer)gui);
    
    XtAddCallback(gui->transparency_panel.transparencies[i], XmNactivateCallback,
		  Selected_body_changedCB, (XtPointer)gui);
  }
  
  if(XtIsManaged(gui->color_panel.bodies[0])){
    XtVaSetValues(gui->color_panel.body_menu,
		  XmNmenuHistory,gui->color_panel.bodies[1],
		  NULL);        
    XtUnmanageChild(gui->color_panel.bodies[0]); 
  }
  if(XtIsManaged(gui->transparency_panel.transparencies[0])){
    XtVaSetValues(gui->transparency_panel.transparency_menu,
		  XmNmenuHistory, gui->transparency_panel.transparencies[1],
		  NULL); 
    XtUnmanageChild(gui->transparency_panel.transparencies[0]);
  }

  gui->selected_body = 1;
  
  XmScaleSetValue(gui->color_panel.r_slider, gui->bod[1].r);
  XmScaleSetValue(gui->color_panel.g_slider, gui->bod[1].g);
  XmScaleSetValue(gui->color_panel.b_slider, gui->bod[1].b);
  
  init_color(gui,gui->bod[1].r,gui->bod[1].g,gui->bod[1].b,&color);
  XtVaSetValues(gui->color_panel.main_color_swatch,
		XmNbackground, color.pixel,
		NULL);
  
  XmScaleSetValue(gui->transparency_panel.t_slider, gui->bod[1].t);

  DEBUG_TRACE_OUT printf("Done with fill_forms_with_body_names\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Scaling_changedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: changes the global scaling factor and redraws
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Scaling_changedCB (Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;

  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;

  DEBUG_TRACE_IN printf("Entered Scaling_changedCB\n");

  gui->scaling = ((float)cbs->value)/100.0;
  if (gui->fast_rotation_type != 3)
    draw_the_high_lists(gui);
  else
  {
      /* Take care of unsetting the high lists */
      gui->draw_high_lists = 0;
      gui->rotating_messages = 0;
      draw_all(gui);
  }
  

  DEBUG_TRACE_OUT printf("Done with Scaling_changedCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Locking_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: sets the rotational locks for locking bodies
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Locking_ChangedCB (Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int lock_type;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;
  
  DEBUG_TRACE_IN printf("Entered Locking_ChangedCB\n");

  lock_type = (int)clientData;

  if (w == gui->mouse_panel.lock[0]){
    if (cbs ->set){
      gui->bodies_locked = 1;
      gui->locked_rx = gui->rotation_x;
      gui->locked_ry = gui->rotation_y;
      gui->locked_rz = gui->rotation_z;
    }else{ 
      gui->bodies_locked = 0;
      gui->rotation_x = gui->locked_rx;
      gui->rotation_y = gui->locked_ry;
      gui->rotation_z = gui->locked_rz;
    }
  }else if (w == gui->mouse_panel.lock[1]){
    if (cbs ->set){ 
      gui->clip_planes_locked = 1;
      gui->locked_rx = gui->rotation_x;
      gui->locked_ry = gui->rotation_y;
      gui->locked_rz = gui->rotation_z;
    }else{
      gui->clip_planes_locked = 0;
      gui->rotation_x = gui->locked_rx;
      gui->rotation_y = gui->locked_ry;
      gui->rotation_z = gui->locked_rz;
    }
  }else{
    if (cbs ->set){ 
      gui->slice_locked = 1;
      gui->locked_rx = gui->rotation_x;
      gui->locked_ry = gui->rotation_y;
      gui->locked_rz = gui->rotation_z;
    }else{
      gui->slice_locked = 0;
      gui->rotation_x = gui->locked_rx;
      gui->rotation_y = gui->locked_ry;
      gui->rotation_z = gui->locked_rz;
    }
  }

  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Locking_ChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : AutoRotateCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: starts/stops the auto rotate feature
%%%           5 buttons on main control panel
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Auto_RotateCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  static int inside = 0;
  static int auto_rotate_on,old_dir=0;
  int dir = 1;
  
 DEBUG_TRACE_IN printf("Entered Auto_RotateCB\n");

  if      (w == gui->view_panel.auto_rotate_left) dir = 1;
  else if (w == gui->view_panel.auto_rotate_right) dir = 2;
  else if (w == gui->view_panel.auto_rotate_down) dir = 3;
  else if (w == gui->view_panel.auto_rotate_up) dir = 4;
  else if (w == gui->view_panel.auto_rotate_stop) dir = 5;

 if (dir == 5/*old_dir == dir*/){ 
   auto_rotate_on = 0;
   inside = 0;
 }else{
   auto_rotate_on = 1; inside = 1;
   old_dir = dir;
 }
 while (auto_rotate_on){
   switch(dir){
   case 1:  gui->rotation_y -= gui->auto_rot_degs; if (gui->rotation_y<0) gui->rotation_y+=360; break;
   case 2:  gui->rotation_y += gui->auto_rot_degs; gui->rotation_y = gui->rotation_y%360; break;
   case 3:  gui->rotation_x -= gui->auto_rot_degs; if (gui->rotation_x<0) gui->rotation_x+=360; break;
   case 4:  gui->rotation_x += gui->auto_rot_degs; gui->rotation_x = gui->rotation_x%360; break;
   }
   if (gui->beam_line_view) auto_rotate_on = 0;

   wait_on_xserver(gui);
   draw_all(gui);
   wait_on_xserver(gui);
 }

 DEBUG_TRACE_OUT printf("Done with Auto_RotateCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Auto_Rotate_Degs_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: increments/decrements the number of degrees for auto
%%%           rotating
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Auto_Rotate_Degs_ChangedCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  XmString xmstr;
  char return_string[25];

  DEBUG_TRACE_IN printf("Entered Auto_Rotate_Degs_ChangedCB\n");
 
  if (strstr(XtName(w),"up")) gui->auto_rot_degs+= 5;
  else gui->auto_rot_degs-=5;
  
  sprintf(return_string,"%d Degs\n",gui->auto_rot_degs);
  xmstr = XmStringCreateLocalized(return_string);
  XtVaSetValues(gui->view_panel.deg_label,XmNlabelString, xmstr, NULL);
  XmStringFree(xmstr);

  DEBUG_TRACE_OUT printf("Done with Auto_Rotate_Degs_ChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Frustum_ChangedCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: adjusts the viewing frustum (not really useful)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Frustum_ChangedCB(Widget w, XtPointer clientData, XtPointer callData)
{ 
  main_gui_t *gui = (main_gui_t *)clientData;
  int val;
  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;

  DEBUG_TRACE_IN printf("Entered Frustum_ChangedCB\n");

  val = cbs->value;  

  glMatrixMode (GL_PROJECTION);	 
  glLoadIdentity ();	
  glFrustum (-val, val, -20, 20, gui->front_z, gui->back_z);  
  glMatrixMode (GL_MODELVIEW);	

  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Frustum_ChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Buffering_ToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: switches between Single & Double buffering
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Buffering_ToggledCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int type;

  DEBUG_TRACE_IN printf("Entered Buffering_ToggledCB\n");

  if (w == gui->view_params_panel.single_buff_button) type = 1;
  else type = 2;

  if (type == 2){
    glXMakeCurrent(gui->display, XtWindow(gui->multiview_panel.gl_lview_area), gui->glxcontext);
    glDrawBuffer(GL_BACK);
    glXMakeCurrent(gui->display, XtWindow(gui->multiview_panel.gl_rview_area), gui->glxcontext);
    glDrawBuffer(GL_BACK);
    glXMakeCurrent(gui->display, XtWindow(gui->multiview_panel.gl_tview_area), gui->glxcontext);
    glDrawBuffer(GL_BACK);
    glXMakeCurrent(gui->display, XtWindow(gui->glxarea), gui->glxcontext);
    glDrawBuffer(GL_BACK);
    gui->single_buffer_on = 0;
  }else{
    glXMakeCurrent(gui->display, XtWindow(gui->multiview_panel.gl_lview_area), gui->glxcontext);
    glDrawBuffer(GL_FRONT);
    glXMakeCurrent(gui->display, XtWindow(gui->multiview_panel.gl_rview_area), gui->glxcontext);
    glDrawBuffer(GL_FRONT);
    glXMakeCurrent(gui->display, XtWindow(gui->multiview_panel.gl_tview_area), gui->glxcontext);
    glDrawBuffer(GL_FRONT);
    glXMakeCurrent(gui->display, XtWindow(gui->glxarea), gui->glxcontext);
    glDrawBuffer(GL_FRONT);
    gui->single_buffer_on = 1;
  }

  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Buffering_ToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Cross_Hair_Changed
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: changes the crosshair type 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Cross_Hair_ChangedCB(Widget w, XtPointer clientdata, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  static int old_ch = 0,new_ch;
  int i;

  DEBUG_TRACE_IN printf("Entered Cross_Hair_ChangedCB\n");

  for (i=0;i<4;i++)
    if (w == gui->beam_panel.cross_hair_b[i]){
      new_ch = i; break;
    }
  
  if (old_ch != new_ch){
    XtVaSetValues(w, XmNshadowType, XmSHADOW_IN, NULL);
    XtVaSetValues(gui->beam_panel.cross_hair_b[old_ch], XmNshadowType, XmSHADOW_OUT, NULL);
    
    DisplayBusyCursor(gui->mainwindow);
    gui->crosshair_type = new_ch;
    
    draw_all(gui);
    RemoveBusyCursor(gui->mainwindow);
    old_ch = new_ch;
  }
  DEBUG_TRACE_OUT printf("Done with Cross_Hair_ChangedCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Show_Frame_Rate_ToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the frame rate on/off in the main window
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Show_Frame_Rate_ToggledCB(Widget w, XtPointer clientdata, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;

  DEBUG_TRACE_IN printf("Entered Show_Frame_Rate_ToggledCB\n");

  if (cbs->set) gui->show_frame_rate = 1;
  else gui->show_frame_rate = 0;

  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Show_Frame_Rate_ToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Surface_Texturing_Toggled_ToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles surface texturing
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Surface_Texturing_ToggledCB(Widget w, XtPointer clientdata, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;

  DEBUG_TRACE_IN printf("Entered Surface_Texturing_ToggledCB\n");

  if (cbs->set) gui->surface_texturing_on = 1;
  else gui->surface_texturing_on = 0;

  draw_all(gui);

  DEBUG_TRACE_OUT printf("Done with Surface_Texturing_ToggledCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Gamma_ToggledCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Gamma_ToggledCB(Widget w, XtPointer clientdata, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)callData;

  DEBUG_TRACE_IN printf("Entered Gamma_ToggledCB\n");

  DisplayBusyCursor(gui->mainwindow);

  if (cbs->set){ 
    gui->texture_gamma_correction = 1;
    rebuild_3d_texture_from_texture_volume(gui);
  }else{ 
    gui->texture_gamma_correction = 0;
    rebuild_3d_texture_from_texture_volume(gui);
  }
  draw_all(gui);

  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Gamma_ToggledCB\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Polygonal_AlgorithmCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Polygonal_AlgorithmCB(Widget w, XtPointer clientdata, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  int i;

  DEBUG_TRACE_IN printf("Entered Polygonal_AlgorithmCB\n");

  DisplayBusyCursor(gui->mainwindow);

  for (i=0;i<3;i++)
    if (w == gui->polygon_panel.algorithm_type[i]){
      gui->polygonal_algorithm = i;
      break;
    }

  if (gui->polygonal_algorithm == VERTEXCELL_WITH_SURFACE_NORMALS)
    glShadeModel(GL_FLAT);
  else
    glShadeModel(GL_SMOOTH);

  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Polygonal_AlgorithmCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : LaunchAppCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles nurb texturing
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void LaunchAppCB(Widget w, XtPointer clientdata,XtPointer calldata)
{
  DEBUG_TRACE_IN printf("Entered LaunchAppCB\n");
  
  if (strstr(XtName(w),"Dos"))
    system("../DoseDisplay/Contours/doseplay &");
  else if (strstr(XtName(w),"Image"))
    system("../ImageTools/Editor/image_tools &");
  
  DEBUG_TRACE_OUT printf("Done with LaunchAppCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : ResetProgramCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (callback)
%%%
%%%  Purpose: Removes all of the info from the main widgets,
%%%           Deletes all of the OpenGL Display Lists.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void ResetProgramCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  int i;
  Contour_grid_t *current_grid;

  DEBUG_TRACE_IN printf("entered ResetProgramCB\n");

  gui->images_loaded = 0;

  strcpy(gui->uvfile,(char *)"-----");
  set_uv_name_in_message_bar(gui);

  /*** Delete the current bodies **/
  switch(gui->view_style){
  case VIEW_STYLE_WIREFRAME:
    glDeleteLists(WIREFRAME_LISTS,gui->num_bodies);
    break;
  case VIEW_STYLE_OUTLINE:
    glDeleteLists(WIREFRAME_LISTS,gui->num_bodies);
    glDeleteLists(1,gui->num_bodies);
    break;
  case VIEW_STYLE_SOLID:
    glDeleteLists(WIREFRAME_LISTS,gui->num_bodies);
    glDeleteLists(1,gui->num_bodies);
    break;
  case VIEW_STYLE_POLYGONAL:
    glDeleteLists(WIREFRAME_LISTS,gui->num_bodies);
    glDeleteLists(1,gui->num_bodies);
    break;
  }

  if (gui->num_contour_surfaces != 0) glDeleteLists(CONTOUR_LISTS, gui->num_contour_surfaces);

  gui->view_style = VIEW_STYLE_WIREFRAME;

  glDeleteLists(AXIAL_SQUARE,1);
  glDeleteLists(CORONAL_SQUARE,1);
  glDeleteLists(SAGITTAL_SQUARE,1);
  glDeleteLists(UNIVEL,1);

  /** remove the names from the body list **/                                                                      
 for (i=0;i<gui->num_bodies;i++){      
    XmListDeleteItem(gui->bodylist, XmStringCreateLocalized(gui->bod[i].name));
    gui->bod[i].enabled = 0;
  }

  /*****************************************************/
  /** Take the names off of the color & transparency
  /** menu widgets.
  /*****************************************************/
  remove_names_from_selectors(gui);
  remove_bodies_from_body_clipping(gui);

  /*** Delete the current images (texture map) ***/
  if (gui->images_loaded) MT_free( (void *) gui->texture_volume );
  gui->images_loaded = 0;

  /** reset all needed controls **/
  gui->draw_high_lists = 0;
  gui->selected_body = 0;
  gui->Loaded = 0;

  /** we now have zero bodies **/
  gui->num_bodies = 0;

  /** if the contour grids are loaded, free them all **/
  if (gui->contour_grids_loaded){
    while (1){
      current_grid = gui->Grids.next;
      while (current_grid->next != NULL) current_grid = current_grid->next;
      if ( current_grid == gui->Grids.next ) break;
      MT_free( (void *) current_grid->next);
      /*printf("freed %d\n",current_grid->next);*/
    }
    MT_free( (void *)gui->Grids.next);
    /*printf("freed the first grid, all done\n");*/
  }
  gui->contour_grids_loaded = 0;

  if (gui->num_particle_paths != 0) MT_free( (void *)gui->particle_path);
  gui->num_particle_paths = 0;
  

  gui->beam_num = 0;

  /*************************************************/
  /** Set the view style push buttons back to original
  /*************************************************/
  XtVaSetValues(gui->view_panel.view_s[0], XmNshadowType, XmSHADOW_IN,NULL);
  XtVaSetValues(gui->view_panel.view_s[1], XmNshadowType, XmSHADOW_OUT,NULL);
  XtVaSetValues(gui->view_panel.view_s[2], XmNshadowType, XmSHADOW_OUT,NULL);
  XtVaSetValues(gui->view_panel.view_s[3], XmNshadowType, XmSHADOW_OUT,NULL);



  /*************************************************/
  /** Redraw (black the screen).
  /*************************************************/
  draw_all(gui);

  DEBUG_TRACE_OUT printf("done with ResetProgramCB\n");
}

void Cell8_ChangedCB (Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;

  XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) callData;

  gui->current_8cell = (cbs->value);
  
  draw_all(gui);
}

void Display_Centers_of_Mass_for_BodiesCB (Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;
  
  gui->display_center_of_mass_strings = 1;

  draw_all(gui);
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : calculate_bboxes_for_bodies
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (callback)
%%%
%%%  Purpose: calculates the lowest and highest univel position
%%%           in each direction for each body
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void calculate_bboxes_for_bodies(main_gui_t *gui)
{
    int x,y,z;
    int i;
    int min_z,max_z,min_x,max_x,min_y,max_y;
    int found;

    int sliceOffset;
    int rowOffset;
    int sliceSize = 256 * 256;
    int regionNum;

    DEBUG_TRACE_IN printf("Entered calculate_bboxes_for_bodies\n");

    if(!read_uv_file(gui)){
        printf("Couldn't read the uv file : %s, aborting\n",gui->uvfile);
        return;
    }

    PostMessage(gui,"Calculating Bounding Boxes for Regions . . .");

    for( i = 1; i < gui->num_bodies; i++ )
    {
        regionNum = gui->bod[i].region_num;
      
        /*printf("finding min_z\n");*/    
        /** min z_direction **/
        found = 0;
        for( z = 0; z < gui->num_slices; z++ )
        {
            sliceOffset = z * sliceSize;
        
            for( y = 0; y < 256; y++ )
            {
                rowOffset = y * 256;

                for( x = 0; x < 256; x++ )
                {
                    if( gui->volume[ sliceOffset + rowOffset + x ] == regionNum )
                    {
                        min_z = z;
                        found = 1;
                        break;
                    }
                }
                if (found) break;
            }
            if (found) break;
        }
        if (!found){ min_z = 0; max_z = 0; min_x = 0; max_x = 0; min_y = 0; max_y = 0; continue;} 

        /*printf("finding max_z\n");    */
        /** max_z direction **/
        found = 0;
        for( z = gui->num_slices - 1; z >= 0; z-- )
        {
            sliceOffset = z * sliceSize;
        
            for( y = 0; y < 256; y++ )
            {
                rowOffset = y * 256;
            
                for( x = 0; x < 256; x++ )
                {
                    if( gui->volume[ sliceOffset + rowOffset + x ] == regionNum )
                    {
                        max_z = z;
                        found = 1;
                        break;
                    }
                }
                if (found) break;
            }
            if (found) break;
        }
        if (!found){ min_z = 0; max_z = 0; min_x = 0; max_x = 0; min_y = 0; max_y = 0; continue;} 

        /*printf("finding min_x\n");    */
        /** min_x direction **/
        found = 0;
        for( x = 0; x < 256; x++ )
        {
            for( z = 0; z < gui->num_slices; z++ )
            {
                sliceOffset = z * sliceSize;
          
                for( y = 0; y < 256; y++ )
                {
                    rowOffset = y * 256;
              
                    if( gui->volume[ sliceOffset + rowOffset + x ] == regionNum )
                    {
                        min_x = x;
                        found = 1;
                        break;
                    }
                }
                if (found) break;
            }
            if (found) break;
        }
        if (!found){ min_z = 0; max_z = 0; min_x = 0; max_x = 0; min_y = 0; max_y = 0; continue;} 

        /*printf("finding max_x\n");    */
        /** max_x direction **/
        found = 0;
        for( x = 255; x >=0 ; x-- )
        {
            for( z = 0; z < gui->num_slices; z++ )
            {
                sliceOffset = z * sliceSize;
          
                for( y = 0; y < 256; y++ )
                {
                    rowOffset = y * 256;
            
                    if( gui->volume[ sliceOffset + rowOffset + x ] == regionNum )
                    {
                        max_x = x;
                        found = 1;
                        break;
                    }
                }
                if (found) break;
            }
            if (found) break;
        }
        if (!found){ min_z = 0; max_z = 0; min_x = 0; max_x = 0; min_y = 0; max_y = 0; continue;} 

        /*printf("finding min_y\n");    */
        /** min_y direction **/
        found = 0;
        for( y = 0; y < 256; y++ )
        {
            rowOffset = y * 256;
        
            for( z = 0; z < gui->num_slices; z++ )
            {
                sliceOffset = z * sliceSize;
            
                for( x = 0; x < 256; x++ )
                {
                    if( gui->volume[ sliceOffset + rowOffset + x ] == regionNum )
                    {
                        min_y = y;
                        found = 1;
                        break;
                    }
                }
                if (found) break;
            }
            if (found) break;
        }
        if (!found){ min_z = 0; max_z = 0; min_x = 0; max_x = 0; min_y = 0; max_y = 0; continue;} 

        /*printf("finding max_y\n");    */
        /** max_y direction **/
        found = 0;
        for( y = 255; y >= 0; y-- )
        {
            rowOffset = y * 256;
        
            for( z = 0; z < gui->num_slices; z++ )
            {
                sliceOffset = z * sliceSize;
            
                for( x = 0; x < 256; x++ )
                {
                    if( gui->volume[ sliceOffset + rowOffset + x ] == regionNum )
                    {
                        max_y = y;
                        found = 1;
                        break;
                    }
                }
                if (found) break;
            }
            if (found) break;
        }
        if (!found){ min_z = 0; max_z = 0; min_x = 0; max_x = 0; min_y = 0; max_y = 0; continue;} 


        gui->bod[i].min_x = min_x;
        gui->bod[i].max_x = max_x;

        gui->bod[i].min_y = min_y;
        gui->bod[i].max_y = max_y;

        gui->bod[i].min_z = min_z;
        gui->bod[i].max_z = max_z;
    }
  
    MT_free( (void *)gui->volume);
  
    DEBUG_TRACE_OUT printf("done with calculate_bboxes_for_bodies\n");
}

void dump_opengl_implementation_dependent_state_variables()
{
  GLint gl_max_lights;
  GLint gl_max_clip_planes;
  GLint gl_max_modelview_stack_depth;
  GLint gl_max_projections_stack_depth;
  GLint gl_max_texture_stack_depth;
  GLint gl_subpixel_bits;
  GLint gl_max_texture_size;
  GLint gl_max_pixel_map_table;
  GLint gl_max_name_stack_depth;
  GLint gl_max_list_nesting;
  GLint gl_max_eval_order;
  GLint gl_max_viewport_dims;
  GLint gl_max_attrib_stack_depth;
  GLboolean gl_aux_buffers;
  GLboolean gl_rgba_mode;
  GLboolean gl_index_mode;
  GLboolean gl_doublebuffer;
  GLboolean gl_stereo;
  GLfloat gl_point_size_range[2];
  GLfloat gl_point_size_granularity;
  GLfloat gl_line_width_range[2];
  GLint gl_red_bits;
  GLint gl_green_bits;
  GLint gl_blue_bits;
  GLint gl_alpha_bits;
  GLint gl_index_bits;
  GLint gl_depth_bits;
  GLint gl_stencil_bits;
  GLint gl_accum_red_bits;
  GLint gl_accum_green_bits;
  GLint gl_accum_blue_bits;
  GLint gl_accum_alpha_bits;


  glGetIntegerv(GL_MAX_LIGHTS                 ,&gl_max_lights);
  glGetIntegerv(GL_MAX_CLIP_PLANES            ,&gl_max_clip_planes);
  glGetIntegerv(GL_MAX_MODELVIEW_STACK_DEPTH  ,&gl_max_modelview_stack_depth);
  glGetIntegerv(GL_MAX_PROJECTION_STACK_DEPTH ,&gl_max_projections_stack_depth);
  glGetIntegerv(GL_MAX_TEXTURE_STACK_DEPTH    ,&gl_max_texture_stack_depth);
  glGetIntegerv(GL_SUBPIXEL_BITS              ,&gl_subpixel_bits);
  glGetIntegerv(GL_MAX_TEXTURE_SIZE           ,&gl_max_texture_size);
  glGetIntegerv(GL_MAX_PIXEL_MAP_TABLE        ,&gl_max_pixel_map_table);
  glGetIntegerv(GL_MAX_NAME_STACK_DEPTH       ,&gl_max_name_stack_depth);
  glGetIntegerv(GL_MAX_LIST_NESTING           ,&gl_max_list_nesting);
  glGetIntegerv(GL_MAX_EVAL_ORDER             ,&gl_max_eval_order);
  glGetIntegerv(GL_MAX_VIEWPORT_DIMS          ,&gl_max_viewport_dims);
  glGetIntegerv(GL_MAX_ATTRIB_STACK_DEPTH     ,&gl_max_attrib_stack_depth);

  glGetBooleanv(GL_AUX_BUFFERS                ,&gl_aux_buffers);
  glGetBooleanv(GL_RGBA_MODE                  ,&gl_rgba_mode);
  glGetBooleanv(GL_INDEX_MODE                 ,&gl_index_mode);
  glGetBooleanv(GL_DOUBLEBUFFER               ,&gl_doublebuffer);
  glGetBooleanv(GL_STEREO                     ,&gl_stereo);
  
  glGetFloatv(GL_POINT_SIZE, gl_point_size_range);
  glGetFloatv(GL_POINT_SIZE_GRANULARITY, &gl_point_size_granularity);
  glGetFloatv(GL_LINE_WIDTH_RANGE,gl_line_width_range);

  glGetIntegerv(GL_RED_BITS                   ,&gl_red_bits);
  glGetIntegerv(GL_GREEN_BITS                 ,&gl_green_bits);
  glGetIntegerv(GL_BLUE_BITS                  ,&gl_blue_bits);
  glGetIntegerv(GL_ALPHA_BITS                 ,&gl_alpha_bits);
  glGetIntegerv(GL_INDEX_BITS                 ,&gl_index_bits);
  glGetIntegerv(GL_DEPTH_BITS                 ,&gl_depth_bits);
  glGetIntegerv(GL_STENCIL_BITS               ,&gl_stencil_bits);
  glGetIntegerv(GL_ACCUM_RED_BITS             ,&gl_accum_red_bits);
  glGetIntegerv(GL_ACCUM_GREEN_BITS           ,&gl_accum_green_bits);
  glGetIntegerv(GL_ACCUM_BLUE_BITS            ,&gl_accum_blue_bits);
  glGetIntegerv(GL_ACCUM_ALPHA_BITS           ,&gl_accum_alpha_bits);



  printf("********************************************************************************\n");
  printf("    OpenGL  Implementation Dependent State Variable Values\n\n");
  

  printf("GL_MAX_LIGHTS                 :  %d\n",gl_max_lights);
  printf("GL_MAX_CLIP_PLANES            :  %d\n",gl_max_clip_planes);
  printf("GL_MAX_MODELVIEW_STACK_DEPTH  :  %d\n",gl_max_modelview_stack_depth);
  printf("GL_MAX_PROJECTION_STACK_DEPTH :  %d\n",gl_max_projections_stack_depth);
  printf("GL_MAX_TEXTURE_STACK_DEPTH    :  %d\n",gl_max_texture_stack_depth);
  printf("GL_SUBPIXEL_BITS              :  %d\n",gl_subpixel_bits);
  printf("GL_MAX_TEXTURE_SIZE           :  %d\n",gl_max_texture_size);
  printf("GL_MAX_PIXEL_MAP_TABLE        :  %d\n",gl_max_pixel_map_table);
  printf("GL_MAX_NAME_STACK_DEPTH       :  %d\n",gl_max_name_stack_depth);
  printf("GL_MAX_LIST_NESTING           :  %d\n",gl_max_list_nesting);
  printf("GL_MAX_EVAL_ORDER             :  %d\n",gl_max_eval_order);
  printf("GL_MAX_VIEWPORT_DIMS          :  %d\n",gl_max_viewport_dims);
  printf("GL_MAX_ATTRIB_STACK_DEPTH     :  %d\n",gl_max_attrib_stack_depth);

  printf("GL_AUX_BUFFERS                :  %d\n",gl_aux_buffers);
  printf("GL_RGBA_MODE                  :  %d\n",gl_rgba_mode);
  printf("GL_INDEX_MODE                 :  %d\n",gl_index_mode);
  printf("GL_DOUBLEBUFFER               :  %d\n",gl_doublebuffer);
  printf("GL_STEREO                     :  %d\n",gl_stereo);
  
  printf("GL_POINT_SIZE                 :  %f  %f\n", gl_point_size_range[0], gl_point_size_range[1]);
  printf("GL_POINT_SIZE_GRANULARITY     :  %f\n", gl_point_size_granularity);
  printf("GL_LINE_WIDTH_RANGE           :  %f  %f\n", gl_line_width_range[0],gl_line_width_range[1]);

  printf("GL_RED_BITS                   :  %d\n",gl_red_bits);
  printf("GL_GREEN_BITS                 :  %d\n",gl_green_bits);
  printf("GL_BLUE_BITS                  :  %d\n",gl_blue_bits);
  printf("GL_ALPHA_BITS                 :  %d\n",gl_alpha_bits);
  printf("GL_INDEX_BITS                 :  %d\n",gl_index_bits);
  printf("GL_DEPTH_BITS                 :  %d\n",gl_depth_bits);
  printf("GL_STENCIL_BITS               :  %d\n",gl_stencil_bits);
  printf("GL_ACCUM_RED_BITS             :  %d\n",gl_accum_red_bits);
  printf("GL_ACCUM_GREEN_BITS           :  %d\n",gl_accum_green_bits);
  printf("GL_ACCUM_BLUE_BITS            :  %d\n",gl_accum_blue_bits);
  printf("GL_ACCUM_ALPHA_BITS           :  %d\n",gl_accum_alpha_bits);

  printf("\n********************************************************************************\n");

}


void CheckVersionCB(Widget w, XtPointer clientData, XtPointer callData)
{
    main_gui_t * gui = (main_gui_t *) clientData;

    CT_check_version( gui->toplevel, "sera3d" );
}

static void initAxisInfo( main_gui_t * gui )
{
    /* Initialize things to the default Transverse case */
    /*
     * ImageSliceAxis  := IS+
     * ImageRowAxis    := PA-
     * ImageColumnAxis := RL+
     */

    strcpy( gui->axisLabels[SLICE_AXIS].name, "IS" );
    gui->axisLabels[SLICE_AXIS].first = 'I';
    gui->axisLabels[SLICE_AXIS].last  = 'S';

    strcpy( gui->axisLabels[ROW_AXIS].name, "PA" );
    gui->axisLabels[ROW_AXIS].first = 'A';
    gui->axisLabels[ROW_AXIS].last  = 'P';

    strcpy( gui->axisLabels[COLUMN_AXIS].name, "RL" );
    gui->axisLabels[COLUMN_AXIS].first = 'R';
    gui->axisLabels[COLUMN_AXIS].last  = 'L';    

    /* Initialize things for when we load a uvh file */
    
    /* Initialize the IS axis name */
    strcpy( gui->axisLabelMap[IS_AXIS], "IS" );

    /* Initialize the PA axis name */
    strcpy( gui->axisLabelMap[PA_AXIS], "PA" );

    /* Initialize the RL axis name */
    strcpy( gui->axisLabelMap[RL_AXIS], "RL" );
}
