#define Local_vars
#include "toqsh.h"
#include "launch_tools.h"
#include "libhelp.h"
#include "environment_tools.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Variables used only in this file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

static String fallbackResources[] = {
                     "*viewarea*width: 600", "*viewarea*height: 600",
		      NULL};

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Local Prototypes (only used in this file)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

void init_gui_globals(main_gui_t *gui);
void init_gui_gc(main_gui_t *gui);
void build_gui(main_gui_t *gui);

void init_image_block(main_gui_t *gui);
void Load_QshCB(Widget w, XtPointer clientdata, XtPointer calldata);

void CreateOptionsPane(main_gui_t *gui);
void CreateFilePane(main_gui_t *gui);
void CreateEditPane( main_gui_t * gui );
void CreatePreferencesPane(main_gui_t *gui);
void CreateHelpPane(main_gui_t *gui);
void CreateMenuBar(main_gui_t *gui);
void ExitCallback (Widget w, XtPointer clientData, XtPointer callData);

void Messages_toggleCB(Widget w, XtPointer clientData,XtPointer callData);
void Mouse_Functions_ToggleCB(Widget w, XtPointer clientData, XtPointer callData);
void LaunchAppCB(Widget w, XtPointer clientdata,XtPointer calldata);

void print_main_gui(main_gui_t *gui);

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
  main_gui_t gui;

  ET_checkEnvironment( ); /* check environment variables */

  gui.qsh_gui = (qsh_gui_t *)MT_malloc(sizeof(qsh_gui_t));


  /********************************************************/
  /*  Initialize the global variables
  /********************************************************/
  init_gui_globals(&gui);

  /********************************************************/
  /*  Initialize the top level app
  /********************************************************/
  gui.toplevel = XtAppInitialize(&gui.app, "SeraImage", 
				 options, XtNumber(options), 
				 &argc, argv,
				 fallbackResources, NULL, 0);

  set_debug_values( argv[0], gui.toplevel);

  if (argc > 1) debug_syntax(argc,argv);

  /********************************************************/
  /*  Setup the display 
  /********************************************************/
  gui.display = XtDisplay( gui.toplevel );
  gui.screen  = DefaultScreen( gui.display );

  gui.Screenwidth  = DisplayWidth( gui.display, gui.screen );
  gui.Screenheight = DisplayHeight( gui.display, gui.screen );
  printf("Screen size is :   %d x %d\n",gui.Screenwidth,gui.Screenheight);

  gui.qsh_gui->qsh_toplevel = gui.toplevel;
  gui.qsh_gui->qsh_app = gui.app;

  /********************************************************
   *  Initialize colors, and the colormap
   ********************************************************/
  init_colors(&gui);
  add_guaranteed_colors(&gui);
  load_gamma_colormap(&gui,gui.color_info.cmap_values,(float)(gui.color_info.gamma)/10.0);
  colormap_load_rgb(&gui);

  /********************************************************/
  /* Define Red and Green Pixels */
  /********************************************************/
  if (gui.color_info.colortype == PseudoColor){
    gui.red_pixel = RESERVED_RED;
    gui.green_pixel = RESERVED_GREEN;
    gui.blue_pixel = RESERVED_BLUE;
    gui.cyan_pixel = RESERVED_CYAN;
    gui.magenta_pixel = RESERVED_MAGENTA;
    gui.yellow_pixel = RESERVED_YELLOW;
  }else{
    gui.red_pixel = gui.color_info.cmap_pixels[RESERVED_RED];
    gui.green_pixel = gui.color_info.cmap_pixels[RESERVED_GREEN];
    gui.blue_pixel = gui.color_info.cmap_pixels[RESERVED_BLUE];
    gui.cyan_pixel = gui.color_info.cmap_pixels[RESERVED_CYAN];
    gui.magenta_pixel = gui.color_info.cmap_pixels[RESERVED_MAGENTA];
    gui.yellow_pixel = gui.color_info.cmap_pixels[RESERVED_YELLOW];
  }

  /********************************************************/
  /*  Build the GUI for all widgets and windows
  /********************************************************/
  build_gui(&gui);

  /****************************************************
   * init_remembered_files will copy over gui.app to
   * the remembered_files_t structure, so we have to
   * call it after the call to XtAppInitialize
   ****************************************************/
  init_remembered_files( &gui );

  /*****************************************************/
  /*** realize the top level and start the main loop ***/
  /*****************************************************/
  XtRealizeWidget(gui.toplevel);

  init_gui_gc(&gui);

  if (argc >= 2){ 
    if(strstr(argv[1],".qim") || strstr(argv[1],".qhd"))
      load_qsh(&gui,argv[1]);
  }

  XtAppMainLoop(gui.app);
  
  return 0;             /* ANSI C requires main to return int. */
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
					     xmMainWindowWidgetClass,gui->toplevel,
					     NULL);

 /*************************************************/
 /** Build the Menubar
 /*************************************************/
 CreateMenuBar(gui);
 DEBUG_GUI  printf("Built the menubar\n");

  /*************************************************/
  /** build the main form on which everything**/
  /** is built**/
  /*************************************************/
  gui->mainform = XmCreateForm(gui->mainwindow, "mainform", NULL, 0);
  XtManageChild(gui->mainform);
  DEBUG_GUI printf("Built the mainform\n");

  XtVaGetValues(gui->mainform,
		XmNhighlightColor,    &(gui->hl.pixel),
		XmNforeground,        &(gui->fg.pixel),  
		XmNbackground,        &(gui->bg.pixel),
		XmNtopShadowColor,    &(gui->ts.pixel),
		XmNbottomShadowColor, &(gui->bs.pixel),
		NULL);
  
  /*printf("got all the window colors\n");*/

  DEBUG_GUI printf("Got the decoration colors\n");

  build_top_level_forms(gui);
  DEBUG_GUI printf("Built top_level_forms\n");

  build_controls(gui);
  DEBUG_GUI printf("Built the controls\n");

  init_image_block(gui);
  DEBUG_GUI printf("Initialized the image block\n");

  build_message_pad(gui);
  DEBUG_GUI printf("Built message_pad\n");

  build_unknown_raw_popup_shell(gui);
  DEBUG_GUI printf("Built the unknown_raw_popup\n");

  build_manipulation_gui ( gui );
  DEBUG_GUI printf("Built the manipulation dialog\n");

  build_reslice_gui ( gui );
  DEBUG_GUI printf("Built the reslice dialog\n");

  BuildRegisterGui (gui->app, gui->toplevel, &(gui->register_gui), 
     &(gui->color_info));
  DEBUG_GUI printf("Built the register dialog\n");

  build_mark_images_gui ( gui );
  DEBUG_GUI printf ( "Built the mark images dialog\n" );

  DEBUG_TRACE_OUT printf("Done with build_gui\n");
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
  DEBUG_GUI  printf("built the FILE Pane\n");             
  
  CreateOptionsPane(gui);
  DEBUG_GUI  printf("built the Options Pane\n");

  CreateEditPane( gui );
  DEBUG_GUI  printf("built the Edit Pane\n");
  
  CreatePreferencesPane(gui);
  DEBUG_GUI  printf("built the Preferences Pane\n");
  
  CreateHelpPane(gui);
  DEBUG_GUI  printf("built the Help Pane\n");
  
  XtManageChild(gui->menubar);
  
  DEBUG_TRACE_OUT printf("Done with CreateMenuBar\n");
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
  Widget file_cascade, submenu, load[4],exit_b,
    load_submenu[4],load_cascade[4],
    save_button, load_dicom, save_dicom_button,
    gen_raw_images_button,
    separator, check_version;

    /*
     * The following have been replaced with 
     * LT_make_launch_menu(). mbr 1-7-99 
     */

    /*launch_submenu, launch_cascade,launch[3],*/
 
  DEBUG_TRACE_IN  printf("Entered CreateFilePane\n");

  /*************************************************/
  /** Create the File Pulldown with 3 buttons:
  /**   open, close, exit.
  /*************************************************/
  submenu = (Widget)XmCreatePulldownMenu(gui->menubar, "fileSubmenu", NULL,0);
  
  file_cascade = XtVaCreateManagedWidget("File", 
					 xmCascadeButtonWidgetClass,gui->menubar,
					 XmNsubMenuId, submenu,
					 NULL);
  
  load[0] = XtVaCreateManagedWidget("Load QSH",
				    xmPushButtonWidgetClass, submenu,
				    NULL);
  XtAddCallback(load[0],XmNactivateCallback,
		Load_QshCB,(XtPointer)gui);

  load[1] = XtVaCreateManagedWidget("Load Single Images (Replace)",
				    xmPushButtonWidgetClass, submenu,
				    NULL);
  XtAddCallback(load[1],XmNactivateCallback,
		Load_Single_Images_ReplaceCB,(XtPointer)gui);

  load[2] = XtVaCreateManagedWidget("Load Single Images (Append)",
				    xmPushButtonWidgetClass, submenu,
				    NULL);
  XtAddCallback(load[2],XmNactivateCallback,
		Load_Single_Images_AppendCB,(XtPointer)gui);
  /*  
      XtVaSetValues(load[2],
      XmNsensitive, FALSE,
      NULL);
  */
  load[3] = XtVaCreateManagedWidget("Unload Images",
				    xmPushButtonWidgetClass,submenu,
				    NULL);
  XtAddCallback(load[3],XmNactivateCallback,
		Unload_ImagesCB,(XtPointer)gui);
  
  gen_raw_images_button = XtVaCreateManagedWidget("Generate Raw Images",
				    xmPushButtonWidgetClass, submenu,
				    NULL);
  XtAddCallback(gen_raw_images_button,XmNactivateCallback,
		Generate_Raw_ImagesCB,(XtPointer)gui);

  save_button = XtVaCreateManagedWidget("Save QSH",
				    xmPushButtonWidgetClass, submenu,
				    NULL);
  XtAddCallback(save_button,XmNactivateCallback,
		Save_QshCB,(XtPointer)gui);

  separator 
      = XtVaCreateManagedWidget ( "separator", xmSeparatorWidgetClass,
				  submenu, NULL );
  
  load_dicom = XtVaCreateManagedWidget("Load Dicom",
				    xmPushButtonWidgetClass, submenu,
				    NULL);
  XtAddCallback(load_dicom,  XmNactivateCallback,
		Load_DicomCB,(XtPointer)gui);

  save_dicom_button = XtVaCreateManagedWidget("Save Dicom",
				    xmPushButtonWidgetClass, submenu,
				    NULL);
  XtAddCallback(save_dicom_button,XmNactivateCallback,
		Save_DicomCB,(XtPointer)gui);

  separator 
      = XtVaCreateManagedWidget ( "separator", xmSeparatorWidgetClass,
				  submenu, NULL );
  
  LT_make_launch_menu( submenu, "seraImage" ); /* added 1-7-99 mbr */

  /*  
  launch_submenu = XmCreatePulldownMenu(submenu,"ls",NULL,0);
  launch_cascade = XtVaCreateManagedWidget("Launch",
					   xmCascadeButtonWidgetClass,
					   submenu,
					   XmNsubMenuId,launch_submenu,
					   NULL);
  launch[0] = XtVaCreateManagedWidget("Image Tools",
				      xmPushButtonWidgetClass,
				      launch_submenu,NULL,0);
  launch[1] = XtVaCreateManagedWidget("Sera3D",
				      xmPushButtonWidgetClass,
				      launch_submenu,NULL,0);
  launch[2] = XtVaCreateManagedWidget("Doseplay",
				      xmPushButtonWidgetClass,
				      launch_submenu,NULL,0);
  XtAddCallback(launch[0],XmNactivateCallback,LaunchAppCB,NULL);
  XtAddCallback(launch[1],XmNactivateCallback,LaunchAppCB,NULL);
  XtAddCallback(launch[2],XmNactivateCallback,LaunchAppCB,NULL);
  */

  check_version
      = XtVaCreateManagedWidget ( "Check Version", 
				  xmPushButtonWidgetClass,
				  submenu, NULL );

  XtAddCallback ( check_version, XmNactivateCallback,
		  check_version_CB, NULL );

  exit_b = XtCreateManagedWidget ("Exit", 
				  xmPushButtonWidgetClass,
				  submenu, NULL, 0);
  XtAddCallback (exit_b, XmNactivateCallback,
		 ExitCallback, (XtPointer)gui);

  DEBUG_TRACE_OUT  printf("Done with CreateFilePane\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     CreateEditPane
%%%
%%%  Purpose:      Create the Edit menu on the main menubar.
%%%
%%%  Parameters:   gui -> A ptr to the main_gui_t structure.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void CreateEditPane( main_gui_t * gui )
{
    Widget editCascadeButton;
    Widget editPane;
    Widget colormapButton;
    XmString xmstr;
    
    DEBUG_TRACE_IN printf("Entering CreateEditPane\n");

    /* Create the Edit button on the main menubar */
    xmstr = XmStringCreateLtoR( "Edit", XmSTRING_DEFAULT_CHARSET );
    editCascadeButton =
        XtVaCreateManagedWidget( "editCascadeButton", xmCascadeButtonWidgetClass,
                                 gui->menubar,
                                 XmNlabelString, xmstr,
                                 NULL );
    XmStringFree( xmstr );

    /* Create the Edit pulldown menu */
    editPane = XmCreatePulldownMenu( gui->menubar, "editPane", NULL, 0 );

    XtVaSetValues( editCascadeButton,
                   XmNsubMenuId, editPane,
                   NULL );

    /* Add buttons to the pulldown menu */
    xmstr = XmStringCreateLtoR( "Colormap", XmSTRING_DEFAULT_CHARSET );
    colormapButton =
        XtVaCreateManagedWidget( "colormapButton", xmPushButtonWidgetClass,
                                 editPane,
                                 XmNlabelString, xmstr,
                                 NULL );
    XmStringFree( xmstr );

    /* Add a callback for the color map button */

    XtAddCallback( colormapButton, XmNactivateCallback,
                   showColorToolCB, (XtPointer) gui );

    DEBUG_TRACE_OUT printf("Leaving CreateEditPane\n");
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
  Widget cascade, submenu, axis_cascade, 
    message_button, labeling,
    tool_bar_button,
    mouse_functions_button,
    color_tool_button,
    qhd_button,
    /*resize_images_button,*/
    reverse_images_button,
    flip_images_button,
    rotate_button,
    reslice_button,
    register_button;

  DEBUG_TRACE_IN  printf("Entered CreateOptionsPane\n");

  /*************************************************/
  /** Create the Options Pulldown with 2 buttons:
  /**   axis, multiview.
  /*************************************************/
  submenu = (Widget)XmCreatePulldownMenu (gui->menubar, "optionsSubmenu", NULL, 0);

  cascade = XtVaCreateManagedWidget ("Options",
				     xmCascadeButtonWidgetClass,gui->menubar,
				     XmNsubMenuId, submenu,
				     NULL);
 	 
  qhd_button = XtVaCreateManagedWidget("Edit QHD", 
				       xmPushButtonWidgetClass, submenu,
				       NULL);
  XtAddCallback(qhd_button,XmNactivateCallback,
		Show_QhdCB, (XtPointer)gui);

  tool_bar_button = XtVaCreateManagedWidget("Tool Bar", 
					    xmToggleButtonWidgetClass, submenu,
					    XmNset, TRUE,
					    NULL);
  XtAddCallback(tool_bar_button,XmNvalueChangedCallback,
		Tool_Bar_ToggledCB,(XtPointer)gui);
	 	 
  /*color_tool_button = XtVaCreateManagedWidget("Color Tool", 
					      xmPushButtonWidgetClass, submenu,
					      NULL);*/
  /*XtAddCallback(color_tool_button,XmNactivateCallback,
		Toggle_Color_ToolCB,(XtPointer)gui);*/
  
  message_button = XtVaCreateManagedWidget ("Messages", 
					    xmToggleButtonWidgetClass, submenu,
					    NULL);
  XtAddCallback(message_button,XmNvalueChangedCallback,
		Messages_toggleCB,(XtPointer)gui);
  XtVaSetValues(message_button,XmNset,TRUE,NULL);

  mouse_functions_button = XtVaCreateManagedWidget ("Mouse Functions",
						    xmToggleButtonWidgetClass, submenu,
						    XmNset, True,
						    NULL);
  XtAddCallback(mouse_functions_button, XmNvalueChangedCallback,
		Mouse_Functions_ToggleCB, (XtPointer) gui);
  gui->show_mouse_functions = 1;
  

  /*resize_images_button = XtVaCreateManagedWidget ("Resize Images", 
    xmPushButtonWidgetClass, submenu,
    NULL);
    XtAddCallback(resize_images_button, XmNactivateCallback,
    Resize_ImagesCB, (XtPointer)gui);*/

  reverse_images_button = XtVaCreateManagedWidget ("Reverse Images", 
					    xmPushButtonWidgetClass, submenu,
					    NULL);
  XtAddCallback(reverse_images_button, XmNactivateCallback,
		ReverseImagesCB, (XtPointer)gui);

  flip_images_button = XtVaCreateManagedWidget ("Flip Images", 
					    xmPushButtonWidgetClass, submenu,
					    NULL);
  XtAddCallback(flip_images_button, XmNactivateCallback,
		FlipImagesCB, (XtPointer)gui);

  rotate_button = XtVaCreateManagedWidget ("Manipulate Images", 
					   xmPushButtonWidgetClass, submenu,
					   NULL);
  XtAddCallback(rotate_button, XmNactivateCallback,
		manipulate_images_CB, (XtPointer)gui);

  reslice_button = XtVaCreateManagedWidget ("Reslice Images", 
					   xmPushButtonWidgetClass, submenu,
					   NULL);
  XtAddCallback(reslice_button, XmNactivateCallback,
		reslice_images_CB, (XtPointer)gui);

  register_button = XtVaCreateManagedWidget ("Register Images", 
					   xmPushButtonWidgetClass, submenu,
					   NULL);
                                           
  XtAddCallback(register_button, XmNactivateCallback,
		RegisterImagesCB, (XtPointer) &(gui->register_gui));

  DEBUG_TRACE_OUT  printf("Done with CreateOptionsPane\n");
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
  Widget cascade, submenu, 
         button1, preferences_dialog;

  DEBUG_TRACE_IN   printf("Entered CreatePreferencesPane\n");

  /*************************************************/
  /** Create the Preferences Pulldown with 1 button:
  /**   set preferences
  /*************************************************/
  submenu = (Widget)XmCreatePulldownMenu (gui->menubar, "preferencesSubmenu", 
					  NULL, 0);

  cascade = XtVaCreateManagedWidget ("Preferences",
				     xmCascadeButtonWidgetClass,gui->menubar,
				     XmNsubMenuId, submenu,
				     XmNsensitive, False,
				     NULL);
  
  button1 = XtVaCreateManagedWidget ("Preferences . . .", 
				     xmPushButtonWidgetClass,
				     submenu, NULL);
  /*
  pr eferences_dialog = (Widget)build_preferences_dialog(button1);
  /*  
  XtAddCallback (button1, XmNactivateCallback,
		 Show_preferences_dialogCB, preferences_dialog);
		 */
  DEBUG_TRACE_OUT  printf("Done with CreatePreferencesPane\n");
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

  DEBUG_TRACE_IN  printf("Entered CreateHelpPane\n");
	     
  /*************************************************/
  /** Create the Help Pulldown
  /*************************************************/
  submenu = (Widget)XmCreatePulldownMenu (gui->menubar, "helpSubmenu", NULL, 0);

  cascade = XtVaCreateManagedWidget ("Help",
				     xmCascadeButtonWidgetClass,gui->menubar,
				     XmNsubMenuId, submenu,
				     NULL);

  XtVaSetValues(gui->menubar, XmNmenuHelpWidget, cascade, NULL);

  button1 = XtCreateManagedWidget("HelponContext", xmPushButtonWidgetClass,
				  submenu, NULL, 0);

  /** set the directory for Context Sensitive Help **/
  set_preference_directory("/SeraImage/");
  
  XtAddCallback (button1, XmNactivateCallback, 
		 ContextHelpCallback, (XtPointer)gui->mainwindow);
		 
  DEBUG_TRACE_OUT printf("Done with CreateHelpPane\n");
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

  DEBUG_TRACE_IN  printf("Entered Messeges_toggleCB\n");

  /*XtVaSetValues(gui->toplevel, XmNallowShellResize, TRUE, NULL);*/
  DisplayBusyCursor(gui->mainwindow);

  if (gui->messages_on) {
    XtUnmanageChild(gui->message_frame);
    XtVaSetValues( gui->display_outer_form,
		   XmNtopAttachment, XmATTACH_WIDGET,
		   XmNtopWidget,     gui->menubar,
		   NULL );
    gui->messages_on = 0;
  }else {
    XtManageChild(gui->message_frame);
    XtVaSetValues( gui->display_outer_form,
		   XmNtopAttachment, XmATTACH_WIDGET,
		   XmNtopWidget,     gui->message_frame,
		   NULL );    
    gui->messages_on = 1;
  }
  RemoveBusyCursor(gui->mainwindow);
  /*XtVaSetValues(gui->toplevel, XmNallowShellResize, FALSE, NULL);*/

  DEBUG_TRACE_OUT printf("Done with Messeges_toggleCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: toggles the message bar on/off
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Show_QhdCB(Widget w, XtPointer clientData,XtPointer callData)
{
  main_gui_t *gui = (main_gui_t *)clientData;

  DEBUG_TRACE_IN printf("Entered Show_QhdCB\n");

  if( is_allowed_callback( gui ) )
  {
    if (!gui->images_loaded){
      DEBUG_TRACE_OUT printf("Done with Show_QhdCB\n");
      return;
    }
    gui->qsh_gui->num_mappings = 0;
    gui->qsh_gui->mode = QSH_EDITING;

    check_and_report_qhd_values(gui->qsh_gui);
    update_zvalues_on_images( gui );
    
    /*printf("calling edit_qhd_values\n");
      edit_qhd_values(gui->qsh_gui);*/
  }
  DEBUG_TRACE_OUT printf("Done with Show_QhdCB\n");
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
    main_gui_t * gui = (main_gui_t *) clientData;

    if( is_allowed_callback( gui ) )
    {
        /* Print any memory that hasn't been freed. */
        MT_onexit();
        
        /*************************************************/
        /** Quitting time.
        /*************************************************/
        exit(0);
    }
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Init_gui_globals
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: initializes all of the global variables.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_gui_globals(main_gui_t *gui)
{
  int i;

  DEBUG_TRACE_IN  printf("Entered init_gui_globals\n");

  gui->image_block.num_columns = 4;
  gui->messages_on = 1;
  gui->images_loaded = 0;
  gui->specify_ils_when_saving = 1;
  gui->color_normalization = COLOR_OPT_NONE;

  gui->image_block.width = 128;
  gui->image_block.height = 128;
  gui->image_block.num_images = 0;


  for (i=0; i<256; i++) {
    gui->gray_colormapping[i] = (int)((float)i*(float)(MAX_GRAY - MIN_GRAY + 1)/256.0 + (float)MIN_GRAY);
  }

  DEBUG_TRACE_OUT  printf("Done with init_gui_globals\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Init_gui_gc
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: none
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_gui_gc(main_gui_t *gui)
{
  static XGCValues gcv;

  DEBUG_TRACE_IN  printf("Entered init_gui_gc\n");

  gcv.function = GXcopy;
  /*  
  gui->image_block.gc = XCreateGC(gui->display,
				  XtWindow(gui->mainwindow), 
				  GCFunction, &gcv);
  */
  gui->gc = XCreateGC(gui->display,
		      XtWindow(gui->mainwindow), 
		      GCFunction, &gcv);

  DEBUG_TRACE_OUT  printf("Done with init_gui_gc\n");
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

  DEBUG_TRACE_IN  printf("Entered get_resource_path_name\n");

  ResourcePath = (char *)getenv("SERA_RESOURCES");

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
  DEBUG_TRACE_OUT  printf("Done with get_resource_path_name\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void ReverseImagesCB(Widget w, XtPointer clientdata,XtPointer calldata)
{
    main_gui_t *gui = (main_gui_t *)clientdata;

    DEBUG_TRACE_IN printf("Entered ReverseImagesCB\n");

    if( gui->images_loaded )
    {
        DisplayBusyCursor(gui->mainwindow);
        reverse_images_in_qsh_structure(gui);
        destroy_and_rebuild_images(gui);
        RemoveBusyCursor(gui->mainwindow);
    }
    DEBUG_TRACE_OUT printf("Done with ReverseImagesCB\n");
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
void reverse_images_in_qsh_structure(main_gui_t *gui)
{
  int i;
  int num_images;
  int image_size;
  unsigned char *new_images;
  unsigned char *old_images;

  if (!gui->images_loaded) return;

  num_images = gui->qsh_gui->qsh_info->size_of_dimension[0];
  image_size = gui->qsh_gui->qsh_info->size_of_dimension[1] * gui->qsh_gui->qsh_info->size_of_dimension[2];
  old_images = gui->qsh_gui->qsh_info->images;

  new_images = (unsigned char *) MT_malloc(num_images*image_size);

  for (i=0;i<num_images;i++){
    memcpy(&new_images[i*image_size],&old_images[(num_images-i-1)*image_size],image_size);
  }

  MT_free( (void *) old_images);
  gui->qsh_gui->qsh_info->images = new_images;
  
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Gary Harkin (copied)
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void FlipImagesCB(Widget w, XtPointer clientdata,XtPointer calldata)
{
    main_gui_t *gui = (main_gui_t *)clientdata;

    DEBUG_TRACE_IN printf("Entered FlipImagesCB\n");

    if( gui->images_loaded )
    {
        DisplayBusyCursor(gui->mainwindow);
        flip_images_in_qsh_structure(gui);
        destroy_and_rebuild_images(gui);
        RemoveBusyCursor(gui->mainwindow);
    }
    DEBUG_TRACE_OUT printf("Done with FlipImagesCB\n");
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
void flip_images_in_qsh_structure(main_gui_t *gui)
{
  int i, sizex, sizey, row, col;
  int num_images;
  int image_size;
  unsigned char *new_images, *np;
  unsigned char *old_images, *op;

  if (!gui->images_loaded) return;

  num_images = gui->qsh_gui->qsh_info->size_of_dimension[0];
  sizex = gui->qsh_gui->qsh_info->size_of_dimension[1];
  sizey = gui->qsh_gui->qsh_info->size_of_dimension[2];

  image_size = sizex * sizey;
  old_images = gui->qsh_gui->qsh_info->images;

  new_images = (unsigned char *) MT_malloc(num_images*image_size);

  for (i=0;i<num_images;i++)
  {
    for (row = 0; row < sizey; row ++)
    {
      np = new_images + image_size * i + sizex * row;
      op = old_images + image_size * i + sizex * row + sizex - 1;
      for (col = 0; col < sizex; col ++)
         *np++ = *op--;
    }
  }

  MT_free( (void *) old_images);
  gui->qsh_gui->qsh_info->images = new_images;
  
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : LaunchAppCB
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: (cb)
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void LaunchAppCB(Widget w, XtPointer clientdata,XtPointer calldata)
{
  DEBUG_TRACE_IN  printf("Entered LaunchAppCB\n");

  if (strstr(XtName(w),"Image Tools"))
      system("../../ImageTools/Editor/image_tools &");
  else if (strstr(XtName(w),"Sera3D"))
      system("../../Bnct3D/sera3d &");
  else if (strstr(XtName(w),"Doseplay"))
      system("../../DoseDisplay/Contours/doseplay &");

  DEBUG_TRACE_OUT  printf("Done with LaunchAppCB\n");
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
void init_image_block(main_gui_t *gui)
{
  int i;

  DEBUG_TRACE_IN   printf("Entered init_image_block\n");

  /*printf("initializing the image_block.rc\n");*/
  gui->image_block.rc = XtVaCreateWidget("rc",
					 xmRowColumnWidgetClass, gui->display_form,
					 XmNorientation, XmHORIZONTAL,
					 XmNpacking, XmPACK_COLUMN,
					 NULL);
  /*set_image_block_rc_columns(gui);*/

  gui->image_block.num_images = 0;

  DEBUG_TRACE_OUT printf("Done with init_image_block\n");
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
void Unload_ImagesCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  int i;
  
  DEBUG_TRACE_IN   printf("Entered Unload_ImagesCB\n");

  if( is_allowed_callback( gui ) )
  {
    if (gui->images_loaded){
      /** get rid of the images in the image block **/
      remove_images_in_image_block(gui);
    
      /** get rid of the qsh_info structure **/
      Free_Qsh(gui->qsh_gui->qsh_info);

      /** reset the specify_ils_when_saving flag **/
      gui->specify_ils_when_saving = 1;
      
    }

    adjust_manip_window_to_program_changes ( gui );
  }
  DEBUG_TRACE_OUT  printf("Done with Unload_ImagesCB\n");
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
void destroy_and_rebuild_images(main_gui_t *gui)
{
  int i;  
  int size_x,size_y, size_z;
  unsigned char *ptr;

  DEBUG_TRACE_IN   printf("Entered destroy_and_rebuild_images\n");

  /*print_main_gui(gui);*/

  /* remove if need be */
  remove_images_in_image_block(gui);

  size_x = gui->qsh_gui->qsh_info->size_of_dimension[1];
  size_y = gui->qsh_gui->qsh_info->size_of_dimension[2];
  size_z = gui->qsh_gui->qsh_info->size_of_dimension[0];

  /* Allocate memory for image block images */
  gui->image_block.image = (image_t *) MT_malloc ( sizeof(image_t)*size_z );

  /** get the row column ready **/
  gui->image_block.rc = XtVaCreateWidget("rc",
					 xmRowColumnWidgetClass, gui->display_form,
					 XmNorientation, XmHORIZONTAL,
					 XmNpacking, XmPACK_COLUMN,
					 NULL);     
					 

  DEBUG_DATA printf("rebuilding %d images\n",gui->qsh_gui->qsh_info->size_of_dimension[0]);
  for (i=0; i < size_z; i++){
    ptr = &gui->qsh_gui->qsh_info->images[i * size_x * size_y];

    build_ximage_and_add_to_image_block(gui,size_x,size_y,ptr);
    XtAddCallback ( gui->image_block.image[i].drawing_area, XmNexposeCallback, PictureExposedCB, (XtPointer)gui);

    if( gui->move_images_button.state == END_IMAGE_MOVE ) /* have to add if still in "move image" mode */
    {
      XtAddEventHandler(gui->image_block.image[i].drawing_area, ButtonReleaseMask, FALSE, image_selected_EH,    (XtPointer)gui);   
      XtAddEventHandler(gui->image_block.image[i].drawing_area, EnterWindowMask,   FALSE, highlight_image_EH,   (XtPointer)gui);
      XtAddEventHandler(gui->image_block.image[i].drawing_area, LeaveWindowMask,   FALSE, dehighlight_image_EH, (XtPointer)gui);
    }

    if( gui->move_images_button.state == START_IMAGE_MOVE ){ /* don't add if in the "move image" mode */
      XtAddCallback(gui->image_block.image[i].drawing_area, XmNactivateCallback, open_manip_on_double_click_CB, (XtPointer)gui);
    }
    gui->image_block.image[i].marked_for_delete = 0;
  }

  gui->image_block.image_move_started = 0;
  gui->images_loaded = 1;
  set_image_block_rc_columns(gui);  
  /*printf("calling color_normalize_images\n");*/
  color_normalize_images(gui);
  
  XtManageChild(gui->image_block.rc);
  
  for (i=0;i<gui->image_block.num_images;i++)
    register_colormap_ehs_for_widget(gui, gui->image_block.image[i].drawing_area,gui->color_info.cmap);
      
  DEBUG_DATA printf("done, there are now %d images in the image_block\n",gui->image_block.num_images);
  DEBUG_TRACE_OUT  printf("Done with destroy_and_rebuild_images\n");
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
void remove_images_in_image_block(main_gui_t *gui)
{
  int i;

  DEBUG_TRACE_IN printf("Entered remove_images_in_image_block, images_loaded is : %d\n",gui->images_loaded);

  if (gui->image_block.num_images == 0){
    DEBUG_TRACE_OUT printf("Done with remove_images_in_image_block\n");    
    return;
  }

  DEBUG_DATA printf("deleting the %d images from the data_block\n",gui->image_block.num_images);
  /*XtUnmanageChild(gui->image_block.rc);*/
  for (i = 0;i<gui->image_block.num_images;i++){
    XtRemoveCallback ( gui->image_block.image[i].drawing_area, 
		       XmNexposeCallback, PictureExposedCB,
		       (XtPointer)gui);
    XDestroyImage(gui->image_block.image[i].ximage);
    gui->image_block.image[i].z_value = 0.0;    
    XtDestroyWidget(gui->image_block.image[i].frame); 
  } 
  MT_free ( (void *) gui->image_block.image );
  
  /* destroy the whole rc widget */
  XtDestroyWidget(gui->image_block.rc);
  
  gui->image_block.num_images = 0; 
  gui->images_loaded = 0;
  
  DEBUG_TRACE_OUT printf("Done with remove_images_in_image_block\n");
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
void build_ximage_and_add_to_image_block(main_gui_t *gui,int x_size,int y_size,
					 unsigned char *data)
{
    int i;
    int image_num;
    unsigned char *new_image;
    unsigned char *temp_ptr;
    static XmString xmstr;
    static char z_label[32];
    static char imageNumber[32];
    int size;

    DEBUG_TRACE_IN printf("Entered build_ximage_and_add_to_image_block\n");

    image_num = gui->image_block.num_images;
    size = gui->image_block.width * gui->image_block.height;
  
    gui->image_block.image[image_num].frame = 
        XtVaCreateManagedWidget("frame",
                                xmFrameWidgetClass,gui->image_block.rc,
                                XmNshadowThickness, 2,
                                XmNborderWidth, 0,
                                XmNshadowType, XmSHADOW_OUT,
                                NULL);

    gui->image_block.image[image_num].form = 
        XtVaCreateWidget( "form", xmFormWidgetClass,
                          gui->image_block.image[image_num].frame,
                          XmNmarginWidth, 0,
                          XmNmarginHeight, 0,
                          XmNverticalSpacing, 0,
                          XmNhorizontalSpacing, 0,
                          NULL );

    if( gui->image_block.width >= 96 ) /* big enough to display z_values */
    {
        /* find the z_value of this image */
        sprintf( z_label, "%-3d z=%4.2f", image_num + 1, gui->qsh_gui->qsh_info->image_location[image_num] );
        xmstr = XmStringCreateLocalized( z_label );

        gui->image_block.image[image_num].label =
            XtVaCreateManagedWidget( "label",
                                     xmLabelWidgetClass, gui->image_block.image[image_num].form,
                                     XmNlabelString,    xmstr,
                                     XmNalignment,      XmALIGNMENT_BEGINNING,
                                     XmNleftAttachment, XmATTACH_FORM,
                                     XmNtopAttachment,  XmATTACH_FORM, 
                                     XmNrightAttachment,XmATTACH_FORM,
                                     NULL );
        XmStringFree( xmstr );

        gui->image_block.image[image_num].drawing_area = 
            XtVaCreateManagedWidget("darea",
                                    xmDrawnButtonWidgetClass, gui->image_block.image[image_num].form,
                                    XmNwidth,gui->image_block.width,
                                    XmNhighlightThickness, 0,
                                    XmNborderWidth, 0,
                                    XmNshadowThickness, 0,
                                    XmNheight,gui->image_block.height,
                                    XmNleftAttachment, XmATTACH_FORM,
                                    XmNrightAttachment, XmATTACH_FORM,
                                    XmNbottomAttachment, XmATTACH_FORM,
                                    XmNtopAttachment, XmATTACH_WIDGET,
                                    XmNtopWidget, gui->image_block.image[image_num].label,
                                    XmNtopOffset, 2,
				    XmNmultiClick, XmMULTICLICK_KEEP,
                                    NULL);
    }
    else /* images too small to display z_values */
    {
        gui->image_block.image[image_num].drawing_area = 
            XtVaCreateManagedWidget("darea",
                                    xmDrawnButtonWidgetClass, gui->image_block.image[image_num].form,
                                    XmNwidth,gui->image_block.width,
                                    XmNhighlightThickness, 0,
                                    XmNborderWidth, 0,
                                    XmNshadowThickness,0,
                                    XmNheight,gui->image_block.height,
                                    XmNleftAttachment, XmATTACH_FORM,
                                    XmNrightAttachment, XmATTACH_FORM,
                                    XmNbottomAttachment, XmATTACH_FORM,
                                    XmNtopAttachment, XmATTACH_FORM,
                                    NULL);
    }    
  
    XtManageChild( gui->image_block.image[image_num].form );

    /*
      register_colormap_ehs_for_widget(gui, gui->image_block.image[image_num].drawing_area,gui->color_info.cmap);
      printf("\n\n\n************\nCalling registercolormap_ehs from build_ximage_and_add \n*********\n");
    */

    new_image = (unsigned char *) MT_malloc( size );
    generic_resize_image(data,new_image,
                         x_size,y_size,
                         gui->image_block.width,
                         gui->image_block.height,
                         1);
    temp_ptr = new_image;

    /* map the images into the gray range of the colormap */
    for (i=0; i < size; i++) {
        /*if (temp_ptr[i] > 250 ) printf("WARNING hit a value > 250\n");*/

        temp_ptr[i]= gui->gray_colormapping[temp_ptr[i]];

    
        /** make sure they are all in the gray range **/
        if (temp_ptr[i] > MAX_GRAY || temp_ptr[i] < MIN_GRAY){
            printf("WARNING pixel value out of grays\n");
        }
    }
  
    gui->image_block.image[image_num].ximage = 
        XCreateImage(gui->display, gui->visual, gui->color_info.depth,
                     ZPixmap, 0, (char *)new_image,
                     gui->image_block.width,
                     gui->image_block.height,
                     BitmapPad( gui->display ), 0 );

    /* don't free it, XCreateImage is not making a copy of the data, just pointing to ours **/

    /** we now have another image **/
    gui->image_block.num_images++;     

    DEBUG_TRACE_OUT printf("Done with build_ximage_and_add_to_image_block\n");
}

/* =====================================================================
   Function:      refresh_images_in_image_block

   Purpose:       Forces expose events on all the images in the image 
                  block.  Useful for changing the colormap during 
		  thresholding.

   Parameters:    Main Gui Structure.

   Returned:      None.

   MTC 3/8/99
   ===================================================================*/
void refresh_images_in_image_block ( main_gui_t *gui )
{
    int i;

    for ( i = 0; i < gui->image_block.num_images; i++ )
    {
	XClearArea ( gui->display, 
		     XtWindow ( gui->image_block.image[i].drawing_area ),
		     0, 0, gui->image_block.width, gui->image_block.height,
		     TRUE );
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
void PictureExposedCB (Widget w,XtPointer clientData, XtPointer calldata)
{
  static XExposeEvent *event;
  static int i;
  main_gui_t *gui = (main_gui_t *)clientData;
  image_t *the_image = NULL;
  /*  image_t *the_image = (image_t *)clientData;*/
  XmDrawingAreaCallbackStruct *cbs = (XmDrawingAreaCallbackStruct *)calldata;

  DEBUG_TRACE_IN  printf("Entered PictureExposedCB for image\n");

  /*printf("in PictureExposedCB, the number of images is : %d\n",gui->image_block.num_images);
  printf("gui->image_block.image[0].drawing_area : %d\n",gui->image_block.image[0].drawing_area);
  */
  for (i=0;i<gui->image_block.num_images;i++){
    /*printf("w : %d, gui->image_bock.image[%d].drawing_area : %d\n",w,i,gui->image_block.image[i].drawing_area);*/
    if (w == gui->image_block.image[i].drawing_area){
      the_image = &gui->image_block.image[i]; break;
    }
  }
  if (the_image == NULL){
    printf("didn't get the image in PictureExposesCB\n");
    DEBUG_TRACE_OUT  printf("Done with PictureExposedCB for image\n");
    return;
  }
  
  /*printf("In PictureExpose, the_image->marked_for_delete is : %d\n",the_image->marked_for_delete);*/

  if (the_image->marked_for_delete){ 
    /*printf("displaying the removed_ximage\n");*/
    myPutImage(gui, 
	       XtWindow(the_image->drawing_area), 
	       gui->gc, the_image->ximage, 
	       event->x, event->y, event->x, event->y,
	       event->width, event->height);    

    draw_x_on_image ( gui, the_image );

    DEBUG_TRACE_OUT  printf("Done with PictureExposedCB for image\n");
    return;
  }

  event=(XExposeEvent*)(cbs->event);
  if (XtIsManaged(the_image->drawing_area)){
    myPutImage(gui, 
	       XtWindow(the_image->drawing_area), 
	       gui->gc, the_image->ximage, 
	       event->x, event->y, event->x, event->y,
	       event->width, event->height);
    /*
    XPutImage(XtDisplay(w), 
	      XtWindow(the_image->drawing_area), 
	      gc, the_image->ximage, 
	      event->x, event->y, event->x, event->y,
	      event->width, event->height);
	      */
  }

  DEBUG_TRACE_OUT  printf("Done with PictureExposedCB for image\n");
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
void Tool_Bar_ToggledCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  XmToggleButtonCallbackStruct * cbs = 
    (XmToggleButtonCallbackStruct *) calldata;

  DEBUG_TRACE_IN printf("Entered Tool_Bar_ToggledCB\n");

  if( is_allowed_callback( gui ) )
  {
  
    if (XtIsManaged(gui->controls_form)){ 
      XtUnmanageChild(gui->controls_form);
      XtVaSetValues(gui->display_outer_form,
		    XmNrightAttachment, XmATTACH_FORM,
		    NULL);
      XtVaSetValues(gui->mouse_form,
		    XmNrightAttachment, XmATTACH_FORM,
		    NULL);
      DEBUG_GUI printf("Unmanaged the controls_form\n");
    }else{ 
      XtManageChild(gui->controls_form);
      XtVaSetValues(gui->display_outer_form,
		    XmNrightAttachment, XmATTACH_WIDGET,
		    XmNrightWidget, gui->controls_form,
		    NULL);
      XtVaSetValues(gui->mouse_form,
		    XmNrightAttachment, XmATTACH_WIDGET,
		    XmNrightWidget, gui->controls_form,
		    NULL);
      DEBUG_GUI printf("Managed the controls_form\n");
    }
  }
  else
  {
    XmToggleButtonSetState( w, !cbs->set, False );
  }

  DEBUG_TRACE_OUT printf("Done with Tool_Bar_ToggledCB\n");
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
void set_image_block_rc_columns(main_gui_t *gui)
{
  int calculated_num_columns;
  int num_columns;

  num_columns = gui->image_block.num_columns;
  calculated_num_columns = (int) ceil((float)gui->image_block.num_images/(float)num_columns);

  /*
  printf("setting the columns,num_images : %d, numcolumns : %d, num calculated columns : %d\n",gui->image_block.num_images,num_columns,
	 calculated_num_columns);
	 */

  XtVaSetValues(gui->image_block.rc, 
		XmNnumColumns, calculated_num_columns,
		NULL);
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
void Save_QshCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  char filename[256];
  char warning_string[512];
  int ok_to_save;
  int i;
  
  
  DEBUG_TRACE_IN printf("Entered Save_QshCB\n");

  if( is_allowed_callback( gui ) )
  {
    if (!gui->images_loaded){
      DEBUG_TRACE_OUT printf("Done with Load_QSHCB\n");
      return;
    }

    if( gui->specify_ils_when_saving )
    {
        get_image_location_values( gui );
        
        while( gui->specify_ils_when_saving == 1 )
            XtAppProcessEvent( gui->app, XtIMAll );
    }
    
    if (!get_file_name(gui,filename)){
      DEBUG_TRACE_OUT printf("Done with Load_QSHCB\n");
      return;
    }

    /*
     * We want the size_of_dimension[0] to match the number
     * of image locations, so make any extra image_locations
     * passed size_of_dimension[0] invalid here.
     */
    for( i = gui->qsh_gui->qsh_info->size_of_dimension[0]; i < MAX_QSH_SLICES; i++ )
    {
        gui->qsh_gui->qsh_info->valid.image_location[i] = 0;
    }
    
    
    /*
      printf("calling error_check_qhd\n");
      if (!error_check_qhd(gui->qsh_gui,QSH_ERROR_CHECK_FULL)){ 
      printf("calling check_and_report_qhd_values\n ");
      check_and_report_qhd_values(gui->qsh_gui);  
      }*/
  
    ok_to_save = 0;
    
    if (strstr(filename,".qhd") || strstr(filename,".qim")) 
      filename[strlen(filename)-4] = '\0';
    /*printf("the base filename is : %s\n",filename);*/
      
    strcat(filename,".qhd");
    
    /*
     * See if the file already exists.
     * Ask the user if we can overwrite it if it does.
     */
    if( FT_fileExists( filename ) )
    {
        sprintf( warning_string, "OK to overwrite\n%s?", filename );
        
        if( DT_decide(gui->toplevel, gui->app, warning_string,
                      "File Already Exists", "YES", "NO") )
        {
            ok_to_save = 1;
        }
    }
    else
    {
        ok_to_save = 1;
    }

    if( ok_to_save )
    {
        if (!write_qhd(gui->qsh_gui->qsh_info,filename))
        {
            DT_error( gui->toplevel, "Error writing the qhd file!", NULL, NULL );
            DEBUG_TRACE_OUT printf("Done with Save_QSHCB\n");
            return;
        }
    }
    else
    {
        return;
    }

    ok_to_save = 0;    

    filename[strlen(filename)-4] = '\0';
    strcat(filename,".qim");

    if( FT_fileExists( filename ) )
    {
        sprintf( warning_string, "OK to overwrite\n%s?", filename );

        if( DT_decide(gui->toplevel, gui->app, warning_string,
                      "File Already Exists", "YES", "NO") )
        {
            ok_to_save = 1;
        }
    }
    else
    {
        ok_to_save = 1;
    }

    if( ok_to_save )
    {
        DisplayBusyCursor(gui->mainwindow);
        
        if (!write_qim(gui->qsh_gui->qsh_info,filename))
        {
            DT_error( gui->toplevel, "Error writing the qim file!", NULL, NULL );
        }
        
        RemoveBusyCursor(gui->mainwindow);
    }
  }
  
  DEBUG_TRACE_OUT printf("Done with Save_QshCB\n");
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
void Load_QshCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
    main_gui_t *gui = (main_gui_t *)clientdata;
    char filename[256];

    DEBUG_TRACE_IN printf("Entered Load_QSHCB\n");

    /*
     * If the file ~/sera1/Resources/SeraImage/qsh_files.sav
     * already exists, popup the list of recently used files.
     * Otherwise, have the user use the file selection box.
     */
    if( is_allowed_callback( gui ) )
    {
        if( gui->rf.save_file_present )
        {
            if( get_filename_from_remembered_files_list( gui, filename ) )
            {
                DisplayBusyCursor(gui->mainwindow);
                load_qsh( gui, filename );
                gui->specify_ils_when_saving = 0;
                RemoveBusyCursor(gui->mainwindow);
            }
        }
        else
        {
            if( DT_select_file( gui->toplevel, gui->app, filename, "Load QSH" ) )
            {
                if( FT_fileExists( filename ) )
                {
                    if( is_a_valid_qsh_file( filename ) )
                    {
                        DisplayBusyCursor(gui->mainwindow);
                        add_to_saved_files( &gui->rf, filename );
                        gui->rf.save_file_present = 1;
                        load_qsh( gui, filename );
                        gui->specify_ils_when_saving = 0;
                        RemoveBusyCursor(gui->mainwindow);      
                    }
                    else
                    {
                        DT_error( gui->toplevel, "That is not a valid .qim or .qhd file!", "File Error", NULL );
                    }
                }
            }
        }

        adjust_manip_window_to_program_changes ( gui );
    }

    DEBUG_TRACE_OUT printf("Done with Load_QSHCB\n");
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
void Load_Single_Images_AppendCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  char filename[256];
  int files_safely_read;
  int i;

  DEBUG_TRACE_IN printf("Entered Load_Single_Images_AppendCB\n");

  if (!get_multiple_files(gui->toplevel,gui->app,&gui->mfb)){
    DEBUG_TRACE_OUT printf("Done with Load_Single_ImagesCB\n");
    return;
  }

  XtPopup(gui->ur.shell,XtGrabNone);
  smart_set_unknown_raw_popup(gui);
  if (!get_user_values_for_unknown_raw_image(gui)){
    printf("get_user_values_for_unkown_raw_image return 0\n");
    DEBUG_TRACE_OUT printf("Done with Load_Single_ImagesCB\n");
    return;
  }

  if (gui->ur.cancelled){
    DEBUG_TRACE_OUT printf("Done with Load_Single_ImagesCB\n");
    return;
  }

  DisplayBusyCursor(gui->mainwindow);  
  files_safely_read = 0;
  for (i=0;i<gui->mfb.num_files;i++){
    if (!read_unknown_raw_file(gui,gui->mfb.files[i])) break;
    files_safely_read++;
  }

  destroy_and_rebuild_images(gui);  
  adjust_manip_window_to_program_changes ( gui );

  if( gui->move_images_button.state == END_IMAGE_MOVE )
    gui->move_images_button.images_were_added = 1;

  /*
   * We've read some single images, which won't
   * have image locations to begin with.
   */
  gui->specify_ils_when_saving = 1;
  
  RemoveBusyCursor(gui->mainwindow);

  DEBUG_TRACE_OUT printf("Done with Load_Single_Images_AppendCB\n");
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
void Load_Single_Images_ReplaceCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  char filename[256];
  int files_safely_read;
  int i;

  DEBUG_TRACE_IN printf("Entered Load_Single_Images_ReplaceCB\n");

  if( is_allowed_callback( gui ) )
  {

    if (!get_multiple_files(gui->toplevel,gui->app,&gui->mfb)){
      DEBUG_TRACE_OUT printf("Done with Load_Single_ImagesCB\n");
      return;
    }

    XtPopup(gui->ur.shell,XtGrabNone);
    smart_set_unknown_raw_popup(gui);
    if (!get_user_values_for_unknown_raw_image(gui)){
      printf("get_user_values_for_unkown_raw_image return 0\n");
      DEBUG_TRACE_OUT printf("Done with Load_Single_ImagesCB\n");
      return;
    }

    if (gui->ur.cancelled){
      DEBUG_TRACE_OUT printf("Done with Load_Single_ImagesCB\n");
      return;
    }

    DisplayBusyCursor(gui->mainwindow);  
    Unload_ImagesCB(w,clientdata,calldata);

    files_safely_read = 0;
    for (i=0;i<gui->mfb.num_files;i++){
      if (!read_unknown_raw_file(gui,gui->mfb.files[i])) break;
      files_safely_read++;
    }

    /** reconstruct the new images **/
    destroy_and_rebuild_images(gui);  

    adjust_manip_window_to_program_changes ( gui );

    /*
     * We've read some single images, which won't
     * have image locations to begin with.
     */
    gui->specify_ils_when_saving = 1;
    
    
    RemoveBusyCursor(gui->mainwindow);
  }

  DEBUG_TRACE_OUT printf("Done with Load_Single_Images_ReplaceCB\n");
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
int load_qsh(main_gui_t *gui,char *filename)
{

  unsigned char *ptr;
  int i,size_x,size_y,bpp;
  int j, sum = 0;
  int reallyLoad;

  DEBUG_TRACE_IN printf("Entered load_qsh, filename of : %s\n",filename);

  if (gui->images_loaded)
  {
      reallyLoad =
          DT_decide( gui->toplevel, gui->app,
                     "Your current images will be overwritten.\nDo you wish to continue anyway?",
                     "Load New Images", "Yes", "No" );
  }
  else
      reallyLoad = 1;
  
  if( reallyLoad )
  {
      if( gui->images_loaded ) Unload_ImagesCB(gui->toplevel,(XtPointer)gui,NULL);
      
      gui->qsh_gui->qsh_info = (qsh_info_t *) MT_malloc(sizeof(qsh_info_t));
 
      if (!fill_correct_qhd_qim_filenames(gui->qsh_gui,filename)) return 0;

      gui->qsh_gui->mode = QSH_LOADING;
      /******************************************************************/
      /** read the qhd   **/
      /******************************************************************/
      if (!read_qhd(gui->qsh_gui)){
          printf("Could not read the qhd file, aborting\n");return 0;
      }
      
      /******************************************************************/
      /** after the qhd is successful get the images   **/
      /******************************************************************/
      if (!read_qim(gui->qsh_gui)){
          printf("Could not read the qim file, aborting\n");return 0;
      }
      
      size_x = gui->qsh_gui->qsh_info->size_of_dimension[1];
      size_y = gui->qsh_gui->qsh_info->size_of_dimension[2];
      
      destroy_and_rebuild_images(gui);
      
      gui->images_loaded = 1;
  }

  DEBUG_TRACE_OUT printf("Done with load_qsh\n");
  return( 1 );
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
void print_main_gui(main_gui_t *gui)
{
  int i;

  printf("\n\n======================================\n");
  printf("the main gui vars\n");
  printf("======================================\n");

  printf("\tdisplay        : %p\n",gui->display);
  printf("\tscreen         : %d\n",gui->screen);
  printf("\tapp            : %p\n",gui->app);
  printf("\tvisinfo        : %p\n",&gui->visinfo);
  printf("\tScreenwidth    : %d\n",gui->Screenwidth);
  printf("\tScreenheight   : %d\n",gui->Screenheight);
  printf("\tmessages_on    : %d\n",gui->messages_on);
  printf("\tWidggets:\n");
  printf("\ttoplevel       : %p\n",gui->toplevel);
  printf("\tmainwindow     : %p\n",gui->mainwindow);
  printf("\tmenubar        : %p\n",gui->menubar);
  printf("\tmainform       : %p\n",gui->mainform);
  printf("\tcontrols_form  : %p\n",gui->controls_form);
  printf("\tmessage_form   : %p\n",gui->message_form);
  printf("\tmessage_frame  : %p\n",gui->message_frame);
  printf("\tmessage_pad    : %p\n",gui->message_pad);
  printf("\n");
  printf("\timages_loaded  : %d\n",gui->images_loaded);
  printf("\tImage Block Struct\n");
  printf("\trc             : %p\n",gui->image_block.rc);
  printf("\tnum_images     : %d\n",gui->image_block.num_images);

  for (i=0;i<gui->image_block.num_images;i++){
    printf("\n\t\tImage #%d:\n",i);
    printf("\t\tximage       : %p\n",gui->image_block.image[i].ximage);
    /*printf("\t\tgc           : %d\n",gui->image_block.image[i].gc);*/
    printf("\t\tz_value      : %f\n",gui->image_block.image[i].z_value);
    printf("\t\tdrawing_area : %p\n",gui->image_block.image[i].drawing_area);
    printf("\t\tframe        : %p\n",gui->image_block.image[i].frame);
    printf("\t\tlabel        : %p\n",gui->image_block.image[i].label);
    printf("\t\tmarked for delete : %d\n",gui->image_block.image[i].marked_for_delete);
  }
  printf("======================================\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : Generate_Raw_ImagesCB
%%%
%%%  Written by: Cory
%%%
%%%  Parameters: 
%%%
%%%  Purpose:    simply calls write_all_qim_single_images from libqsh
%%%              to output the qsh structure in single image files
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Generate_Raw_ImagesCB(Widget w, XtPointer clientdata, XtPointer call_data)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  int i;
  unsigned char *ptr;
  char base_filename[256] = {""};
  
  DEBUG_TRACE_IN printf("Entered Generate_Raw_ImagesCB\n");
 
  if( is_allowed_callback( gui ) )
  {
    /********************************************/
    /* Temporary. CLA, we havent yet            */
    /* loaded the images with libqsh, but       */
    /* would like to output all images with the */		    
    /* so temporarily, just fill the structure  */
    /* to fool it                               */
    /********************************************/
    if (gui->images_loaded == 0) return;

    DisplayBusyCursor(gui->mainwindow);
    /*if (!get_file_name(gui,base_filename)) return;*/

    if (!write_all_qim_single_images(gui->qsh_gui->qsh_info,base_filename,gui->app,gui->toplevel)){
      printf("error writing the images\n");
    }
    RemoveBusyCursor(gui->mainwindow);
  }
  
  DEBUG_TRACE_OUT printf("Done with Generate_Raw_ImagesCB\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure :   draw_x_on_image
%%%
%%%  Written by:   Matt Cohen
%%%
%%%  Parameters:   main_gui_t *gui, image_t *the_image
%%%
%%%  Purpose:      Draw an x on an image to be removed
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void draw_x_on_image ( main_gui_t *gui, image_t *the_image )
{
    int startX, endX, startY, endY, height, width;

    width = gui->image_block.width;
    height = gui->image_block.height;

    startX = 0;
    startY = 0;

    endX = startX + width;
    endY = startY + height;

    XSetForeground ( gui->display, gui->gc, gui->red_pixel );

    XSetLineAttributes ( gui->display, gui->gc, 5, LineOnOffDash, CapNotLast, JoinMiter );

    XDrawLine ( gui->display, XtWindow ( the_image->drawing_area ),
		gui->gc, startX, startY, endX, endY );	

    XDrawLine ( gui->display, XtWindow ( the_image->drawing_area ),
		gui->gc, endX, startY, startX, endY );	

}


/*=========================================================================
  Function:    check_version_CB

  Purpose:     Is the callback for the check_version button.  Calls the
               function CT_check_version.

  Parameters:  Normal callback parameters.

  Returned:    None.

  MTC 12/29/98
  =======================================================================*/
void check_version_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
    DEBUG_TRACE_IN printf ( "Entering check_version_CB\n" );
    CT_check_version ( w, "seraImage" );
    DEBUG_TRACE_OUT printf ( "Leaving check_version_CB\n" );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     Mouse_Functions_ToggleCB
%% 
%% Purpose:      Manage/Unmanage the frame at the bottom of the window with
%%               the mouse button functions.
%% 
%% Parameters:   Callback
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Mouse_Functions_ToggleCB(Widget w, XtPointer clientData, XtPointer callData)
{
  main_gui_t * gui = (main_gui_t *) clientData;

  DEBUG_TRACE_IN  printf("Entering Mouse_Functions_ToggleCB\n");

  /*
   * If currently shown, should unmanage it. 
   */
  if( gui->show_mouse_functions ) 
  {
    XtUnmanageChild( gui->mouse_form );
    XtVaSetValues( gui->display_outer_form,
		   XmNbottomAttachment, XmATTACH_FORM,
		   NULL );
    gui->show_mouse_functions = 0;
  }
  else /* Not shown right now, so manage it */
  {
    XtManageChild( gui->mouse_form );
    XtVaSetValues( gui->display_outer_form,
		   XmNbottomAttachment, XmATTACH_WIDGET,
		   XmNbottomWidget,     gui->mouse_form,
		   NULL );
    gui->show_mouse_functions = 1;
  }

  DEBUG_TRACE_OUT printf("Leaving Mouse_Functions_ToggleCB\n");
}
