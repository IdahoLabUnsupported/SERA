/************************************************************/
/* image_tools.c                                            */
/* Routines written by Mike Frandsen, 6-10-1997             */
/************************************************************/
#include <stdio.h>
#include <stdlib.h>

#define DECLARATION
#include "include.h"
#include "image_matrix.h"
#include "MenuSupport.h"
#include "menus.h"
#include "functions.h"
#include "iconCB.h"
#include "sliderText.h"
#include "helpers.h"
#include "segment.h"
#include "debug_tools.h"
#include "choose_text_files.h"
#include "dialog_tools.h"
#include "pixel_mapping.h"
#include "environment_tools.h"
#include "overlay_info.h"

#define MAX_IMAGES 1024

Widget make_main_window(Widget, MenuDescription *);
Widget make_left_bottom_tools_frame(Widget);
Widget make_properties_frame(Widget);

/* Is called for nonfatal x errors if DEBUG_GUI is used
 */
int x_error_handler(Display *display, XErrorEvent *error){
  char msg[80], inchar;
  int a;
  int b = 0;
  XGetErrorText(display, error->error_code, msg, 80);
  fprintf(stderr, "Error code %s\n", msg);
  fprintf(stderr, "[e]xit, [c]ontinue, [k]ill?");
  inchar = getchar();
  switch( inchar){
  case 'e':
  case 'E':
    exit(1);
    break;
  case 'c':
  case 'C':
    break;
  case 'k':
  case 'K':
    /* case a segfault so a core file is generated or the debugger is 
       gone to */
    a = 1/b;
    break;
  default:
    break;
  }
  return 0;
}

int main(int argc, char **argv) {
  static String fallback_resources[]={
    /* A resource file will override any of these defaults */
    "*fontList: -*-Fixed-Medium-R-*-*-*-120-*-*-*-*-*-*",
    "*background:                    SkyBlue",
    "*borderWidth:                   0",
    "*foreground:                    Black",
    
    "*scaleMultiple:                 1",        /* goes up in increments of ... */
    "*XmToggleButton*indicatorOn:    TRUE",     /* else prints a toggle box     */
    "*.menubar.popup_File.Edit.Undo.accelerator: Ctrl<Key>U",
    "*.menubar.popup_File.Edit.Undo.acceleratorText: Ctrl+U",
    "*.menubar.popup_File.File.Quick Save.accelerator: Ctrl<Key>S",
    "*.menubar.popup_File.File.Quick Save.acceleratorText: Ctrl+S",

    /* "ImageTools.width:                800", */
    /* "ImageTools.height:               600", */
    NULL
  };
  Widget toplevel;
  XtAppContext app;
  Widget main_window;
  MenuDescription *menubar_ptr=menubar;
  program_defaults_type * pdt_ptr; 

  ET_checkEnvironment( ); /* check environment variables */

  debug("Performing initialization.\n");

  /* Do this first -- sets up some things that don't fit into
   * other categories
   */
  perform_primary_initialization();

  /* Use some standard defaults for the program */
  init_program_defaults_standard();

  /* Let any user preferences override our defaults */
  init_program_defaults_resource_file_override();

  toplevel = XtVaAppInitialize(&app, "SeraModel", 
			       options, XtNumber (options),
			       &argc, argv,
			       fallback_resources,
			       NULL);

  /* Set up use of debug_tools */
  set_debug_values ( argv[0], toplevel );
  
  /* Check arguments that aren't debug levels */
  if ( argc > 1 )
      debug_syntax ( argc, argv );
  
  DEBUG_GUI XSetErrorHandler(x_error_handler); 
  DEBUG_MEMORY atexit(MT_onexit);

  DEBUG_TRACE_IN printf ( "Entering main\n" );

  /* MakeIconifyIcon(toplevel);*/ /* See animated icon when iconified */

  XtVaSetValues(toplevel,
		XmNtitle, "seraModel",
		XmNwidth, get_program_defaults()->ProgramWindowWidth,
		XmNheight, get_program_defaults()->ProgramWindowHeight,
		/*XmNallowShellResize, True,*/
		NULL);

  debug("toplevel values set.\n");

  XtRealizeWidget(toplevel);

  main_window = make_main_window(toplevel, menubar_ptr);

  debug("main window made.\n");

  /* Get recently used list of uv files */
  initialize_saved_regions();

  /* Get recently used list of image files */
  initialize_saved_images();

  /*print_supported_visuals(XtDisplay(toplevel));*/ /* not necessary -- just prints */

  init_colors(toplevel);
  add_guaranteed_colors(toplevel, &(get_color_info()->cmap));
  load_gamma_colormap(XtDisplay(toplevel), get_color_info()->cmap_vals);
  colormap_load_rgb(XtDisplay(toplevel));

  /* Initialize the image matrix structure.
   * It can hold up to MAX_IMAGES images.
   */
  init_image_matrix(toplevel, app, MAX_IMAGES);

  debug("image_matrix initialized.\n");

  /*####################################################################
   * Not doing this here anymore, moved inside of the callback for the
   * popup used to select the text files. 
   *
   * The next two will MAKE the fiducial and constraint marker shells 
   *
   *edit_fiducial_markers_FCN(0);
   *edit_constraint_markers_FCN(0);
   *
   * Read information in body_data.txt 
   *  read_body_data();
   *####################################################################*/

  debug("Initialization complete.\n");

  if ((argc==1)||(argc==2) /*||(argc==5)*/) {
    if (argc>1) {
      /*int w=256, h=256;*/
      
      /*if (argc>4) { */
	/* Loads images assuming 1 bpp */ /*
	w = (int)strtol(argv[2], NULL, 10);
	h = (int)strtol(argv[3], NULL, 10);
	if (w<=0) w=256;
	if (h<=0) h=256;
	set_cursor(WATCH_CURSOR);
	load_image_file_wh(argv[1], w, h, 1 */ /* bpp */ /*);
	set_cursor(NORMAL_CURSOR);
      } else { */
      set_cursor(WATCH_CURSOR);
      load_image_file(argv[1]);
      set_cursor(NORMAL_CURSOR);
	/* } */
    }
  } else {
    printf("\n************************************************************\n");
    printf("Usage:\n");
    printf("(1) %s\n", argv[0]);
    printf("    Used to start program without images.\n\n");
    printf("(2) %s input_filename\n", argv[0]);
    printf("    Used to load a set of images that have a valid header file.\n\n");
    /*printf("(3) %s input_filename dimension_x dimension_y\n", argv[0]);
      printf("    Used to load a set of images of any dimensions.\n\n");*/
    printf("NOTE:  ** Full filename must be used.\n");
    printf("       ** Compressed .gz (gzipped) files are also recognized.\n\n");
    printf("************************************************************\n");
    exit(EXIT_SUCCESS);
  }

  /* Set up default mouse mode */
  set_mouse_function(MM_STANDARD);

  /* This will make sure that PROPERTIES comes up however set in
   * preferences
   */
  if (get_program_defaults()->PropertiesViewable) {
    set_menu_toggle(NULL, "Properties Toolbar", True);
    XtManageChild(get_widget_entity(PROPERTIES_FRAME));
  } else {
    set_menu_toggle(NULL, "Properties Toolbar", False);
    XtUnmanageChild(get_widget_entity(PROPERTIES_FRAME));
  }
  /* This will make sure that EDIT REGIONS comes up however set in
   * preferences
   */
  if (get_program_defaults()->EditRegionsViewable) {
    set_menu_toggle(NULL, "Edit Regions", True);
    grow_regions_FCN(get_image_matrix(), 1);
  } else {
    set_menu_toggle(NULL, "Edit Regions", False);
    grow_regions_FCN(get_image_matrix(), 0);
  }
  /* These two come up automatically now -- make sure the
   * menus know their state
   */
  set_menu_toggle(NULL, "Context Help", True);
  set_menu_toggle(NULL, "Bottom Information", True);

  XtVaSetValues(get_image_matrix()->threshold_color_button,
		XmNbackground,
		get_color_info()->truecolors[THRESHOLD_INDEX],
		NULL);

  startAutoSaver ( );

  make_colormap_window_children(toplevel, get_color_info()->cmap);

  /*print_children(toplevel);*/

  register_enter_children_event_handlers(toplevel);

  print_environment_integrity_check();

  /*
   * Check to see if the edit regions pane is managed automatically
   * because EditRegionsViewable is set to true in the user's itrc
   * file. If it is, the user must load a body data file before they
   * can edit regions
   */
  pdt_ptr = get_program_defaults();
  if( pdt_ptr->EditRegionsViewable == 1 ) 
  {
    display_body_and_material_popup();
    DT_warn( toplevel, "You must load a body data\nfile before editing regions.",
	       "Required File Not Loaded", NULL );
    while( !get_image_matrix()->choose_files.body_data_read )
      XtAppProcessEvent( app, XtIMAll );
  }

  XtAppMainLoop(app);

  DEBUG_TRACE_OUT printf ( "Leaving main\n" );
  return( 0 );
}

Widget make_main_window(Widget toplevel, MenuDescription * menubar_ptr) {
  int ac=0;
  Arg al[10];
  Widget PIC_menu, image_tools_label, image_tools_label_form;
  Widget big_form;
  Widget Form1, bottom_parent_form, left_parent_form, left_bottom_form,
    left_top_form, properties_form, properties_frame,
    edit_regions_form, edit_regions_frame,
    this_window, left_bottom_tools_frame;
  XmString xmstr;
  XColor bg, fg;
  
  big_form = XmCreateForm(toplevel, "big_form", NULL, 0);
  XtManageChild(big_form);
  
  /* Add Lynn's Menu Bar */
  PIC_menu = CreateMyMenu ( MENUBAR, "menubar", big_form, menubar_ptr, NULL );
  XtVaSetValues(PIC_menu,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtopOffset, 10,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftOffset, 10,
		XmNrightOffset, 10,
		NULL);
  set_constructed_widget_entity( MENU_BAR, PIC_menu );

  /*
   * The routine in launch_apps.c requires that the name
   * of the widget be an executable file. But because of
   * how MenuSupport works, you can't name it one thing
   * and give it a labelString at the same time. Therefore,
   * in menus.h we name the widget by its executable name, 
   * and here we will assign it a XmNlablelString.
   *
   * NOTE: See page 279 in the X Toolkit Reference Manual
   *       for more info on XtNameToWidget.
   */

  xmstr = XmStringCreateLocalized("Sera Image");
  XtVaSetValues( XtNameToWidget( big_form, "*seraImage" ),
		 XmNlabelString, xmstr, NULL );
  XmStringFree( xmstr );

  xmstr = XmStringCreateLocalized("Sera Model");
  XtVaSetValues( XtNameToWidget( big_form, "*seraModel" ),
		 XmNlabelString, xmstr, NULL );
  XmStringFree( xmstr );

  xmstr = XmStringCreateLocalized("Sera 3D");
  XtVaSetValues( XtNameToWidget( big_form, "*sera3d" ),
		 XmNlabelString, xmstr, NULL );
  XmStringFree( xmstr );

  xmstr = XmStringCreateLocalized("Sera Dose");
  XtVaSetValues( XtNameToWidget( big_form, "*seraDose" ),
		 XmNlabelString, xmstr, NULL );
  XmStringFree( xmstr );

  xmstr = XmStringCreateLocalized("Sera Plot");
  XtVaSetValues( XtNameToWidget( big_form, "*seraPlot" ),
                 XmNlabelString, xmstr, NULL );
  XmStringFree( xmstr );

  xmstr = XmStringCreateLocalized("Sera Calc");
  XtVaSetValues( XtNameToWidget( big_form, "*seraCalc" ),
		 XmNlabelString, xmstr, NULL );
  XmStringFree( xmstr );

  xmstr = XmStringCreateLocalized("Sera Plan");
  XtVaSetValues( XtNameToWidget( big_form, "*seraPlan" ),
		 XmNlabelString, xmstr, NULL );
  XmStringFree( xmstr );

  /*
   * We are already in seraModel, so make that button
   * insensitive so it cannot be launched again.
   */

  XtVaSetValues( XtNameToWidget( big_form, "*seraModel" ), 
		 XmNsensitive, FALSE, NULL );


  /*
   * Make the Slice Orientation button under the Edit menu
   * insensitive because no images have been loaded
   */
  xmstr = XmStringCreateLocalized( "Slice Orientation" );
  XtVaSetValues( XtNameToWidget( big_form, "*orientation_button" ),
		 XmNlabelString, xmstr, 
		 XmNsensitive, False,
		 NULL );
  XmStringFree( xmstr );
  
  /*******************************************************/
  image_tools_label_form = XmCreateForm(big_form,
					"image_tools_label_form",
					NULL, 0);
  XtVaSetValues(image_tools_label_form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, PIC_menu,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftOffset, 10,
		XmNrightOffset, 10,
		NULL);
  XtManageChild(image_tools_label_form);
  
  image_tools_label = XmCreateLabel(image_tools_label_form,
				    "image_tools_label",
				    NULL, 0);
/*
  XtVaGetValues(image_tools_label,
		XmNbackground, &bg,
		XmNforeground, &fg,
		NULL);
*/
  bg.pixel = BlackPixel(XtDisplay(image_tools_label_form),
			DefaultScreen(XtDisplay(image_tools_label_form)));
  fg.pixel = WhitePixel(XtDisplay(image_tools_label_form),
			DefaultScreen(XtDisplay(image_tools_label_form)));

  xmstr = XmStringCreateLtoR(" ", MY_CHARSET);
  XtVaSetValues(image_tools_label,
		XmNlabelString, xmstr,
		XmNforeground, bg.pixel,
		XmNbackground, fg.pixel,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftOffset, 0,
		XmNrightOffset, 0,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(image_tools_label);
  set_ptr_to_global_label(image_tools_label);
  
  /*******************************************************/
  Form1 = XmCreateForm(big_form, "Form1", NULL, 0);
  XtVaSetValues(Form1,
		XmNheight, 700,
		XmNwidth, get_program_defaults()->ProgramWindowWidth,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, image_tools_label_form,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftOffset, 10,
		XmNrightOffset, 10,
		XmNtopOffset, 5,
		XmNbottomOffset, 10,
		NULL);
  XtManageChild(Form1);
  set_constructed_widget_entity( MAIN_FORM, Form1 );
  
  /*******************************************************/
  bottom_parent_form = XmCreateForm(Form1, "bottom_parent_form", NULL, 0);
  
  XtVaSetValues (bottom_parent_form,
		 XmNleftAttachment, XmATTACH_FORM,
		 XmNrightAttachment, XmATTACH_FORM,
		 XmNtopAttachment, XmATTACH_FORM,
		 XmNbottomAttachment, XmATTACH_FORM,
		 NULL);
  
  /*******************************************************/
  /* This is what the region editing panel goes in */
  edit_regions_form = XmCreateForm(bottom_parent_form, "edit_regions_form", NULL, 0);
  
  XtVaSetValues (edit_regions_form,
		 XmNrightAttachment, XmATTACH_FORM,
		 XmNtopAttachment, XmATTACH_FORM,
		 XmNbottomAttachment, XmATTACH_FORM,
		 NULL);
  
  /*******************************************************/
  /* This is what the properties panel goes in */
  properties_form = XmCreateForm(bottom_parent_form, "properties_form", NULL, 0);
  
  XtVaSetValues (properties_form,
		 XmNrightAttachment, XmATTACH_WIDGET,
		 XmNrightWidget, edit_regions_form,
		 XmNtopAttachment, XmATTACH_FORM,
		 XmNbottomAttachment, XmATTACH_FORM,
		 NULL);
  
  /*******************************************************/
  left_parent_form = XmCreateForm(bottom_parent_form, "left_parent_form", NULL, 0);
  
  XtVaSetValues (left_parent_form,
		 XmNleftAttachment, XmATTACH_FORM,
		 XmNrightAttachment, XmATTACH_WIDGET,
		 XmNrightWidget, properties_form,
		 XmNtopAttachment, XmATTACH_FORM,
		 XmNbottomAttachment, XmATTACH_FORM,
		 NULL);
  
  
  /*******************************************************/
  left_bottom_form = XmCreateForm(left_parent_form, "left_bottom_form", NULL, 0);
  
  XtVaSetValues (left_bottom_form,
		 XmNleftAttachment, XmATTACH_FORM,
		 XmNrightAttachment, XmATTACH_FORM,
		 XmNbottomAttachment, XmATTACH_FORM,
		 NULL);
    
  /*******************************************************/
  left_top_form = XmCreateForm(left_parent_form, "left_top_form", NULL, 0);
  
  XtVaSetValues (left_top_form,
		 XmNleftAttachment, XmATTACH_FORM,
		 XmNrightAttachment, XmATTACH_FORM,
		 XmNtopAttachment, XmATTACH_FORM,
		 XmNbottomAttachment, XmATTACH_WIDGET,
		 XmNbottomWidget, left_bottom_form,
		 NULL);
  
  
  /*******************************************************/
  ac=0;
  XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
  XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
  this_window = XmCreateScrolledWindow(left_top_form,
				       "image_matrix_window", al, ac);
  XtVaSetValues(this_window,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  set_constructed_widget_entity(WINDOW, this_window);
  XtManageChild(this_window);
  
  /*******************************************************/
  edit_regions_frame = make_edit_regions_frame(edit_regions_form);
  /* XtManageChild(edit_regions_frame); --> Now a preference */
  set_constructed_widget_entity(EDIT_REGIONS_FRAME, edit_regions_frame);
  /*******************************************************/
  properties_frame = make_properties_frame(properties_form);
  /* XtManageChild(properties_frame); --> Now a preference */
  set_constructed_widget_entity(PROPERTIES_FRAME, properties_frame);
  /*******************************************************/
  left_bottom_tools_frame = make_left_bottom_tools_frame(left_bottom_form);
  XtManageChild(left_bottom_tools_frame);
  set_constructed_widget_entity(BOTTOM_TOOLBAR_FRAME, left_bottom_tools_frame);
  /*******************************************************/
  XtManageChild(left_bottom_form);
  XtManageChild(left_top_form);
  XtManageChild(edit_regions_form);
  XtManageChild(properties_form);
  XtManageChild(left_parent_form);
  XtManageChild(bottom_parent_form);

  return(big_form);
}

Widget make_left_bottom_tools_frame(Widget form) {
  Widget main_frame, parent_thing;
  Widget top_form, bottom_form;
  Widget bottom_left_form, bottom_right_form;
  Widget locate_button, locate_label;
  Widget mouse_rc, mouse_label_rc;
  Widget mouse_1, mouse_2, mouse_3;
  Widget mouse_label1, mouse_label2, mouse_label3;
  Widget sep;
  Widget info_form,main_image_range_label;
  XmString xmstr;

  main_frame = XmCreateFrame(form, "main_frame", NULL, 0);
  XtVaSetValues(main_frame,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopOffset, 3,
		NULL);

  parent_thing = XmCreateRowColumn(main_frame, "parent_thing", NULL, 0);
  XtManageChild(parent_thing);

  top_form = XmCreateForm(parent_thing, "top_form", NULL, 0);
  XtManageChild(top_form);

  sep = XmCreateSeparator(parent_thing, "sep", NULL, 0);
  XtManageChild(sep);

  bottom_form = XmCreateForm(parent_thing, "bottom_form", NULL, 0);
  XtManageChild(bottom_form);

  bottom_left_form = XmCreateForm(bottom_form, "bottom_left_form", NULL, 0);
  XtVaSetValues(bottom_left_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(bottom_left_form);

  bottom_right_form = XmCreateForm(bottom_form, "bottom_right_form", NULL, 0);
  XtVaSetValues(bottom_right_form,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, bottom_left_form,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftOffset, 3,
		NULL);
  XtManageChild(bottom_right_form);

  /*******************************************************************/
  /* Now, on bottom left have left and right forms                   */
  /*******************************************************************/
  locate_button = XmCreateToggleButton(top_form, "locate_button", NULL, 0);
  xmstr = XmStringCreateLtoR("Locate", MY_CHARSET);
  XtVaSetValues(locate_button,
		XmNlabelString, xmstr,
		XmNset, False,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(locate_button);
  /*******************************************************************/
  locate_label = XmCreateLabel(top_form, "locate_label", NULL, 0);
  XtVaSetValues(locate_label,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, locate_button,
		XmNleftOffset, 3,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		XmNbottomWidget, locate_button,
		XmNbottomOffset, 3,
		XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		XmNtopWidget, locate_button,
		XmNtopOffset, 3,
		NULL);
  XtManageChild(locate_label);
  /*******************************************************************/

  mouse_rc = XmCreateRowColumn(bottom_left_form, "mouse_rc", NULL, 0);
  XtManageChild(mouse_rc);
  XtVaSetValues(mouse_rc,
		XmNnumColumns, 3,
		XmNadjustLast, False,
		XmNpacking, XmPACK_COLUMN,
		XmNorientation, XmHORIZONTAL,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

  info_form = XtVaCreateManagedWidget("info_form",
				      xmFormWidgetClass, bottom_right_form,
				      XmNrightAttachment, XmATTACH_FORM,
				      XmNtopAttachment, XmATTACH_FORM,
				      XmNbottomAttachment, XmATTACH_FORM,
				      XmNwidth, 100,
				      NULL);

  main_image_range_label = XtVaCreateManagedWidget("Image Range: ",
						   xmLabelWidgetClass, info_form,
						   XmNleftAttachment, XmATTACH_FORM,
						   XmNtopAttachment, XmATTACH_FORM,
						   XmNleftOffset, 5,
						   XmNtopOffset, 5,
						   NULL);



  mouse_label_rc = XmCreateRowColumn(bottom_right_form, "mouse_label_rc", NULL, 0);
  XtManageChild(mouse_label_rc);
  XtVaSetValues(mouse_label_rc,
		XmNnumColumns, 3,
		XmNadjustLast, False,
		XmNpacking, XmPACK_COLUMN,
		XmNorientation, XmHORIZONTAL,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, info_form,
		NULL);




  mouse_1 = XmCreateLabel(mouse_rc, "mouse_1", NULL, 0);
  xmstr = XmStringCreateLtoR("Left:", MY_CHARSET);
  XtVaSetValues(mouse_1,
		XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(mouse_1);
  mouse_label1 = XmCreateLabel(mouse_label_rc, "mouse_label1", NULL, 0);
  XtManageChild(mouse_label1);
  mouse_2 = XmCreateLabel(mouse_rc, "mouse_2", NULL, 0);
  xmstr = XmStringCreateLtoR("Middle:", MY_CHARSET);
  XtVaSetValues(mouse_2,
		XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(mouse_2);
  mouse_label2 = XmCreateLabel(mouse_label_rc, "mouse_label2", NULL, 0);
  XtManageChild(mouse_label2);
  mouse_3 = XmCreateLabel(mouse_rc, "mouse_3", NULL, 0);
  xmstr = XmStringCreateLtoR("Right:", MY_CHARSET);
  XtVaSetValues(mouse_3,
		XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(mouse_3);
  mouse_label3 = XmCreateLabel(mouse_label_rc, "mouse_label3", NULL, 0);
  XtManageChild(mouse_label3);

  set_constructed_widget_entity(MB1_LABEL, mouse_label1);
  set_constructed_widget_entity(MB2_LABEL, mouse_label2);
  set_constructed_widget_entity(MB3_LABEL, mouse_label3);
  set_constructed_widget_entity(LOCATE_BUTTON, locate_button);
  set_constructed_widget_entity(LOCATE_LABEL, locate_label);
  set_constructed_widget_entity(IMAGE_RANGE_LABEL, main_image_range_label);
  set_locate_label("Locate");

  return(main_frame);
}

Widget make_properties_frame(Widget parent) {
  XmString xmstr;
  Widget right_form_frame, right_form_child, title, synch_win;
  int TEXT_WIDTH=75;

  right_form_frame = XmCreateFrame(parent, "right_form_frame", NULL, 0);
  
  XtVaSetValues (right_form_frame,
		 XmNleftOffset, 10,
		 XmNleftAttachment, XmATTACH_FORM,
		 XmNrightAttachment, XmATTACH_FORM,
		 XmNtopAttachment, XmATTACH_FORM,
		 XmNbottomAttachment, XmATTACH_FORM,
		 NULL);
  
  /*******************************************************/
  right_form_child = XmCreateForm(right_form_frame, "right_form_child", NULL, 0);

  /*******************************************************/
  title = XmCreateLabel(right_form_child, "title", NULL, 0);
  xmstr = XmStringCreateLtoR("Properties", MY_CHARSET);
  XtVaSetValues(title,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(title);
  /*******************************************************/
  {
    Widget sep0, num_cols_w, num_cols_w_form, num_cols_w_textbox,
      sep1, zoomtitle, zoom, emptyzoom, zoom_in, zoom_out, sep2,
      winsizetitle, winsize, emptywinsize,
      winsize_smaller, winsize_larger, sep3,
      viewtypetitle, viewtype, sep35,
      colortitle, color, sep355, sep4,
      kill, sep5, empty_form;
    Widget current_units_label;
    button_type *remap_button_list;
    static char *remap_button_text[4]={
      "Global",
      "Local",
      "Assume Full Range",
      "None"
    };
    button_type viewbutton_list[4];
    char *viewbutton_text[4]={
      "Superimpose",
      "Images Only",
      "Regions Only",
      "Unlabelled Regions"
    };
    int i;
      
    sep0 = XmCreateSeparator(right_form_child, "sep0", NULL, 0);
    XtVaSetValues(sep0,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, title,
		  XmNtopOffset, 5,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XtManageChild(sep0);
    
    /*******************************************************/
    num_cols_w_form = CreateSliderText(&num_cols_w, &num_cols_w_textbox,
				       right_form_child,
				       "Columns",
				       (int)True, 0,
				       1, 10, get_program_defaults()->MaximumColumns);
    set_constructed_widget_entity(NUM_COLS_W, num_cols_w);
    XtVaSetValues(num_cols_w,
		  XmNrightAttachment, XmATTACH_NONE,
		  NULL);
    XtVaSetValues(num_cols_w_textbox,
		  XmNwidth, TEXT_WIDTH,
		  NULL);
    XtVaSetValues(num_cols_w_form,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, sep0,
		  XmNtopOffset, 3,
		  XmNleftOffset, 3,
		  XmNrightOffset, 3,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XtManageChild(num_cols_w_form);
    XtAddCallback(num_cols_w,XmNvalueChangedCallback,num_columns_CB,(XtPointer) 0);
    
    /*******************************************************/
    sep1 = XmCreateSeparator(right_form_child, "sep1", NULL, 0);
    XtVaSetValues(sep1,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, num_cols_w_form,
		  XmNtopOffset, 3,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XtManageChild(sep1);
    
    /*******************************************************/
    zoomtitle = XmCreateLabel(right_form_child, "zoomtitle", NULL, 0);
    xmstr = XmStringCreateLtoR("Zoom", MY_CHARSET);
    XtVaSetValues(zoomtitle,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, sep1,
		  XmNtopOffset, 3,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XmStringFree(xmstr);
    XtManageChild(zoomtitle);
    /*******************************************************/
    zoom = XmCreateTextField(right_form_child, "zoom", NULL, 0);
    XtAddCallback(zoom, XmNactivateCallback, set_global_zoom_CB, (XtPointer)1);
    XtVaSetValues(zoom,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, zoomtitle,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNrightOffset, 3,
		  XmNwidth, TEXT_WIDTH,
		  NULL);
    XtManageChild(zoom);
    XmTextSetString(zoom, "");
    XmTextSetCursorPosition(zoom, 0);
    /*******************************************************/
    zoom_out = XmCreatePushButton(right_form_child, "zoom_out", NULL, 0);
    xmstr = XmStringCreateLtoR(" - ", MY_CHARSET);
    XtVaSetValues(zoom_out,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, zoomtitle,
		  XmNtopOffset, 0,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNleftOffset, 3,
		  XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		  XmNbottomWidget, zoom,
		  NULL);
    XtManageChild(zoom_out);
    XmStringFree(xmstr);
    XtAddCallback(zoom_out,XmNactivateCallback,zoom_inout_CB,(XtPointer) 0);
    /*******************************************************/
    zoom_in = XmCreatePushButton(right_form_child, "zoom_in", NULL, 0);
    xmstr = XmStringCreateLtoR(" + ", MY_CHARSET);
    XtVaSetValues(zoom_in,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, zoomtitle,
		  XmNtopOffset, 0,
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, zoom_out,
		  XmNleftOffset, 3,
		  XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		  XmNbottomWidget, zoom,
		  NULL);
    XtManageChild(zoom_in);
    XmStringFree(xmstr);
    XtAddCallback(zoom_in,XmNactivateCallback,zoom_inout_CB,(XtPointer) 1);
    /*******************************************************/
    emptyzoom = XmCreateForm(right_form_child, "emptyzoom", NULL, 0);
    XtVaSetValues(emptyzoom,
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, zoom_in,
		  XmNrightAttachment, XmATTACH_WIDGET,
		  XmNrightWidget, zoom,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, zoomtitle,
		  XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		  XmNbottomWidget, zoom,
		  NULL);
    XtManageChild(emptyzoom);
    /*******************************************************/
    sep2 = XmCreateSeparator(right_form_child, "sep2", NULL, 0);
    XtVaSetValues(sep2,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, zoom,
		  XmNtopOffset, 3,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XtManageChild(sep2);
    /*******************************************************/
    winsizetitle = XmCreateLabel(right_form_child, "winsizetitle", NULL, 0);
    xmstr = XmStringCreateLtoR("Window Size", MY_CHARSET);
    XtVaSetValues(winsizetitle,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, sep2,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  NULL);
    XmStringFree(xmstr);
    XtManageChild(winsizetitle);
    /*******************************************************/
    winsize = XmCreateTextField(right_form_child, "winsize", NULL, 0);
    XtAddCallback(winsize, XmNactivateCallback, set_prev_winsize_CB, (XtPointer)NULL);
    XtVaSetValues(winsize,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, winsizetitle,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNrightOffset, 3,
		  XmNwidth, TEXT_WIDTH, 
		  NULL);
    XtManageChild(winsize);
    XmTextSetString(winsize, "");
    XmTextSetCursorPosition(winsize, 0);
    /*******************************************************/
    winsize_smaller = XmCreatePushButton(right_form_child, "winsize_smaller", NULL, 0);
    xmstr = XmStringCreateLtoR(" - ", MY_CHARSET);
    XtVaSetValues(winsize_smaller,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, winsizetitle,
		  XmNtopOffset, 0,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNleftOffset, 3,
		  XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		  XmNbottomWidget, winsize,
		  NULL);
    XtManageChild(winsize_smaller);
    XmStringFree(xmstr);
    XtAddCallback(winsize_smaller,XmNactivateCallback,construct_images_CB,(XtPointer) 1);
    /*******************************************************/
    winsize_larger = XmCreatePushButton(right_form_child, "winsize_larger", NULL, 0);
    xmstr = XmStringCreateLtoR(" + ", MY_CHARSET);
    XtVaSetValues(winsize_larger,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, winsizetitle,
		  XmNtopOffset, 0,
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, winsize_smaller,
		  XmNleftOffset, 3,
		  XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		  XmNbottomWidget, winsize,
		  NULL);
    XtManageChild(winsize_larger);
    XmStringFree(xmstr);
    XtAddCallback(winsize_larger,XmNactivateCallback,construct_images_CB,(XtPointer) 0);
    /*******************************************************/
    emptywinsize = XmCreateForm(right_form_child, "emptywinsize", NULL, 0);
    XtVaSetValues(emptywinsize,
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, winsize_larger,
		  XmNrightAttachment, XmATTACH_WIDGET,
		  XmNrightWidget, winsize,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, winsizetitle,
		  XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		  XmNbottomWidget, winsize,
		  NULL);
    XtManageChild(emptywinsize);
    /*******************************************************/
    sep3 = XmCreateSeparator(right_form_child, "sep3", NULL, 0);
    XtVaSetValues(sep3,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, winsize_smaller,
		  XmNtopOffset, 3,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XtManageChild(sep3);
    /*******************************************************/
    /* Make pulldown for type of view (superimpose, image, regions) */
    /*******************************************************/
    viewtypetitle = XmCreateLabel(right_form_child, "viewtypetitle", NULL, 0);
    xmstr = XmStringCreateLtoR("View Selection", MY_CHARSET);
    XtVaSetValues(viewtypetitle,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, sep3,
		  XmNtopOffset, 3,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XmStringFree(xmstr);
    XtManageChild(viewtypetitle);
    /*******************************************************/
    for (i=0; i<4; i++) {
      viewbutton_list[i].menuText=viewbutton_text[i];
    }
    viewtype = CreateMenu(right_form_child, viewbutton_list, 3);
    XtAddCallback(viewbutton_list[0].w, XmNactivateCallback,
		  set_viewtype_CB, (XtPointer)SUPERIMPOSED);
    XtAddCallback(viewbutton_list[1].w, XmNactivateCallback,
		  set_viewtype_CB, (XtPointer)IMAGES_ONLY);
    XtAddCallback(viewbutton_list[2].w, XmNactivateCallback,
		  set_viewtype_CB, (XtPointer)REGIONS_ONLY);
    /*XtAddCallback(viewbutton_list[3].w, XmNactivateCallback,
      set_viewtype_CB, (XtPointer)UNLABELLED_REGIONS);*/
    XtVaSetValues(viewtype,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, viewtypetitle,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNleftOffset, 0,
		  NULL);
    XtManageChild(viewtype);
    /*******************************************************/
    sep35 = XmCreateSeparator(right_form_child, "sep35", NULL, 0);
    XtVaSetValues(sep35,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, viewtype,
		  XmNtopOffset, 3,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XtManageChild(sep35);
    /*******************************************************/
    /* Make pulldown for color optimization */
    /*******************************************************/
    colortitle = XmCreateLabel(right_form_child, "colortitle", NULL, 0);
    xmstr = XmStringCreateLtoR("Color Optimization", MY_CHARSET);
    XtVaSetValues(colortitle,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, sep35,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XmStringFree(xmstr);
    XtManageChild(colortitle);
    /*******************************************************/
    remap_button_list = get_remap_button_list();
    for (i=0; i<4; i++) {
      remap_button_list[i].menuText=remap_button_text[i];
    }
    color = CreateMenu(right_form_child, remap_button_list, 4);
    
    set_constructed_widget_entity(REMAP_PULLDOWN, color);
    XtAddCallback(remap_button_list[0].w, XmNactivateCallback,
		  register_images_CB, (XtPointer)CO_GLOBAL);
    XtAddCallback(remap_button_list[1].w, XmNactivateCallback,
		  register_images_CB, (XtPointer)CO_LOCAL);
    XtAddCallback(remap_button_list[2].w, XmNactivateCallback,
		  register_images_CB, (XtPointer)CO_ASSUME_FULL);
    XtAddCallback(remap_button_list[3].w, XmNactivateCallback,
		  register_images_CB, (XtPointer)CO_NONE);
    XtVaSetValues(color,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, colortitle,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNleftOffset, 0,
		  NULL);
    XtManageChild(color);
    /*******************************************************/
    sep355 = XmCreateSeparator(right_form_child, "sep355", NULL, 0);
    XtVaSetValues(sep355,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, color,
		  XmNtopOffset, 3,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XtManageChild(sep355);
    /*******************************************************/
    /* Lock windows on area -- as you move sliders in one
     * window, do they affect other windows or not?
     */
    synch_win = XmCreateToggleButton(right_form_child, "synch_win", NULL, 0);
    xmstr = XmStringCreateLtoR("Synchronize Windows", MY_CHARSET);
    XtVaSetValues(synch_win,
		  XmNlabelString, xmstr,
		  XmNset, SYNCHRONIZE_WINDOWS_INITIAL,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNleftOffset, 3,
		  XmNtopOffset, 3,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, sep355,
		  NULL);
    XmStringFree(xmstr);
    XtManageChild(synch_win);
    XtAddCallback(synch_win, XmNvalueChangedCallback, synch_win_CB, (XtPointer)NULL);
    /*******************************************************/
    sep4 = XmCreateSeparator(right_form_child, "sep4", NULL, 0);
    XtVaSetValues(sep4,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, synch_win,
		  XmNtopOffset, 3,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    XtManageChild(sep4);

    /* Add a button to allow the user to switch between image sets */
    /* overlayInfo is a global structure from overlay_info.h */
    xmstr = XmStringCreateLtoR( "Switch Image Sets", MY_CHARSET );
    overlayInfo.switchSetsButton  =
        XtVaCreateManagedWidget( "switchSetsButton", xmPushButtonWidgetClass,
                                 right_form_child,
                                 XmNlabelString, xmstr,
                                 XmNtopAttachment, XmATTACH_WIDGET,
                                 XmNtopWidget, sep4,
                                 XmNtopOffset, 15,
                                 XmNleftAttachment, XmATTACH_FORM,
                                 XmNleftOffset, 5,
                                 XmNsensitive, False,
                                 NULL );
    XmStringFree( xmstr );

    XtAddCallback( overlayInfo.switchSetsButton, XmNactivateCallback,
                   switchImageSetsCB, (XtPointer) NULL );

    

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Not allowing image removal anymore! MBR 8-10-99
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*******************************************************
    kill = XmCreatePushButton(right_form_child, "kill", NULL, 0);
    xmstr = XmStringCreateLtoR("Remove Image(s)", MY_CHARSET);
    XtVaSetValues(kill,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, sep4,
		  XmNtopOffset, 3,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNleftOffset, 3,
		  NULL);
    XtManageChild(kill);
    XmStringFree(xmstr);
    XtAddCallback(kill,XmNactivateCallback,kill_image_CB,(XtPointer) 0);

    sep5 = XmCreateSeparator(right_form_child, "sep5", NULL, 0);
    XtVaSetValues(sep5,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, kill,
		  XmNtopOffset, 3,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
                  XtManageChild(sep5);
*******************************************************/
                  
    empty_form = XmCreateForm(right_form_child, "empty_form", NULL, 0);
    XtManageChild(empty_form);
    XtVaSetValues(empty_form,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, sep4,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  NULL);
    /** added by CLA **/

    /*
      current_units_label = XtVaCreateManagedWidget("Current Units (CM)",
      xmLabelWidgetClass, empty_form,
      XmNtopAttachment,XmATTACH_FORM,
      XmNtopOffset, 10,
      XmNleftAttachment, XmATTACH_FORM,
      XmNleftOffset, 10,
      NULL);
    */    
    
  }
  /*******************************************************/
  XtManageChild(right_form_child);
  return(right_form_frame);
}
