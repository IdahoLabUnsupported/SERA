/*
 *
 * bnct dose contouring program.
 * authors: Dan Wessol, Gabor Kinces, Jeremy Cook, Fred McClurg, 
 *          and Mike Frandsen
 *
 */


/*
 * USER SUPPLIED INCLUDE FILES
 */
#define DECLARATION  /* turn on the declarations and turn off the externs */
#include "global.h"
#include "common.h"
#include "picshell.h"
#include "commonfcns.h"
#include "read_raw.h"
#include "preferences.h"
#include "color.h"
#include "environment_tools.h"
#include <X11/IntrinsicP.h>



/*
 * GLOBAL VARIABLE DECLARATIONS
 */
XColor color;



/*
 * DEFINE SECTION
 */
#define BX_APP_NAME "xcontours"
#define BX_APP_CLASS "XContours"


/*
 * SHELL CALLBACK PROCEDURE DECLARATIONS
 */


/*
 * GLOBAL WIDGET VARIABLES
 */
Widget mainWindowDrawingArea, AppShell;
Widget drawingAreaShell; /* mwf 1-20-96 --> contains mainWindowDrawingArea */
Widget scaleWidgetForm;
Widget contourColorsForm;
Widget textWidgetForm;
Widget editColorScrolledWindow;
Widget fileSelectionBox;
Widget fileSelectionDialogShell;
Widget MainWindow;
Widget doseSelectionBox;
Widget maskWindow;
Widget textWidgetForm;
Widget preferences_form;
Widget contour_labels_options_form;


/*
 * GLOBAL FUNCTIONS
 */
Widget CreatemainWindow(Widget parent);
Widget CreatedrawingAreaShell(Widget parent);
Widget CreatehelpMainWindow(Widget parent);
Widget CreatescaleDialogShell(Widget parent);
Widget CreatedoseDialogShell(Widget parent);
Widget CreateeditContourColorsDialogShell(Widget parent);
Widget CreatefileSelectionDialogShell(Widget parent);
Widget CreatemaskWindow(Widget w);
Widget CreatetextDialogShell(Widget parent);
Widget create_contour_labels_options_shell(Widget parent);


/*
 * Local prototypes
 */
void load_contour_levels(void);
void OK_Exit_Callback (Widget w, XtPointer clientData, XtPointer callData);



/* MAIN PROGRAM */
int main(int argc, char *argv[])
{
  Position main_x,main_y;
  Dimension width;
  Arg           args[256];
  Boolean       argok;
  Cardinal      argcnt;
  Display      *display;
  Pixmap        pixmap;
  Screen       *screen;
  int           scr;
  XGCValues     gc_values;
  Colormap      def_colormap;
  XColor        Colors[MAXCOLORS];
  /* MWF added these... */
  static String fallback_resources[]={
    /* A resource file will override any of these defaults */
    "*fontList:\
    -*-Fixed-Medium-R-*-*-12-120-*-*-*-*-*-*=char_set, \
    -*-Fixed-Medium-R-*-*-14-120-*-*-*-*-*-*=char_set_large, \
    -*-Fixed-Medium-R-*-*-17-120-*-*-*-*-*-*=char_set_huge, \
    -*-Fixed-Medium-R-*-*-10-120-*-*-*-*-*-*=char_set_small, \
    -*-Fixed-Medium-R-*-*-8-120-*-*-*-*-*-*=char_set_tiny",
    "*background:                    SkyBlue",
    "*borderWidth:                   0",
    "*foreground:                    Black",
    "*scaleMultiple:                 1",    /* goes up in increments of ... */
    "*XmToggleButton*indicatorOn:    TRUE", /* else prints a toggle box     */
    NULL
  };

  ET_checkEnvironment( ); /* check environment variables */
  atexit(MT_onexit);

  /* and MWF added these... */
  /**** Initializations **********************************/

  verbose=0;
  char_set=XmSTRING_DEFAULT_CHARSET;
  image_matrix_init();
  image_matrix.pic_width=256; /* Mike-->Try to set these in a resource file */
  image_matrix.pic_height=256;
  image_matrix.image_set=0;
  cbar_image=NULL;
  /*******************************************************/
  
  AppShell = XtVaAppInitialize(&context, "SeraDose", 
                               options, XtNumber (options), 
                               &argc, argv, 
                               fallback_resources, 
			       NULL);
  
  /* MTC added this for debugging 7/1/98 */
  set_debug_values ( argv[0], AppShell);

  if (argc > 1)
      debug_syntax (argc, argv);

  /***************************************/

  DEBUG_TRACE_IN printf( "Entering main\n" );

  /* Clean up any temporary files that seraDose may have left from prior runs */
  /*remove_temp_contour_files ( );*/
  remove_temp_mask_files ( );
  
  if (DefaultDepthOfScreen(XtScreen(AppShell)) < 2) {
    puts("You must be using a color screen.");
    exit(1);
  }
  
  color.flags = DoRed|DoGreen|DoBlue;
  /* initialize first color */
  XAllocColor(XtDisplay(AppShell),
	      DefaultColormapOfScreen(XtScreen(AppShell)), &color);
  
  di = XtDisplay(AppShell);
  wi = XtWindow(AppShell);
  
  maxx = maxy = 0.0; minx = miny = 1.0;
  value[0] = 9; value[1] = 1;
  SureEvent.xexpose.count = 0;
  dosage_is_there = FALSE;
  measure_data.fov = 0;  /* default it to unmodified */
  
  doseString = (char *)MT_malloc(25);
  strcpy( doseString, "Total Dose" );

  /*
   * Determine the number of colors to be edited.
   */
  ncolors = DisplayCells(di, DefaultScreen(di));
  if(ncolors > MAXCOLORS) ncolors = MAXCOLORS;
#if defined (MORE_VERBOSE)
  printf("   Number of colors = %d \n", ncolors);
#endif /* MORE_VERBOSE */
  /* Mike:  Why not exit if ncolors < MAXCOLORS? */
  
  if (argv[1] == NULL) is_argv = FALSE;
  else is_argv = TRUE;
  
  argcnt = 0;
  XtSetArg(args[argcnt], XmNiconName, "seraDose"); argcnt++;
  XtSetArg(args[argcnt], XmNx, 560 ); argcnt++;
  XtSetArg(args[argcnt], XmNy, 25 ); argcnt++;
  xcontoursTopLevelShell = XtCreatePopupShell( "xcontoursTopLevelShell",
					       topLevelShellWidgetClass, 
					       AppShell, args, argcnt);
  image_matrix.shell = xcontoursTopLevelShell;
  /*print_supported_visuals (di);*/
  init_colors (xcontoursTopLevelShell);
  add_guaranteed_colors(&(get_color_info()->cmap));
  
  argcnt = 0;
  XtSetArg(args[argcnt], XmNx, 500); argcnt++;
  XtSetArg(args[argcnt], XmNy, 41); argcnt++;
  helpTopLevelShell = XtCreatePopupShell( "helpTopLevelShell",
					  topLevelShellWidgetClass, 
					  xcontoursTopLevelShell, 
					  args, argcnt);

  textWidgetForm = CreatetextDialogShell(xcontoursTopLevelShell);
  scaleWidgetForm = CreatescaleDialogShell(xcontoursTopLevelShell);
  doseSelectionBox = CreatedoseDialogShell(xcontoursTopLevelShell);
  fileSelectionBox = CreatefileSelectionDialogShell(xcontoursTopLevelShell);
  contourColorsForm = 
    CreateeditContourColorsDialogShell(xcontoursTopLevelShell);  
  
  contour_labels_options_form = 
    create_contour_labels_options_shell (xcontoursTopLevelShell);

  preferences_form = create_preferences_shell (xcontoursTopLevelShell);

  MainWindow = CreatemainWindow(xcontoursTopLevelShell);
  drawingAreaShell = CreatedrawingAreaShell(xcontoursTopLevelShell);
  color_tool_shell = make_color_tool_shell(xcontoursTopLevelShell);

  helpMainWindow = CreatehelpMainWindow(helpTopLevelShell);
  XtManageChild(helpMainWindow);
  
  maskWindow = CreatemaskWindow (xcontoursTopLevelShell);
  XtRealizeWidget(XtParent(scaleWidgetForm));
  XtRealizeWidget(XtParent(textWidgetForm));   
  XtRealizeWidget(XtParent(doseSelectionBox));
  XtRealizeWidget(XtParent(fileSelectionBox));

  /* MWF --> not really shells anymore... */
  XtRealizeWidget(XtParent(drawingAreaShell)); 

  /* MWF --> (parent is a shell instead) */
  XtRealizeWidget(XtParent(color_tool_shell));
 
  XtRealizeWidget(XtParent(contourColorsForm));
  XtRealizeWidget(XtParent(preferences_form));
  XtRealizeWidget(XtParent(contour_labels_options_form));

  XtManageChild(drawingAreaShell);
  XtPopup(xcontoursTopLevelShell, XtGrabNone);

  /** added by CLA, trying ot get the window to come up
      next to each other **/
  /*  
      XtVaSetValues(XtParent(xcontoursTopLevelShell),
      XmNx, 600,
      XmNy, 100,
      NULL);
      
      XtVaGetValues(AppShell,
      XmNx,&main_x,
      XmNy,&main_y,
      NULL);
      printf("main window was put at : %d,%d\n",(int)main_x,(int)main_y);  
      
      
      XtVaGetValues(XtParent(drawingAreaShell),
      XmNwidth,&width,
      NULL);
      printf("width of bigwindow is : %d\n",width);
      
      XtVaSetValues(xcontoursTopLevelShell,
      XmNx, main_x -  width,
      XmNy, main_y,
      NULL);
      
      XtVaGetValues(XtParent(drawingAreaShell),
      XmNx,&main_x,
      XmNy,&main_y,
      NULL);
      printf("big window was put at : %d,%d\n",(int)main_x,(int)main_y);
  */
  
  
  setup_gc(mainWindowDrawingArea);
  
  /* Set these 2 globals before calling init_colors_old */
  image_matrix.dpy = di;   

  image_matrix.screen = DefaultScreen(image_matrix.dpy);
  image_matrix.maxHWcmaps = 
    MaxCmapsOfScreen(DefaultScreenOfDisplay(image_matrix.dpy));
  
  /* create a couple of pixmaps */
  scr = DefaultScreen(di);
  
  mask_pack.mask_region = XCreatePixmap(di, XtWindow(mainWindowDrawingArea), 
					512, 512, 1);
  newGC = XCreateGC(di, mask_pack.mask_region, 0, &gc_values);
  XSetFunction(di, newGC, GXset);
  XCopyArea(di, mask_pack.mask_region, mask_pack.mask_region, newGC, 
	    0, 0, 512, 512, 0, 0);  /* clear the mask */
  XSetFunction(di, newGC, GXcopy);
  
  init_colors_old(); /* mwf modified this function */
  load_gamma_colormap(); /* mwf added */
  colormap_loadrgb(); /* mwf added */

  make_colormap_window_children(fileSelectionBox, 
				get_color_info()->cmap); /* mwf 6-25-1996 */

  /* What does this do?  -MTC 7/1/98 */
  /*
  if (is_argv){
    Drawable bnct;
    
    sscanf(argv[1],"%d", &bnct);
    load_image_BNCT(bnct);
    slice_is_there = TRUE;
  }
  */
  

  /* 
   * block of code below will correctly ???
   * treat systems with multiple hardware
   * colormaps - DEW 8/16/94
   * this method is having trouble with XtWindow(AppShell)
   * also this block of code may not be necessary if only
   * one colormap needs installed see Young 2nd ed page 295
   * and Xlib Programming Manual page 420.
   */  
  {
    int  maxHWcmaps;
    maxHWcmaps = MaxCmapsOfScreen(DefaultScreenOfDisplay(di));
    
    if(maxHWcmaps > 1)  /* mwf 7-13-95 */
      {
	XSetWMColormapWindows( di, wi, &XtWindow(xcontoursTopLevelShell), 1);
      }

    /* mwf 7-20-95:  moved out of if */    
    make_colormap_window_children(AppShell, 
				  get_color_info()->cmap); 
  }

  /* added by mwf on 5/11/96 */
  dose_factor_shell = make_dose_factor_shell(xcontoursTopLevelShell);

  /* added by mwf on 1/13/97 */
  create_confirm_popup();

  /* Initialize the remembered files structures */
  /* rememberedFiles is global, in global.h     */
  init_remembered_files( rememberedFiles );


  /*
   * Load the preferences from the doseplay_prefs.txt file
   * in the resouces directory.
   */
  set_default_preferences();
  load_preferences();
  update_colorwash_values();

  /* Initialize the qsh info structure to null to start */
  qsh_info = NULL;
  
  XtAppMainLoop(context);
  return 0;
}




/*
**  Function: make_colormap_window_children() - also in gui/createslice.c
**  Arguments:
**    Widget: any widget
**    cmap:   colormap to drop in with XSetWindowColormap()
**  Purpose:
**    Recursively traverses widget heirarchy under widget w and calls XSetWindowColormap()
**    for all widgets that have been realized and that contain valid drawables.  This
**    *hopefully* ensures that systems with multiple window colormaps will drop in the
**    correct colormap for all underlying children.
**  mwf 7-20-95:  New Addition -> functionality remains the same for systems with
**    multiple colormaps.  For single colormap systems, an event handler is added to each
**    widget encountered in make_colormap_window_children that causes the current colormap
**    to be loaded when an EnterNotify occurs on that widget.  Purpose:  xcontours loads
**    its own colormap(s) and BNCT_RTPE needs a way to get its colormap back.  With use of
**    this routine, BNCT_RTPE gets its colormap back on an enter notify to any of its windows.
*/
void make_colormap_window_children(Widget w, Colormap cmap)
{
  int i;
  
  /******** NEW  NEW  NEW  NEW ****************************/
  static int initial_call = 1;
  
  DEBUG_TRACE_IN printf( "Entering make_colormap_window_children\n" );

  if (get_color_info() -> depth != 8)
  {
    DEBUG_TRACE_OUT printf( "Leaving make_colormap_window_children\n" );
    return;
  }
  
  /* On the first call to this function, install the sent colormap
   * on systems that have only one hardware colormap.
   */
  if ((MaxCmapsOfScreen(DefaultScreenOfDisplay(di))==1)&&(initial_call)) {
    XInstallColormap(XtDisplay(w), cmap);
    initial_call = 0;
  }
  /********************************************************/
  
  if (XtIsComposite(w) == FALSE)
    {
      if (XtIsRealized(w)) {
	/******** NEW  NEW  NEW  NEW ****************************/
	if (MaxCmapsOfScreen(DefaultScreenOfDisplay(di))==1)
	  XtAddEventHandler(w, EnterWindowMask, False,
			    (XtEventHandler)colormap_install, NULL);
	else
	  /********************************************************/
	  XSetWindowColormap(di, XtWindow(w), cmap);
      }
      DEBUG_TRACE_OUT printf( "Leaving make_colormap_window_children\n" );
      return;
    }
  else
    {
      WidgetList theList;
      Cardinal listCount;
      
      if (XtIsRealized(w)){
	/******** NEW  NEW  NEW  NEW ****************************/
	if (MaxCmapsOfScreen(DefaultScreenOfDisplay(di))==1)
	  XtAddEventHandler(w, EnterWindowMask, False,
			    (XtEventHandler)colormap_install, NULL);
	else
	  /********************************************************/
	  XSetWindowColormap(di, XtWindow(w), cmap);
      }
      
      XtVaGetValues(w, XmNnumChildren, &listCount,
		    XmNchildren, &theList, NULL); 
      for (i = 0; i < listCount; i ++ )
	if (XtIsWidget(theList[i]))
	  make_colormap_window_children(theList[i], cmap);
      theList = w->core.popup_list;
      for(i = 0; i < w->core.num_popups; i ++)
	if (XtIsWidget(theList[i]))
	  make_colormap_window_children(theList[i], cmap);
    }
    DEBUG_TRACE_OUT printf( "Leaving make_colormap_window_children\n" ); 	
}
