/* ==========================================================================
 *                                                                           
 * register_gui.c                                                            
 *                                                                           
 * INEEL BNCT Research Project                                               
 * Montana State University - Bozeman                                        
 *                                                                           
 * Builds widgets for the registration tool.                                 
 *                                                                           
 * Harkin 6/99                                                               
 *                                                                           
 * ==========================================================================
 */

#include "toqsh.h"

static void BuildRegisterMenubar (register_gui_t *);
static void BuildImageStrips (register_gui_t *);
static void BuildMessageBox (register_gui_t *);


/* ==========================================================================
 *
 * Function:    name 
 *
 * Purpose:      
 *
 * Syntax:       void name (gui *)
 *                  register_gui_t *gui  - pointer to the main register gui
 *                                     structure.
 *
 * ==========================================================================
 */


void name ()
{
   DEBUG_TRACE_IN printf ("Entering name\n");


   DEBUG_TRACE_OUT printf ( "Leaving name\n" );       
   return;
}


/* ==========================================================================
 *
 * Function:     BuildRegisterGui
 *
 * Purpose:      Build all the widgets in the register interface.
 *
 * Syntax:       void BuildRegisterGui (gui *)
 *                  register_gui_t *gui  - pointer to the register gui
 *                                         structure.
 *
 * ==========================================================================
 */

void BuildRegisterGui 
(  XtAppContext app, 
   Widget toplevel, 
   register_gui_t *register_gui,
   color_info_t   *color_info
)
{
   XmString    xstr;
   XImage      *image;
   Colormap    cmap;
   XColor      color, exact;


   DEBUG_TRACE_IN printf ("Entering BuildRegisterGui\n");

   register_gui->app = app;
   register_gui->toplevel = toplevel;
   register_gui->display = XtDisplay(toplevel);         
   register_gui->screen = DefaultScreen (register_gui->display);

   memcpy 
   (  (char *) &(register_gui->color_info),
      (char *)color_info, 
      sizeof (color_info_t)
   );

   cmap = DefaultColormap (register_gui->display, register_gui->screen);

   XAllocNamedColor 
   (  register_gui->display, cmap, 
      "red", &color, &exact
   );
   register_gui->red_pixel = color.pixel;

   XAllocNamedColor 
   (  register_gui->display, cmap, 
      "blue", &color, &exact
   );
   register_gui->blue_pixel = color.pixel;

   XAllocNamedColor 
   (  register_gui->display, cmap, 
      "green2", &color, &exact
   );
   register_gui->green_pixel = color.pixel;

   XAllocNamedColor 
   (  register_gui->display, cmap, 
      "magenta3", &color, &exact
   );
   register_gui->magenta_pixel = color.pixel;

   XAllocNamedColor 
   (  register_gui->display, cmap, 
      "cyan4", &color, &exact
   );
   register_gui->cyan_pixel = color.pixel;

   XAllocNamedColor 
   (  register_gui->display, cmap, 
      "yellow2", &color, &exact
   );
   register_gui->yellow_pixel = color.pixel;

 

   /*
    * Build the dialog shell.
    */

   DEBUG_GUI printf ("Building register shell\n");

   register_gui->shell = XmCreateFormDialog 
   (  register_gui->toplevel, "register_shell",
      NULL, 0
   );                                

   XtVaSetValues 
   (  register_gui->shell,
      XmNallowShellResize, True,
      XtVaTypedArg, XmNdialogTitle, XmRString, "Register Images", 16,
      NULL
   );

/*
   register_gui->shell=XmCreateDialogShell
   (  register_gui->toplevel, "register_shell",
      NULL,0
   );

   register_gui->shellform = XmCreateForm
   (  register_gui->shell, "register_form",
      NULL, 0
   );

   XtVaSetValues 
   (  register_gui->shellform,
      XmNallowShellResize, True,
      XtVaTypedArg, XmNdialogTitle, XmRString, "Register Images", 16,
      NULL
   );

   XtManageChild (register_gui->shellform);
*/

   /*
    * Build the main window.
    */

    DEBUG_GUI printf ( "Building register main window\n" );

    register_gui->mainwindow = XtVaCreateManagedWidget 
       (  "register_window", xmMainWindowWidgetClass,
          register_gui->shell,
          NULL
       );


   /*
    * Build the menu bar.
    */

   DEBUG_GUI printf ("Building register menubar\n");

   BuildRegisterMenubar (register_gui);

   /* 
    * Build Row Column manager widget 
    */

   DEBUG_GUI printf ( "Building register rowcol\n" );

   register_gui->rowcol = XtVaCreateManagedWidget 
   (  "registere_rowcol",
      xmRowColumnWidgetClass,
      register_gui->mainwindow,
      NULL
   );

   XtVaSetValues 
   (  register_gui->mainwindow,
      XmNmenuBar, register_gui->menubar.menu,
      XmNworkWindow, register_gui->rowcol, 
      NULL
   );

   /*
    * Build the main form.
    */
   
   DEBUG_GUI printf ( "Building register form\n" );

   register_gui->frame = XtVaCreateManagedWidget 
   (  "register_frame", 
      xmFrameWidgetClass,
      register_gui->rowcol, 
      NULL
   );

   register_gui->form = XtVaCreateManagedWidget 
   (  "register_form", 
      xmFormWidgetClass,
      register_gui->frame,
      NULL
   );

   XtAddEventHandler 
   (  register_gui->form,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );


   /*
    * Build the two image strips
    */
 
   BuildImageStrips (register_gui);

   /*
    * Build the information frame at the bottom
    */

   DEBUG_GUI printf ("Building information frame\n");

   register_gui->info_frame = XtVaCreateManagedWidget
   (
      "frame",
      xmFrameWidgetClass,
      register_gui->form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNbottomAttachment, XmATTACH_FORM,
      XmNbottomOffset, 10,
      XmNtopAttachment, XmATTACH_WIDGET, 
      XmNtopWidget, register_gui->lstrip_frame, 
      XmNtopOffset, 15,
      XmNborderWidth, 1,
      NULL
   );

   XtAddEventHandler 
   (  register_gui->info_frame,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );


   xstr = XmStringCreateLocalized ("Info");                 
   register_gui->frame_label = XtVaCreateManagedWidget
   (
      "info",
      xmLabelWidgetClass,
      register_gui->info_frame, 
      XmNchildType, XmFRAME_TITLE_CHILD, 
      XmNlabelString, xstr,
      NULL
   );
   XmStringFree (xstr);


   register_gui->information = XtVaCreateManagedWidget 
   (  "Register", 
      xmLabelWidgetClass,
      register_gui->info_frame, 
      XmNheight, 20,
      NULL 
   );

   /*
    * Set up a message box - mostly for image linking.
    */

   BuildMessageBox (register_gui);
   

   /*
    * Housekeeping.
    */

   XtVaGetValues
   (  register_gui->form,
      XmNhighlightColor, &register_gui->hl_color,
      XmNforeground, &register_gui->fg_color,
      XmNbackground, &register_gui->bg_color,
      XmNtopShadowColor, &register_gui->ts_color,
      XmNtopShadowColor, &register_gui->bs_color,
      NULL
   );

   XtVaSetValues 
   (  register_gui->information,
      XmNforeground, register_gui->blue_pixel,
      NULL
   );

   register_gui->link_cursor = 
      XCreateFontCursor (register_gui->display, XC_right_ptr);

   DEBUG_TRACE_OUT printf ( "Leaving BuildRegisterGui\n" );       
   return;
}


/* ==========================================================================
 *
 * Function:     BuildRegisterMenubar
 *
 * Purpose:      Build the menubar widget
 *
 * Syntax:       void BuildRegisterMenubar (register_gui *)
 *                  main_gui_t *gui  - pointer to the main register gui
 *                                     structure.
 *
 * ==========================================================================
 */

void BuildRegisterMenubar (register_gui_t *register_gui)
{

   Colormap    cmap;
   XColor      pixel, exact;

   DEBUG_TRACE_IN printf ("Entering BuildRegisterMenubar\n");

   register_gui->menubar.menu = XmCreateMenuBar 
   (  register_gui->mainwindow, 
      "register_menu",
      NULL, 0
   );

   XtAddEventHandler 
   (  register_gui->menubar.menu,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );

   /*
    * File menu -------------------------------------------------------
    */

   DEBUG_GUI printf ("Building file menu\n");

   register_gui->menubar.file_menu = XmCreatePulldownMenu 
   (  register_gui->menubar.menu,
      "viewSubmenu", 
      NULL, 0
   );

   register_gui->menubar.file_cascade = XtVaCreateManagedWidget 
   (  "File", 
       xmCascadeButtonWidgetClass,
       register_gui->menubar.menu,
       XmNsubMenuId, register_gui->menubar.file_menu, 
       NULL
   );

   register_gui->menubar.open = XtCreateManagedWidget 
   (  "Open",
      xmPushButtonWidgetClass,
      register_gui->menubar.file_menu, 
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.open, 
      XmNactivateCallback,
      RegisterOpenFileCB, 
      (XtPointer) register_gui
   );

   register_gui->menubar.save = XtCreateManagedWidget 
   (  "Save",
      xmPushButtonWidgetClass,
      register_gui->menubar.file_menu, 
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.save, 
      XmNactivateCallback,
      RegisterSaveFileCB, 
      (XtPointer) register_gui
   );

   register_gui->menubar.exit = XtCreateManagedWidget 
   (  "Exit",
      xmPushButtonWidgetClass,
      register_gui->menubar.file_menu, 
      NULL, 0
   );

   
   cmap = DefaultColormap 
   (   register_gui->display, 
       register_gui->screen 
   );

   XtVaSetValues 
   (  register_gui->menubar.exit,
      XmNforeground, register_gui->red_pixel,
      NULL
   );

   XtAddCallback 
   (  register_gui->menubar.exit, 
      XmNactivateCallback,
      RegisterExitCB, 
      (XtPointer) register_gui
   );



   /*
    * Options menu -----------------------------------------------------
    */ 

   DEBUG_GUI printf ("Building file menu\n");

   register_gui->menubar.option_menu = XmCreatePulldownMenu 
   (  register_gui->menubar.menu,
      "optionSubmenu", 
      NULL, 0
   );

   register_gui->menubar.option_cascade = XtVaCreateManagedWidget 
   (  "Options", 
      xmCascadeButtonWidgetClass,
      register_gui->menubar.menu,
      XmNsubMenuId, register_gui->menubar.option_menu, 
      NULL
   );

   XtManageChild (register_gui->menubar.menu);

   /*
    * Image size submenu
    */

   register_gui->menubar.size_menu = XmCreatePulldownMenu 
   (  register_gui->menubar.option_menu,
      "optionSubmenu", 
      NULL, 0
   );

   register_gui->menubar.size_cascade = XtVaCreateManagedWidget 
   (  "Image Size", 
      xmCascadeButtonWidgetClass,
      register_gui->menubar.option_menu,
      XmNsubMenuId, register_gui->menubar.size_menu, 
      NULL
   );

   register_gui->menubar.size_128= XtCreateManagedWidget 
   (  "Width = 128", 
      xmPushButtonWidgetClass,
      register_gui->menubar.size_menu,
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.size_128,
      XmNactivateCallback,
      RegisterSizeChangeCB, ( XtPointer ) register_gui 
   );

   register_gui->menubar.size_256= XtCreateManagedWidget 
   (  "Width = 256", 
      xmPushButtonWidgetClass,
      register_gui->menubar.size_menu,
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.size_256,
      XmNactivateCallback,
      RegisterSizeChangeCB, ( XtPointer ) register_gui 
   );

   register_gui->menubar.size_512 = XtCreateManagedWidget 
   (  "Width = 512", 
      xmPushButtonWidgetClass,
      register_gui->menubar.size_menu,
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.size_512,
      XmNactivateCallback,
      RegisterSizeChangeCB, ( XtPointer ) register_gui 
   );

   /*
    * Registration Model.
    */

   register_gui->menubar.model_menu = XmCreatePulldownMenu 
   (  register_gui->menubar.option_menu,
      "optionSubmenu", 
      NULL, 0
   );

   register_gui->menubar.model_cascade = XtVaCreateManagedWidget 
   (  "Model", 
      xmCascadeButtonWidgetClass,
      register_gui->menubar.option_menu,
      XmNsubMenuId, register_gui->menubar.model_menu, 
      NULL
   );

   register_gui->menubar.model_lreg= XtCreateManagedWidget 
   (  "Linear Regression", 
      xmPushButtonWidgetClass,
      register_gui->menubar.model_menu,
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.model_lreg,
      XmNactivateCallback,
      RegisterModelChangeCB, ( XtPointer ) register_gui 
   );


   /*
    * Registration Method.
    */

   register_gui->menubar.method_menu = XmCreatePulldownMenu 
   (  register_gui->menubar.option_menu,
      "optionSubmenu", 
      NULL, 0
   );

   register_gui->menubar.method_cascade = XtVaCreateManagedWidget 
   (  "Method", 
      xmCascadeButtonWidgetClass,
      register_gui->menubar.option_menu,
      XmNsubMenuId, register_gui->menubar.method_menu, 
      NULL
   );

   register_gui->menubar.method_alternate = XtCreateManagedWidget 
   (  "Alternating", 
      xmPushButtonWidgetClass,
      register_gui->menubar.method_menu,
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.method_alternate,
      XmNactivateCallback,
      RegisterMethodChangeCB, ( XtPointer ) register_gui 
   );

   register_gui->menubar.method_link = XtCreateManagedWidget 
   (  "Linking", 
      xmPushButtonWidgetClass,
      register_gui->menubar.method_menu,
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.method_link,
      XmNactivateCallback,
      RegisterMethodChangeCB, ( XtPointer ) register_gui 
   );

   register_gui->menubar.method_overlay = XtCreateManagedWidget 
   (  "Overlay", 
      xmPushButtonWidgetClass,
      register_gui->menubar.method_menu,
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.method_overlay,
      XmNactivateCallback,
      RegisterMethodChangeCB, ( XtPointer ) register_gui 
   );

   /*
    * Point color menu
    */

   register_gui->menubar.point_menu = XmCreatePulldownMenu 
   (  register_gui->menubar.option_menu,
      "optionSubmenu", 
      NULL, 0
   );

   register_gui->menubar.point_cascade = XtVaCreateManagedWidget 
   (  "Point Color", 
      xmCascadeButtonWidgetClass,
      register_gui->menubar.option_menu,
      XmNsubMenuId, register_gui->menubar.point_menu, 
      NULL
   );

   /*
    * Yellow
    */

   register_gui->menubar.point_yellow = XtCreateManagedWidget 
   (  "Yellow", 
      xmPushButtonWidgetClass,
      register_gui->menubar.point_menu,
      NULL, 0
   );

   XtVaSetValues 
   (  register_gui->menubar.point_yellow,
      XmNforeground, register_gui->yellow_pixel,
      NULL
   );

   XtAddCallback 
   (  register_gui->menubar.point_yellow,
      XmNactivateCallback,
      PointColorChangeCB, ( XtPointer ) register_gui 
   );

   /*
    * Red
    */

   register_gui->menubar.point_red= XtCreateManagedWidget 
   (  "Red", 
      xmPushButtonWidgetClass,
      register_gui->menubar.point_menu,
      NULL, 0
   );

   XtVaSetValues 
   (  register_gui->menubar.point_red,
      XmNforeground, register_gui->red_pixel,
      NULL
   );

   XtAddCallback 
   (  register_gui->menubar.point_red,
      XmNactivateCallback,
      PointColorChangeCB, ( XtPointer ) register_gui 
   );

   /*
    * Blue
    */

   register_gui->menubar.point_blue= XtCreateManagedWidget 
   (  "Blue", 
      xmPushButtonWidgetClass,
      register_gui->menubar.point_menu,
      NULL, 0
   );

   XtVaSetValues 
   (  register_gui->menubar.point_blue,
      XmNforeground, register_gui->blue_pixel,
      NULL
   );

   XtAddCallback 
   (  register_gui->menubar.point_blue,
      XmNactivateCallback,
      PointColorChangeCB, ( XtPointer ) register_gui 
   );

   /*
    * Green
    */

   register_gui->menubar.point_green= XtCreateManagedWidget 
   (  "Green", 
      xmPushButtonWidgetClass,
      register_gui->menubar.point_menu,
      NULL, 0
   );

   XtVaSetValues 
   (  register_gui->menubar.point_green,
      XmNforeground, register_gui->green_pixel,
      NULL
   );

   XtAddCallback 
   (  register_gui->menubar.point_green,
      XmNactivateCallback,
      PointColorChangeCB, ( XtPointer ) register_gui 
   );

   /*
    * Magenta
    */

   register_gui->menubar.point_magenta= XtCreateManagedWidget 
   (  "Magenta", 
      xmPushButtonWidgetClass,
      register_gui->menubar.point_menu,
      NULL, 0
   );

   XtVaSetValues 
   (  register_gui->menubar.point_magenta,
      XmNforeground, register_gui->magenta_pixel,
      NULL
   );

   XtAddCallback 
   (  register_gui->menubar.point_magenta,
      XmNactivateCallback,
      PointColorChangeCB, ( XtPointer ) register_gui 
   );

   /*
    * Cyan
    */

   register_gui->menubar.point_cyan= XtCreateManagedWidget 
   (  "Cyan", 
      xmPushButtonWidgetClass,
      register_gui->menubar.point_menu,
      NULL, 0
   );

   XtVaSetValues 
   (  register_gui->menubar.point_cyan,
      XmNforeground, register_gui->cyan_pixel,
      NULL
   );

   XtAddCallback 
   (  register_gui->menubar.point_cyan,
      XmNactivateCallback,
      PointColorChangeCB, ( XtPointer ) register_gui 
   );

   /*
    * Commands menu -----------------------------------------------------
    */ 

   DEBUG_GUI printf ("Building commands menu\n");

   register_gui->menubar.cmd_menu = XmCreatePulldownMenu 
   (  register_gui->menubar.menu,
      "optionSubmenu", 
      NULL, 0
   );

   XtAddEventHandler 
   (  register_gui->menubar.cmd_menu,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );

   register_gui->menubar.cmd_cascade = XtVaCreateManagedWidget 
   (  "Commands", 
      xmCascadeButtonWidgetClass,
      register_gui->menubar.menu,
      XmNsubMenuId, register_gui->menubar.cmd_menu, 
      NULL
   );

   register_gui->menubar.apply_to_all = XtCreateManagedWidget 
   (  "Register All",
      xmPushButtonWidgetClass,
      register_gui->menubar.cmd_menu, 
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.apply_to_all, 
      XmNactivateCallback,
      RegisterApplyToAllCB, 
      (XtPointer) register_gui
   );

   XtAddEventHandler 
   (  register_gui->menubar.apply_to_all,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );


   register_gui->menubar.revert = XtCreateManagedWidget 
   (  "Unregister All",
      xmPushButtonWidgetClass,
      register_gui->menubar.cmd_menu, 
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.revert, 
      XmNactivateCallback,
      RegisterRevertCB, 
      (XtPointer) register_gui
   );

   XtAddEventHandler 
   (  register_gui->menubar.revert,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );


   register_gui->menubar.remove_all= XtCreateManagedWidget 
   (  "Remove All Marks",
      xmPushButtonWidgetClass,
      register_gui->menubar.cmd_menu, 
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.remove_all, 
      XmNactivateCallback,
      RegisterRemoveAllCB, 
      (XtPointer) register_gui
   );

   XtAddEventHandler 
   (  register_gui->menubar.remove_all,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );


   register_gui->menubar.print = XtCreateManagedWidget 
   (  "Print",
      xmPushButtonWidgetClass,
      register_gui->menubar.cmd_menu, 
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.print, 
      XmNactivateCallback,
      RegisterPrintCB, 
      (XtPointer) register_gui
   );

/*
   register_gui->menubar.interpolate_marks = XtCreateManagedWidget 
   (  "Interpolate Marks",
      xmPushButtonWidgetClass,
      register_gui->menubar.cmd_menu, 
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->menubar.interpolate_marks, 
      XmNactivateCallback,
      RegisterInterpolateCB, 
      (XtPointer) register_gui
   );

   XtAddEventHandler 
   (  register_gui->menubar.interpolate_marks,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );
*/


   XtManageChild (register_gui->menubar.menu);


   XtManageChild (register_gui->menubar.menu);


   DEBUG_TRACE_OUT printf ( "Leaving BuildRegisterMenubar\n" );       

   return;
}


/* ==========================================================================
 *
 * Function:     BuildImageStrips
 *
 * Purpose:      Build the image strips
 *
 * Syntax:       void BuildImageStrips (register_gui)
 *                  register_gui_t *gui  - pointer to the main register gui
 *                                     structure.
 *
 * ==========================================================================
 */

void BuildImageStrips (register_gui_t *register_gui)
{
   XmString    xstr;
   Widget      w;

   DEBUG_TRACE_IN printf ("Entering BuildImageStrips\n");

   /*
    * Left hand side
    */

   DEBUG_GUI printf ("Building left image strip\n");

   register_gui->lstrip_frame = XtVaCreateManagedWidget
   ( "frame", 
      xmFrameWidgetClass,
      register_gui->form,
      XmNtopAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_POSITION,
      XmNrightPosition, 45,
      NULL, 0
   );

   register_gui->lstrip_form = XtVaCreateManagedWidget 
   (  "register_form", 
      xmFormWidgetClass,
      register_gui->lstrip_frame, 
      XmNwidth, 300,
      NULL
   );

   XtAddEventHandler 
   (  register_gui->lstrip_form,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );

   DEBUG_GUI printf ("Building left title \n");

   register_gui->lstrip_text_frame = XtVaCreateManagedWidget
   ( "frame", 
      xmFrameWidgetClass,
      register_gui->lstrip_form,
      XmNtopAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      NULL, 0
   );


   register_gui->lstrip_text = XtVaCreateManagedWidget
   ( "label", 
      xmLabelWidgetClass,
      register_gui->lstrip_text_frame,
      NULL, 0
   );


   XtAddEventHandler 
   (  register_gui->lstrip_text,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );


   xstr = XmStringCreateLocalized ("Fixed Image Set");                 
   XtVaSetValues
   (
      register_gui->lstrip_text,
      XmNlabelString, xstr,
      NULL
   );
   XmStringFree (xstr);

   DEBUG_GUI printf ("Building left scroll window\n");

   register_gui->lstrip_swindow = XtVaCreateManagedWidget
   (
      "swindow",
      xmScrolledWindowWidgetClass,
      register_gui->lstrip_form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNtopAttachment, XmATTACH_WIDGET,
      XmNtopWidget, register_gui->lstrip_text_frame,

      XmNscrollingPolicy, XmAUTOMATIC,
      XmNscrollBarDisplayPolicy, XmAS_NEEDED,
      XmNvisualPolicy, XmVARIABLE,

      XmNheight, 600, 
/*      XmNwidth, 300,   */

      NULL, 0
   );

   register_gui->left.rowcol = XtVaCreateManagedWidget
   (
      "rowcol",
      xmRowColumnWidgetClass,
      register_gui->lstrip_swindow,
      XmNorientation, XmVERTICAL,
      XmNnumColumns, 1,
      NULL, 0
   );

   /*
    * left strip buttons
    */

   DEBUG_GUI printf ("Building left buttons\n");

   register_gui->lstrip_buttons.form = XtVaCreateManagedWidget
   (
      "form",
      xmFormWidgetClass,
      register_gui->lstrip_form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNbottomAttachment, XmATTACH_FORM,
      XmNtopAttachment, XmATTACH_WIDGET,
      XmNtopWidget, register_gui->lstrip_swindow,
      XmNtopOffset, 10,
      NULL, 0
   );

   register_gui->lstrip_buttons.open_frame = XtVaCreateManagedWidget
   (
      "frame",
      xmFrameWidgetClass,
      register_gui->lstrip_buttons.form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNleftOffset, 10,
      XmNwidth, 50,
      NULL, 0
   );

   register_gui->lstrip_buttons.open = XtVaCreateManagedWidget
   (
      "Open",
      xmPushButtonWidgetClass,
      register_gui->lstrip_buttons.open_frame,
      XmNwidth, 50,
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->lstrip_buttons.open, 
      XmNactivateCallback,
      RegisterOpenLeftFileCB, 
      (XtPointer) register_gui
   );

   XtAddEventHandler 
   (  register_gui->lstrip_buttons.open,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );


   register_gui->lstrip_buttons.close_frame = XtVaCreateManagedWidget
   (
      "frame",
      xmFrameWidgetClass,
      register_gui->lstrip_buttons.form,
      XmNleftAttachment, XmATTACH_WIDGET,
      XmNleftWidget, register_gui->lstrip_buttons.open_frame,
      XmNleftOffset, 20,
      XmNwidth, 50,
      NULL, 0
   );

   register_gui->lstrip_buttons.close = XtVaCreateManagedWidget
   (
      "Close",
      xmPushButtonWidgetClass,
      register_gui->lstrip_buttons.close_frame,
      XmNwidth, 50,
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->lstrip_buttons.close, 
      XmNactivateCallback,
      RegisterCloseLeftFileCB, 
      (XtPointer) register_gui
   );

   XtAddEventHandler 
   (  register_gui->lstrip_buttons.close,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );

   /*
    * Space between strips
    */


   DEBUG_GUI printf ("Building interstrip space\n");

   register_gui->space_frame = XtVaCreateManagedWidget
   ( "frame", 
      xmFrameWidgetClass,
      register_gui->form,
      XmNtopAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_POSITION,
      XmNleftPosition, 45,
      XmNrightAttachment, XmATTACH_POSITION,
      XmNrightPosition, 55,
      NULL, 0
   );
   register_gui->space_form = XtVaCreateManagedWidget 
   (  "register_form", 
      xmFormWidgetClass,
      register_gui->space_frame, 
      NULL
   );

   XtAddEventHandler 
   (  register_gui->space_form,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );



   /*
    * Right hand side
    */

   DEBUG_GUI printf ("Building right image strip\n");

   register_gui->rstrip_frame = XtVaCreateManagedWidget
   ( "frame", 
      xmFrameWidgetClass,
      register_gui->form,
      XmNtopAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_POSITION,
      XmNleftPosition, 55,
      NULL, 0
   );

   register_gui->rstrip_form = XtVaCreateManagedWidget 
   (  "register_form", 
      xmFormWidgetClass,
      register_gui->rstrip_frame, 
      NULL
   );

   XtAddEventHandler 
   (  register_gui->rstrip_form,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );

   DEBUG_GUI printf ("Building right strip title\n");

   register_gui->rstrip_text_frame = XtVaCreateManagedWidget
   ( "frame", 
      xmFrameWidgetClass,
      register_gui->rstrip_form,
      XmNrightAttachment, XmATTACH_FORM,
      XmNleftAttachment, XmATTACH_FORM,
      XmNtopAttachment, XmATTACH_FORM,
      NULL, 0
   );

   register_gui->rstrip_text = XtVaCreateManagedWidget
   ( "form", 
      xmLabelWidgetClass,
      register_gui->rstrip_text_frame,
      NULL, 0
   );

   XtAddEventHandler 
   (  register_gui->rstrip_text,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );


   xstr = XmStringCreateLocalized ("Variable Image Set");                 
   XtVaSetValues 
   (
      register_gui->rstrip_text,
      XmNlabelString, xstr,
      NULL
   );
   XmStringFree (xstr);

   DEBUG_GUI printf ("Building right scroll window\n");

   register_gui->rstrip_swindow = XtVaCreateManagedWidget
   (
      "swindow",
      xmScrolledWindowWidgetClass,
      register_gui->rstrip_form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNtopAttachment, XmATTACH_WIDGET,
      XmNtopWidget, register_gui->rstrip_text_frame,

      XmNscrollingPolicy, XmAUTOMATIC,
      XmNscrollBarDisplayPolicy, XmAS_NEEDED,
      XmNvisualPolicy, XmVARIABLE,

      XmNheight, 600, 
      XmNwidth, 300,  

      NULL, 0
   );

   register_gui->right.rowcol = XtVaCreateManagedWidget
   (
      "rowcol",
      xmRowColumnWidgetClass,
      register_gui->rstrip_swindow,
      XmNorientation, XmVERTICAL,
      XmNnumColumns, 1,
      NULL, 0
   );


   /*
    * right strip buttons
    */

   DEBUG_GUI printf ("Building right buttons\n");

   register_gui->rstrip_buttons.form = XtVaCreateManagedWidget
   (
      "form",
      xmFormWidgetClass,
      register_gui->rstrip_form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNbottomAttachment, XmATTACH_FORM,
      XmNtopAttachment, XmATTACH_WIDGET,
      XmNtopWidget, register_gui->rstrip_swindow,
      XmNtopOffset, 10,
      NULL, 0
   );

   register_gui->rstrip_buttons.open_frame = XtVaCreateManagedWidget
   (
      "frame",
      xmFrameWidgetClass,
      register_gui->rstrip_buttons.form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNleftOffset, 10,
      XmNwidth, 50,
      NULL, 0
   );

   register_gui->rstrip_buttons.open = XtVaCreateManagedWidget
   (
      "Open",
      xmPushButtonWidgetClass,
      register_gui->rstrip_buttons.open_frame,
      XmNwidth, 50,
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->rstrip_buttons.open, 
      XmNactivateCallback,
      RegisterOpenRightFileCB, 
      (XtPointer) register_gui
   );

   XtAddEventHandler 
   (  register_gui->rstrip_buttons.open,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );


   register_gui->rstrip_buttons.close_frame = XtVaCreateManagedWidget
   (
      "frame",
      xmFrameWidgetClass,
      register_gui->rstrip_buttons.form,
      XmNleftAttachment, XmATTACH_WIDGET,
      XmNleftWidget, register_gui->rstrip_buttons.open_frame,
      XmNleftOffset, 20,
      XmNwidth, 50,
      NULL, 0
   );

   register_gui->rstrip_buttons.close = XtVaCreateManagedWidget
   (
      "Close",
      xmPushButtonWidgetClass,
      register_gui->rstrip_buttons.close_frame,
      XmNwidth, 50,
      NULL, 0
   );

   XtAddCallback 
   (  register_gui->rstrip_buttons.close, 
      XmNactivateCallback,
      RegisterCloseRightFileCB, 
      (XtPointer) register_gui
   );

   XtAddEventHandler 
   (  register_gui->rstrip_buttons.close,
      EnterWindowMask, False,
      RegisterInfoEH, (XtPointer)register_gui
   );

   DEBUG_TRACE_OUT printf ("BuildImageStrips\n");

   return;
}


/* ==========================================================================
 *
 * Function:     BuildMessageBox
 *
 * Purpose:      Build a message box for various uses.
 *
 * Syntax:       void BuildMessageBox (register_gui)
 *                  register_gui_t *gui  - pointer to the main register gui
 *                                     structure.
 *
 * ==========================================================================
 */

void BuildMessageBox (register_gui_t *register_gui)
{
   XmString    xstr;
   Widget      w;

   DEBUG_TRACE_IN printf ("Entering BuildMessageBox\n");

   /*
    * Left hand side
    */

   DEBUG_GUI printf ("Building left image strip\n");

   register_gui->message_box = XmCreateInformationDialog 
   (  register_gui->toplevel, "register_shell",
      NULL, 0
   );                                

   w = XmMessageBoxGetChild (register_gui->message_box, XmDIALOG_OK_BUTTON);

   XtVaSetValues
   (  w,
      XmNsensitive, False, 
      NULL
   );

   w = XmMessageBoxGetChild (register_gui->message_box, XmDIALOG_HELP_BUTTON);

   XtVaSetValues
   (  w,
      XmNsensitive, False, 
      NULL
   );

   XtAddCallback
   (  register_gui->message_box,
      XmNcancelCallback, AltCancelCB,
      (XtPointer) register_gui
   );

   DEBUG_TRACE_OUT printf ("Leaving BuildMessageBox\n");

   return;
}
