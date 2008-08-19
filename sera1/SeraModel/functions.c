#include "functions.h"

#include "bitmaps/SeraModel/b1_curs.xbm"
#include "bitmaps/SeraModel/b2_curs.xbm"
#include "bitmaps/SeraModel/b3_curs.xbm"
#include "bitmaps/SeraModel/b4_curs.xbm"
#include "bitmaps/SeraModel/b5_curs.xbm"
#include "bitmaps/SeraModel/b1_cursb.xbm"
#include "bitmaps/SeraModel/b2_cursb.xbm"
#include "bitmaps/SeraModel/b3_cursb.xbm"
#include "bitmaps/SeraModel/b4_cursb.xbm"
#include "bitmaps/SeraModel/b5_cursb.xbm"
#include "sliderText.h"
#include "keyval_tools.h"
#include "gui_tools.h"
#include "image_matrix.h"
#include "overlay_info.h"
#include "file_tools.h"

Cursor * global_image_cursor_ptr = (Cursor *)NULL;
int see_watch = 0;

/* Assert, we wish to keep track of .uv files so we can start BNCT3D with
 * them.  We will remember up to REMEMBER_REGION_FILES files.  We'll also keep
 * track of these in an external file.  The list will be ordered from the
 * most recently accessed file to the least recently accessed file.
 */

/* Globals */
Widget global_label;
Boolean preferences_to_save[12];
button_type remap_button_list[4];


void generic_cancel_load_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering generic_cancel_load_CB\n" );
  
  XtUnrealizeWidget((Widget)clientData);
  XtDestroyWidget((Widget)clientData);

  DEBUG_TRACE_OUT printf ( "Leaving generic_cancel_load_CB\n" );
}

void fsb_okCB(Widget widget, XtPointer clientData, XtPointer callData) {
    XmFileSelectionBoxCallbackStruct *cbs =
        (XmFileSelectionBoxCallbackStruct *) callData;
    char *filename;
    image_matrix_type *image_matrix_ptr;

    DEBUG_TRACE_IN printf ( "Entering fsb_okCB\n" );

    image_matrix_ptr = get_image_matrix();

  
    XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &filename);

    switch((int)clientData)
    {
        case 0:
            debug("OK has been selected.\n");
            if( FT_fileExists( filename ) && is_a_valid_qsh_file( filename ) )
            {
                XtUnmanageChild( widget );
                set_cursor(WATCH_CURSOR);
                load_image_file(filename);
                set_cursor(NORMAL_CURSOR);
            }
            else
            {
                DT_error( widget, "That is not a valid QSH file!", NULL, NULL );
            }
      
            break;
        case 1:
            debug("overlay load images.\n");
            printf("Can nolonger overlay images with this method\n");
            /*overlay_load_images(filename);*/
            break;
        case 2:
            if( FT_fileExists( filename ) && is_uv( filename ) )
            {
                XtUnmanageChild( widget );
                overlay_load_bodies(filename);
                debug("overlay load bodies.\n");
            }
            else
            {
                DT_error( widget, "That is not a valid UV file!", NULL, NULL );
            }
        
            break;
        case 3:
            /* Save Regions */
            XtUnmanageChild(widget);
            save_current_regions(filename);
            break;
        case 4:
            /* save images */
            XtUnmanageChild(widget);
            save_current_images(filename);
            break;
        case 5: /* Launch BNCT3D */
            render_FCN(image_matrix_ptr, filename);
            break;
    }

    XtFree( filename );

    DEBUG_TRACE_OUT printf ( "Leaving fsb_okCB\n" );
}

void fsb_cancelCB(Widget w, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering fsb_cancelCB\n" );
  XtUnrealizeWidget(w);
  DEBUG_TRACE_OUT printf ( "Leaving fsb_cancelCB\n" );
}

void fsb_helpCB(Widget w, XtPointer clientData, XtPointer callData) {
  image_matrix_type * imp;

  DEBUG_TRACE_IN printf ( "Entering fsb_helpCB\n" );

  imp = get_image_matrix();
  switch((int)clientData)
    {
    case 0:
      DT_inform( imp->toplevel,"Please select a file with raw image data to be loaded.\n\
Raw images must be 1 byte (8 bits) per pixel.\n\
Gzipped raw image files are also acceptable provided they end in .gz.\n\
If the file contains multiple images, all images in the file will be loaded.\n\
If necessary, you will be prompted for the dimensions of the images.\n\
If any images are already loaded, the new images will be appended to\n\
the end of the image set.", "File Selection Help", NULL);
      break;
    case 1:
      DT_inform( imp->toplevel,"You've chosen to load images over top of your current images.\n\
These images must be the same size and depth as the original images.\n\
All regions will be preserved.", "File Selection Help", NULL);
      break;
    case 2:
      DT_inform( imp->toplevel,"You've chosen to load a set of regions that correspond to the\n\
current images.  The dimensions of the region file must be the same as the\n\
dimensions of the current images.  (For example, both 256x256.)  All\n\
loaded images will be preserved.", "File Selection Help", NULL);
      break;
    case 3:
      DT_inform( imp->toplevel,"You've chosen to save the regions you've created.  Simply\n\
choose an appropriate filename and path.  If the filename does not have\n\
a .uv extension, one will be added.  A generic .uvh file will also be\n\
written.", "File Selection Help", NULL);
      break;
    case 4:
      DT_inform( imp->toplevel,"You've chosen to save the currently displayed images.  This\n\
may be useful if you've filtered or altered the images in any way and\n\
would like to preserve your changes.  Though it's recommended you use the\n\
default extension of .raw, no suffix rules are enforced.", "File Selection Help", NULL);
      break;
    case 5:
      DT_inform( imp->toplevel,"You've chosen to launch BNCT3D.  Now, choose a set of regions\n\
to view when BNCT3D begins.", "File Selection Help", NULL);
    }
  DEBUG_TRACE_OUT printf ( "Leaving fsb_helpCB\n" );
}

/*******************************************************/
/* A simple little routine that allows you to pop up a
   dialog box with your 'message' in it and an O.K.
   button.  User will see message then select the O.K.
   button.                                             */
/*******************************************************/
void Confirm_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
    DEBUG_TRACE_IN printf ( "Entering Confirm_CB\n" );
    XtDestroyWidget(w);
    DEBUG_TRACE_OUT printf ( "Leaving Confirm_CB\n" );
}

void Confirm( char *message )
{
    Arg al[10];
    int ac;
    Widget confirm_widget;
    image_matrix_type * image_matrix_ptr;

    DEBUG_TRACE_IN printf ( "Entering Confirm\n" );

    image_matrix_ptr = get_image_matrix();

    ac=0;
    XtSetArg(al[ac], XmNtitle, "Confirm"); ac++;
    XtSetArg(al[ac], XmNmessageString, XmStringCreateLtoR
	     (message,image_matrix_ptr->char_set)); ac++;
    /*XtSetArg(al[ac], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL); ac++;*/
    confirm_widget=XmCreateMessageDialog(image_matrix_ptr->toplevel,"Confirm_dialog",al,ac);
    XtAddCallback(confirm_widget,XmNokCallback,Confirm_CB, (XtPointer)NULL);
    XtUnmanageChild(XmMessageBoxGetChild(confirm_widget,XmDIALOG_HELP_BUTTON));
    XtUnmanageChild(XmMessageBoxGetChild(confirm_widget,XmDIALOG_CANCEL_BUTTON));

    XtManageChild(confirm_widget);

    DEBUG_TRACE_OUT printf ( "Leaving Confirm\n" );
}

/*
 * Acts the same as Confirm(), but you can specify
 * what widget you would like to use as the parent
 */
void widget_confirm( Widget parent, char * message )
{

  Widget confirm_widget;
  XmString title;
  XmString msg;

  DEBUG_TRACE_IN printf("Entering widget_confirm\n" );

  title = XmStringCreateLtoR( "Confirm", MY_CHARSET );
  msg   = XmStringCreateLtoR( message, MY_CHARSET );

  confirm_widget = XmCreateMessageDialog( parent, "confirm_widget", NULL, 0 );
  XtVaSetValues( confirm_widget,
		 XmNdialogTitle, title,
		 XmNmessageString, msg,
		 NULL );
  XtUnmanageChild( XmMessageBoxGetChild( confirm_widget, XmDIALOG_HELP_BUTTON ) );
  XtUnmanageChild( XmMessageBoxGetChild( confirm_widget, XmDIALOG_CANCEL_BUTTON ) );

  XtAddCallback( confirm_widget, XmNokCallback, Confirm_CB, NULL );

  XtManageChild( confirm_widget );

  XmStringFree( title );
  XmStringFree( msg );

  DEBUG_TRACE_OUT("Leaving widget_confirm\n");
}


void Ask_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
  int * ans_ptr;
  
  DEBUG_TRACE_IN printf ( "Entering Ask_CB\n" );

  ans_ptr = (int*)client_data;

  if (((XmAnyCallbackStruct *)call_data)->reason==XmCR_OK) {
    *ans_ptr = 1;
  } else {
    *ans_ptr = 0;
  }

  XtDestroyWidget(w);

  DEBUG_TRACE_OUT printf ( "Leaving Ask_CB\n" );
}

/* Returns 1 if 'yes', 0 if 'no' */
int Ask( char *message )
{
    Arg al[10];
    int ac;
    Widget ask_widget;
    image_matrix_type * image_matrix_ptr;
    int * answer, an_int=-1;

    DEBUG_TRACE_IN printf ( "Entering Ask\n" );

    image_matrix_ptr = get_image_matrix();
    answer = &an_int;

    ac=0;
    XtSetArg(al[ac], XmNtitle, "You sure?"); ac++;
    XtSetArg(al[ac], XmNmessageString, XmStringCreateLtoR
	     (message,image_matrix_ptr->char_set)); ac++;
    /*XtSetArg(al[ac], XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL); ac++;*/
    ask_widget=XmCreateMessageDialog(image_matrix_ptr->toplevel,"Ask_dialog",al,ac);
    XtAddCallback(ask_widget,XmNokCallback,Ask_CB, (XtPointer)answer);
    XtAddCallback(ask_widget,XmNcancelCallback,Ask_CB, (XtPointer)answer);
    XtUnmanageChild(XmMessageBoxGetChild(ask_widget,XmDIALOG_HELP_BUTTON));

    XtManageChild(ask_widget);

    while (*answer==-1) {
      wait_on_xserver2();
    }

    DEBUG_TRACE_OUT printf ( "Leaving Ask\n" );
    return(*answer);
}

void exit_FCN(image_matrix_type * image_matrix_ptr)
{
    int i;
    
    DEBUG_TRACE_IN printf ( "Entering exit_FCN\n" );

    if (DT_decide( image_matrix_ptr->toplevel, 
                   image_matrix_ptr->app,
                   "Are you sure you want to exit?",
                   "You Sure?", "Yes", "No"))
    {
        /* Free any images we may have */
        for( i = 0; i < image_matrix_ptr->num_pics; i++ )
        {
            MT_free( (void *) image_matrix_ptr->img_arr[i].data );
            MT_free( (void *) image_matrix_ptr->img_arr[i].pimage_data );
            MT_free( (void *) image_matrix_ptr->img_arr[i].region_data );
        }
        MT_free( (void *) image_matrix_ptr->img_arr );

        /* Free dose information */
        freeDoseInfoList( image_matrix_ptr->dose_info_list );

        /* Free body data title */
        MT_free( (void *) image_matrix_ptr->body_data_title );
        
        exit(EXIT_SUCCESS);
    }
    DEBUG_TRACE_OUT printf ( "Leaving exit_FCN\n" );
}


void overlay_load_bodies_file(char * name, int w, int h,
			       int * is_valid, unsigned char * remap) {
  gz_FILE * gz_fptr;
  unsigned char * arr;
  int num=0, total;
  char * uvFileName;
  image_matrix_type * image_matrix_ptr;

  DEBUG_TRACE_IN printf ( "Entering overlay_load_bodies_file\n" );

  image_matrix_ptr = get_image_matrix();
  total = image_matrix_ptr->num_pics;
  uvFileName = get_uv_name( name );
  
  gz_fptr = gz_fopen(uvFileName, "r");

  if (gz_fptr) {
    add_to_saved_regions_list( name );

    arr = (unsigned char *)MT_malloc(w*h*sizeof(unsigned char));
    
    while((gz_fread(arr, 1, w*h, gz_fptr)==w*h)&&(num<total)) {
      replace_bodies(num, w, h, arr, is_valid, remap);
      num++;
    }
    
    gz_fclose(gz_fptr);
    
    MT_free((void*)arr);
  }

  MT_free( (void *) uvFileName );

  DEBUG_TRACE_OUT printf ( "Leaving overlay_load_bodies_file\n" );
}

/*****************************************************************
 * this is an event handler that installs the colormap
 * on single colormap systems that is called whenever the
 * mouse cursor enters a drawable window - mwf 7-20-95
 * (make_colormap_window_children sets the event handler)
 */
void colormap_install_CB(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
  image_matrix_type * image_matrix_ptr;

  DEBUG_TRACE_IN printf ( "Entering colormap_install_CB\n" );

  image_matrix_ptr = get_image_matrix();

  XInstallColormap(XtDisplay(w), get_color_info()->cmap); 

  DEBUG_TRACE_OUT printf ( "Leaving colormap_install_CB\n" );
}
/****************************************************************/
/*
**  Function: make_colormap_window_children()
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
  static int initial_call = 1;	
  static int maxHWcmaps;
  static Window wi;
  image_matrix_type * image_matrix_ptr;
  
  DEBUG_TRACE_IN printf ( "Entering make_colormap_window_children\n" );

  image_matrix_ptr = get_image_matrix();

  if (get_color_info()->colortype!=PseudoColor){
    DEBUG_TRACE_OUT printf ( "Leaving make_colormap_window_children\n" );
    return;
  }

  /* On the first call to this function, install the sent colormap
   * on systems that have only one hardware colormap.
   */
  if (initial_call) {
    maxHWcmaps = MaxCmapsOfScreen(DefaultScreenOfDisplay(image_matrix_ptr->dpy));
    if (maxHWcmaps==1)
      XInstallColormap(XtDisplay(w), cmap);
    initial_call = 0;
  }
  
  if (XtIsRealized(w)) {
    if (maxHWcmaps==1)
      XtAddEventHandler(w, EnterWindowMask, False,
			(XtEventHandler)colormap_install_CB, NULL);
    else {
      wi = XtWindow(w);
      XSetWMColormapWindows( image_matrix_ptr->dpy, XtWindow(image_matrix_ptr->toplevel), &wi, 1);
      XSetWindowColormap(image_matrix_ptr->dpy, XtWindow(w), cmap);
    }
  }
  if (XtIsComposite(w)) {
    WidgetList theList;
    Cardinal listCount;
    int i;
    
    XtVaGetValues(w, XmNnumChildren, &listCount,
		  XmNchildren, &theList, NULL); 
    for (i = 0; i < listCount; i ++ )
      if (XtIsWidget(theList[i]))
	make_colormap_window_children(theList[i], cmap);
  }
  DEBUG_TRACE_OUT printf ( "Leaving make_colormap_window_children\n" );
}

void show_color_tool(image_matrix_type * image_matrix_ptr) {
  static int first_call = 1;
  static Widget color_tool_shell;
  static colorToolGui_t colorToolGui;
  
  DEBUG_TRACE_IN printf ( "Entering show_color_tool\n" );

  if (first_call) {
    first_call = 0;
    color_tool_shell = make_color_tool_shell(image_matrix_ptr, &colorToolGui);
  }
  XtRealizeWidget(color_tool_shell);

  DEBUG_TRACE_OUT printf ( "Leaving show_color_tool\n" );
}

Widget make_color_tool_shell(image_matrix_type * image_matrix_ptr, colorToolGui_t * colorToolGui) {
    Arg al[10];
    int ac = 0;
    static int reg = 7;
    static int wi=5;    /* width */
    Widget shell, CT_title, CT_sep1, CT_sep2, CT_sep3, CT_done, CT_apply, CT_load,
      CT_reset, CT_form, cbar_window, CT_BACKGROUND, CT_SATURATION,
      ct_background_form, ct_background_textbox,
      ct_saturation_form, ct_saturation_textbox,
      CT_OFFSET, CT_GAMMA, cbar_w,
      ct_offset_form, ct_offset_textbox,
      ct_gamma_form, ct_gamma_textbox;
    XmString xmstr;

    DEBUG_TRACE_IN printf ( "Entering make_color_tool_shell\n" );

    shell = XtAppCreateShell("Color Adjustments", "shell",
			     applicationShellWidgetClass,
			     image_matrix_ptr->dpy, NULL, 0);

    /* Disable the window menu (closes program) */
    XtVaSetValues ( shell, 
		    XmNmwmDecorations, MWM_DECOR_ALL|MWM_DECOR_MENU, 
		    NULL );
/*****/
    ac=0;
    XtSetArg(al[ac], XmNfractionBase, wi*reg); ac++;
    CT_form = XmCreateForm(shell, "CT_form", al, ac);
    XtVaSetValues(CT_form,
		  NULL);
    XtManageChild(CT_form);
    
/*****/
    CT_title = XmCreateLabel(CT_form, "CT_title", NULL, 0);
    xmstr = XmStringCreateLtoR("Set Color Map Properties", image_matrix_ptr->char_set);
    XtVaSetValues(CT_title,
		  XmNlabelString,
		  xmstr,
		  NULL);
    XmStringFree(xmstr);
    XtManageChild(CT_title);
    
/*****/
    CT_sep1 = XmCreateSeparator(CT_form, "CT_sep1", NULL, 0);
    XtManageChild(CT_sep1);
    
/*****/
    CT_done = XmCreatePushButton(CT_form, "CT_done", NULL, 0);
    xmstr = XmStringCreateLtoR("Done", image_matrix_ptr->char_set);
    XtVaSetValues(CT_done,
		  XmNlabelString, xmstr,
		  NULL);
    XtManageChild(CT_done);
    XmStringFree(xmstr);
    XtAddCallback(CT_done,XmNactivateCallback,color_toolCB,(XtPointer) 0);
/*****
    CT_apply = XmCreatePushButton(CT_form, "CT_apply", NULL, 0);
    xmstr = XmStringCreateLtoR("Apply", image_matrix_ptr->char_set);
    XtVaSetValues(CT_apply,
		  XmNlabelString, xmstr,
		  NULL);
    XtManageChild(CT_apply);
    XmStringFree(xmstr);
    XtAddCallback(CT_apply,XmNactivateCallback,cbar_exposed_CB,(XtPointer) 1);
*****/
    CT_load = XmCreatePushButton(CT_form, "CT_load", NULL, 0);
    xmstr = XmStringCreateLtoR("Load\nCmap", image_matrix_ptr->char_set);
    XtVaSetValues(CT_load,
		  XmNlabelString, xmstr,
		  NULL);
    XtManageChild(CT_load);
    XmStringFree(xmstr);
    XtAddCallback(CT_load,XmNactivateCallback,CT_loadCB,NULL);
/*****/
    CT_reset = XmCreatePushButton(CT_form, "CT_reset", NULL, 0);
    xmstr = XmStringCreateLtoR("Reset", image_matrix_ptr->char_set);
    XtVaSetValues(CT_reset,
		  XmNlabelString, xmstr,
		  NULL);
    XtManageChild(CT_reset);
    XmStringFree(xmstr);
    XtAddCallback(CT_reset,XmNactivateCallback,resetColorToolCB, (XtPointer) colorToolGui);
    
/*****/
    ct_background_form = 
      CreateSliderText(&CT_BACKGROUND,
		       &ct_background_textbox,
		       CT_form, "Background",
		       (int)True, 0,
		       -255, 511, get_BNCT_color_info()->background);
    XtManageChild(ct_background_form);
/*****/
    ct_saturation_form = 
      CreateSliderText(&CT_SATURATION,
		       &ct_saturation_textbox,
		       CT_form, "Saturation",
		       (int)True, 0,
		       -255, 511, get_BNCT_color_info()->saturation);
    XtManageChild(ct_saturation_form);
/*****/
    ct_offset_form = 
      CreateSliderText(&CT_OFFSET,
		       &ct_offset_textbox,
		       CT_form, "Rotate Colormap",
		       (int)True, 0,
		       0, 256, get_BNCT_color_info()->offset);
    XtManageChild(ct_offset_form);
/*****/
    CT_sep3 = XmCreateSeparator(CT_form, "CT_sep3", NULL, 0);
    XtManageChild(CT_sep3);
/*****/
    ct_gamma_form = 
      CreateSliderText(&CT_GAMMA,
		       &ct_gamma_textbox,
		       CT_form, "Set Gamma, Load Gamma Colormap",
		       (int)True, 1,
		       5, 30, 20);
    XtManageChild(ct_gamma_form);
/*****/
    ac=0;
    XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
    cbar_window = XmCreateScrolledWindow(CT_form, "cbar_window", al, ac);

    XtVaSetValues(cbar_window, 
		  XmNwidth, 256+12,
		  XmNheight, 34+12,
		  NULL);
    XtManageChild(cbar_window);
/**/
    cbar_w=XtVaCreateManagedWidget("drawingarea",
				   xmDrawingAreaWidgetClass, cbar_window,
				   XmNresizable, False,
				   NULL);
    image_matrix_ptr->cbar_w = cbar_w;
    XtVaSetValues ( cbar_w, 
		    XmNborderColor, RESERVED_BLACK,
		    XmNborderWidth, 2,
		    XmNwidth, 256,
		    XmNheight, 34,
		    NULL );
    XtManageChild(cbar_w);
    XtAddCallback(cbar_w, XmNexposeCallback, cbar_exposed_CB, NULL);
/*****/
    CT_sep2 = XmCreateSeparator(CT_form, "CT_sep2", NULL, 0);
    XtManageChild(CT_sep2);
/*****/
    SetFormPercent(CT_title, wi*0, wi*0+1, wi*reg-1, wi*1-1);
    SetFormPercent(CT_sep1, wi*1-1, wi*0+1, wi*reg-1, wi*1);
    SetFormPercent(CT_sep2, wi*(reg-1), wi*0+1, wi*reg-1, wi*(reg-1)+1);
    SetFormPercent(CT_load, wi*(reg-1)+1, wi*(reg-3), wi*(reg-2)-1, wi*reg-1);
    SetFormPercent(CT_reset, wi*(reg-1)+1, wi*(reg-2), wi*(reg-1)-1, wi*reg-1);
    /*SetFormPercent(CT_apply, wi*(reg-1)+1, wi*(reg-2), wi*(reg-1)-1, wi*reg-1);*/
    SetFormPercent(CT_done, wi*(reg-1)+1, wi*(reg-1), wi*reg-1, wi*reg-1);

    SetFormPercent(ct_background_form, wi*1, wi*0+1, wi*reg-1, wi*2);
    SetFormPercent(ct_saturation_form, wi*2, wi*0+1, wi*reg-1, wi*3);
    SetFormPercent(ct_offset_form, wi*3, wi*0+1, wi*reg-1, wi*4);
    SetFormPercent(cbar_window, wi*4+2, wi*1+1, wi*(reg-1)-1, wi*5);
    SetFormPercent(CT_sep3, wi*5, wi*0+1, wi*reg-1, wi*5+1);
    SetFormPercent(ct_gamma_form, wi*5+1, wi*0+1, wi*reg-1, wi*6);

    XtAddCallback(CT_BACKGROUND,XmNdragCallback,color_tool_adjustCB,(XtPointer) 0);
    XtAddCallback(CT_BACKGROUND,XmNvalueChangedCallback,color_tool_adjustCB,(XtPointer) 0);
    XtAddCallback(CT_SATURATION,XmNdragCallback,color_tool_adjustCB,(XtPointer) 1);
    XtAddCallback(CT_SATURATION,XmNvalueChangedCallback,color_tool_adjustCB,(XtPointer) 1);
    XtAddCallback(CT_OFFSET,XmNdragCallback,color_tool_adjustCB,(XtPointer) 2);
    XtAddCallback(CT_OFFSET,XmNvalueChangedCallback,color_tool_adjustCB,(XtPointer) 2);
    XtAddCallback(CT_GAMMA,XmNdragCallback,color_tool_adjustCB,(XtPointer) 3);
    XtAddCallback(CT_GAMMA,XmNvalueChangedCallback,color_tool_adjustCB,(XtPointer) 3);

    /* Get handles to the important widgets so we can reset them */
    colorToolGui->backgroundSlider = CT_BACKGROUND;
    colorToolGui->saturationSlider = CT_SATURATION;
    colorToolGui->offsetSlider     = CT_OFFSET;
    colorToolGui->gammaSlider      = CT_GAMMA;

    colorToolGui->backgroundText   = ct_background_textbox;
    colorToolGui->saturationText   = ct_saturation_textbox;
    colorToolGui->offsetText       = ct_offset_textbox;
    colorToolGui->gammaText        = ct_gamma_textbox;
    
    DEBUG_TRACE_OUT printf ( "Leaving make_color_tool_shell\n" );
    return(shell);
}

void color_toolCB(Widget w, XtPointer clientData, XtPointer callData) {
  int reason = (int) clientData;

  DEBUG_TRACE_IN printf ( "Entering color_toolCB\n");
  switch(reason)
    {
    case 0:
      debug("done\n");
      XtUnrealizeWidget(XtParent(XtParent(w)));
      break;
    }
  DEBUG_TRACE_OUT printf ( "Leaving color_toolCB\n");
}

void CT_loadCB(Widget w, XtPointer clientData, XtPointer callData)
{
    char filename[256];
    char * ptr;
    int successful;
    image_matrix_type * imp;
    
    DEBUG_TRACE_IN printf("Entering CT_loadCB\n");

    imp = get_image_matrix();

    /* get the name of the colormap file */
    successful = DT_select_file( w, imp->app, filename, "Load Color Map File" );
    if( successful )
    {
        if( FT_fileExists( filename ) )
        {
            /* make sure its a colormap file */
            ptr = strstr( filename, ".cmap" );
            if( ptr != NULL && strlen( ptr ) == 5 )
            {
                if( CT_load_cmap( filename ) )
                {
                    cbar_exposed_CB( NULL, (XtPointer) 1, NULL );
                }
                else
                    DT_error( w, "Error loading the new color map!", NULL, NULL );
            }
            else
                DT_error( w, "That is not a colormap file!", "Invalid File", NULL );
        }
        else
            DT_error( w, "That file does not exist!", "Invalid File", NULL );
    }

    DEBUG_TRACE_OUT printf("Leaving CT_loadCB\n");
 
}

/* load a new colormap from a file */
int CT_load_cmap( char * filename )
{
    FILE * filePtr;
    unsigned char tempCmapValues[3*256];
    static unsigned int tempInt;
    int i;
    color_t * cInfo;
    int returnValue = 1;
    
    DEBUG_TRACE_IN printf("Entering CT_load_cmap\n");

    filePtr = fopen( filename, "r" );

    if( filePtr != NULL )
    {
        i = 0;
        while( i < 256*3 && returnValue == 1 )
        {
            if( fscanf( filePtr, "%u", &tempInt ) != EOF )
            {
                tempCmapValues[i] = (unsigned char) tempInt;
                i++;
            }
            else
                returnValue = 0;
        }
        fclose( filePtr );

        /* if we got all of the values successfully copy them over */
        if( returnValue == 1 )
        {
            cInfo = get_color_info();
            for( i = 0; i < 256*3; i++ )
                cInfo->cmap_vals[i] = tempCmapValues[i];

            colormap_load_rgb(get_image_matrix()->dpy);
        }
    }
    else
        returnValue = 0;  /* couldn't open file */

    DEBUG_TRACE_OUT printf("Leaving CT_load_cmap\n");
    return( returnValue );
}
        
/* Resets the color tool to the default values found in BNCT_color_info */
void resetColorToolCB( Widget w, XtPointer clientData, XtPointer callData )
{
    colorToolGui_t * colorToolGui = (colorToolGui_t *) clientData;
    image_matrix_type * imp;
    BNCT_color_info_type * colorInfo;
    char string[32];
    
    
    DEBUG_TRACE_IN printf("Entering resetColorToolCB\n");

    imp = get_image_matrix();
    colorInfo = get_BNCT_color_info();

    /* First set the default values for the sliders */
    XmScaleSetValue( colorToolGui->backgroundSlider, colorInfo->defaultBackground );
    XmScaleSetValue( colorToolGui->saturationSlider, colorInfo->defaultSaturation );
    XmScaleSetValue( colorToolGui->offsetSlider,     colorInfo->defaultOffset     );
    XmScaleSetValue( colorToolGui->gammaSlider,      colorInfo->defaultGamma      );

    /* Now update the values in the textboxes */
    sprintf( string, "%d", colorInfo->defaultBackground );
    XmTextFieldSetString( colorToolGui->backgroundText, string );
    sprintf( string, "%d", colorInfo->defaultSaturation );
    XmTextFieldSetString( colorToolGui->saturationText, string );
    sprintf( string, "%d", colorInfo->defaultOffset );
    XmTextFieldSetString( colorToolGui->offsetText, string );
    sprintf( string, "%.1f", (float)colorInfo->defaultGamma / 10.0 );
    XmTextFieldSetString( colorToolGui->gammaText, string );

    /* Now reset the "real" values */
    colorInfo->background = colorInfo->defaultBackground;
    colorInfo->saturation = colorInfo->defaultSaturation;
    colorInfo->offset     = colorInfo->defaultOffset;
    colorInfo->gamma      = colorInfo->defaultGamma;

    /* Update the display */
    load_gamma_colormap( imp->dpy, get_color_info()->cmap_vals );
    colormap_load_rgb(imp->dpy);
    cbar_exposed_CB( NULL, (XtPointer) 1, NULL );

    DEBUG_TRACE_OUT printf("Leaving resetColorToolCB\n");
}

void color_tool_adjustCB(Widget w, XtPointer clientData, XtPointer callData) {
  int reason = (int) clientData;
  image_matrix_type * image_matrix_ptr;
  XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) callData;
  
  image_matrix_ptr = get_image_matrix();

  DEBUG_TRACE_IN printf ( "Entering color_tool_adjustCB\n");

  switch(reason)
    {
    case 0:
      debug("background\n");
      XtVaGetValues(w,
		    XmNvalue, &(get_BNCT_color_info()->background),
		    NULL);
      break;
    case 1:
      debug("saturation\n");
      XtVaGetValues(w,
		    XmNvalue, &(get_BNCT_color_info()->saturation),
		    NULL);
      break;
    case 2:
      debug("offset\n");
      XtVaGetValues(w,
		    XmNvalue, &(get_BNCT_color_info()->offset),
		    NULL);
      break;
    case 3:
      debug("gamma\n");
      XtVaGetValues(w,
		    XmNvalue, &(get_BNCT_color_info()->gamma),
		    NULL);
      load_gamma_colormap(image_matrix_ptr->dpy, get_color_info()->cmap_vals);
      break;
    case 4:
      debug("reset\n");
      break;
    }

  /* Only update the display on value changed callback */
  if( cbs->reason == XmCR_VALUE_CHANGED )
  {
      colormap_load_rgb(image_matrix_ptr->dpy);
      cbar_exposed_CB( NULL, (XtPointer) 1, NULL );
  }
  
  DEBUG_TRACE_OUT printf ( "Leaving color_tool_adjustCB\n");
}

void cbar_exposed_CB(Widget unused1, XtPointer clientData, XtPointer unused2) {
  image_matrix_type * image_matrix_ptr;
  int i, j, bytes;
  static int first_call = 1;
  static unsigned char * data;
  static XImage * cbar_image;
  static GC cbar_gc;
  static XGCValues gcv; 
  Widget cbar_w;

  DEBUG_TRACE_IN printf ( "Entering cbar_exposed_CB\n");

  image_matrix_ptr = get_image_matrix();
  cbar_w = image_matrix_ptr->cbar_w;

  debug("clientData = %d\n", (int)clientData);

  if ((int)clientData==1) { /* apply */
    debug("Drawing all images.\n");
    for (i=0; i<image_matrix_ptr->num_pics; i++) {
      clear_and_draw_image(image_matrix_ptr, i);
    }
  }

  switch(get_color_info()->depth)
    {
    case 8:
      bytes = 1;
      break;
    case 16:
      bytes = 2;
      break;
    case 24:
    case 32:
      bytes = 4;
      break;
    default:
      DEBUG_TRACE_OUT printf("Leaving cbar_exposed_CB\n");
      return;
      break;
    }
  

  /* On the first call, create the XImage and GC for the colorbar and
   * make sure its colors get set up correctly
   */
  if (first_call) {
    first_call = 0;
    if (!(data=(unsigned char*)MT_malloc(256*34*bytes)))
      exit(EXIT_FAILURE);
    cbar_image = XCreateImage (image_matrix_ptr->dpy,
			       DefaultVisual(image_matrix_ptr->dpy,
					     image_matrix_ptr->screen),
			       get_color_info()->depth, ZPixmap, 0,
			       (char *)data,
			       256, 34, BitmapPad(image_matrix_ptr->dpy), 256*bytes);
    gcv.function = GXcopy;
    cbar_gc = XCreateGC(image_matrix_ptr->dpy, XtWindow(cbar_w), GCFunction, &gcv);
    image_matrix_ptr->cbar_image = cbar_image;
    
    make_colormap_window_children(cbar_w, get_color_info()->cmap);
  }

  for (j=16; j<18; j++)
    for (i=0; i<256; i++) {
      data[i+j*256]=(unsigned char)0;
    }
  for (j=18; j<34; j++)
    for (i=0; i<256; i++) {
      data[i+j*256]=(unsigned char)i;
    }
  
  update_cbar();
  use_new_color_depth(get_color_info()->depth,
		      (unsigned char *)cbar_image->data, 256*34);
  XPutImage(image_matrix_ptr->dpy, XtWindow(cbar_w), cbar_gc, cbar_image, 0,0,0,0,256,34);
  DEBUG_TRACE_OUT printf ( "Leaving cbar_exposed_CB\n");
}

void update_cbar(void) {
  int i, j;
  static float old_no_colors=256, new_no_colors, expansion_ratio, low_color_index;
  image_matrix_type * image_matrix_ptr;
  XImage * cbar_image;

  DEBUG_TRACE_IN printf ( "Entering update_cbar\n");

  image_matrix_ptr = get_image_matrix();
  cbar_image = image_matrix_ptr->cbar_image;

  new_no_colors = GRAYS_AVAILABLE;
  expansion_ratio = new_no_colors / old_no_colors;
  low_color_index = MIN_GRAY_INDEX;
  
  if (!cbar_image){
    DEBUG_TRACE_OUT printf ( "Leaving update_cbar\n");
    return; /* NULL --> hasn't been created yet */
  }

  for (j=0; j<16; j++) {
    for (i=0; i<256; i++) {
      (cbar_image->data)[i+j*256]=i*expansion_ratio+low_color_index+0.001;
    }
  }

  /* Force an expose callback to redraw this area */
  /* cbarExposedCB(NULL, NULL, NULL); */
  DEBUG_TRACE_OUT printf ( "Leaving update_cbar\n");
}

void set_cursor(MOUSE_CURSOR type) {
    static Cursor curs=(Cursor)NULL;
    static Cursor skull_curs=(Cursor)NULL;
    static Cursor cross_curs=(Cursor)NULL;
    static Cursor crosshair_curs = (Cursor)NULL;
    static Cursor circle_curs[5];
    static Cursor * current_circle_curs_ptr;
    static Pixmap circle_pic[5], circleb_pic[5];
    static unsigned char *b_bits[5], *bg_bits[5];
    static Pixmap b_width[5], b_height[5], bg_width[5], bg_height[5];
    static XColor xc_fg, xc_bg;
    static int first_call=1;
    static int skull_mode = 0, circle_mode = 0;
    static int ignore_off_count = 0;
    int i;
    Widget toplevel;
    image_matrix_type * image_matrix_ptr;

    DEBUG_TRACE_IN printf ( "Entering set_cursor\n");

    image_matrix_ptr = get_image_matrix();

    toplevel = image_matrix_ptr->toplevel;

    if (first_call) {
      first_call = 0;
      b_bits[0] = b1_curs_bits;
      b_width[0] = b1_curs_width;
      b_height[0] = b1_curs_height;
      b_bits[1] = b2_curs_bits;
      b_width[1] = b2_curs_width;
      b_height[1] = b2_curs_height;
      b_bits[2] = b3_curs_bits;
      b_width[2] = b3_curs_width;
      b_height[2] = b3_curs_height;
      b_bits[3] = b4_curs_bits;
      b_width[3] = b4_curs_width;
      b_height[3] = b4_curs_height;
      b_bits[4] = b5_curs_bits;
      b_width[4] = b5_curs_width;
      b_height[4] = b5_curs_height;
      bg_bits[0] = b1_cursb_bits;
      bg_width[0] = b1_cursb_width;
      bg_height[0] = b1_cursb_height;
      bg_bits[1] = b2_cursb_bits;
      bg_width[1] = b2_cursb_width;
      bg_height[1] = b2_cursb_height;
      bg_bits[2] = b3_cursb_bits;
      bg_width[2] = b3_cursb_width;
      bg_height[2] = b3_cursb_height;
      bg_bits[3] = b4_cursb_bits;
      bg_width[3] = b4_cursb_width;
      bg_height[3] = b4_cursb_height;
      bg_bits[4] = b5_cursb_bits;
      bg_width[4] = b5_cursb_width;
      bg_height[4] = b5_cursb_height;
      xc_fg.pixel = RESERVED_BLACK;
      myXQueryColor(image_matrix_ptr->dpy, get_color_info()->cmap, &xc_fg);
      xc_bg.pixel = RESERVED_WHITE;
      myXQueryColor(image_matrix_ptr->dpy, get_color_info()->cmap, &xc_bg);
      for (i=0; i<5; i++) {
	circle_pic[i] = XCreatePixmapFromBitmapData(XtDisplay(toplevel),
						    RootWindowOfScreen(XtScreen(toplevel)),
						    (char *)b_bits[i],
						    b_width[i],
						    b_height[i],
						    1,
						    0,
						    1);
	circleb_pic[i] = XCreatePixmapFromBitmapData(XtDisplay(toplevel),
						    RootWindowOfScreen(XtScreen(toplevel)),
						    (char *)bg_bits[i],
						    bg_width[i],
						    bg_height[i],
						    1,
						    0,
						    1);
	circle_curs[i]=XCreatePixmapCursor(image_matrix_ptr->dpy, circle_pic[i], circleb_pic[i], &xc_fg, &xc_bg, b_width[i]/2, b_height[i]/2);
      }
    }

    switch(type)
      {
      case NORMAL_CURSOR:
	/* debug("NORMAL CURSOR.\n"); */

	if (ignore_off_count) {
	  ignore_off_count--;
	  DEBUG_TRACE_OUT printf ( "Leaving set_cursor\n");
	  return;
	}

	XUndefineCursor(image_matrix_ptr->dpy, XtWindow(toplevel));
	
	/* When we turn this off, we must consider what we are returning
	 * to normal from.  Things that can be on from highest to lowest
	 * precedence are:
	 *  (1) watch
	 *  (2) skull
	 *  (3) circle cursor
	 * So, we look to see which of these (if any) of highest precedence
	 * was on and then turn on the next thing of next highest
	 * precedence.
	 */
	if (see_watch) {
	  see_watch = 0;
	  /* This is enough to revert to previous mode */
	} else if (skull_mode) {
	  /* Need to turn _back_ on circle mode _if_ appropriate */
	  skull_mode = 0;
	  if (circle_mode) {
	    global_image_cursor_ptr = current_circle_curs_ptr;
	  } else {
	    global_image_cursor_ptr = (Cursor *)NULL;
	  }
	} else if (circle_mode) {
	  circle_mode = 0;
	  global_image_cursor_ptr = (Cursor *)NULL;
	}
	break;
      case WATCH_CURSOR:
	if (see_watch) {
	  ignore_off_count++;
	}
	see_watch = 1;
	if (!curs)
	  curs=XCreateFontCursor(image_matrix_ptr->dpy, XC_watch);
	XDefineCursor(image_matrix_ptr->dpy, XtWindow(toplevel), curs);
	break;
      case CROSS_CURSOR:
	see_watch = 0;
	if(!cross_curs)
	  cross_curs = XCreateFontCursor(image_matrix_ptr->dpy,XC_cross);
	global_image_cursor_ptr = &cross_curs;
	break;
      case CROSSHAIR_CURSOR:
	see_watch = 0;
	if(!crosshair_curs)
	  crosshair_curs = XCreateFontCursor(image_matrix_ptr->dpy,XC_crosshair);
	global_image_cursor_ptr = &crosshair_curs;
	break;
      case DEATH_CURSOR:
	see_watch = 0;
	/* debug("SKULL CURSOR.\n"); */
	if (!skull_curs)
	  skull_curs=XCreateFontCursor(image_matrix_ptr->dpy, XC_pirate);
	global_image_cursor_ptr = &skull_curs;
	skull_mode = 1;
	break;
      case BRUSHES_BEGIN_CURSOR:
      case BRUSHES_BEGIN_CURSOR+1:
      case BRUSHES_BEGIN_CURSOR+2:
      case BRUSHES_BEGIN_CURSOR+3:
      case BRUSHES_BEGIN_CURSOR+4:
	see_watch = 0;
	/* debug("CIRCLE CURSOR.\n"); */
	global_image_cursor_ptr = &(circle_curs[type-3]);
	current_circle_curs_ptr = &(circle_curs[type-3]);
	circle_mode = 1;
	break;
      }
    wait_on_xserver();

    DEBUG_TRACE_OUT printf ( "Leaving set_cursor\n");
}

void wait_on_xserver(void) {
  XFlush(get_image_matrix()->dpy);
}

void wait_on_xserver2(void) {
  XtInputMask m;
  XtAppContext app;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  app = image_matrix_ptr->app;

  while ((m=XtAppPending(app)))
    XtAppProcessEvent(app, m);
}

/*void * warn_malloc(size_t size) {
  void * retval;

  DEBUG_TRACE_IN printf ( "Entering warn_malloc\n");

  if (size==0) {
    debug("Warning.  Trying to malloc 0.\n");
  } else if (size<0) {
    debug("Warning.  Trying to malloc negative amount %d.\n", size);
  } else {
    retval = (void *) MT_malloc(size);
    if (!retval) {
      debug("Malloc of %d failed.  Exiting", size);
      exit(EXIT_FAILURE);
    }
  }
  DEBUG_TRACE_OUT printf ( "Leaving warn_malloc\n");
  return(retval);
  }*/

/* This routine is an interface that takes information in this program,
 * transfers it to the geom_info structure, and then calls the libuv
 * routine to store the uvh header.
 */
void write_uvh_interface(image_matrix_type * image_matrix_ptr, char * base, int * values_in_uv_file) {
  geom_info_t geom_info;
  geom_info_t * geom_ptr;
  char outname[256];

  DEBUG_TRACE_IN printf ("Entering write_uvh_interface\n");

  if (image_matrix_ptr->num_pics<=0) {
    DT_warn( image_matrix_ptr->toplevel, "Nothing to write.",
	     NULL, NULL );
    DEBUG_TRACE_OUT printf("Leaving write_uvh_interface\n");
    return;
  }

  strcpy(outname, base);
  strcat(outname, ".uvh");

  if (!ok_to_write_file(outname)) {
    DT_warn( image_matrix_ptr->toplevel, "File cannot be created for write.",
	     NULL, NULL );
    DEBUG_TRACE_OUT printf ( "Leaving write_uvh_interface\n");
    return;
  }

  geom_ptr = &geom_info;

  initialize_geom_info(geom_ptr);

  transfer_image_matrix_info_to_geom_info(image_matrix_ptr, geom_ptr, values_in_uv_file);
  
  fill_computed_uvh_values(image_matrix_ptr, geom_ptr);

  write_uvh(geom_ptr, outname);

  DEBUG_TRACE_OUT printf ( "Leaving write_uvh_interface\n");
}

/* This routine does the transferring of the information in this
 * program into the geom_info structure for libuv.
 */
void transfer_image_matrix_info_to_geom_info(image_matrix_type * ip, geom_info_t * gp, int * values_in_uv_file) {
    int i, uvval;
    int body_count = 0;
    geom_info_valid_t * v;
    float x_size, y_size, z_size, z_incr=1, x_min, y_min, z_min, x_max, y_max, z_max;
    char *body_name, output_body_name[256],
        lname1[256], lname2[256], *tmp;
    dose_info_t * mptr, * toptr;
    marker_type * marker;
    int r,g,b;

    DEBUG_TRACE_IN printf ( "Entering transfer_image_matrix_info_to_geom_info\n");

    v = &(gp->valid);

    x_size = ip->img_arr[0].pixel_size_x;
    y_size = ip->img_arr[0].pixel_size_y;
    if (ip->num_pics-1>0) {
        /* ASSUME uniform spacing -- take difference between high z_val
         * - low_zval and divide by (ip->num_pics-1)
         */
        z_incr = (ip->img_arr[ip->num_pics-1].z_val-ip->img_arr[0].z_val)/((float)(ip->num_pics-1));
        z_size = z_incr;

        if (z_incr<0.0)
            z_size = -z_size;
    
    } else /* only have one slice -- how thick is it? */ {
        /* GUESS */
        z_size = x_size*5.0;
        DT_inform( ip->toplevel,"In saving only 1 slice, couldn't\ndetermine z-pixel size so guessed.",
                   NULL, NULL );
    }

    x_min = ip->img_arr[0].startx;
    y_min = ip->img_arr[0].starty;
    x_max = x_min + ip->input_width*x_size;
    y_max = y_min + ip->input_height*y_size;
    if (z_incr>0.0) {
        z_min = ip->img_arr[0].z_val-0.5*z_size;
        z_max = z_min + ip->num_pics*z_size;
    } else {
        z_min = ip->img_arr[ip->num_pics-1].z_val-0.5*z_size;
        z_max = z_min + ip->num_pics*z_size;    
    }
    /****************************************************************/

    /* currently, using our constraint and fiducial markers list */
    gp->num_fiducial_markers = ip->num_fiducial_markers;
    gp->num_constraint_markers = ip->num_constraint_markers;
    gp->fiducial_markers = ip->fiducial_markers;
    gp->constraint_markers = ip->constraint_markers;
  
    v->num_fiducial_markers = 1;
    v->num_constraint_markers = 1;
    v->fiducial_markers = 1;
    v->constraint_markers = 1;

    strcpy(gp->imagerowaxis, ip->orient_gui.returnValues[ROW]);
    v->imagerowaxis=1;

    strcpy(gp->imagecolumnaxis, ip->orient_gui.returnValues[COLUMN]); 
    v->imagecolumnaxis=1;

    strcpy(gp->imagesliceaxis, ip->orient_gui.returnValues[SLICE]);
    v->imagesliceaxis=1;

    strcpy(gp->bodydatatitle, ip->body_data_title);
    v->bodydatatitle=1;
  
    strcpy(gp->dimensionality, "cm");
    v->dimensionality=1;

    strcpy(gp->sliceorientation, ip->orient_gui.sliceOrientation);
    v->sliceorientation=1;

    gp->imageslices = ip->num_pics;
    v->imageslices=1;

    gp->imagecolumns = ip->input_width;
    v->imagecolumns=1;

    gp->imagerows = ip->input_height;
    v->imagerows=1;

    gp->pixelsizeslices = z_size;
    v->pixelsizeslices=1;

    gp->pixelsizecolumns=x_size;
    v->pixelsizecolumns=1;

    gp->pixelsizerows=y_size;
    v->pixelsizerows=1;
  
    /*
     * Determine bounding box for entire model
     * based on the slice orientation. Below is
     * the coordinate system that is assumed.
     * Also, Superior is more positive than
     * Inferior, Anterior is more positive than
     * Posterior, and Left is more positive than
     * Right. Therefore, based on the different
     * types of orientations for each direction
     * (like PA+, PA-) the min and max for
     * each of the axis can be different.
     *
     *    ------------------------------
     *    |(x_min,y_max)  (x_max,y_max)|
     *    |                            |
     *    |                            |
     *    |                            |
     *    |                            |
     *    |             X              |
     *    |           (0,0)            |
     *    |                            |
     *    |                            |
     *    |                            |
     *    |                            |
     *    |(x_min,y_min)  (x_max,y_min)|
     *    ------------------------------
     */
    if( ip->orient_gui.sliceOrientationType == TRANSVERSE ||
        ip->orient_gui.sliceOrientationType == AXIAL )
    {
        /*
         * We know that the slice axis is IS, and the
         * correct value of z_min and z_max have been
         * computed above. However, the images could
         * be rotated, so we must do some figuring
         * based on the PA and RL types of orientations.
         */
        switch( ip->orient_gui.elements[ROW].which_orientation )
        {
            /* Images with nose of patient at top or bottom of image */
            case PA_MINUS:
            case PA_PLUS:
                gp->paaxismin = y_min;
                gp->paaxismax = y_max;
                break;

            /* Images with nose of patient at left or right of image */
            case RL_PLUS:
            case RL_MINUS:
                gp->paaxismin = x_min;
                gp->paaxismax = x_max;
                break;
        }

        switch( ip->orient_gui.elements[COLUMN].which_orientation )
        {
            /* Images with nose of patient at top or bottom of image */
            case RL_PLUS:
            case RL_MINUS:
                gp->rlaxismin = x_min;
                gp->rlaxismax = x_max;
                break;

            /* Images with nose of patient at left or right of image */
            case PA_PLUS:
            case PA_MINUS:
                gp->rlaxismin = y_min;
                gp->rlaxismax = y_max;
                break;
        }

        gp->isaxismin = z_min;
        gp->isaxismax = z_max;
    }
    else if( ip->orient_gui.sliceOrientationType == CORONAL )
    {
        /*
         * We know that the slice axis is PA, but the
         * row and column axis can be different. So, like
         * above, we must figure things out based on the
         * IS and RL axis.
         */
        switch( ip->orient_gui.elements[ROW].which_orientation )
        {
            /* Top of head at top or bottom of image */ 
            case IS_PLUS:
            case IS_MINUS:
                gp->isaxismin = y_min;
                gp->isaxismax = y_max;
                break;

            /* Top of head at left or right of image */
            case RL_PLUS:
            case RL_MINUS:
                gp->isaxismin = x_min;
                gp->isaxismax = x_max;
                break;
        }

        switch( ip->orient_gui.elements[COLUMN].which_orientation )
        {
            /* Top of head at top or bottom of image */
            case RL_PLUS:
            case RL_MINUS:
                gp->rlaxismin = x_min;
                gp->rlaxismax = x_max;
                break;

            /* Top of head at left or right of image */
            case IS_PLUS:
            case IS_MINUS:
                gp->rlaxismin = y_min;
                gp->rlaxismax = y_max;
                break;
        }

        gp->paaxismin = z_min;
        gp->paaxismax = z_max;
    }
    else if( ip->orient_gui.sliceOrientationType == SAGITTAL )
    {
        /*
         * Slice axis is RL, but the row and column axis
         * can be different. So like above we must use
         * the PA and IS to figure things out
         */
        switch( ip->orient_gui.elements[ROW].which_orientation )
        {
            /* Top of head at top or bottom of image */
            case IS_PLUS:
            case IS_MINUS:
                gp->isaxismin = y_min;
                gp->isaxismax = y_max;
                break;

            /* Top of head at left or right of image */
            case PA_PLUS:
            case PA_MINUS:
                gp->isaxismin = x_min;
                gp->isaxismax = x_max;
                break;
        }

        switch( ip->orient_gui.elements[COLUMN].which_orientation )
        {
            /* Top of head at top or bottom of image */
            case PA_PLUS:
            case PA_MINUS:
                gp->paaxismin = x_min;
                gp->paaxismax = x_max;
                break;

            /* Top of head at left or right of image */
            case IS_PLUS:
            case IS_MINUS:
                gp->paaxismin = y_min;
                gp->paaxismax = y_max;
                break;
        }

        gp->rlaxismin = z_min;
        gp->rlaxismax = z_max;
    }
  
    /* Mark all axes as valid */
    v->paaxismin=1;
    v->paaxismax=1;
    v->rlaxismin=1;
    v->rlaxismax=1;  
    v->isaxismin=1;
    v->isaxismax=1;

    /* v->inv_pixelsizeslices=0; */
    /* v->inv_pixelsizecolumns=0; */
    /* v->inv_pixelsizerows=0; */

    i=0;
    uvval=DEFAULT_MIN_BODY_COLOR_INDEX;

    while (i<MAXNUMBODIES) {
        if ((strlen(body_name=get_body_name(i))>0)
            ||(values_in_uv_file[uvval])) {
            get_region_color(i,&r,&g,&b);
            body_count++;
            if (strlen(body_name)<=0) {
                /* Need to make a 'fake' name since they forgot
                 * to label their body.  Naughty user.
                 * --> Note: we know this body isn't absent
                 *     because 'values_in_uv_file' indicates
                 *     its presence.
                 */
                sprintf(output_body_name, "genericbody%d", body_count);
            } else {
                /* If the body is present, just copy the name but
                 * if it's not present, then add 'ABSENT' to the
                 * name of the body so user knows it's missing
                 * 03-12-98 --> but, don't add ABSENT again
                 * if it was already there from before -- note:
                 * looks to see if 'absent' is present currently
                 * as the program converts to lowercase when it
                 * loads.
                 */
                if ((values_in_uv_file[uvval])||(strstr(body_name, "absent"))) {
                    strcpy(output_body_name, body_name);
                } else {
                    /* sprintf(output_body_name, "ABSENT%s", body_name); */
                    /* Marking as ABSENT determined to not be helpful */
                    sprintf(output_body_name, "%s", body_name);
                }
                /* Note:  Bodies that are not present AND not
                 *        named are simply ignored
                 */
            }

            /* Do something similar for the constraint marker information */
            mptr = ip->dose_info_list;
            /* Will stop when run out of list _or_ break out if find
             * the right member
             */
            strcpy(lname1, body_name);
            KV_make_lower_string(lname1);
            KV_trim_string(lname1);
            while (mptr) {
                strcpy(lname2, mptr->bodyname);
                KV_make_lower_string(lname2);
                KV_trim_string(lname2);
                if (!strcmp(lname1, lname2)) {
                    copy_dose_info(&(gp->dose[uvval]), mptr);
                    gp->dose[uvval].next = NULL;
                    break;
                }
                mptr = mptr->next;
            }

            gp->regionnum[uvval]=body_count;
            v->regionnum[uvval]=1;

            strcpy(gp->bodyname[uvval], output_body_name);
            v->bodyname[uvval]=1;

            gp->uvval[uvval]=uvval;
            v->uvval[uvval]=1;

            gp->color_red[uvval] = r;
            gp->valid.color_red[uvval] = 1;
            gp->color_green[uvval] = g;
            gp->valid.color_green[uvval] = 1;
            gp->color_blue[uvval] = b;
            gp->valid.color_blue[uvval] = 1;
      

        }
        uvval++;
        i++;
    }

    /* Now, look through constraint markers and use info from body_data.txt
     * to fill values in the dose info -- outer loop is to do for each
     * constraint marker
     */
    for (i=0; i<get_image_matrix()->num_constraint_markers; i++) {
        marker = &(get_image_matrix()->constraint_markers[i]);
        toptr = &(marker->dose);
        if (marker->is_used) {
            mptr = ip->dose_info_list;
            /* Will stop when run out of list _or_ break out if find
             * the right member
             */
            strcpy(lname1, marker->bodyname);
            KV_make_lower_string(lname1);
            KV_trim_string(lname1);
            while (mptr) {
                strcpy(lname2, mptr->bodyname);
                KV_make_lower_string(lname2);
                KV_trim_string(lname2);
                if (!strcmp(lname1, lname2)) {
                    copy_dose_info(toptr, mptr);
                    toptr->next = NULL;
                    break;
                }
                mptr = mptr->next;
            }
        }
    }
    DEBUG_TRACE_OUT printf ( "Leaving transfer_image_matrix_info_to_geom_info\n");
}

Widget make_save_preferences_popup(Widget parent) {
  Arg al[10];
  int ac = 0, i;
  XmString xmstr;
  image_matrix_type * image_matrix_ptr;
  Widget shell, form1, frame, form2, form3, form_top, form_bot, title, sep, sep2, save, cancel;
  int numselections = 12;
  static Widget savelist[12];
  char *widget_text[] = {
    "Properties Viewable",
    "Edit Regions Viewable",
    "Program Window Width",
    "Program Window Height",
    "Maximum Columns",
    "Preview Windows Width",
    "Preview Windows Height",
    /*"Drawing Speed",*/
    "Number of Regions",
    "Active Region",
    "Current Region Names",
    "Current Region Colors",
    "Automatic Save Values"
  };

  DEBUG_TRACE_IN printf ( "Entering make_save_preferences_popup\n");

  image_matrix_ptr = get_image_matrix();
  shell = XtAppCreateShell("Save Preferences", "shell",
			   applicationShellWidgetClass,
			   image_matrix_ptr->dpy, NULL, 0);

  /* Disable the window menu (closes program) */
  XtVaSetValues ( shell, 
		  XmNmwmDecorations, 
		  MWM_DECOR_ALL|MWM_DECOR_MENU,
		  NULL );

  form1 = XmCreateForm(shell, "form1", NULL, 0);
  XtManageChild(form1);

  frame = XmCreateFrame(form1, "frame", NULL, 0);
  XtVaSetValues(frame,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNbottomOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		NULL);
  XtManageChild(frame);

  form2 = XmCreateForm(frame, "form2", NULL, 0);
  XtManageChild(form2);
  
  form3 = XmCreateForm(form2, "form3", NULL, 0);
  XtVaSetValues(form3,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNbottomOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		NULL);
  XtManageChild(form3);

  form_bot = XmCreateForm(form3, "form_bot", NULL, 0);
  XtVaSetValues(form_bot,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(form_bot);

  form_top = XmCreateForm(form3, "form_top", NULL, 0);
  XtVaSetValues(form_top,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_WIDGET,
		XmNbottomWidget, form_bot,
		XmNbottomOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(form_top);
  
  sep2 = XmCreateSeparator(form_bot, "sep2", NULL, 0);
  XtVaSetValues(sep2,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(sep2);

  save = XmCreatePushButton(form_bot, "save", NULL, 0);
  xmstr = XmStringCreateLtoR("Save", image_matrix_ptr->char_set);
  XtVaSetValues(save,
		XmNlabelString, xmstr,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, sep2,
		XmNtopOffset, 5,
		NULL);
  XtManageChild(save);
  XmStringFree(xmstr);
  XtAddCallback(save,XmNactivateCallback,save_preferences_save_CB,(XtPointer) shell);

  cancel = XmCreatePushButton(form_bot, "cancel", NULL, 0);
  xmstr = XmStringCreateLtoR("Cancel", image_matrix_ptr->char_set);
  XtVaSetValues(cancel,
		XmNlabelString, xmstr,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, save,
		XmNrightOffset, 10,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, sep2,
		XmNtopOffset, 5,
		NULL);
  XtManageChild(cancel);
  XmStringFree(xmstr);
  XtAddCallback(cancel,XmNactivateCallback,save_preferences_cancel_CB,(XtPointer) shell);

  title = XmCreateLabel(form_top, "title", NULL, 0);
  xmstr = XmStringCreateLtoR("Please check preferences to save.\nCurrent values of all checked items\nwill become new defaults.  Defaults for\nunchecked items will remain unchanged.\n(Defaults are stored in the itrc file.)",
			     image_matrix_ptr->char_set);
  XtVaSetValues(title,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(title);

  sep = XmCreateSeparator(form_top, "sep", NULL, 0);
  XtVaSetValues(sep,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, title,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		NULL);
  XtManageChild(sep);

  for ( i = 0; i < numselections; i++ )
  {
    savelist[i] = XmCreateToggleButton(form_top, "savelist", NULL, 0);
    xmstr = XmStringCreateLtoR(widget_text[i], image_matrix_ptr->char_set);
    preferences_to_save[i] = True;
    XtVaSetValues(savelist[i],
		  XmNset, preferences_to_save[i],
		  XmNlabelString, xmstr,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNtopOffset, 5,
		  NULL);
    XtAddCallback(savelist[i],XmNvalueChangedCallback,toggle_save_preference_CB,(XtPointer)i);
    XmStringFree(xmstr);
    if (i==0) {
      XtVaSetValues(savelist[i],
		    XmNtopAttachment, XmATTACH_WIDGET,
		    XmNtopWidget, sep,
		    NULL);
    } else if (i==numselections-1) {
      XtVaSetValues(savelist[i],
		    XmNtopAttachment, XmATTACH_WIDGET,
		    XmNtopWidget, savelist[i-1],
		    XmNbottomAttachment, XmATTACH_FORM,
		    XmNbottomOffset, 5,
		    NULL);
    } else {
      XtVaSetValues(savelist[i],
		    XmNtopAttachment, XmATTACH_WIDGET,
		    XmNtopWidget, savelist[i-1],
		    NULL);
    }
    XtManageChild(savelist[i]);
  }

  DEBUG_TRACE_OUT printf ( "Leaving make_save_preferences_popup\n");
  return(shell);
}

void save_preferences_popup_FCN(image_matrix_type * image_matrix_ptr) {
  static int first_call = 1;
  static Widget shell;

  DEBUG_TRACE_IN printf ( "Entering save_preferences_popup_FCN\n");

  if (first_call) {
    shell = make_save_preferences_popup(image_matrix_ptr->toplevel);
    first_call = 0;
  }

  XtRealizeWidget(shell);
  DEBUG_TRACE_OUT printf ( "Leaving save_preferences_popup_FCN\n");
}

void save_preferences_cancel_CB(Widget wid, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering save_preferences_cancel_CB\n");
  XtUnrealizeWidget((Widget)clientData);
  DEBUG_TRACE_OUT printf ( "Leaving save_preferences_cancel_CB\n");
}

void save_preferences_save_CB(Widget wid, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering save_preferences_save_CB\n");
  XtUnrealizeWidget((Widget)clientData);
  save_chosen_preferences();
  DEBUG_TRACE_OUT printf ( "Leaving save_preferences_cancel_CB\n");
}

void toggle_save_preference_CB(Widget wid, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering toggle_save_preferences_CB\n");
  XtVaGetValues(wid,
		XmNset, &(preferences_to_save[(int)clientData]),
		NULL);
  DEBUG_TRACE_OUT printf ( "Leaving toggle_save_preferences_CB\n");
}

void save_chosen_preferences(void) {
    int i, j, val, red, green, blue;
    Dimension Dimensionval;
    FILE *fptr;
    char *fname;
    program_defaults_type * pdt_ptr;
    image_matrix_type * image_matrix_ptr;

    DEBUG_TRACE_IN printf ( "Entering save_chosen_preferences\n");

    image_matrix_ptr = get_image_matrix();

    fname = get_resource_name();
  
    pdt_ptr = get_program_defaults();

    for (i=0; i<12; i++) {
        if (preferences_to_save[i]==True) {

            /*printf ( "Should be making %d a default.\n", i);*/

            debug("Should be making %d a default.\n", i);
            switch(i)
            {
                case 0:  /* Properties Viewable */
                    if (XtIsManaged(image_matrix_ptr->properties_frame))
                        val = 1;
                    else
                        val = 0;
                    pdt_ptr->PropertiesViewable=val;
                    break;
                case 1:  /* Edit Regions Viewable */
                    if (XtIsManaged(image_matrix_ptr->edit_regions_frame))
                        val = 1;
                    else
                        val = 0;
                    pdt_ptr->EditRegionsViewable=val;
                    break;
                case 2:  /* Program Window Width */
                    XtVaGetValues(image_matrix_ptr->toplevel,
                                  XmNwidth, &Dimensionval,
                                  NULL);
                    pdt_ptr->ProgramWindowWidth=Dimensionval;
                    break;
                case 3:  /* Program Window Height */
                    XtVaGetValues(image_matrix_ptr->toplevel,
                                  XmNheight, &Dimensionval,
                                  NULL);
                    pdt_ptr->ProgramWindowHeight=Dimensionval;
                    break;
                case 4:  /* Maximum Columns */
                    XtVaGetValues(image_matrix_ptr->num_cols_w,
                                  XmNvalue, &val,
                                  NULL);
                    pdt_ptr->MaximumColumns=val;
                    break;
                case 5:  /* Preview Windows Width */
                    val = image_matrix_ptr->window_width;
                    pdt_ptr->PreviewWindowsWidth=val;
                    break;
                case 6:  /* Preview Windows Height */
                    val = image_matrix_ptr->window_height;
                    pdt_ptr->PreviewWindowsHeight=val;
                    break;
                    /*case 7:  Drawing Speed 
                      val = get_drawing_speed();
                      pdt_ptr->DrawingSpeed=val;
                      break;
                    */
                case 7:  /* Number Regions */
                    val = get_number_regions();
                    pdt_ptr->NumberRegions=val;
                    break;
                case 8:  /* Active Region */
                    val = get_active_region()+1;
                    pdt_ptr->ActiveRegion=val;
                    break;
                case 9: /* Region Name */
                    for (j=0; j<MAXNUMBODIES; j++) {
                        strcpy(pdt_ptr->RegionName[j],
                               get_body_name(j));
                    }
                    break;
                case 10: /* Color */
                    for (j=0; j<MAXNUMBODIES; j++) {
                        get_region_color(j, &red, &green, &blue);
                        pdt_ptr->Color[j][0] = red;
                        pdt_ptr->Color[j][1] = green;
                        pdt_ptr->Color[j][2] = blue;
                    }
                    break;
                case 11:
                    /* Autosave case... shouldn't need to do anything! */
                    break;
                default:
                    /* Do nothing */
                    break;       
            }
        }
    }

    fptr = fopen(fname, "w");
  
    if (!fptr) {
        debug("Couldn't open itrc resource file to write.\n");
        DEBUG_TRACE_OUT printf ( "Leaving save_chosen_preferences\n");
        return;
    }
 
    fprintf(fptr, "Properties Viewable:%d\n", pdt_ptr->PropertiesViewable);
    fprintf(fptr, "Edit Regions Viewable:%d\n", pdt_ptr->EditRegionsViewable);
    fprintf(fptr, "Program Window Width:%d\n", pdt_ptr->ProgramWindowWidth);
    fprintf(fptr, "Program Window Height:%d\n", pdt_ptr->ProgramWindowHeight);
    fprintf(fptr, "Maximum Columns:%d\n", pdt_ptr->MaximumColumns);
    fprintf(fptr, "Preview Windows Width:%d\n", pdt_ptr->PreviewWindowsWidth);
    fprintf(fptr, "Preview Windows Height:%d\n", pdt_ptr->PreviewWindowsHeight);
    fprintf(fptr, "Drawing Speed:%d\n", pdt_ptr->DrawingSpeed);
    fprintf(fptr, "Number Regions:%d\n", pdt_ptr->NumberRegions);
    fprintf(fptr, "Active Region:%d\n", pdt_ptr->ActiveRegion);
    fprintf ( fptr, "Autosave On: %d\n", pdt_ptr->AutoSaveOn );
    fprintf ( fptr, "Autosave Delay: %d\n", pdt_ptr->AutoSaveDelay );
    fprintf ( fptr, "Autosave Directory: %s\n", pdt_ptr->AutoSaveDir );
    fprintf ( fptr, "Autosave Default Filename: %s\n", pdt_ptr->AutoSaveFilename );
    
    /*printf ( "about to write region names\n" );*/

    for (i=0; i<MAXNUMBODIES; i++) {
        fprintf(fptr, "Region Name %d:%s\n", i+1, pdt_ptr->RegionName[i]);
    }

    /*printf ( "wrote region names\n" );*/

    for (i=0; i<MAXNUMBODIES; i++) {
        fprintf(fptr, "Color %d:%3d %3d %3d\n",
                i+1,
                pdt_ptr->Color[i][0],
                pdt_ptr->Color[i][1],
                pdt_ptr->Color[i][2]);
    }

    append_range_values_to_file( fptr );

    fclose(fptr);

    DEBUG_TRACE_OUT printf ( "Leaving save_chosen_preferences\n");
}

void replace_bodies(int num, int w, int h, unsigned char * arr, int * is_valid, unsigned char * remap) {
  image_matrix_type * image_matrix_ptr;
  unsigned char *copied_data, val;
  static unsigned char current = DEFAULT_MIN_BODY_COLOR_INDEX;
  int j;

  image_matrix_ptr = get_image_matrix();

  DEBUG_TRACE_IN printf ( "Entering replace_bodies\n");

  debug("Overlaying body...\n");
  copied_data = image_matrix_ptr->img_arr[num].region_data;
  memcpy(copied_data, arr, w*h*sizeof(unsigned char));

  /* Alter any region data outside of range */
  for (j=0; j<w*h; j++) {
    val = copied_data[j];
    if (!is_valid[val]) {
      is_valid[val] = 1;

      if (val == 255){ /** added by CLA  9-30-98, if 255 is found, it was an unlabeled pixel **/
	remap[val] = 0;
      }else if ((val<DEFAULT_MIN_BODY_COLOR_INDEX)||
		(val>=DEFAULT_MIN_BODY_COLOR_INDEX+MAXNUMBODIES)) {
	remap[val] = current;
      } else {
	remap[val] = val;
      }
      current++;
      if (current>=DEFAULT_MIN_BODY_COLOR_INDEX+MAXNUMBODIES) {
	current = DEFAULT_MIN_BODY_COLOR_INDEX;
      }
      debug("remap[%d] set to %d.\n", val, remap[val]);
    }
    copied_data[j] = remap[val];
  }
  /******************************************/

  trace_edge_unspecified(num, BORDER_MASK);
  draw_image(image_matrix_ptr, num);

  DEBUG_TRACE_OUT printf ( "Leaving replace_bodies\n");
}


int is_uv(char * name)
{
    int returnValue = 0;
    DEBUG_TRACE_IN printf("Entering is_uv, name = %s\n", name);

    if( FT_filenameEndsIn(name, ".uvh"  ) ||
        FT_filenameEndsIn(name, ".uv.gz") ||
        FT_filenameEndsIn(name, ".uv"   ) )
    {
        returnValue = 1;
    }
    
    DEBUG_TRACE_OUT printf("Leaving is_uv\n");
    return( returnValue );
}



/* Given the name of a uv/uvh file, returns the name of the file
 * corresponding to the .uv body file
 */
char * get_uv_name(char * filename) {
  char tmp[512], *uvhpart, *retval;

  DEBUG_TRACE_IN printf ( "Entering get_uv_name\n");

  strcpy(tmp, filename);
  if ((uvhpart = strstr(tmp, ".uvh"))) {
    uvhpart[0] = '\0';
  } else if (strstr(tmp, ".uv")) {
    /* already the correct file */
    retval = (char *)MT_malloc((strlen(tmp)+1)*sizeof(char));
    strcpy(retval, tmp);
    DEBUG_TRACE_OUT printf ( "Leaving get_uv_name\n");
    return(retval);
  } /* else neither case -- probably just a bare filename */
  strcat(tmp, ".uv.gz");
  retval = (char *)MT_malloc((strlen(tmp)+1)*sizeof(char));
  strcpy(retval, tmp);

  if (FT_fileExists(retval)){
    DEBUG_TRACE_OUT printf ( "Leaving get_uv_name\n");
    return(retval);                    /* return file with .uv.gz ending */
  }
  else {
    retval[strlen(retval)-3]='\0';
    DEBUG_TRACE_OUT printf ( "Leaving get_uv_name\n");
    return(retval);                    /* return file with .uv ending */
  }
}


/* Given the name of a uv/uvh file, returns the name of the file
 * corresponding to the .uvh header file
 */
char * get_uvh_name(char * filename) {
  char tmp[512], *uvpart, *retval;

  DEBUG_TRACE_IN printf ( "Entering get_uvh_name\n");

  strcpy(tmp, filename);
  if (strstr(tmp, ".uvh")) {
    /* already the correct name */
    retval = (char *)MT_malloc((strlen(tmp)+1)*sizeof(char));
    strcpy(retval, tmp);
    DEBUG_TRACE_OUT printf ( "Leaving get_uvh_name\n");
    return(retval);
  } else if ((uvpart = strstr(tmp, ".uv"))) {
    uvpart[0] = '\0';
  } /* else neither case -- probably just a bare filename */
  strcat(tmp, ".uvh");
  retval = (char *)MT_malloc((strlen(tmp)+1)*sizeof(char));
  strcpy(retval, tmp);

  DEBUG_TRACE_OUT printf ( "Leaving get_uvh_name\n");
  return(retval);
}


int get_uv_wh(char * name, int *w_ptr, int *h_ptr) {
  char * uvh_filename;
  int retval;
  geom_info_t geom;

  DEBUG_TRACE_IN printf ( "Entering get_uv_wh\n");

  uvh_filename = get_uvh_name(name);

  if (!FT_fileExists(uvh_filename)) {
    retval = 0;
  } else {
    /* If the file exists, assume it is valid */
    debug("The uvh file is %s\n", uvh_filename);
    read_uvh(&geom, uvh_filename);

    free_read_uvh_markers(&geom); /* not used here */
        
    debug("The width and height found are: %dx%d\n", geom.imagecolumns, geom.imagerows);
    if ((geom.valid.imagecolumns)&&(geom.valid.imagerows)) {
      *w_ptr = geom.imagecolumns;
      *h_ptr = geom.imagerows;
      retval = 1;
    } else {
      retval = 0;
    }
  }
  MT_free((void*)uvh_filename);

  DEBUG_TRACE_OUT printf ( "Leaving get_uv_wh\n");
  return(retval);
}


/* Reads up to maxsize-1 characters from a file and
 * terminates the returned buffer with a '\0'.  Designed
 * to read until buffer is filled, '\n' is encountered,
 * or EOF is encountered.  The '\n' is read from the file
 * but _not_ stored in the buffer.  An EOF is placed back
 * on the file's stream UNLESS EOF was the only character
 * read.
 * Returns:  The number of characters in the buffer
 *           (0 to maxsize-1 inclusive).
 *            OR -1 if only an EOF was read.
 */
int readln3(FILE * fptr, char * buf, int maxsize) {
    int i, ch;
    DEBUG_TRACE_IN printf("Entering readln3\n");
  
    for (i=0; i<maxsize-1; i++) {
        ch = fgetc(fptr);
        if (ch==EOF) {
            buf[i] = '\0';
            if (i==0) {
                DEBUG_TRACE_OUT printf("Leaving readln3, just read EOF\n");
                return(-1);        /* indicate no characters read */
            } else {
                ungetc(EOF, fptr); /* save the EOF for next time... */
                DEBUG_TRACE_OUT printf("Leaving readln3\n");
                return(i);
            }
        } else if (ch=='\n') {
            buf[i] = '\0';
            DEBUG_TRACE_OUT printf("Leaving readln3\n");
            return(i);
        } else {
            buf[i] = (char)ch;
        }
    }
    /* if got to here, filled all of the passed buffer before
     * an eoln of eof and so must terminate the string.
     */
    buf[maxsize-1]='\0';
    DEBUG_TRACE_OUT printf("Leaving readln3\n");
    return(maxsize-1);
}

char * right_keyval(char * aline, char * keyname, int len) {
    char * realstart, *retval;
    int i, size;

    DEBUG_TRACE_IN printf("Entering right_keyval\n");
  
    if (!strstr(aline, keyname))
    {
        DEBUG_TRACE_OUT printf("Leaving right_keyval\n");
        return(NULL);
    }
  
    /* if here, know the name exists */
    realstart = aline;
    i=0;
    while(isspace(aline[i])) i++;
    realstart+=i;
  
    if (strncmp(realstart, keyname, len)!=0)
    {
        DEBUG_TRACE_OUT printf("Leaving right_keyval\n");
        return(NULL);
    }
  

    i=len;
    while(isspace(realstart[i])) i++;

    if (realstart[i]!=':')
    {
        DEBUG_TRACE_OUT printf("Leaving right_keyval\n");
        return(NULL);
    }
    i++;
    if (realstart[i]!='=')
    {
        DEBUG_TRACE_OUT printf("Leaving right_keyval\n");
        return(NULL);
    }
    i++;
    while(isspace(realstart[i])) i++;
    size = strlen(realstart+i);
    if (size>0) {
        retval = (char *)MT_malloc((size+1)*sizeof(char));
        strcpy(retval, realstart+i);
        DEBUG_TRACE_OUT printf("Leaving right_keyval\n");
        return(retval);
    } else {
        DEBUG_TRACE_OUT printf("Leaving right_keyval\n");
        return(NULL);
    } 
}


/*******************************************************/
/* Builds a generic pulldown menu given an item list   */
/* and the number of buttons in the list               */
/*******************************************************/

Widget CreateMenu ( parent, button_list, num_buttons )
    Widget parent;
    button_type *button_list;
    int num_buttons;
{
    int i;
    Widget menu, pane;

    DEBUG_TRACE_IN printf ( "Entering CreateMenu\n");

    /*
     * Create a pulldown pane and attach it to the option menu
     */
    pane = XmCreatePulldownMenu ( parent, "pane", NULL, 0 );

    /*
     * Create the option menu.
     */
    menu = XtVaCreateManagedWidget( "menu", xmRowColumnWidgetClass,
                                    parent,
                                    XmNmarginHeight,       0,
                                    XmNmarginWidth,        0,
                                    XmNpacking,            XmPACK_TIGHT,
                                    XmNpopupEnabled,       TRUE,
                                    XmNrowColumnType,      XmMENU_OPTION,
                                    XmNspacing,            0,
                                    XmNsensitive,          True,
                                    XmNsubMenuId,          pane,
                                    NULL );                             

    /*
     * Add buttons to the pane and register callbacks
     * to define the action associated with each menu entry.
     */

    for (i=0; i<num_buttons; i++)
    {
        button_list[i].w = 
            XtCreateManagedWidget ( button_list[i].menuText, 
                                    xmPushButtonWidgetClass,
                                    pane, NULL, 0 );
    }

    DEBUG_TRACE_OUT printf ( "Leaving CreateMenu\n");
    return (menu);
}

void manual_set_pulldown_menu(Widget menu, button_type whichb) {
  image_matrix_type * image_matrix_ptr;
  XmString xmstr;

  DEBUG_TRACE_IN printf ( "Entering manual_set_pulldown_menu\n");

  image_matrix_ptr = get_image_matrix();

  XtVaSetValues(menu,
		XmNmenuHistory, whichb.w,
		NULL);
  xmstr = XmStringCreateLtoR(whichb.menuText, image_matrix_ptr->char_set);
  XtVaSetValues(XmOptionButtonGadget(menu),
		XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);

  DEBUG_TRACE_OUT printf ( "Leaving manual_set_pulldown_menu\n");
}

void set_global_label(char * str) {
  XmString xmstr;
  image_matrix_type * image_matrix_ptr;

  /*DEBUG_TRACE_IN printf("Entering set_global_label\n");*/
  image_matrix_ptr = get_image_matrix();

  xmstr = XmStringCreateLtoR(str, image_matrix_ptr->char_set);
  XtVaSetValues(global_label,
		XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);
  /*DEBUG_TRACE_OUT printf("Leaving set_global_label\n");*/
}

void set_ptr_to_global_label(Widget gl) {
    DEBUG_TRACE_IN printf("Entering set_ptr_to_global_label\n");
    global_label = gl;
    DEBUG_TRACE_OUT printf("Leaving set_ptr_to_global_label\n");
}

void show_unshow_context_help(Boolean want_shown) {
    image_matrix_type * image_matrix_ptr;
    
    DEBUG_TRACE_IN printf("Entering show_unshow_context_help\n");

    image_matrix_ptr = get_image_matrix();

    if (want_shown) {
        XtVaSetValues( XtParent( global_label ),
                       XmNtopAttachment, XmATTACH_WIDGET,
                       XmNtopWidget, image_matrix_ptr->menubar,
                       NULL );
        XtManageChild( XtParent( global_label ) );
        
        XtVaSetValues( image_matrix_ptr->mainform,
                       XmNtopAttachment, XmATTACH_WIDGET,
                       XmNtopWidget, XtParent( global_label ),
                       NULL );
        XtManageChild(global_label);
    } else {
        XtVaSetValues( image_matrix_ptr->mainform,
                       XmNtopAttachment, XmATTACH_WIDGET,
                       XmNtopWidget, image_matrix_ptr->menubar,
                       NULL );
        
        XtUnmanageChild( XtParent( global_label ) );
        XtUnmanageChild(global_label);
    }
    DEBUG_TRACE_OUT printf("Leaving show_unshow_context_help\n");
}

void register_enter_children_event_handlers(Widget w)
{
  static int initial_call = 1;	
  static int maxHWcmaps;
  static Window wi;
  image_matrix_type * image_matrix_ptr;
  
  /*DEBUG_TRACE_IN printf ( "Entering register_enter_children_event_handlers\n");*/

  image_matrix_ptr = get_image_matrix();

  XtAddEventHandler(w, EnterWindowMask, False,
		    (XtEventHandler)enter_widgetEH, (XtPointer)w);
  debug("Adding event handler for widget %s\n", XtName(w));

  if (XtIsComposite(w)) {
    WidgetList theList;
    Cardinal listCount;
    int i;
    
    XtVaGetValues(w, XmNnumChildren, &listCount,
		  XmNchildren, &theList, NULL); 
    for (i = 0; i < listCount; i ++ )
      if (XtIsWidget(theList[i]))
	register_enter_children_event_handlers(theList[i]);
  }

  /*DEBUG_TRACE_OUT printf ( "Leaving register_enter_children_event_handlers\n");*/
}

void enter_widgetEH ( Widget w, XtPointer clientData, XtPointer callData) {
  int i, widget_name_mode=1;
  char * name_hierarchy;

  if (widget_name_mode) {
    /* set_global_label(XtName(w)); this did just the single name */

    /* This will print [ancestor ---> parent_name widget_name] */
    name_hierarchy = get_name_hierarchy(w);
    if (strlen(name_hierarchy)>70) {
      for (i=70; i>=0; i--) {
	if (name_hierarchy[i]==' ') {
	  name_hierarchy[i]='\n';
	  break;
	}
      }
    } else {
      name_hierarchy=(char*)MT_realloc((char *)name_hierarchy, sizeof(char)*
				    strlen(name_hierarchy)+2);
      strcat(name_hierarchy, "\n");
    }
    if (strstr(name_hierarchy, "File")) {
      set_global_label("File Menu");
    } else if (strstr(name_hierarchy, "Edit")) {
      set_global_label("Edit Menu");
    } else if (strstr(name_hierarchy, "View")) {
      set_global_label("View Menu");
    } else if (strstr(name_hierarchy, "Preferences")) {
      set_global_label("Preferences Menu");
    } else if (strstr(name_hierarchy, "image_tools_label")) {
      set_global_label("Pointer Info.  Displays context sensitive help as mouse is moved.");
    } else if (strstr(name_hierarchy, "mouse_rc")
	 ||strstr(name_hierarchy, "mouse_label_rc")) {
      set_global_label("Current function of mouse buttons");
    } else if (strstr(name_hierarchy, "locate_button")
	 ||strstr(name_hierarchy, "locate_label")) {
      set_global_label("When on, displays position of mouse in image window or length of measuring segment.");
    } else if (strstr(name_hierarchy, "right_form_child title")) {
      set_global_label("Set properties for how images are to be displayed");
    } else if (strstr(name_hierarchy, "zoomtitle")
	       ||strstr(name_hierarchy, "emptyzoom")) {
      set_global_label("Zoom in or out on images");
    } else if (strstr(name_hierarchy, "zoom_out")) {
      set_global_label("Zoom out.  Click to make images smaller.");
    } else if (strstr(name_hierarchy, "zoom_in")) {
      set_global_label("Zoom in.  Click to make images larger.");
    } else if (strstr(name_hierarchy, "zoom")) {
      set_global_label("Set Zoom Level.  Enter magnification and press return.");
    } else if (strstr(name_hierarchy, "winsizetitle")
	       ||strstr(name_hierarchy, "emptywinsize")) {
      set_global_label("Set image window sizes.");
    } else if (strstr(name_hierarchy, "winsize_smaller")) {
      set_global_label("Smaller windows.  Click to make image windows smaller.");
    } else if (strstr(name_hierarchy, "winsize_larger")) {
      set_global_label("Larger windows.  Click to make image windows larger.");
    } else if (strstr(name_hierarchy, "winsize")) {
      set_global_label("Set image window sizes.  Enter number of pixels for square.");
    } else if (strstr(name_hierarchy, "viewtypetitle")) {
      set_global_label("View.  Display images only, regions only, or regions superimposed on images.");
    } else if (strstr(name_hierarchy, "colortitle")) {
      set_global_label("Color optimization.  Optimize colors globally or locally to fill colormap.");
    } else if (strstr(name_hierarchy, "kill")) {
      set_global_label("Remove Image(s).  Use to select images for removal.");
    } else if (strstr(name_hierarchy, "synch_win")) {
      set_global_label("Use to toggle between all windows focusing on same area or not.");

    } else if (strstr(name_hierarchy, "cp0")) {
      set_global_label("Thresholding");
    } else if (strstr(name_hierarchy, "cp1")) {
      set_global_label("Manual Draw");
    } else if (strstr(name_hierarchy, "cp2")) {
      set_global_label("Fill");
    } else if (strstr(name_hierarchy, "cp3")) {
      set_global_label("3D Grow");
    } else if (strstr(name_hierarchy, "cp4")) {
      set_global_label("Copy Body");
    } else if (strstr(name_hierarchy, "cp5")) {
      set_global_label("Make Margin");
    } else if (strstr(name_hierarchy, "cp6")) {
      set_global_label("Wand");
    } else if (strstr(name_hierarchy, "cp7")) {
      set_global_label("Setup");

    } else {
      set_global_label(/*name_hierarchy*/ " ");
    }
    MT_free((void*)name_hierarchy);
  } else {
    if (!strcmp(XtName(w), "bordersonly")) {
      set_global_label("Borders.  Toggle viewing filled regions versus only borders of regions.");
    } else if (!strcmp(XtName(w), "RG_LOW_THRESHOLD")) {
      set_global_label("Low Threshold.  Values between low and high threshold are highlighted.");
    } else if (!strcmp(XtName(w), "RG_HIGH_THRESHOLD")) {
      set_global_label("High Threshold.  Values between low and high threshold are highlighted.");
    } else if (!strcmp(XtName(w), "brush_button")) {
      set_global_label("Activate Brush.  Click to use selected brush size.");
    } else if (!strcmp(XtName(w), "RG_overwrite")) {
      set_global_label("Overwrite mode toggle.  Select overwrite mode to enable changing bodies other than active body.");
    } else if (!strcmp(XtName(w), "base")) {
      set_global_label("Base index.  Select slice number for 3D growth to start from.");
    } else if (!strcmp(XtName(w), "start")) {
      set_global_label("Start index.  3D body is generated between start and end indices only.");
    } else if (!strcmp(XtName(w), "end")) {
      set_global_label("End index.  3D body is generated between start and end indices only.");
    } else if (!strcmp(XtName(w), "RG_3D_grow")) {
      set_global_label("Begin 3D grow.  Click to grow active body in 3D using slider settings.");
    } else if (!strcmp(XtName(w), "RG_noise")) {
      set_global_label("Eliminate noise.  Click reduce noise to differentiate image from background.");
    } else if (!strcmp(XtName(w), "RG_speed")) {
      set_global_label("Set Drawing Speed.  Choose low numbers to view in slow motion, high numbers for speed.");
    } else if (!strcmp(XtName(w), "RG_numbodies")) {
      set_global_label("Set Number of Bodies.");
    } else if (!strcmp(XtName(w), "RG_body_color")) {
      set_global_label("Color of corresponding body.  Click to change color.");
    } else if (!strcmp(XtName(w), "RG_body_active")) {
      set_global_label("Active body.  Click to make body the currently active body.");
    } else if (!strcmp(XtName(w), "RG_body_name")) {
      set_global_label("Body name.  View or change name of corresponding body.");
    } else if (!strcmp(XtName(w), "RG_done")) {
      set_global_label("Hide Edit Regions panel.");
    } else if (!strcmp(XtName(w), "region_button")) {
      set_global_label("Display/Hide Edit Regions panel.");
    } else if (!strcmp(XtName(w), "RG_restore")) {
      set_global_label("Redo.  Use to restore last undo.");
    } else if (!strcmp(XtName(w), "RG_undo")) {
      set_global_label("Undo.  Click to undo most recent change(s).");
    } else if (!strcmp(XtName(w), "RG_undo_on_image")) {
      set_global_label("Undo Mode.  In undo mode, clicking on any image causes an undo on that image.");
    } else if (XtParent(w)!=NULL) {
      /* If can't find name, looks for parent */
      enter_widgetEH( XtParent(w), clientData, callData);
    } else {
      set_global_label(/*XtName(w)*/" ");
    }
  }
}

/* Given the mouse mode, determine if certain callbacks are allowed.
 * Basically, assume 'yes' unless told otherwise
 */
int is_allowed_callback(SOME_CALLBACK CB) {
  image_matrix_type * image_matrix_ptr;
  MOUSE_MODE mm;

  DEBUG_TRACE_IN printf ( "Entering is_allowed_callback\n");

  image_matrix_ptr = get_image_matrix();
  mm = image_matrix_ptr->mousebutton_input_function;

  /* Note:  mm refers to the current mouse mode
   *        CB refers to the name of the callback
   *        that wishes to see if it can function or not
   */

  switch(mm)
    {
    case MM_UNDO:
      switch(CB)
	{
	case CB_ACTIVATE_CONTROL_PANEL:
	case CB_DESTROY_IMAGES:
	case CB_MENU_TOGGLE_GROW_REGIONS:
	  DT_warn( image_matrix_ptr->toplevel, "You may not choose this option until you turn\n\
undo mode off.", NULL, NULL );
	  return(0);
	  break;
	default:
	  break;
	}
      break;
    case MM_KILL:
      switch(CB)
	{
	case CB_ACTIVATE_CONTROL_PANEL:
	case CB_SET_ZOOM:
	case CB_WINSIZE:
	case CB_CONSTRUCT_IMAGES:
	case CB_REGISTER_IMAGES:
	case CB_3D_GROW:
	case CB_COPY_BODY:
	case CB_ELIMINATE_NOISE:
	case CB_MENU_LOAD_FILE:
	case CB_MENU_OVERLAY_LOAD_FILE:
	case CB_MENU_OVERLAY_LOAD_BODIES:
	case CB_MENU_RESET:
	case CB_MENU_SAVE_IMAGES:
	case CB_MENU_SAVE_REGIONS:
	case CB_MENU_RENDER:
	case CB_MENU_EXIT:
	case CB_MENU_TOGGLE_PROPERTIES:
	case CB_MENU_TOGGLE_GROW_REGIONS:
	case CB_MENU_SINGLE_IMAGE:
	case CB_MENU_COLOR_TOOL:
	case CB_MENU_REVERSE_IMAGES:
	case CB_MENU_ROTATE_IMAGES:
	case CB_MENU_PREFERENCES:
	case CB_UNDO_MODE:
	case CB_UNDO:
	case CB_REDO:
	case CB_MENU_SORT_IMAGES:
	case CB_EDIT_ZOOM:
	case CB_EDIT_FIDUCIAL:
	case CB_EDIT_CONSTRAINT:
	  DT_warn( image_matrix_ptr->toplevel, "You may not choose this option until you end\n\
Remove Image(s).", NULL, NULL );
	  return(0);
	  break;
	default:
	  break;
	}
      break;
    case MM_FIDUCIAL:
      switch(CB)
	{
	case CB_ACTIVATE_CONTROL_PANEL:
	case CB_MENU_TOGGLE_GROW_REGIONS:
	case CB_UNDO_MODE:
	case CB_DESTROY_IMAGES:
	case CB_MENU_REVERSE_IMAGES:
	case CB_MENU_ROTATE_IMAGES:
	case CB_MENU_SORT_IMAGES:
	case CB_EDIT_CONSTRAINT:
	  DT_warn( image_matrix_ptr->toplevel, "You may not choose this option until you end\n\
Edit Fiducial Markers.", NULL, NULL );
	  return(0);
	  break;
	default:
	  break;
	}
      break;
    case MM_CONSTRAINT:
      switch(CB)
	{
	case CB_ACTIVATE_CONTROL_PANEL:
	case CB_MENU_TOGGLE_GROW_REGIONS:
	case CB_UNDO_MODE:
	case CB_DESTROY_IMAGES:
	case CB_MENU_REVERSE_IMAGES:
	case CB_MENU_ROTATE_IMAGES:
	case CB_MENU_SORT_IMAGES:
	case CB_EDIT_FIDUCIAL:
	  DT_warn( image_matrix_ptr->toplevel, "You may not choose this option until you end\n\
Edit Constraint Markers.", NULL, NULL );
	  return(0);
	  break;
	default:
	  break;
	}
    default:
      /* Should this be an error JJC */
      break;
    }

  DEBUG_TRACE_OUT printf ( "Leaving is_allowed_callback\n");
  return(1);
}

button_type * get_remap_button_list(void) {
    DEBUG_TRACE_IN printf("Entering get_remap_button_list\n");
    DEBUG_TRACE_OUT printf("Leaving get_remap_button_list\n");
    return(remap_button_list);
}

void reset_display_menu(void) {
  int use_index=-1;
  image_matrix_type * image_matrix_ptr;
  
  DEBUG_TRACE_IN printf ( "Entering reset_display_menu\n");

  image_matrix_ptr = get_image_matrix();

  if (image_matrix_ptr->num_pics>0) {
    switch(image_matrix_ptr->img_arr[0].color_optimization_type)
      {
      case CO_GLOBAL:
	use_index = 0;
	break;
      case CO_LOCAL:
	use_index = 1;
	break;
      case CO_ASSUME_FULL:
	use_index = 2;
	break;
      case CO_NONE:
	use_index = 3;
	break;
      }
  } else {
    /* This only happens when no images are up.  Reset to 
     * mode 0 == Global Color Optimization
     */
    use_index = 0;
  }
  if ((use_index>=0)&&(use_index<=3)) {
    manual_set_pulldown_menu(image_matrix_ptr->remap_pulldown,
			     (get_remap_button_list())[use_index]);
  }

  DEBUG_TRACE_OUT printf ( "Leaving reset_display_menu\n");
}

int verify_slices_increasing(void) {
  image_matrix_type * image_matrix_ptr;
  int num, i;
  float next, prev;
  DEBUG_TRACE_IN printf("Entering verify_slices_increasing\n");
  
  image_matrix_ptr = get_image_matrix();
  num = image_matrix_ptr->num_pics;

  if (num<=1)
  {
      DEBUG_TRACE_OUT printf("Leaving verify_slices_increasing\n");
      return(1);
  }
  
  prev = image_matrix_ptr->img_arr[0].z_val;
  for (i=1; i<num; i++) {
    next = image_matrix_ptr->img_arr[i].z_val;
    if (prev>next)
    {
        DEBUG_TRACE_OUT printf("Leaving verify_slices_increasing\n");
        return(0);
    }
    prev = next;
  }
  DEBUG_TRACE_OUT printf("Leaving verify_slices_increasing\n");
  return(1);
}

int verify_slices_increasing_or_decreasing(void) {
  image_matrix_type * image_matrix_ptr;
  int num, i;
  float next, prev, sgn;
  DEBUG_TRACE_IN printf("Entering verify_slices_increasing_or_decreasing\n");
  
  image_matrix_ptr = get_image_matrix();
  num = image_matrix_ptr->num_pics;

  if (num<=1)
  {
      DEBUG_TRACE_OUT printf("Leaving verify_slices_increasing_or_decreasing\n");
      return(1);
  }
  
  prev = image_matrix_ptr->img_arr[0].z_val;
  if (image_matrix_ptr->img_arr[1].z_val-prev > 0.0)
    sgn = 1.0; /* increasing */
  else
    sgn = -1.0; /* decreasing */
  for (i=1; i<num; i++) {
    next = image_matrix_ptr->img_arr[i].z_val;
    if ((next-prev)*sgn<0.0)
    {
        DEBUG_TRACE_OUT printf("Leaving verify_slices_increasing_or_decreasing\n");
        return(0);
    }
    
    prev = next;
  }
  DEBUG_TRACE_OUT printf("Leaving verify_slices_increasing_or_decreasing\n");
  return(1);
}

int verify_uniform_zvalues(void) {
  image_matrix_type * image_matrix_ptr;
  int num, i, numdiffs;
  int retval = 1;
  float *diffs, ave = 0.0;
  DEBUG_TRACE_IN printf("Entering verify_uniform_zvalues\n");
  
  image_matrix_ptr = get_image_matrix();
  num = image_matrix_ptr->num_pics;

  if (num<=1)
  {
      DEBUG_TRACE_OUT printf("Leaving verify_uniform_zvalues\n");
      return(1);
  }
  
  numdiffs = num-1; /* diffs[i] = z_val[i+1] - z_val[i] */
  diffs = (float*)MT_malloc(numdiffs*sizeof(float));
  for (i=0; i<numdiffs; i++) {
    diffs[i] = image_matrix_ptr->img_arr[i+1].z_val-
      image_matrix_ptr->img_arr[i].z_val;
    ave += diffs[i];
  }
  ave/=(float)numdiffs;
  if (ave!=0.0) {
    for (i=0; i<numdiffs; i++) {
      diffs[i]/=ave;
      /* Now, all numbers should be very close to 1 */
      if ((diffs[i]<0.995)||(diffs[i]>1.005)) {
	/* Off by more than 0.5% from 'good' z-spacing */
	retval = 0;
	break;
      }
    }
  } else {
    retval = 0;
  }
  MT_free((void*)diffs);
  DEBUG_TRACE_OUT printf("Leaving verify_uniform_zvalues\n");
  return(retval);
}

/**********************************************************************
 * Callback corresponding to get_values that simply turns off the
 * waiting flag so get_values knows when the values are ready and
 * after storing the values can return.
 **********************************************************************/
void get_valuesCB(Widget widget, XtPointer clientData, XtPointer callData) {
  int *waiting;

  DEBUG_TRACE_IN printf ( "Entering get_valuesCB\n");

  waiting = (int*)(clientData);
  *waiting = 0;

  DEBUG_TRACE_OUT printf ( "Leaving get_valuesCB\n");
}


/**********************************************************************
 * get_values(title, numstrings, label1, outstr1, label2, outstr2, ...);
 **********************************************************************
 * Procedure creates a shell with a title, a column of labels, a column
 * of values to be entered by user, and a done button.  Procedure
 * takes a variable number of arguments:
 * 
 * Parameters:
 *   title:        Title to give the shell widget
 *   numstrings:   The number of label/outstring pairs
 *   label1:       String to be displayed
 *   outstr1:      (char **) variable for which memory will
 *                 be malloc'ed and string1 entered by user
 *                 will be stored
 *   ...:          More label/outstr pairs (based on numstrings)
 *
 * Notes:
 *   Memory is malloced for each outstr and so must be freed.
 *
 * Sample call:
 *   get_values("Enter Values", 2, "Value 1", &strptr1, "Value 2", &strptr2);
 *********************************************************************/
void get_values(char *title_str, int numstrings, ...) {
  va_list varargs;
  Widget shell, gv_form, gv_rc, gv_done, label, gv_sep, gv_title;
  image_matrix_type * image_matrix_ptr;
  int waiting = 1;
  XmString xmstr;
  int i, length;
  char **strings, ***value_ptr_ptrs, *astring;
  Widget *textbox;

  DEBUG_TRACE_IN printf ( "Entering get_values\n");

  strings = (char **)MT_malloc(numstrings*sizeof(char*));
  value_ptr_ptrs = (char ***)MT_malloc(numstrings*sizeof(char**));
  textbox = (Widget *)MT_malloc(numstrings*sizeof(Widget));

  va_start(varargs, numstrings);
  for (i=0; i<numstrings; i++) {
    strings[i] = va_arg(varargs, char *);
    value_ptr_ptrs[i] = va_arg(varargs, char **);
  }
  va_end(varargs);

  image_matrix_ptr = get_image_matrix();

  shell = XtAppCreateShell("Get Values", "shell",
			applicationShellWidgetClass,
			image_matrix_ptr->dpy, NULL, 0);
  
  /* Disable the window menu (closes program) */
  XtVaSetValues ( shell, 
		  XmNmwmDecorations, 
		  MWM_DECOR_ALL|MWM_DECOR_MENU|MWM_DECOR_MINIMIZE|
		                               MWM_DECOR_MAXIMIZE, 
		  NULL );

  gv_form = XmCreateForm(shell, "gv_form", NULL, 0);
  XtManageChild(gv_form);

  gv_title = XmCreateLabel(gv_form, "gv_title", NULL, 0);
  xmstr = XmStringCreateLtoR(title_str, image_matrix_ptr->char_set);
  XtVaSetValues(gv_title,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 5,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(gv_title);

  gv_sep = XmCreateSeparator(gv_form, "gv_sep", NULL, 0);
  XtVaSetValues(gv_sep,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gv_title,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 5,
		NULL);
  XtManageChild(gv_sep);

  gv_rc = XmCreateRowColumn(gv_form, "gv_rc", NULL, 0);
  XtManageChild(gv_rc);
  XtVaSetValues(gv_rc,
		XmNnumColumns, numstrings,
		XmNadjustLast, False,
		XmNpacking, XmPACK_COLUMN,
		XmNorientation, XmHORIZONTAL,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gv_sep,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		NULL);

  gv_sep = XmCreateSeparator(gv_form, "gv_sep", NULL, 0);
  XtVaSetValues(gv_sep,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gv_rc,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 5,
		NULL);
  XtManageChild(gv_sep);

  gv_done = XmCreatePushButton(gv_form, "gv_done", NULL, 0);
  xmstr = XmStringCreateLtoR("Done", MY_CHARSET);
  XtVaSetValues(gv_done,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gv_sep,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNbottomOffset, 5,
		NULL);
  XtManageChild(gv_done);
  XmStringFree(xmstr);
  XtAddCallback(gv_done, XmNactivateCallback, get_valuesCB, (XtPointer)(&waiting));

  for (i=0; i<numstrings; i++) {
    label = XmCreateLabel(gv_rc, "label", NULL, 0);
    xmstr = XmStringCreateLtoR(strings[i], image_matrix_ptr->char_set);
    XtVaSetValues(label,
		  XmNlabelString, xmstr,
		  NULL);
    XmStringFree(xmstr);
    XtManageChild(label);

    textbox[i] = XmCreateTextField(gv_rc, "textbox", NULL, 0);
    XtManageChild(textbox[i]);
  }

  XtRealizeWidget(shell);
  while (waiting) {
    wait_on_xserver2();
  }

  /* Now, need to extract the string values */
  for (i=0; i<numstrings; i++) {
    astring = XmTextGetString(textbox[i]);
    length = strlen(astring);
    if (length>0) {
      *(value_ptr_ptrs[i]) = (char *)MT_malloc(length*sizeof(char));
      strcpy(*(value_ptr_ptrs[i]), astring);
    } else {
      *(value_ptr_ptrs[i]) = (char *)MT_malloc(sizeof(char));
      (*(value_ptr_ptrs[i]))[0] = '\0';
    }
    XtFree(astring);
  }

  MT_free((void*)strings);
  MT_free((void*)value_ptr_ptrs);
  MT_free((void*)textbox);
  XtDestroyWidget(shell);

  DEBUG_TRACE_OUT printf ( "Leaving get_values\n");
}


/**********************************************************************
 * get_values_some_defaults(title, numstrings,
 *                          label1, defval1, outstr1,
 *                          label2, defval2, outstr2, ...);
 **********************************************************************
 * Procedure creates a shell with a title, a column of labels, a column
 * of values to be entered by user, and a done button.  Procedure
 * takes a variable number of arguments:
 * 
 * Parameters:
 *   title:        Title to give the shell widget
 *   numstrings:   The number of label/outstring pairs
 *   label1:       String to be displayed
 *   defval1:      String containing default value
 *   outstr1:      (char **) variable for which memory will
 *                 be malloc'ed and string1 entered by user
 *                 will be stored
 *   ...:          More label/outstr pairs (based on numstrings)
 *
 * Notes:
 *   Memory is malloced for each outstr and so must be freed.
 *
 * Sample call:
 *   get_values("Enter Values", 2, "Value 1", &strptr1, "Value 2", &strptr2);
 *********************************************************************/
void get_values_some_defaults(char *title_str, int numstrings, ...) {
  va_list varargs;
  Widget shell, gv_form, gv_rc, gv_done, label, gv_sep, gv_title;
  image_matrix_type * image_matrix_ptr;
  int waiting = 1;
  XmString xmstr;
  int i, length;
  char **strings, **defaults, ***value_ptr_ptrs, *astring;
  Widget *textbox;

  DEBUG_TRACE_IN printf ( "Entering get_values_some_defaults\n");

  strings = (char **)MT_malloc(numstrings*sizeof(char*));
  defaults = (char **)MT_malloc(numstrings*sizeof(char*));
  value_ptr_ptrs = (char ***)MT_malloc(numstrings*sizeof(char**));
  textbox = (Widget *)MT_malloc(numstrings*sizeof(Widget));

  va_start(varargs, numstrings);
  for (i=0; i<numstrings; i++) {
    strings[i] = va_arg(varargs, char *);
    defaults[i] = va_arg(varargs, char *);
    value_ptr_ptrs[i] = va_arg(varargs, char **);
  }
  va_end(varargs);

  image_matrix_ptr = get_image_matrix();

  /*
   * Changed shell to be a BulletinBoardDialog instead of an
   * application shell so it can lock out the application
   * until the user presses the done button. mbr -> 1-12-99
   */

  shell = XmCreateBulletinBoardDialog( image_matrix_ptr->toplevel, "Get Values",
				       NULL, 0 );
  XtVaSetValues( shell, 
		 XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
		 XmNdialogTitle, XmStringCreateLocalized("Get Values"),
		 XmNnoResize, TRUE,
		 XmNmarginHeight, 0,
		 NULL );

  /* Disable the window menu (closes program) */
  XtVaSetValues ( XtParent(shell), 
		  XmNmwmDecorations, 
		  MWM_DECOR_ALL|MWM_DECOR_MENU|MWM_DECOR_MINIMIZE|
		                               MWM_DECOR_MAXIMIZE, 
		  NULL );

  gv_form = XmCreateForm(shell, "gv_form", NULL, 0);
  XtManageChild(gv_form);

  gv_title = XmCreateLabel(gv_form, "gv_title", NULL, 0);
  xmstr = XmStringCreateLtoR(title_str, image_matrix_ptr->char_set);
  XtVaSetValues(gv_title,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 5,
		NULL);
  XmStringFree(xmstr);
  XtManageChild(gv_title);

  gv_sep = XmCreateSeparator(gv_form, "gv_sep", NULL, 0);
  XtVaSetValues(gv_sep,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gv_title,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 5,
		NULL);
  XtManageChild(gv_sep);

  gv_rc = XmCreateRowColumn(gv_form, "gv_rc", NULL, 0);
  XtManageChild(gv_rc);
  XtVaSetValues(gv_rc,
		XmNnumColumns, numstrings,
		XmNadjustLast, False,
		XmNpacking, XmPACK_COLUMN,
		XmNorientation, XmHORIZONTAL,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gv_sep,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		NULL);

  gv_sep = XmCreateSeparator(gv_form, "gv_sep", NULL, 0);
  XtVaSetValues(gv_sep,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gv_rc,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 5,
		NULL);
  XtManageChild(gv_sep);

  gv_done = XmCreatePushButton(gv_form, "gv_done", NULL, 0);
  xmstr = XmStringCreateLtoR("Done", MY_CHARSET);
  XtVaSetValues(gv_done,
		XmNlabelString, xmstr,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, gv_sep,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNbottomOffset, 5,
		NULL);
  XtManageChild(gv_done);
  XmStringFree(xmstr);
  XtAddCallback(gv_done, XmNactivateCallback, get_valuesCB, (XtPointer)(&waiting));

  for (i=0; i<numstrings; i++) {
    label = XmCreateLabel(gv_rc, "label", NULL, 0);
    xmstr = XmStringCreateLtoR(strings[i], image_matrix_ptr->char_set);
    XtVaSetValues(label,
		  XmNlabelString, xmstr,
		  NULL);
    XmStringFree(xmstr);
    XtManageChild(label);

    textbox[i] = XmCreateTextField(gv_rc, "textbox", NULL, 0);
    /* Fill textbox with default string */
    if (defaults[i]) {
      XmTextSetString(textbox[i], defaults[i]);
      XmTextSetCursorPosition(textbox[i], strlen(defaults[i]));
    }
    XtManageChild(textbox[i]);
  }

  XtManageChild( shell );

  while (waiting) {
    wait_on_xserver2();
  }

  /* Now, need to extract the string values */
  for (i=0; i<numstrings; i++) {
    astring = XmTextGetString(textbox[i]);
    length = strlen(astring);
    if (length>0) {
      *(value_ptr_ptrs[i]) = (char *)MT_malloc(length*sizeof(char));
      strcpy(*(value_ptr_ptrs[i]), astring);
    } else {
      *(value_ptr_ptrs[i]) = (char *)MT_malloc(sizeof(char));
      (*(value_ptr_ptrs[i]))[0] = '\0';
    }
    XtFree(astring);
  }

  MT_free((void*)strings);
  MT_free((void*)defaults);
  MT_free((void*)value_ptr_ptrs);
  MT_free((void*)textbox);
  XtDestroyWidget(shell);
  wait_on_xserver2();

  DEBUG_TRACE_OUT printf ( "Leaving get_values_some_defaults\n");
}

void set_label(Widget w, char * text) {
  XmString xmstr;
  
  DEBUG_TRACE_IN printf ( "Entering set_label\n");

  xmstr = XmStringCreateLtoR(text, MY_CHARSET);

  XtVaSetValues(w,
		XmNlabelString, xmstr,
		NULL);
  XmStringFree(xmstr);

  DEBUG_TRACE_OUT printf ( "Leaving set_label\n");
}


void get_image_editor_resource_path_name(char *fname, char *basename) {
  char * ResourcePath;

  ResourcePath = (char *)getenv("SERA_RESOURCES");

  DEBUG_TRACE_IN printf ( "Entering get_image_editor_resource_path_name\n");

  if (ResourcePath) {
    /* Use this path for the itrc file */
    strcpy(fname, ResourcePath);
    if (fname[strlen(fname)-1]!='/')
      strcat(fname, "/");
    strcat(fname, "SeraModel/");
    strcat(fname, basename);
  } else {
    /* Use the current directory */
    strcpy(fname, basename);
  }

  DEBUG_TRACE_OUT printf ( "Leaving get_image_editor_resource_path_name\n");
}


void get_shared_path_name(char *fname, char *basename) {
  char * ResourcePath;

  ResourcePath = (char *)getenv("SERA_RESOURCES");

  DEBUG_TRACE_IN printf ( "Entering get_shared_path_name\n");

  if (ResourcePath) {
    /* Use this as the base path for the file */
    strcpy(fname, ResourcePath);
    if (fname[strlen(fname)-1]!='/')
      strcat(fname, "/");
    strcat(fname, "Shared/");
    strcat(fname, basename);
  } else {
    /* Use the current directory */
    strcpy(fname, basename);
  }

  DEBUG_TRACE_OUT printf ( "Leaving get_shared_path_name\n");
}

void get_shared_markers_path_name(char *fname, char *basename) {
  char * ResourcePath;

  DEBUG_TRACE_IN printf ( "Entering get_shared_markers_path_name\n");

  ResourcePath = (char *)getenv("SERA_RESOURCES");

  if (ResourcePath) {
    /* Use this as the base path for the file */
    strcpy(fname, ResourcePath);
    if (fname[strlen(fname)-1]!='/')
      strcat(fname, "/");
    strcat(fname, "SeraModel/");
    strcat(fname, basename);
  } else {
    /* Use the current directory */
    strcpy(fname, basename);
  }

  DEBUG_TRACE_OUT printf ( "Leaving get_shared_markers_path_name\n");
}


/* Returns ptr to location of the Fiducial.txt file.  Do not free. */
char * get_fiducial_markers_fname(void) {
  static char fname[256];
  char * basename = "Fiducial.txt.sz";

  /* The markers were in Resources/SeraModel */
  /* get_shared_markers_path_name(fname, basename); */

  /* But all of a sudden moved to Resources/SeraModel */
  DEBUG_TRACE_IN printf ( "Entering get_fiducial_markers_fname\n");
  get_image_editor_resource_path_name(fname, basename);
  DEBUG_TRACE_OUT printf ( "Leaving get_fiducial_markers_fname\n");

  return(fname);
}

/* Returns ptr to location of the Constraint.txt file.  Do not free. */
char * get_constraint_markers_fname(void) {
  static char fname[256];
  char * basename = "Constraint.txt.sz";

  DEBUG_TRACE_IN printf ( "Entering get_constraint_markers_fname\n");
  get_shared_markers_path_name(fname, basename);
  DEBUG_TRACE_OUT printf ( "Leaving get_constraint_markers_fname\n");

  return(fname);
}

/* Returns ptr to location of the body_data.txt file.  Do not free. */
char * get_body_data_fname(void) {
  static char fname[256];
  char * basename = "body_data.txt.sz";

  DEBUG_TRACE_IN printf ( "Entering get_body_data_fname\n");
  get_shared_path_name(fname, basename);
  DEBUG_TRACE_OUT printf ( "Leaving get_body_data_fname\n");
  return(fname);
}

/* Returns ptr to location of the materials.txt file. */
char * get_materials_fname(void) {

  static char fname[256];
  char * basename = "materials.txt.sz";

  DEBUG_TRACE_IN printf("Entering get_materials_fname\n");
  get_shared_path_name(fname, basename);
  DEBUG_TRACE_OUT printf("Leaving get_materials_fname\n");
  return( fname );
}
  

Widget make_new_loader_widget(char * name, int listsize, char **list, loader_struct * ls) {
  Widget shell, form, scrolled_list_form, scrolled_list, a_sep, fsb_button, load_button, cancel_button;
  int i;
  XmString xmstr;
  image_matrix_type * image_matrix_ptr;
  Arg al[2];

  DEBUG_TRACE_IN printf ( "Entering make_new_loader_widget\n");

  image_matrix_ptr = get_image_matrix();

  /* Else, build a nice little widget to get a file from a list
   * of recently used files or to pop up an fsb.
   */
  XtSetArg( al[0], XmNdeleteResponse, XmDO_NOTHING );
  shell = XtAppCreateShell(name, "shell",
			   applicationShellWidgetClass,
			   image_matrix_ptr->dpy, al, 1);

  form = XmCreateForm(shell, "form", NULL, 0);
  XtManageChild(form);

  scrolled_list_form = XmCreateForm(form, "scrolled_list_form", NULL, 0);
  XtVaSetValues(scrolled_list_form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopOffset, 10,
		XmNleftOffset, 10,
		XmNrightOffset, 10,
		NULL);
  XtManageChild(scrolled_list_form);

  scrolled_list = XmCreateScrolledList(scrolled_list_form, "scrolled_list", NULL, 0);
  XtVaSetValues(scrolled_list,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNvisibleItemCount, listsize,
		NULL);
  XtManageChild(scrolled_list);
  ls->scrolled_list = scrolled_list;

  a_sep = XmCreateSeparator(form, "a_sep", NULL, 0);
  XtVaSetValues(a_sep,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, scrolled_list_form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopOffset, 10,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		NULL);
  XtManageChild(a_sep);

  load_button = XmCreatePushButton(form, "load_button", NULL, 0);
  XtVaSetValues(load_button,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, a_sep,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNrightOffset, 10,
		XmNbottomOffset, 5,
		NULL);
  set_label(load_button, "Load");
  XtManageChild(load_button);
  ls->load_button = load_button;
  
  fsb_button = XmCreatePushButton(form, "fsb_button", NULL, 0);
  XtVaSetValues(fsb_button,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, a_sep,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, load_button,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNrightOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  set_label(fsb_button, "Use FSB");
  XtManageChild(fsb_button);
  ls->fsb_button = fsb_button;
  
  cancel_button = XmCreatePushButton(form, "cancel_button", NULL, 0);
  XtVaSetValues(cancel_button,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, a_sep,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, fsb_button,		
		XmNbottomAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNrightOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  set_label(cancel_button, "Cancel");
  XtManageChild(cancel_button);
  ls->cancel_button = cancel_button;
  XtAddCallback(ls->cancel_button, XmNactivateCallback, generic_cancel_load_CB, (XtPointer)shell);
  
  debug("The size of the list is %d\n", listsize);
  for (i=0; i<listsize; i++) {
    debug("The %dth item is %s\n", i, list[i]);
    xmstr = XmStringCreateLtoR(list[i], MY_CHARSET);
    XmListAddItem(scrolled_list, xmstr, i+1);
    if (i==0) {
      XmListSelectItem(scrolled_list, xmstr, i+1);
    }
    XmStringFree(xmstr);
  }

  DEBUG_TRACE_OUT printf ( "Leaving make_new_loader_widget\n");
  return(shell);
}

void destroy_shell_ancestor(Widget w) {
  DEBUG_TRACE_IN printf ( "Entering destroy_shell_ancestor\n");

  do {
    if (XtIsShell(w)) break;
    w = XtParent(w);
  } while (XtIsWidget(w));
  if ((XtIsWidget(w))&&(XtIsShell(w))) {
    XtUnrealizeWidget(w);
    XtDestroyWidget(w);
  }

  DEBUG_TRACE_OUT printf ( "Leaving destroy_shell_ancestor\n");
}


void load_image_file ( char *name )
{
    image_matrix_type * imp;
    char * filename, *newfilename, *bpp_str, *width_str,
        *height_str;
    int width, height, numbytes, num_images, overlay = 0;
    qsh_info_t * qsh_info;
    
    DEBUG_TRACE_IN printf ( "Entering load_image_file\n");
    
    imp = get_image_matrix();
    qsh_info = (qsh_info_t *) MT_malloc( sizeof( qsh_info_t ) );
    

    if ( read_qsh_pair ( qsh_info, name, imp->toplevel, imp->app ) )
    {
        int size = qsh_info->size_of_dimension[1]*qsh_info->size_of_dimension[2];
        float factor = 0.1;
        int i;

        add_to_saved_images_list ( name );
        
        if ( strcmp ( qsh_info->dimensionality, "cm" ) == 0 )
            factor = 1.0;
        
        if ( imp->num_pics != 0 )
        {
            if(imp->num_pics == qsh_info->size_of_dimension[0])
            {
                int value = 
                    DT_3way_decide(imp->toplevel,imp->app,
                                   "Overlay current regions over new images\n\n"
                                   "Replace images AND regions\n\n"
                                   "or Cancel and don't load images",
                                   "Load Image File",
                                   "Overlay",
                                   "Replace",
                                   "Cancel");
                if(value == 0)
                {
                    if( overlayInfo.numFiles < MAX_OVERLAY_SETS )
                    {
                        /* Save the filename in the overlayInfo structure */
                        strcpy( overlayInfo.filenames[overlayInfo.numFiles], name );
                        overlayInfo.activeFile = overlayInfo.numFiles;
                    
                        /* We have one more file */
                        overlayInfo.numFiles++;
                        overlay = 1;
                    }
                    else
                    {
                        char error[256];
                        sprintf( error, "Cannot overlay more than %d image sets\n",
                                 MAX_OVERLAY_SETS );
                        DT_error( imp->toplevel, error, "Too Many Image Sets", NULL );
                        return;
                    }
                }
                else if(value == 1)
                {
                    /* Reinitialize the overlayInfo structure */
                    initializeOverlayInfo();

                    /* Save the filename */
                    strcpy( overlayInfo.filenames[overlayInfo.numFiles], name );
                    overlayInfo.activeFile = overlayInfo.numFiles;
                    
                    /* We have one more file */
                    overlayInfo.numFiles++;
                    overlay = 0;
                }
                else
                {
                    return;
                }
            }
            else
            { 
                if(!Ask("Replace current regions and images?"))
                    return;
                else
                {
                    /* Reinitialize the overlayInfo structure */
                    initializeOverlayInfo();

                    /* Save the filename */
                    strcpy( overlayInfo.filenames[overlayInfo.numFiles], name );
                    overlayInfo.activeFile = overlayInfo.numFiles;
                    
                    /* We have one more file */
                    overlayInfo.numFiles++;
                }
            }
                
            if(!overlay){
                for ( i = 0; i < imp->num_pics; i++ )
                {
                    MT_free ( imp->img_arr[i].data );
                    MT_free ( imp->img_arr[i].pimage_data );
                    MT_free ( imp->img_arr[i].region_data );
                }
                imp->num_pics = 0;

            }
        }
        else /* First time loading images */
        {
            /* Initialize the overlayInfo structure */
            initializeOverlayInfo();

            /* Save the filename */
            strcpy( overlayInfo.filenames[overlayInfo.numFiles], name );
            overlayInfo.activeFile = overlayInfo.numFiles;
            
            /* We have a file */
            overlayInfo.numFiles++;
        }
        
        set_input_dimensions ( imp, qsh_info->size_of_dimension[1], qsh_info->size_of_dimension[2] );
        for ( i = 0; i < qsh_info->size_of_dimension[0]; i++ )
        {
            if(overlay)
                memcpy(imp->img_arr[i].data , 
                       &(qsh_info->images[i*size]), 
                       qsh_info->size_of_dimension[1]*
                       qsh_info->size_of_dimension[2]);
            else
                add_image ( imp, &(qsh_info->images[i*size]),
                            qsh_info->size_of_dimension[1],
                            qsh_info->size_of_dimension[2] );
        }

        transferQshToGeom ( imp, qsh_info );
        
        construct_images_FCN ( imp, STAY_SAME );
        register_images_FCN ( imp, CO_GLOBAL );

        imp->image_range_high = imp->num_pics-1;
        imp->image_range_low = 0;
        update_image_range_labels ( imp, imp->image_range_low, imp->image_range_high );

        /* Set the "Switch Image Sets" button to sensitive if more than one file */
        if( overlayInfo.numFiles > 1 )
            XtSetSensitive( overlayInfo.switchSetsButton, True );
        else
            XtSetSensitive( overlayInfo.switchSetsButton, False);
    }
    else
	DT_error ( imp->toplevel, "Invalid QSH File", "File Error", NULL );

    /* Free memory from Libqsh */
    Free_Qsh( qsh_info );
    qsh_info = NULL;
    
    DEBUG_TRACE_OUT printf ( "Leaving load_image_file\n");
}


/* Copies values from the qsh structure into the geometry info structure */
/*  MTC 8-27-99 */
void transferQshToGeom ( image_matrix_type *imp, qsh_info_t *qsh_info )
{
    float factor = 0.1;
    int i;
    
    if ( strcmp ( qsh_info->dimensionality, "cm" ) == 0 )
        factor = 1.0;    
    
    for ( i = 0; i < qsh_info->size_of_dimension[0]; i++ )
    {
        imp->img_arr[i].pixel_size_x = qsh_info->x_pixel_size*factor;
        imp->img_arr[i].pixel_size_y = qsh_info->y_pixel_size*factor;

        /*
         * First try to get the image location directly.
         * If that doesn't work, try to calculate it from the reference location and spacing.
         * Finally, if that still doesn't work just make it 0.0.
         */
        if ( qsh_info->valid.image_location[i] )
            imp->img_arr[i].z_val = qsh_info->image_location[i]*factor;
        else if ( qsh_info->valid.uniform_spacing && qsh_info->valid.reference_location )
            imp->img_arr[i].z_val = (qsh_info->reference_location+qsh_info->uniform_spacing*(float)i)*factor;
        else
            imp->img_arr[i].z_val = 0.0;
        
        imp->img_arr[i].startx = -(qsh_info->x_pixel_size*factor)*((float)imp->img_arr[i].data_w)/2.0;
        imp->img_arr[i].starty = -(qsh_info->y_pixel_size*factor)*((float)imp->img_arr[i].data_h)/2.0;
    }

    /*
     * Check for a valid slice orientation
     */
    if ( qsh_info->valid.slice_orientation )
    {
        char lower_string[256];

        strcpy( lower_string, qsh_info->slice_orientation );
        KV_make_lower_string( lower_string );

        /*
         * Now find what type it is
         */
        if( strcmp( lower_string, "transverse" ) == 0 ||
            strcmp( lower_string, "axial"      ) == 0 )
        {
            imp->orient_gui.sliceOrientationType = TRANSVERSE;
            strcpy( imp->orient_gui.sliceOrientation, qsh_info->slice_orientation );
        }
        else if( strcmp( lower_string, "coronal" ) == 0 )
        {
            imp->orient_gui.sliceOrientationType = CORONAL;
            strcpy( imp->orient_gui.sliceOrientation, qsh_info->slice_orientation );            
        }
        else if( strcmp( lower_string, "sagittal" ) == 0 )
        {
            imp->orient_gui.sliceOrientationType = SAGITTAL;
            strcpy( imp->orient_gui.sliceOrientation, qsh_info->slice_orientation );            
        }
        else /* default to transverse */
        {
            imp->orient_gui.sliceOrientationType = TRANSVERSE;
            strcpy( imp->orient_gui.sliceOrientation, qsh_info->slice_orientation );            
        }
    }
    else /* default to transverse */
    {
        imp->orient_gui.sliceOrientationType = TRANSVERSE;
        strcpy( imp->orient_gui.sliceOrientation, qsh_info->slice_orientation );        
    }
        
    /*
     * At this point we have a Slice Orientation, so make the Slice 
     * Orientation button under the Edit menu sensitive
     */
    XtSetSensitive( XtNameToWidget( imp->toplevel, "*orientation_button" ), True );
    imp->orient_gui.prompt_when_saving = 1; /* user hasn't set values through the dialog yet */
}


/*
void overlay_load_images(char *filename) {
  gz_FILE *gz_fptr;
  int k, w, h, size, numbytes, swap_bytes;
  image_matrix_type * image_matrix_ptr;
  unsigned char * tmp_ptr;
  
  DEBUG_TRACE_IN printf ( "Entering overlay_load_images\n");

  image_matrix_ptr = get_image_matrix();

  gz_fptr = gz_fopen(filename, "r");
  
  if (gz_fptr) {
    add_to_saved_images_list(filename);
    
    numbytes = get_qsh_numbytes(filename);
    
    if (numbytes==2) {
      swap_bytes = Ask("Byte swap 2 byte images?"); 
    }

    for (k=0; k<image_matrix_ptr->num_pics; k++) {
      w = image_matrix_ptr->img_arr[k].data_w;
      h = image_matrix_ptr->img_arr[k].data_h;
      tmp_ptr = (unsigned char *) MT_malloc(w*h*numbytes*sizeof(unsigned char));
      
      size = gz_fread(tmp_ptr, 1, w*h*numbytes, gz_fptr);
      if (numbytes==2) {
	convert_to_one_bpp(tmp_ptr, w, h, swap_bytes);
      }
      memcpy(image_matrix_ptr->img_arr[k].data, tmp_ptr, (int)(size/numbytes));
      MT_free((void*)tmp_ptr);
      if (size!=w*h*numbytes) break; 
    }
    
    gz_fclose(gz_fptr);

    register_images_FCN(image_matrix_ptr, CO_GLOBAL);
    
    DEBUG_LOADING printf ("Done overlaying files.\n");
  } else {
    DT_error( image_matrix_ptr->toplevel, "Could not load specified file.",
	      NULL, NULL );
  }

  DEBUG_TRACE_OUT printf ( "Leaving overlay_load_images\n");
  } */

void overlay_load_bodies(char * name) {
  int filetype = 0; /* concatenated files */
  int w, h, i, j, size, matched_bodies;
  image_matrix_type *image_matrix_ptr;
  int is_valid[256];
  unsigned char remap[256];
  geom_info_t geom;
  char * uvhname;
  char **current_body_names;
  char unmatched_body_str[512];
  char lname1[256], lname2[256];
  marker_type * m, * cur;
  char marker_found;
  int trigger_fiducial_error = 0, trigger_constraint_error = 0;
  char errmsg[65535]="\0";
  char fiderrmsg[65535]="\0";
  char conerrmsg[65535]="\0";
  char line[512];
      
  DEBUG_TRACE_IN printf ( "Entering overlay_load_bodies\n");

  memset ( is_valid, 0, 256 * sizeof(int) );

  image_matrix_ptr = get_image_matrix();
  
  uvhname = get_uvh_name ( name );
 
  read_uvh ( &geom, uvhname );
 
  while ( !update_assigned_body_lists ( &geom, unmatched_body_str, remap, is_valid ) )
  {
      /*
       * Found bodies not in body data, ask user to load a different body data file
       */
      if ( DT_decide ( image_matrix_ptr->toplevel, image_matrix_ptr->app, unmatched_body_str, 
		       "Unmatched Bodies", "  Load New  \nBody Data File",
		       " Cancel " ) )
      {
	  image_matrix_ptr->choose_files.body_data_read = 0;
	  display_body_and_material_popup ( );
	  while ( !image_matrix_ptr->choose_files.body_data_read )
	  {
	      XtAppProcessEvent ( image_matrix_ptr->app, XtIMAll );

	      /* exit if the user hits the Cancel button */
	      if( image_matrix_ptr->choose_files.body_and_material_popup.cancel_button_pressed == 1 )
	      {
		/* revert to old body data file */
		image_matrix_ptr->choose_files.body_data_read = 1;
                DEBUG_TRACE_OUT printf ( "Leaving overlay_load_bodies\n");
		return;
	      }
	  }
      }
      else
      {
          DEBUG_TRACE_OUT printf ( "Leaving overlay_load_bodies\n");
	  return;
      }
  }

  /* Fill some data to save from the uvh file */
  read_and_save_uvh_data ( uvhname );

  /* Check that it's in cm */
  if ( geom.valid.dimensionality ) 
  {
      sprintf ( line, geom.dimensionality );
      KV_make_lower_string ( line );
      if ( strcmp ( line, "cm" ) ) 
      {
	  DT_warn ( image_matrix_ptr->toplevel, ".uvh file not given in cm.\nThis is not supported.\nUnits will be incorrect.",
		    NULL, NULL );
      }
  }
  
  MT_free ( ( void * ) uvhname );

  /* Update any constraint/fiducial info */
  
  /* Make sure neither of these popups is up... */
  edit_fiducial_markers_FCN ( 0 );
  edit_constraint_markers_FCN ( 0 );
  
  /* First the Fiducial markers */
  printf ( "Looking for %d fiducial markers...\n", geom.num_fiducial_markers );
  if ( geom.num_fiducial_markers > 0 ) 
  {   
      cur = geom.fiducial_markers;
      while ( cur ) 
      {
	  marker_found = 0;
	  printf("Looking for %s\n", cur->name);
	  strcpy(lname1, cur->name);
	  KV_make_lower_string(lname1);
	  KV_trim_string(lname1);
	  for (i = 0; i < image_matrix_ptr->num_fiducial_markers; i++ ) 
	  {
	      m = &(image_matrix_ptr->fiducial_markers[i]);
	      strcpy(lname2, m->name);
	      KV_make_lower_string(lname2);
	      KV_trim_string(lname2);
	      if (!strcmp(lname1, lname2)) {
		/* We have a match! -- copy fields over */
		/* index, x, and y weren't read from file */
		m->wcf_x = cur->wcf_x;
		m->wcf_y = cur->wcf_y;
		m->wcf_z = cur->wcf_z;
		m->is_used = cur->is_used;
		strcpy(m->bodyname, "");
		get_xyz(m->wcf_x, m->wcf_y, m->wcf_z, &(m->x), &(m->y), &(m->index));
		/* mwf:  here would transfer over saved dose info but instead
		 * always use body_data.txt to assign values when writing
		 * file out
		 */
		associate_marker_to_slice(m, m->index);
		marker_found = 1;
		break;
	      }
	  }
	  if (!marker_found) {
	    trigger_fiducial_error = 1;
	    sprintf(line, "  Fiducial marker '%s' unknown.  Thrown out.\n",
		    cur->name);
	    strcat(fiderrmsg, line);
	  }
	  cur = cur->next;
      }
      set_marker_textboxes(FIDUCIAL);
  }
  
  /* Now the Constraint markers */
  printf("Looking for %d constraint markers...\n", geom.num_constraint_markers);
  if (geom.num_constraint_markers>0) {
    cur = geom.constraint_markers;
    while (cur) {
      marker_found = 0;
      printf("Looking for %s\n", cur->name);
      strcpy(lname1, cur->name);
      KV_make_lower_string(lname1);
      KV_trim_string(lname1);
      for (i=0; i<image_matrix_ptr->num_constraint_markers; i++) {
	m = &(image_matrix_ptr->constraint_markers[i]);
	strcpy(lname2, m->name);
	KV_make_lower_string(lname2);
	KV_trim_string(lname2);
	if (!strcmp(lname1, lname2)) {
	  /* We have a match! -- copy fields over */
	  /* index, x, and y weren't read from file */
	  m->wcf_x = cur->wcf_x;
	  m->wcf_y = cur->wcf_y;
	  m->wcf_z = cur->wcf_z;
	  m->is_used = cur->is_used;
	  if (strlen(cur->bodyname)<256)
	    strcpy(m->bodyname, cur->bodyname);
	  else 
	    strcpy(m->bodyname, "");
	  get_xyz(m->wcf_x, m->wcf_y, m->wcf_z, &(m->x), &(m->y), &(m->index));
	  associate_marker_to_slice(m, m->index);
	  marker_found = 1;
	  break;
	}
      }
      if (!marker_found) {
	trigger_constraint_error = 1;
	sprintf(line, "  Constraint marker '%s' unknown.  Thrown out.\n",
		cur->name);
	strcat(conerrmsg, line);
      }
      cur = cur->next;
    }
    set_marker_textboxes(CONSTRAINT);
  }
  free_read_uvh_markers(&geom);

  /*
   * MTC 1/27/99.... I commented all the following out.  Much of it is now in
   * the above called function, update_assigned_body_lists, which is in
   * body_materials.c.  Some of the code below, mainly that that will change the
   * list of bodies if they don't match the bodies found in the body_data file
   * isn't currently needed.  The way the program "currently" works is that the
   * user cannot load an uv/uvh file without a body_data file containing the 
   * correct files.
   */

  /*for (i=0; i<256; i++) {
    if (geom.valid.regionnum[i]) {
      if (numbodies_in_file<MAXNUMBODIES) {
	index_from_file[numbodies_in_file]=i;
	if (geom.bodyname[i][0]!='\'') {
	  strcpy(body_names_from_file[numbodies_in_file], geom.bodyname[i]);
	} else {
	  strcpy(body_names_from_file[numbodies_in_file], geom.bodyname[i]+1);
	}
	size = strlen(body_names_from_file[numbodies_in_file]);
	if (size>0) {
	  if (body_names_from_file[numbodies_in_file][size-1]=='\'')
	    body_names_from_file[numbodies_in_file][size-1]='\0';
	}
      }
      numbodies_in_file++;
      debug("%d is valid.\n", i);
      debug("  Name is %s\n", geom.bodyname[i]);
    }
  }

  if (numbodies_in_file>MAXNUMBODIES) {
    sprintf(line, "Number of bodies in file (%d) exceeds total\n\
number of allowable bodies (%d)\n\n", numbodies_in_file, MAXNUMBODIES);
    strcat(errmsg, line);
  }*/

  /* Want to use current list of bodies (in program) _if_ it contains
   * all bodies that are present.  Else, we will redo the list.
   */
  /* check if all bodies are present */
  /*current_body_names = get_defined_body_names();
  matched_bodies = 0;
  usable_bodies = numbodies_in_file;
  if (usable_bodies>MAXNUMBODIES) {
    usable_bodies = MAXNUMBODIES;
  }*/

  /*DEBUG_LOADING
  { 
      printf ( "Current names:\n" );
      for ( i = 0; i < MAXNUMBODIES; i++ ) 
      {	
	  printf ( "  %d: %s\n", i, current_body_names[i] );
      }

      printf ( "Names in file:\n" );
      for ( i = 0; i < usable_bodies; i++ )
      {
	  printf ( "  %d: %s\n", i, body_names_from_file[i] );
      }
  }*/

  /*for (i=0; i<usable_bodies; i++) {
    for (j=0; j<MAXNUMBODIES; j++) {
      debug("i is %d, j is %d\n", i, j);
      if (!strcmp(current_body_names[j], body_names_from_file[i])) {
	matched_bodies++;
	is_valid[index_from_file[i]]=1;
	remap[index_from_file[i]]=j+DEFAULT_MIN_BODY_COLOR_INDEX;
	break;
      }
    }
  }*/

  /* In this case, didn't find all bodies so remake body list */
  /*if (matched_bodies!=numbodies_in_file) {
    set_body_names(usable_bodies, body_names_from_file);
    for (i=0; i<numbodies_in_file; i++) {
      is_valid[index_from_file[i]]=1;
      remap[index_from_file[i]]=i+DEFAULT_MIN_BODY_COLOR_INDEX;
    }
  }*/

  /* For now, assume input width and height can be retrieved from image_matrix */
  w = image_matrix_ptr->input_width;
  h = image_matrix_ptr->input_height;
  /*
  strcat(errmsg, "Please note the following about uvh file loads:\n\
* When saving, dose information for each body will be overwritten by\n\
  that in body_data.txt regardless of whether dose information for the\n\
  body was present in the .uvh file.\n\
* The same is true for dose information for constraint markers.\n\
* Allowable Constraint and Fiducial markers are given in Constraint.txt\n\
  and Fiducial.txt.  A warning will be given if an unknown marker\n\
  is used.\n");
  */
  strcat(errmsg, "When there is a conflict in information\n\
between the body_data.txt file and\n\
the currently loaded uvh file,\n\
the body_data.txt file will override\n\
and replace.\n");

  if (trigger_fiducial_error) {
    strcat(errmsg, "\nFiducial marker errors follow:\n");
    strcat(errmsg, fiderrmsg);
  } else {
    strcat(errmsg, "\nAll fiducial markers were known.\n");
  }
  if (trigger_fiducial_error) {
    strcat(errmsg, "\nConstraint marker errors follow:\n");
    strcat(errmsg, conerrmsg);
  } else {
    strcat(errmsg, "\nAll constraint markers were known.\n");
  }

  /* In here, do different cases depending on name */
  /* But, for now just assume concatenated w*h byte images (possibly gzipped) */
  switch (filetype) {
  case 0:
    overlay_load_bodies_file(name, w, h, is_valid, remap);
    break;
  }

  update_matnames_in_image_matrix ( &geom, image_matrix_ptr );

  DT_inform( image_matrix_ptr->toplevel,
	     errmsg, NULL, NULL );

  DEBUG_TRACE_OUT printf ( "Leaving overlay_load_bodies\n");
}


/* ====================================================================
   Function:     check_for_unassigned_matnames

   Purpose:      Before saving the regions, checks if all regions have
                 a material name assigned.

   Parameters:   Image Matrix Pointer
                 String to write the names of the regions without
		 assigned materials

   Returned:     1 if one or more regions without materials assigned
                 0 if none are found.

   MTC 2/5/99
   ===================================================================*/
int check_for_unassigned_matnames ( image_matrix_type *image_matrix_ptr, 
				    char *unassigned_string )
{
    dose_info_t *mptr;
    int found_unassigned = 0;
    char lower_string[256];
    DEBUG_TRACE_IN printf("Entering check_for_unassigned_matnames\n");
    
    mptr = image_matrix_ptr->dose_info_list;

    while ( mptr )
    {
        /* If the current body is NOT buffer */
        strcpy ( lower_string, mptr->bodyname );
        KV_make_lower_string ( lower_string );
	KV_trim_string ( lower_string );
        if ( strcmp ( lower_string, "buffer" ) != 0 )
	{     
	    if ( !mptr->valid.matname )
	    {
	        strcat ( unassigned_string, mptr->bodyname );
		strcat ( unassigned_string, "\n" );
		found_unassigned = 1;
	    }
	    else if ( strlen ( mptr->matname ) == 0 )
	    {
	        strcat ( unassigned_string, mptr->bodyname );
		strcat ( unassigned_string, "\n" );
		found_unassigned = 1;
	    }	      
	}

	mptr = mptr->next;
    }
    DEBUG_TRACE_OUT printf("Leaving check_for_unassigned_matnames\n");
    return ( found_unassigned );
}


/* Note:  this function is nearly identical to perform_load_body_CB */


void use_fsb_load_image_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering use_fsb_load_image_CB\n");

  XtUnrealizeWidget((Widget)clientData);
  XtDestroyWidget((Widget)clientData);
  load_file_fsb_FCN(get_image_matrix());

  DEBUG_TRACE_OUT printf ( "Leaving use_fsb_load_image_CB\n");
}

/*
void use_fsb_overlay_load_image_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering use_fsb_overlay_load_image_CB\n");

  XtUnrealizeWidget((Widget)clientData);
  XtDestroyWidget((Widget)clientData);
  overlay_load_file_fsb_FCN(get_image_matrix());

  DEBUG_TRACE_OUT printf ( "Leaving use_fsb_overlay_load_image_CB\n");
  }*/

void use_fsb_load_body_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering use_fsb_load_body_CB\n");

  XtUnrealizeWidget((Widget)clientData);
  XtDestroyWidget((Widget)clientData);
  overlay_load_bodies_fsb_FCN(get_image_matrix());

  DEBUG_TRACE_OUT printf ( "Leaving use_fsb_load_body_CB\n");
}


void use_fsb_bnct3d_launch_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering use_fsb_bnct3d_launch_CB\n");

  XtUnrealizeWidget((Widget)clientData);
  XtDestroyWidget((Widget)clientData);
  bnct3d_launch_fsb_FCN(get_image_matrix());

  DEBUG_TRACE_OUT printf ( "Leaving use_fsb_bnct3d_launch_CB\n");
}

void load_file_fsb_FCN(image_matrix_type * image_matrix_ptr) {
  static Widget input_dialog = NULL;

  DEBUG_TRACE_IN printf ( "Entering load_file_fsb_FCN.\n");

  if (!input_dialog) { /* set it up on first call */
    input_dialog = XmCreateFileSelectionDialog
      ( image_matrix_ptr->toplevel, "Load_Image(s)", NULL, 0);
    XtVaSetValues( XtParent( input_dialog ), XmNtitle, "Load Images", NULL );
    XtAddCallback(input_dialog, XmNokCallback,
		  fsb_okCB, (XtPointer) 0);
    XtAddCallback(input_dialog, XmNcancelCallback,
		  fsb_cancelCB, (XtPointer) 0);
    XtAddCallback(input_dialog, XmNhelpCallback,
		  fsb_helpCB, (XtPointer) 0);
  }
  XtManageChild( input_dialog );

  DEBUG_TRACE_OUT printf ( "Leaving load_file_fsb_FCN.\n");
}

/*void overlay_load_file_fsb_FCN(image_matrix_type * image_matrix_ptr) {
  static Widget input_dialog = NULL;

  DEBUG_TRACE_IN printf ( "Entering overlay_load_file_fsb_FCN.\n");

  if (!input_dialog) { */ /* set it up on first call */ /*
    input_dialog = XmCreateFileSelectionDialog
      ( image_matrix_ptr->toplevel, "Overlay_Load_Image(s)", NULL, 0);
    XtAddCallback(input_dialog, XmNokCallback,
		  fsb_okCB, (XtPointer) 1);
    XtAddCallback(input_dialog, XmNcancelCallback,
		  fsb_cancelCB, (XtPointer) 1);
    XtAddCallback(input_dialog, XmNhelpCallback,
		  fsb_helpCB, (XtPointer) 1);
  }
  XtManageChild( input_dialog );

  DEBUG_TRACE_OUT printf ( "Leaving overlay_load_file_fsb_FCN.\n");
}*/

void overlay_load_bodies_fsb_FCN(image_matrix_type * image_matrix_ptr) {
  static Widget input_dialog = NULL;

  DEBUG_TRACE_IN printf ( "Entering overlay_load_bodies_fsb_FCN.\n");

  if (!input_dialog) { /* set it up on first call */
    input_dialog = XmCreateFileSelectionDialog
      ( image_matrix_ptr->toplevel, "Load_(Body/Bodies)", NULL, 0);
    XtVaSetValues( XtParent( input_dialog ), XmNtitle, "Load Regions", NULL );
    XtAddCallback(input_dialog, XmNokCallback,
		  fsb_okCB, (XtPointer) 2);
    XtAddCallback(input_dialog, XmNcancelCallback,
		  fsb_cancelCB, (XtPointer) 2);
    XtAddCallback(input_dialog, XmNhelpCallback,
		  fsb_helpCB, (XtPointer) 2);
  }
  XtManageChild( input_dialog );

  DEBUG_TRACE_OUT printf ( "Leaving overlay_load_bodies_fsb_FCN.\n");
}


void bnct3d_launch_fsb_FCN(image_matrix_type * image_matrix_ptr) {
  static Widget input_dialog = NULL;

  DEBUG_TRACE_IN printf ( "Entering bnct3d_launch_fsb_FCN.\n");

  if (!input_dialog) { /* set it up on first call */
    input_dialog = XmCreateFileSelectionDialog
      ( image_matrix_ptr->toplevel, "Launch BNCT3D", NULL, 0);
    XtAddCallback(input_dialog, XmNokCallback,
		  fsb_okCB, (XtPointer) 5);
    XtAddCallback(input_dialog, XmNcancelCallback,
		  fsb_cancelCB, (XtPointer) 5);
    XtAddCallback(input_dialog, XmNhelpCallback,
		  fsb_helpCB, (XtPointer) 5);
  }
  XtManageChild( input_dialog );

  DEBUG_TRACE_OUT printf ( "Leaving bnct3d_launch_fsb_FCN.\n");
}


void dummyfcn(char * parse_string, ...) {
}

/* This is called when we enter a drawing window to install a cursor
 * if necessary.
 */
void cursor_enter_notifyCB ( Widget w, XtPointer clientData, XmDrawingAreaCallbackStruct *cbs) {
  int which_image;
  image_matrix_type * image_matrix_ptr;

  /*DEBUG_TRACE_IN printf ( "Entering cursor_enter_notifyCB\n");*/

  image_matrix_ptr = get_image_matrix();
  which_image = (int)clientData;

  if (global_image_cursor_ptr&&(!see_watch)) {
    XDefineCursor(image_matrix_ptr->dpy, XtWindow(w), *global_image_cursor_ptr);
  } else {
    XUndefineCursor(image_matrix_ptr->dpy, XtWindow(w));
  }

  /*DEBUG_TRACE_OUT printf ( "Leaving cursor_enter_notifyCB\n");*/
}


void show_volumes(void) {
  image_matrix_type * image_matrix_ptr;
  geom_info_t * geom_ptr;
  char thestring[256*MAXNUMBODIES+4096];
  char astring[256];
  char blankstr[]="unlabeled", no_body[]="unnamed", thename[128];
  int voxel_count[256];
  unsigned long int total_voxels=0;
  int i, j, wh;
  char ** body_names;
  unsigned char * region_data;
  float per_frac = 0.0;
  char pc = '%';
  float voxel_size = 0;
  char warning_str[256] = "Could not find:\n";
  int warn = 0;
  int list_this_body;

  DEBUG_TRACE_IN printf ( "Entering show_volumes\n") ;   

  image_matrix_ptr = get_image_matrix();
  geom_ptr = get_geometry_ptr();

  if ( !image_matrix_ptr->uvh_data.x_pixel_size_valid )
  {
      strcat ( warning_str, "  PixelSizeColumns\n" );
      warn = 1;
  } 
  if ( !image_matrix_ptr->uvh_data.y_pixel_size_valid )
  {
      strcat ( warning_str, "  PixelSizeRows\n" );
      warn = 1;
  }
  if ( !image_matrix_ptr->uvh_data.z_increment_valid )
  {
      strcat ( warning_str, "  PixelSizeSlices\n" );
      warn = 1;
  }
  if ( !image_matrix_ptr->uvh_data.dimensionality_valid ) 
  {
    strcat ( warning_str, "  Dimensionality\n" );
    warn = 1;
  }

  voxel_size = image_matrix_ptr->uvh_data.x_pixel_size * 
               image_matrix_ptr->uvh_data.y_pixel_size * 
               image_matrix_ptr->uvh_data.z_increment;
  
  if ( strcmp ( image_matrix_ptr->uvh_data.dimensionality, "mm" ) == 0 )
    voxel_size /= 1000;

  /*printf ( "x_pixel_size = %f\ny_pixel_size = %f\nz_increment = %f\n",
	   image_matrix_ptr->uvh_data.x_pixel_size,
	   image_matrix_ptr->uvh_data.y_pixel_size,
	   image_matrix_ptr->uvh_data.z_increment );*/

  memset(voxel_count, 0, 256*sizeof(int));
  for (i=0; i<image_matrix_ptr->num_pics; i++) {
    wh = image_matrix_ptr->img_arr[i].data_w*
      image_matrix_ptr->img_arr[i].data_h;
    region_data = image_matrix_ptr->img_arr[i].region_data;
    for (j=0; j<wh; j++) {
      voxel_count[region_data[j]&REGION_MASK]++;
    }
  }
  for (i=0; i<256; i++) {
    total_voxels+=voxel_count[i];
  }
  if (total_voxels>0) {
    per_frac = 100.0/(float)total_voxels;
  }

  sprintf(thestring, "NAME              NUMBER    CUBIC CM    PERCENT\n-------------------------------------------------\n");
  sprintf(astring, "%-16s %8d  %8d    %6.2f%c \n-------------------------------------------------", 
	  blankstr, voxel_count[0], (int)(voxel_count[0]*voxel_size), 
	  ((float)voxel_count[0])*per_frac, pc);
  strcat(thestring, astring);

  body_names = get_body_names();
  list_this_body = 1;  /* assume we should list the current body */

  for (i=0; i<MAXNUMBODIES; i++) 
  {
    /* this means the body_name is not present */
    if( !XtIsManaged( RG_body_innerform[i] ) )
    {
	list_this_body = 0;
    } 
    else /* valid body name */
    {
      strcpy(thename, body_names[i]);
    }

    if( list_this_body )
    {
      sprintf(astring, "\n%-16s %8d  %8d    %6.2f%c", thename, 
	      voxel_count[i+DEFAULT_MIN_BODY_COLOR_INDEX], (int)(voxel_count[i+DEFAULT_MIN_BODY_COLOR_INDEX]*voxel_size), 
	      ((float)voxel_count[i+DEFAULT_MIN_BODY_COLOR_INDEX])*per_frac, pc);
      strcat(thestring, astring);
    }
    list_this_body = 1;
  }
  free_body_names(body_names);

  make_scrolled_window_dialog( image_matrix_ptr->toplevel, "Region Volumes",
                               thestring, XmALIGNMENT_CENTER );

  if ( warn )
  {
    strcat ( warning_str, "Conversion to cubic cm may not be correct." );
    DT_warn ( image_matrix_ptr->toplevel, warning_str, NULL, NULL );
  }

  DEBUG_TRACE_OUT printf ( "Leaving show_volumes\n");   
}

void synch_win_CB(Widget widget, XtPointer clientData, XtPointer callData) {

  DEBUG_TRACE_IN printf ( "Entering synch_win_CB\n");   
  XtVaGetValues(widget,
		XmNset, &(get_image_matrix()->synchronize_windows),
		NULL);
  DEBUG_TRACE_OUT printf ( "Leaving synch_win_CB\n");   
}

void free_lines_from_file(char ** lines) {
  int i=0;

  DEBUG_TRACE_IN printf ( "Entering free_lines_from_file\n");   

  while (lines[i]!=NULL) {
    MT_free((void*)lines[i]);
    i++;
  }
  MT_free((void*)lines);

  DEBUG_TRACE_OUT printf ( "Leaving free_lines_from_file\n");   
}

/* Memory is ALWAYS malloced when this is called and so
 * free_lines_from_file must ALWAYS be called afterward
 * to free them.
 * Will return an array of all lines read from the
 * file.  Lines longer than 4095 characters will be
 * broken in half.
 */
char ** get_lines_from_file(char * filename, int * numlines) {
  FILE * fptr;
  char ** lines;
  int linesread = 0;
  int size;
  char aline[4096];
  char * throw_away_title;

  DEBUG_TRACE_IN printf ( "Entering get_lines_from_file\n");   

  lines = (char**)MT_malloc(1*sizeof(char*));
  lines[linesread]=NULL;

  fptr = fopen(filename, "r");

  if (fptr) { /* If not, just read no lines and exit */

    /*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     * Before reading the contents of the file, check to see if there is
     * a header line. The header line is marked by an astrisk in the first
     * line, first column of the file. Currently (1-14-99) the function
     * get_lines_from_file is only being used to read in Fiducial.txt and
     * Constraint.txt (or their equvilant) which do have a header line.
     * Currently the header line is not really being used, so we are just supplying
     * throw_away_title to the function and then freeing it. If a time comes when
     * the title is going to be used, an appropriate char ** should be passed to 
     * check_for_file_header so it can be stored and used.
     *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
    check_for_file_header( fptr, &throw_away_title );
    MT_free( (void *) throw_away_title ); /* aren't using the title yet, just free it */

    do {
      size = readln3(fptr, aline, 4096);
      if (size>=0) {
	lines[linesread] = (char*)MT_malloc((size+1)*sizeof(char));
	strcpy(lines[linesread], aline);
	linesread++;
	/* Always have 1 extra line allocated for
	 * terminating with a NULL.
	 */
	lines = (char**)MT_realloc((char *)lines, (linesread+1)*sizeof(char*));
	lines[linesread]=NULL;
      }
    } while (size>=0); /* while having non-zero lines */
    fclose(fptr);
  }

  *numlines = linesread;
  DEBUG_TRACE_OUT printf ( "Leaving get_lines_from_file\n");   
  return(lines);
}

void edit_constraint_markers_FCN(int show) {
  /* static Widget shell;*/
  static MOUSE_MODE oldvalue;
  /*int first_call=1; */
  int nummarkers;
  marker_type * active_marker;
  marker_type * marker_list;
  image_matrix_type * image_matrix_ptr;
  
  DEBUG_TRACE_IN printf ( "Entering edit_constraint_markers_FCN\n");

  image_matrix_ptr = get_image_matrix();

  /*  if (first_call) {
      first_call = 0;
      shell = make_markers_shell("Edit Constraint Markers", CONSTRAINT);
      }
  */
  
  /*
   * If we are supposed to show the Edit Constraint Makers widget, but
   * it hasn't been built yet, make the user choose which files they 
   * would like to use before continuing
   */
  if( show && !image_matrix_ptr->choose_files.const_built_not_realized ) {
    display_marker_popup();
    while( !image_matrix_ptr->choose_files.const_built_not_realized )
    {
      XtAppProcessEvent( image_matrix_ptr->app, XtIMAll );
      /* return if the user hits the cancel button */
      if( image_matrix_ptr->choose_files.marker_popup.cancel_button_pressed == 1 )
      {
          DEBUG_TRACE_OUT printf ( "Leaving edit_constraint_markers_FCN\n");   
          return;
      }
    }
  }

  active_marker = get_active_marker(CONSTRAINT);
  marker_list = image_matrix_ptr->constraint_markers;
  nummarkers = image_matrix_ptr->num_constraint_markers;

  if (show) {
    if (!XtIsRealized(image_matrix_ptr->choose_files.edit_constraint_shell)) {
      if (!is_allowed_callback(CB_EDIT_CONSTRAINT)){
	DEBUG_TRACE_OUT printf ( "Leaving edit_constraint_markers_FCN\n");   
	return;
      }
      set_marker_textboxes(CONSTRAINT);
      oldvalue = get_mouse_function();
      set_mouse_function(MM_CONSTRAINT);
      XtRealizeWidget(image_matrix_ptr->choose_files.edit_constraint_shell);
      if (active_marker) {
	draw_active_marker_area(active_marker);
      }
    }
  } else if (XtIsRealized(image_matrix_ptr->choose_files.edit_constraint_shell)) {

    set_mouse_function(oldvalue);

    /* Need to redraw whatever marker was active because it shouldn't
     * be highlighted any more
     */

    if ( check_constraint_textboxes ( marker_list, nummarkers ) )
    {
      XtUnrealizeWidget ( image_matrix_ptr->choose_files.edit_constraint_shell );
      image_matrix_ptr->choose_files.const_built_not_realized = 1;
      DEBUG_TRACE_OUT printf ( "Leaving edit_constraint_markers_FCN\n");   
      return;
    }

    if (active_marker) {
      draw_active_marker_area(active_marker);
    }
  }
  DEBUG_TRACE_OUT printf ( "Leaving edit_constraint_markers_FCN\n");   
}

/* Gets all the bodies from body_data.txt and puts them in a pop up */
void display_bodies_CB ( Widget w, XtPointer clientData, XtPointer callData )
{
  char body_array[128][80];   /* Max of 128 bodies with 80 characters each */  
  int i, num_bodies;
  char body_confirm[512] = "The following bodies were\nfound in your body data file\n----------------------------\n";
  image_matrix_type * imp;

  DEBUG_TRACE_IN printf ( "Entering display_bodies_CB\n");

  /*
   * Check to see if a body data file has been read yet
   * and make the user enter one if they need to.
   */
  imp = get_image_matrix();

  if( (!imp->choose_files.body_data_read) ) 
  {
    display_body_and_material_popup();
    while( !imp->choose_files.body_data_read )
    {
      XtAppProcessEvent( imp->app, XtIMAll );
      /* return if the user hits the cancel button */
      if( imp->choose_files.body_and_material_popup.cancel_button_pressed == 1 )
      {
          DEBUG_TRACE_OUT printf ( "Leaving display_bodies_CB\n");
          return;
      }
    }
  }

  num_bodies = build_bodyname_list ( body_array );

  for ( i = 0; i < num_bodies; i++ )
  {
    strcat ( body_confirm, "\n   " );
    strcat ( body_confirm, body_array[i] );    
  }

  widget_confirm ( w, body_confirm );

  DEBUG_TRACE_OUT printf ( "Leaving display_bodies_CB\n");
}
    

/* Checks all the text entered in the constraint body textboxes and compares them
   to body_data.txt.  */
int check_constraint_textboxes ( marker_type * marker, int nummarkers )
{
  int i, j;
  char body_array[128][80];   /* Max of 128 bodies with 80 characters each */  
  char body_confirm[512] = "The following bodies were not found in body_data.txt:\n";
  char *bodyname, *textbox_text;
  float xf, yf, zf;
  int oldindex;
  int num_bodies;
  int body_ok;
  int all_bodies_ok = 1;
  
  DEBUG_TRACE_IN printf ( "Entering check_constraint_textboxes\n");

  num_bodies = build_bodyname_list ( body_array );

  for ( i = 0; i < nummarkers; i++ )
  {
    bodyname = XmTextGetString ( ((marker_widgets_type *)(marker[i].pvt_ptr))->bodyname_textbox );
    if ( strlen ( bodyname ) > 0 )
    {
      textbox_text = XmTextGetString(((marker_widgets_type *)(marker[i].pvt_ptr))->x_textbox);
      xf = (float)atof(textbox_text);
      XtFree(textbox_text);

      textbox_text = XmTextGetString(((marker_widgets_type *)(marker[i].pvt_ptr))->y_textbox);
      yf = (float)atof(textbox_text);
      XtFree(textbox_text);
  
      textbox_text = XmTextGetString(((marker_widgets_type *)(marker[i].pvt_ptr))->z_textbox);
      zf = (float)atof(textbox_text);
      XtFree(textbox_text);

      textbox_text = XmTextGetString(((marker_widgets_type *)(marker[i].pvt_ptr))->bodyname_textbox);
      strcpy(marker[i].bodyname, textbox_text);
      /* printf("Setting marker bodyname for dose info to %s.\n", marker[i].bodyname); */

      /* Check if this defined body is in body_data.text */
      body_ok = 0;
      for ( j = 0; j < num_bodies; j++ )
      {
	  if ( strcmp ( body_array[j], textbox_text ) == 0 )
	  {
	      body_ok = 1;
	      break;
	  }
      }

      /* if this defined body isn't in body_data.txt, then all the bodies aren't ok */
      if ( !body_ok )
      {
	  strcat ( body_confirm, "\n   " );
	  strcat ( body_confirm, textbox_text );
	  all_bodies_ok = 0;
      }
      
      XtFree(textbox_text);

      marker[i].wcf_x = xf;
      marker[i].wcf_y = yf;
      marker[i].wcf_z = zf;

      /* Need to say where to draw the image */
      oldindex = marker[i].index;
      get_xyz(xf, yf, zf, &(marker[i].x), &(marker[i].y), &(marker[i].index));

      /* printf("For x, y, z got:  %d %d %d\n", marker[i].x, marker[i].y, marker[i].index); */
      
      associate_marker_to_slice(&marker[i], marker[i].index);
      if (oldindex!=marker[i].index) {
	draw_image(get_image_matrix(), oldindex);
      }
    }
  }

  /* Found some bodies not in body_data.txt */
  if ( !all_bodies_ok )
  {
      strcat ( body_confirm, "\n\nContinue anyway?" );
      if ( Ask ( body_confirm ) ){
	DEBUG_TRACE_OUT printf ( "Leaving check_constraint_textboxes\n");
	return ( 1 );
      }
      else{
	DEBUG_TRACE_OUT printf ( "Leaving check_constraint_textboxes\n");
	return ( 0 );
      }
  }
  
  DEBUG_TRACE_OUT printf ( "Leaving check_constraint_textboxes\n");
  return ( 1 );
}

void edit_fiducial_markers_FCN(int show) {
  /*  static Widget shell;*/
  static MOUSE_MODE oldvalue;
  /* int first_call=1;*/
  marker_type * active_marker;
  image_matrix_type * image_matrix_ptr;
  
  DEBUG_TRACE_IN printf ( "Entering edit_fiducial_markers_FCN\n");

  image_matrix_ptr = get_image_matrix();
  
  /*  if (first_call) {
      first_call = 0;
      shell = make_markers_shell("Edit Fiducial Markers", FIDUCIAL);
      }
  */

  /*
   * If we are supposed to show the Edit Fiducial widget, but it hasn't
   * been built yet, make the user choose which files they would like
   * to use before proceeding
   */
  if( show && !image_matrix_ptr->choose_files.fid_built_not_realized ) {
    display_marker_popup();
    while( !image_matrix_ptr->choose_files.fid_built_not_realized )
    {
      XtAppProcessEvent( image_matrix_ptr->app, XtIMAll );
      /* return if the user hits the cancel button */
      if( image_matrix_ptr->choose_files.marker_popup.cancel_button_pressed == 1 )
      {
          DEBUG_TRACE_OUT printf ( "Leaving edit_fiducial_markers_FCN\n");
          return;
      }
    }
  }

  active_marker = get_active_marker(FIDUCIAL);
  
  if (show) {
    if (!XtIsRealized(image_matrix_ptr->choose_files.edit_fiducial_shell)) {
      if (!is_allowed_callback(CB_EDIT_FIDUCIAL)){
	DEBUG_TRACE_OUT printf ( "Leaving edit_fiducial_markers_FCN\n");
	return;
      }
      set_marker_textboxes(FIDUCIAL);
      oldvalue = get_mouse_function();
      set_mouse_function(MM_FIDUCIAL);
      XtRealizeWidget(image_matrix_ptr->choose_files.edit_fiducial_shell);

      if (active_marker) {
	draw_active_marker_area(active_marker);
      }
    }
  } else if (XtIsRealized(image_matrix_ptr->choose_files.edit_fiducial_shell)) {
    set_mouse_function(oldvalue);
    XtUnrealizeWidget(image_matrix_ptr->choose_files.edit_fiducial_shell);
    image_matrix_ptr->choose_files.fid_built_not_realized = 1;
    /* Need to redraw whatever marker was active because it shouldn't
     * be highlighted any more
     */
    if (active_marker) {
      draw_active_marker_area(active_marker);
    }
  }

  DEBUG_TRACE_OUT printf ( "Leaving edit_fiducial_markers_FCN\n");
}

/* The main difference between the CONSTRAINT and FIDUCIAL
 * shells is that the CONSTRAINT has an extra dose component
 */
Widget make_markers_shell(char * title, MARKER_KIND marker_kind) {
  Widget shell, sw, sw_form, out_form, out_frame, in_form, label_rc, rc, bodyname_rc, sep, dismiss;
  Widget label, x, y, z, bodyname, toggle_labels, toggle_form, temp_box, label_form, display_bodies;
  image_matrix_type * image_matrix_ptr;
  XmString xmstr;
  int i;
  int numcolumns, nummarkers;
  marker_type * marker_list;
  Arg al[10];
  int ac = 0;
  int sw_width, sw_height, is_active_offset;

  DEBUG_TRACE_IN printf ( "Entering make_markers_shell\n");

  image_matrix_ptr = get_image_matrix();

  switch(marker_kind)
    {
    case FIDUCIAL:
      numcolumns = image_matrix_ptr->num_fiducial_markers;
      nummarkers = image_matrix_ptr->num_fiducial_markers;
      marker_list = image_matrix_ptr->fiducial_markers;
      sw_width = 700;
      sw_height = 250;
      is_active_offset = 10;
      break;
    case CONSTRAINT:
      numcolumns = image_matrix_ptr->num_constraint_markers;
      nummarkers = image_matrix_ptr->num_constraint_markers;
      marker_list = image_matrix_ptr->constraint_markers;
      sw_width = 850;
      sw_height = 300;
      is_active_offset = 30;
      break;
    default:
      DEBUG_TRACE_OUT printf ( "Leaving make_markers_shell\n");
      return (Widget)NULL;
    }

  shell = XtAppCreateShell(title,
			   "shell",
			   applicationShellWidgetClass,
			   image_matrix_ptr->dpy, NULL, 0);

  /* Disable the window menu (closes program) */
  XtVaSetValues ( shell, 
		  XmNmwmDecorations, 
		  MWM_DECOR_ALL|MWM_DECOR_MENU,
		  NULL );

  out_form = XmCreateForm(shell, "out_form", NULL, 0);
  XtManageChild(out_form);

  out_frame = XmCreateFrame(out_form, "out_frame", NULL, 0);
  XtVaSetValues(out_frame,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopOffset, 5,
		XmNleftOffset, 5,
		XmNrightOffset, 5,
		XmNbottomOffset, 5,
		NULL);
  XtManageChild(out_frame);

  in_form = XmCreateForm(out_frame, "in_form", NULL, 0);
  XtManageChild(in_form);


  label 
    = XtVaCreateManagedWidget ( "Name", xmLabelWidgetClass,
				in_form,
				XmNtopAttachment,  XmATTACH_FORM,
				XmNtopOffset,      5,
				XmNleftAttachment, XmATTACH_FORM,
				XmNleftOffset,     5,
				XmNwidth,          175,
				NULL );

  x = XtVaCreateManagedWidget ( "X", xmLabelWidgetClass,
				in_form,
				XmNtopAttachment,  XmATTACH_FORM,
				XmNtopOffset,      5,
				XmNleftAttachment, XmATTACH_WIDGET,
				XmNleftWidget,     label,
				XmNleftOffset,     5,
				XmNwidth,          100,
				NULL );

  y = XtVaCreateManagedWidget ( "Y", xmLabelWidgetClass,
				in_form,
				XmNtopAttachment,  XmATTACH_FORM,
				XmNtopOffset,      5,
				XmNleftAttachment, XmATTACH_WIDGET,
				XmNleftWidget,     x,
				XmNleftOffset,     5,
				XmNwidth,          100,
				NULL );

  z = XtVaCreateManagedWidget ( "Z", xmLabelWidgetClass,
				in_form,
				XmNtopAttachment,  XmATTACH_FORM,
				XmNtopOffset,      5,
				XmNleftAttachment, XmATTACH_WIDGET,
				XmNleftWidget,     y,
				XmNleftOffset,     5,
				XmNwidth,          100,
				NULL );

  if (marker_kind==CONSTRAINT) {
    bodyname
      = XtVaCreateManagedWidget ( "Body Name", xmLabelWidgetClass,
				  in_form,
				  XmNtopAttachment,  XmATTACH_FORM,
				  XmNtopOffset,      5,
				  XmNleftAttachment, XmATTACH_WIDGET,
				  XmNleftWidget,     z,
				  XmNleftOffset,     5,
				  XmNwidth,          150,
				  NULL );
  
    xmstr = XmStringCreateLtoR(" Use     Active\nMarker   Marker", MY_CHARSET);
    toggle_labels
      = XtVaCreateManagedWidget ( " Use     Active\nMarker   Marker", xmLabelWidgetClass,
				  in_form,
				  XmNtopAttachment,  XmATTACH_FORM,
				  XmNtopOffset,      5,
				  XmNleftAttachment, XmATTACH_WIDGET,
				  XmNleftWidget,     bodyname,
				  XmNleftOffset,     5,
				  XmNwidth,          150,
				  XmNlabelString,    xmstr,
				  NULL );
    XmStringFree(xmstr);
  }
  else {
    xmstr = XmStringCreateLtoR(" Use     Active\nMarker   Marker", MY_CHARSET);
    toggle_labels
      = XtVaCreateManagedWidget ( " Use     Active\nMarker   Marker", xmLabelWidgetClass,
				  in_form,
				  XmNtopAttachment,  XmATTACH_FORM,
				  XmNtopOffset,      5,
				  XmNleftAttachment, XmATTACH_WIDGET,
				  XmNleftWidget,     z,
				  XmNleftOffset,     5,
				  XmNwidth,          150,
				  XmNlabelString,    xmstr,
				  NULL );
    XmStringFree(xmstr);
  }

  dismiss = XmCreatePushButton(in_form, "dismiss", NULL, 0);
  xmstr = XmStringCreateLtoR("Dismiss", image_matrix_ptr->char_set);
  XtVaSetValues(dismiss,
		XmNlabelString, xmstr,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 5,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNbottomOffset, 5,
		XmNwidth, 150,
		NULL);
  XtManageChild(dismiss);
  XmStringFree(xmstr);
  XtAddCallback(dismiss,XmNactivateCallback,edit_marker_done_CB,(XtPointer) marker_kind);

  display_bodies 
    = XtVaCreateManagedWidget ( "Display Bodies", xmPushButtonWidgetClass,
				in_form, 
				XmNrightAttachment,  XmATTACH_WIDGET,
				XmNrightWidget,      dismiss,
				XmNrightOffset,      5,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNbottomOffset,     5,
				XmNwidth,            150,
				NULL );

  XtAddCallback ( display_bodies, XmNactivateCallback,
		  display_bodies_CB, (XtPointer) marker_kind);

  sep = XmCreateSeparator(in_form, "sep", NULL, 0);
  XtVaSetValues(sep,
		XmNbottomAttachment, XmATTACH_WIDGET,
		XmNbottomWidget, dismiss,
		XmNbottomOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(sep);

  ac=0;
  XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
  XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
  sw = XmCreateScrolledWindow(in_form, "marker_sw", al, ac);
  XtVaSetValues(sw,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget,     toggle_labels,
		XmNtopOffset, 5,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftOffset, 5,
		XmNrightAttachment, XmATTACH_FORM,
		XmNrightOffset, 5,
		XmNbottomAttachment, XmATTACH_WIDGET,
		XmNbottomWidget, sep,
		XmNbottomOffset, 5,
		XmNwidth, sw_width,
		XmNheight, sw_height,
		NULL);
  XtManageChild(sw);

  sw_form 
      = XtVaCreateManagedWidget ( "sw_form", xmFormWidgetClass,
				  sw, NULL );
  
  label_rc = XmCreateRowColumn(sw_form, "rc", NULL, 0);
  XtVaSetValues(label_rc,
		XmNnumColumns, numcolumns,
		XmNorientation, XmHORIZONTAL,
		XmNadjustLast, True,
		XmNpacking, XmPACK_COLUMN,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(label_rc);

  rc = XmCreateRowColumn(sw_form, "rc", NULL, 0);
  XtVaSetValues(rc,
		XmNnumColumns, numcolumns,
		XmNorientation, XmHORIZONTAL,
		XmNadjustLast, True,
		XmNpacking, XmPACK_COLUMN,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, label_rc,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(rc);

  bodyname_rc = XmCreateRowColumn(sw_form, "rc", NULL, 0);
  XtVaSetValues(bodyname_rc,
		XmNnumColumns, numcolumns,
		XmNorientation, XmHORIZONTAL,
		XmNadjustLast, True,
		XmNpacking, XmPACK_COLUMN,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget,     rc,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
  XtManageChild(bodyname_rc);

  {
    Widget is_used, is_active;
    Boolean setValue;
    
    for (i=0; i<nummarkers; i++) {

      label_form
	= XtVaCreateManagedWidget ( "label_form", xmFormWidgetClass,
				    label_rc, 
				    XmNwidth, 175,
				    NULL );
      temp_box
	= XtVaCreateManagedWidget ( "temp_box", xmTextWidgetClass,
				    label_form,
				    XmNwidth, 1,
				    NULL );

      label 
	= XtVaCreateManagedWidget ( marker_list[i].name, xmLabelWidgetClass,
				    label_form, 
				    NULL );

      /* set XmNcolumns, NOT XmNwidth  mbr->1-21-98 */
      ac=0;
      XtSetArg(al[ac], XmNcolumns, 12 ); ac++; 
      x = XmCreateTextField(rc, "x", al, ac);
      /*XtVaSetValues(x,
		    XmNwidth, 100,
		    NULL);*/
      XtManageChild(x);
      ((marker_widgets_type *)(marker_list[i].pvt_ptr))->x_textbox = x;
      XtAddCallback(x, XmNmodifyVerifyCallback, VerifyNumericModifiedCallback, (XtPointer)NULL);
      XtAddCallback(x, XmNactivateCallback, MarkerValueEnteredCB, (XtPointer)(&(marker_list[i])));

      y = XmCreateTextField(rc, "y", al, ac);
      /*XtVaSetValues(y,
		    XmNwidth, 100,
		    NULL);*/
      XtManageChild(y);
      ((marker_widgets_type *)(marker_list[i].pvt_ptr))->y_textbox = y;
      XtAddCallback(y, XmNmodifyVerifyCallback, VerifyNumericModifiedCallback, (XtPointer)NULL);
      XtAddCallback(y, XmNactivateCallback, MarkerValueEnteredCB, (XtPointer)(&(marker_list[i])));

      z = XmCreateTextField(rc, "z", al, ac);
      /*XtVaSetValues(z,
		    XmNwidth, 100,
		    NULL);*/
      XtManageChild(z);
      ((marker_widgets_type *)(marker_list[i].pvt_ptr))->z_textbox = z;
      XtAddCallback(z, XmNmodifyVerifyCallback, VerifyNumericModifiedCallback, (XtPointer)NULL);
      XtAddCallback(z, XmNactivateCallback, MarkerValueEnteredCB, (XtPointer)(&(marker_list[i])));

    if (marker_kind==CONSTRAINT) {
      ac=0;
      XtSetArg(al[ac], XmNwidth, 150); ac++;
      bodyname = XmCreateTextField(bodyname_rc, "bodyname", al, ac);
      /*XtVaSetValues(bodyname, 
		    XmNwidth, 150,
		    NULL);*/ 
      XtManageChild(bodyname); 
      ((marker_widgets_type *)(marker_list[i].pvt_ptr))->bodyname_textbox = bodyname;
      XtAddCallback(bodyname, XmNactivateCallback, MarkerValueEnteredCB, (XtPointer)(&(marker_list[i])));
    }

    toggle_form 
        = XtVaCreateManagedWidget ( "toggle_form", xmFormWidgetClass,
				    bodyname_rc, 
				    XmNwidth, 150,
				    NULL );

    temp_box
        = XtVaCreateManagedWidget ( "temp_box", xmTextWidgetClass,
				    toggle_form,
				    XmNwidth, 1,
				    NULL );

    if (marker_list[i].is_used) {
      setValue = True;
    } else {
      setValue = False;
    }
    is_used = XmCreateToggleButton(toggle_form, "is_used", NULL, 0);
    xmstr = XmStringCreateLtoR("  ", image_matrix_ptr->char_set);
    XtVaSetValues(is_used,
		  XmNset, setValue,
		  XmNlabelString, xmstr,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNleftOffset,     30,
		  NULL);
    XtManageChild(is_used);
    XmStringFree(xmstr);
    XtAddCallback(is_used,XmNvalueChangedCallback,ToggleUseMarkerCB,(XtPointer)(&(marker_list[i])));
    ((marker_widgets_type *)(marker_list[i].pvt_ptr))->is_used = is_used;
    
    is_active = XmCreateToggleButton(toggle_form, "is_active", NULL, 0);
    xmstr = XmStringCreateLtoR("  ", image_matrix_ptr->char_set);
    if (i==0) {
      setValue = True;
    } else {
      setValue = False;
    }
    XtVaSetValues(is_active,
		  XmNset, setValue,
		  XmNlabelString, xmstr,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNrightOffset,     10,
		  NULL);
    XtManageChild(is_active);
    XmStringFree(xmstr);
    XtAddCallback(is_active,XmNvalueChangedCallback,ToggleActiveMarkerCB,(XtPointer)(&(marker_list[i])));
    ((marker_widgets_type *)(marker_list[i].pvt_ptr))->is_active = is_active;
    }
  }

  DEBUG_TRACE_OUT printf ( "Leaving make_markers_shell\n");
  return(shell);
}


/*=================== build_bodyname_list =========================
  Purpose      - Opens body_data.txt and reads the names of the bodies into
                 an array.

  Parameters   - the array to store the names

  Return       - How many names were read.
 
  MTC 10/14/98 
  =======================================================================*/  
int build_bodyname_list ( char body_array[][80] )
{
     char     *array = NULL;    /* holds the contents of the file */
     char     *arrayBeginning;  /* keep track of the beginning of the array */
     int      arraySize;
     char     file_name[256];
     char     buffer[256];
     char     *key, *value;
     int      i = 0;
     image_matrix_type * imp;

     DEBUG_TRACE_IN printf ( "Entering build_bodyname_list\n");

     KV_set_split_characters (":");

     imp = get_image_matrix();
     strcpy (file_name, imp->choose_files.body_data_file_name );
     
     if( SZ_UnzipFileIntoArray( file_name, &array, &arraySize ) )
     {
         /*
          * We need to know where the array starts, so we can
          * free it properly, because read_next_key_and_value
          * will change where array points.
          */
         arrayBeginning = array;

         while( KV_SZ_read_next_key_and_value( &array, buffer, 100, &key, &value ) )
         {
             if (strcmp(key, "begin") == 0)
             {
                 strcpy ( body_array[i], value );
                 i++;
             }
         }
         free( (void *) arrayBeginning );
         arrayBeginning = NULL;
     }
     else  /* had a problem unzipping the file */
     {
         DEBUG_TRACE_OUT printf ( "Leaving build_bodyname_list\n");
         return( 0 );
     }
     
     DEBUG_TRACE_OUT printf ( "Leaving build_bodyname_list\n");
     return ( i );
}

void MarkerValueEnteredCB(Widget w, XtPointer ClientData, XtPointer CallData) {
  marker_type * marker;
  char * textbox_text;
  float xf, yf, zf;
  char bodyname[256];
  int oldindex;

  DEBUG_TRACE_IN printf ( "Entering MarkerValueEnteredCB\n");

  marker = (marker_type *)ClientData;  

  dissociate_marker_from_all_slices(marker);
  draw_active_marker_area(marker);

  textbox_text = XmTextGetString(((marker_widgets_type *)(marker->pvt_ptr))->x_textbox);
  xf = (float)atof(textbox_text);
  XtFree(textbox_text);

  textbox_text = XmTextGetString(((marker_widgets_type *)(marker->pvt_ptr))->y_textbox);
  yf = (float)atof(textbox_text);
  XtFree(textbox_text);

  textbox_text = XmTextGetString(((marker_widgets_type *)(marker->pvt_ptr))->z_textbox);
  zf = (float)atof(textbox_text);
  XtFree(textbox_text);

  if (marker->marker_kind==CONSTRAINT) {
    textbox_text = XmTextGetString(((marker_widgets_type *)(marker->pvt_ptr))->bodyname_textbox);
    strcpy(marker->bodyname, textbox_text);
    printf("Setting marker bodyname for dose info to %s.\n", marker->bodyname);
    XtFree(textbox_text);
  }

  marker->wcf_x = xf;
  marker->wcf_y = yf;
  marker->wcf_z = zf;
  /* Need to say where to draw the image */
  oldindex = marker->index;
  get_xyz(xf, yf, zf, &(marker->x), &(marker->y), &(marker->index));

  printf("For x, y, z got:  %d %d %d\n", marker->x, marker->y, marker->index);

  associate_marker_to_slice(marker, marker->index);
  if (oldindex!=marker->index) {
    draw_image(get_image_matrix(), oldindex);
  }

  make_marker_active(marker);
  draw_active_marker_area(marker);

  DEBUG_TRACE_OUT printf ( "Leaving MarkerValueEnteredCB\n");
}

void ToggleUseMarkerCB(Widget w, XtPointer ClientData, XtPointer CallData) {
  marker_type * marker;
  Boolean value;

  DEBUG_TRACE_IN printf ( "Entering ToggleUseMarkerCB\n");

  marker = (marker_type *)ClientData;  

  XtVaGetValues(w,
		XmNset, &value,
		NULL);

  /* IF it was just turned on, also make it active */
  if (value) {
    make_marker_active(marker);
    marker->is_used = 1;
  } else {
    marker->is_used = 0;
  }

  /* Need to either draw or erase the marker */
  draw_active_marker_area(marker);

  DEBUG_TRACE_OUT printf ( "Leaving ToggleUseMarkerCB\n");
}

void ToggleActiveMarkerCB(Widget w, XtPointer ClientData, XtPointer CallData) {
  marker_type * marker;
  Boolean value;

  DEBUG_TRACE_IN printf ( "Entering ToggleActiveMarkerCB\n" );

  marker = (marker_type *)ClientData;  

  XtVaGetValues(w,
		XmNset, &value,
		NULL);

  if (value==False) {
    /* Don't let them toggle it off. */
    XtVaSetValues(w,
		  XmNset, True,
		  NULL);
    DEBUG_TRACE_OUT printf ( "Leaving ToggleActiveMarkerCB\n" );
    return;
  }

  center_image(marker->index);

  make_marker_active(marker);

  DEBUG_TRACE_OUT printf ( "Leaving ToggleActiveMarkerCB\n" );
}

/* Need to also make sure that the previously active marker
 * is not drawn in a highlight color and that the newly
 * active marker is highlighted (by redrawing it when
 * it's set active)
 */
void make_marker_active(marker_type * marker) {
  marker_type * marker_list;
  int i, nummarkers;
  image_matrix_type * image_matrix_ptr;
  Boolean currently_set;

  DEBUG_TRACE_IN printf ( "Entering make_marker_active\n" );

  image_matrix_ptr = get_image_matrix();

  /* Now, need to turn all others off */
  switch(marker->marker_kind)
    {
    case FIDUCIAL:
      marker_list = image_matrix_ptr->fiducial_markers;
      nummarkers = image_matrix_ptr->num_fiducial_markers;
      break;
    case CONSTRAINT:
      marker_list = image_matrix_ptr->constraint_markers;
      nummarkers = image_matrix_ptr->num_constraint_markers;
      break;
    default:
      DEBUG_TRACE_OUT printf ( "Leaving make_marker_active\n" );
      return;
    }

  for (i=0; i<nummarkers; i++) {
    if (&(marker_list[i])!=marker) {
      XtVaGetValues(((marker_widgets_type *)(marker_list[i].pvt_ptr))->is_active,
		    XmNset, &currently_set,
		    NULL);
      if (currently_set) {
	XtVaSetValues(((marker_widgets_type *)(marker_list[i].pvt_ptr))->is_active,
		      XmNset, False,
		      NULL);
	/* redraw the marker so it's no longer highlighted */
	draw_active_marker_area(&(marker_list[i]));
      }
    } else {
      XtVaSetValues(((marker_widgets_type *)(marker_list[i].pvt_ptr))->is_active,
		    XmNset, True,
		    NULL);
      /* also, want to make the marker shown */
      XtVaSetValues(((marker_widgets_type *)(marker_list[i].pvt_ptr))->is_used,
		    XmNset, True,
		    NULL);
      /* Redraw to be sure it's shown */
      draw_active_marker_area(&(marker_list[i]));
    }
  }

  DEBUG_TRACE_OUT printf ( "Leaving make_marker_active\n" );
}

void edit_marker_done_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  MARKER_KIND marker_kind;
  
  DEBUG_TRACE_IN printf ( "Entering edit_marker_done_CB\n" );

  marker_kind = (MARKER_KIND)clientData;

  switch(marker_kind)
    {
    case FIDUCIAL:
      edit_fiducial_markers_FCN(0);
      break;
    case CONSTRAINT:
      edit_constraint_markers_FCN(0);
      break;
    }      

  DEBUG_TRACE_OUT printf ( "Leaving edit_marker_done_CB\n" );
}

char * get_name_hierarchy(Widget w) {
  char * retval;
  int cursize;

  if (XtParent(w)!=NULL) {
    retval = get_name_hierarchy(XtParent(w));
    cursize = strlen(retval);
    retval = (char*)MT_realloc( (char *)retval, sizeof(char)*
			    (strlen(XtName(w))+cursize+2));
    strcat(retval, " ");
    strcat(retval, XtName(w));
  } else {
    retval = (char*)MT_malloc(sizeof(char)*(strlen(XtName(w))+1));
    strcpy(retval, XtName(w));
  }

  return(retval);
}

void set_active_marker(int x, int y, int z) {
  Boolean is_active;
  MOUSE_MODE mousemode;
  image_matrix_type * image_matrix_ptr;
  marker_type * marker_list;
  int nummarkers, i, oldindex;
  char str[128];
  float xf, yf, zf;

  DEBUG_TRACE_IN printf ( "Entering set_active_marker\n" );

  image_matrix_ptr = get_image_matrix();
  
  mousemode = get_mouse_function();
  switch(mousemode)
    {
    case MM_FIDUCIAL:
      nummarkers = image_matrix_ptr->num_fiducial_markers;
      marker_list = image_matrix_ptr->fiducial_markers;
      break;
    case MM_CONSTRAINT:
      nummarkers = image_matrix_ptr->num_constraint_markers;
      marker_list = image_matrix_ptr->constraint_markers;
      break;
    default:
      DEBUG_TRACE_OUT printf ( "Leaving set_active_marker\n" );
      return;
    }

  for (i=0; i<nummarkers; i++) {
    XtVaGetValues(((marker_widgets_type *)(marker_list[i].pvt_ptr))->is_active,
		  XmNset, &is_active,
		  NULL);
    if (is_active) {
      dissociate_marker_from_all_slices(&(marker_list[i]));
      draw_active_marker_area(&(marker_list[i]));

      oldindex = marker_list[i].index;

      marker_list[i].x = x;
      marker_list[i].y = y;
      marker_list[i].index = z;
      get_xyzf(x, y, z, &xf, &yf, &zf);
      marker_list[i].wcf_x = xf;
      marker_list[i].wcf_y = yf;
      marker_list[i].wcf_z = zf;

      /* Update the textboxes */
      sprintf(str, "%f", xf);
      set_textbox(((marker_widgets_type *)(marker_list[i].pvt_ptr))->x_textbox, str);
      sprintf(str, "%f", yf);
      set_textbox(((marker_widgets_type *)(marker_list[i].pvt_ptr))->y_textbox, str);
      sprintf(str, "%f", zf);
      set_textbox(((marker_widgets_type *)(marker_list[i].pvt_ptr))->z_textbox, str);

      associate_marker_to_slice(&(marker_list[i]), z);
      if (oldindex!=marker_list[i].index) {
	draw_image(get_image_matrix(), oldindex);
      }
      
      /* Make sure that the marker is shown */
      XtVaSetValues(((marker_widgets_type *)(marker_list[i].pvt_ptr))->is_used,
		    XmNset, True,
		    NULL);
      marker_list[i].is_used = 1;
      draw_active_marker_area(&(marker_list[i]));
      break;
    }
  }

  DEBUG_TRACE_OUT printf ( "Leaving set_active_marker\n" );
}

marker_type * get_active_marker(MARKER_KIND marker_kind) {
  marker_type * marker_list;
  int i, nummarkers;
  Boolean is_set;
  image_matrix_type * image_matrix_ptr;

  DEBUG_TRACE_IN printf ( "Entering get_active_marker\n" );

  image_matrix_ptr = get_image_matrix();

  switch(marker_kind)
    {
    case FIDUCIAL:
      nummarkers = image_matrix_ptr->num_fiducial_markers;
      marker_list = image_matrix_ptr->fiducial_markers;
      break;
    case CONSTRAINT:
      nummarkers = image_matrix_ptr->num_constraint_markers;
      marker_list = image_matrix_ptr->constraint_markers;
      break;
    default:
      DEBUG_TRACE_OUT printf ( "Leaving get_active_marker\n" );
      return(NULL);
    }

  for (i=0; i<nummarkers; i++) {
    XtVaGetValues(((marker_widgets_type *)(marker_list[i].pvt_ptr))->is_active,
		  XmNset, &is_set,
		  NULL);

    if (is_set) {
      DEBUG_TRACE_OUT printf ( "Leaving get_active_marker\n" );
      return(&(marker_list[i]));
    }
  }
  DEBUG_TRACE_OUT printf ( "Leaving get_active_marker\n" );
  return(NULL);
}

void set_marker_textboxes(MARKER_KIND marker_kind) {
  char str[128];
  int i, nummarkers;
  marker_type * marker_list;
  image_matrix_type * image_matrix_ptr;
  Boolean val;

  DEBUG_TRACE_IN printf ( "Entering set_marker_texboxes\n" );

  image_matrix_ptr = get_image_matrix();

  switch(marker_kind)
    {
    case FIDUCIAL:
      nummarkers = image_matrix_ptr->num_fiducial_markers;
      marker_list = image_matrix_ptr->fiducial_markers;
      break;
    case CONSTRAINT:
      nummarkers = image_matrix_ptr->num_constraint_markers;
      marker_list = image_matrix_ptr->constraint_markers;
      break;
    default:
      DEBUG_TRACE_OUT printf ( "Leaving set_marker_texboxes\n" );
      return;
    }

  for (i=0; i<nummarkers; i++) {
    sprintf(str, "%f", marker_list[i].wcf_x);
    set_textbox(((marker_widgets_type *)(marker_list[i].pvt_ptr))->x_textbox, str);
    
    sprintf(str, "%f", marker_list[i].wcf_y);
    set_textbox(((marker_widgets_type *)(marker_list[i].pvt_ptr))->y_textbox, str);
    
    sprintf(str, "%f", marker_list[i].wcf_z);
    set_textbox(((marker_widgets_type *)(marker_list[i].pvt_ptr))->z_textbox, str);

    if (marker_kind==CONSTRAINT) {
      strcpy(str, marker_list[i].bodyname);
      set_textbox(((marker_widgets_type *)(marker_list[i].pvt_ptr))->bodyname_textbox, str);
    }

    if (marker_list[i].is_used) {
      val = True;
    } else {
      val = False;
    }
    XtVaSetValues(((marker_widgets_type *)(marker_list[i].pvt_ptr))->is_used,
		  XmNset, val,
		  NULL);
  }
  DEBUG_TRACE_OUT printf ( "Leaving set_marker_texboxes\n" );
}

/* It's hard to set values for toggle buttons on the menu so
 * this function will be used.  If called with a non-null widget,
 * will keep track of the widget by the passed name.  Later,
 * to set the menu, you need only pass NULL as the widget and
 * give the same name
 */
void set_menu_toggle(Widget w, char * name, Boolean is_set) {
  int i;
  struct _my_menu_toggle_list_type {
    Widget w;
    char * name;
    struct _my_menu_toggle_list_type * next;
  };
  static struct _my_menu_toggle_list_type * my_list =
    (struct _my_menu_toggle_list_type *) NULL;
  struct _my_menu_toggle_list_type * list_ptr;

  DEBUG_TRACE_IN printf ( "Entering set_menu_toggle\n" );

  /* First, if the name is already stored, then just set the value */
  list_ptr = my_list;
  while(list_ptr) {
    if (!strcmp(list_ptr->name, name)) {
      XtVaSetValues(list_ptr->w,
		    XmNset, is_set,
		    NULL);
      DEBUG_TRACE_OUT printf ( "Leaving set_menu_toggle\n" );
      return;
    }
    list_ptr = list_ptr->next;
  }

  /* If we get to here, we'll be inserting a value */
  if (w!=NULL) {
    list_ptr = XtNew(struct _my_menu_toggle_list_type);
    list_ptr->w = w;
    list_ptr->name = XtNewString(name);
    list_ptr->next = my_list;
    my_list = list_ptr;
  }

  DEBUG_TRACE_OUT printf ( "Leaving set_menu_toggle\n" );
}


/* Returns 1 if successful AND marker is in use ELSE returns 0 */
int get_marker_xyz(marker_type * marker, int * x, int * y, int * z) {
  Boolean in_use;
  char * textbox_text;
  float xf, yf, zf;

  DEBUG_TRACE_IN printf ( "Entering get_marker_xyz\n" );

  XtVaGetValues(((marker_widgets_type *)(marker->pvt_ptr))->is_used,
		XmNset, &in_use,
		NULL);

  if (!in_use) {
    DEBUG_TRACE_OUT printf ( "Leaving get_marker_xyz\n" );
    return(0);
  } else {
    textbox_text = XmTextGetString(((marker_widgets_type *)(marker->pvt_ptr))->x_textbox);
    xf = (float)atof(textbox_text);
    XtFree(textbox_text);

    textbox_text = XmTextGetString(((marker_widgets_type *)(marker->pvt_ptr))->y_textbox);
    yf = (float)atof(textbox_text);
    XtFree(textbox_text);

    textbox_text = XmTextGetString(((marker_widgets_type *)(marker->pvt_ptr))->z_textbox);
    zf = (float)atof(textbox_text);
    XtFree(textbox_text);

    get_xyz(xf, yf, zf, x, y, z);

    DEBUG_TRACE_OUT printf ( "Leaving get_marker_xyz\n" );
    return(1);
  } 
}

/* Returns the value field of the marker IF it's a constraint marker.
 * Else, returns -1.0
 */
float get_marker_value(marker_type * marker) {
  char * textbox_text;
  float value;

  DEBUG_TRACE_IN printf ( "Entering get_marker_value\n" );

  if (marker->marker_kind!=CONSTRAINT) {
    DEBUG_TRACE_OUT printf ( "Leaving get_marker_value\n" );
    return(-1.0);
  } else {
    textbox_text = XmTextGetString(((marker_widgets_type *)(marker->pvt_ptr))->bodyname_textbox);
    value = (float)atof(textbox_text);
    XtFree(textbox_text);
  }

  DEBUG_TRACE_OUT printf ( "Leaving get_marker_value\n" );
  return(value);
}

/* Returns 1 if either file doesn't exist or user agrees to
 * overwrite it.  0 otherwise.
 */
int ok_to_write_file(char * name) {
  char message[1024];

  DEBUG_TRACE_IN printf ( "Entering ok_to_write_file\n" );

  if ((FT_fileExists(name))&&(!strstr(name, AUTOSAVE_REGIONS_NAME))) {
    sprintf(message, "OK to overwrite file:\n%s?", name);
    DEBUG_TRACE_OUT printf ( "Leaving ok_to_write_file\n" );
    return(Ask(message));
  } else {
    DEBUG_TRACE_OUT printf ( "Leaving ok_to_write_file\n" );
    return(1);
  }
}

void center_image(int whichimage) {
  image_matrix_type * image_matrix_ptr;
  Widget sw, rc, h_scrollbar, v_scrollbar;
  Dimension rc_columns, rc_rows;
  int image_col, image_row;
  float frac_down, frac_over;

  DEBUG_TRACE_IN printf ( "Entering center_image\n" );

  image_matrix_ptr = get_image_matrix();

  if ((whichimage<0)||(whichimage>=image_matrix_ptr->num_pics)){
    DEBUG_TRACE_OUT printf ( "Leaving center_image\n" );
    return;
  }

  sw = image_matrix_ptr->window;
  rc = image_matrix_ptr->rc;

  XtVaGetValues(image_matrix_ptr->window,
		XmNhorizontalScrollBar, &h_scrollbar,
		XmNverticalScrollBar, &v_scrollbar,
		NULL);

  XtVaGetValues(rc,
		XmNnumColumns, &rc_rows,
		NULL);

  rc_columns = 0;
  while (rc_rows*rc_columns<image_matrix_ptr->num_pics) {
    rc_columns++;
  }

  image_col = whichimage%rc_columns;
  image_row = whichimage/rc_columns;

  frac_down = ((float)image_row+0.5)/((float)rc_rows);
  frac_over = ((float)image_col+0.5)/((float)rc_columns);

  set_scrollbar_by_frac(h_scrollbar, frac_over, True);
  set_scrollbar_by_frac(v_scrollbar, frac_down, True);

  DEBUG_TRACE_OUT printf ( "Leaving center_image\n" );
}

/* This is like the other convert_to_one_bpp function but is less
   work here because the user passes the minpixel and maxpixel in
*/
void convert_to_one_bpp_maxmin(unsigned char * arr, int w, int h, int swap_bytes, int minpixel, int maxpixel) {
  int i, newi, stop, val;
  double mult, add;

  DEBUG_TRACE_IN printf ( "Entering convert_to_one_bpp_maxmin\n" );

  if ((minpixel>maxpixel)||(minpixel<0)||(maxpixel>65535)){
    DEBUG_TRACE_OUT printf ( "Leaving convert_to_one_bpp_maxmin\n" );
    return;
  }

  stop = w*h*2;
  newi = 0;
  mult = 255.0/((double)(maxpixel-minpixel+1));
  add = (double)(-minpixel)*mult + 0.5;
  for (i=0; i<stop; i+=2, newi++) {
    val=swap_bytes?arr[i+1]*256+arr[i]:arr[i]*256+arr[i+1];
    val = (int)(((double)val)*mult + add);
    /* Precautionary measures -- if max/min pixel wrong */
    if (val<0)
      val = 0;
    else if (val>255)
      val = 255;
    arr[i]=(unsigned char)val;
  }

  DEBUG_TRACE_OUT printf ( "Leaving convert_to_one_bpp_maxmin\n" );
}

/* Since just swapping bytes (using only the low byte or the high byte)
   didn't seem to be acceptable, this now will do the appropriate
   swapping and then fit into the 0-255 range.  This is done locally
   for each image -- using local min and max pixel rather than
   global min and max pixel over all slices.  (To do it globally would
   require reading through the whole file once just as a preview to find
   the maximum and minimum pixel.)
*/
void convert_to_one_bpp(unsigned char * arr, int w, int h, int swap_bytes) {
  int maxpixel=-1, minpixel=65536, i, newi, stop, val;
  double mult, add;

  DEBUG_TRACE_IN printf ( "Entering convert_to_one_bpp\n" );

  stop = w*h*2;
  for (i=0; i<stop; i+=2) {
    val=swap_bytes?arr[i+1]*256+arr[i]:arr[i]*256+arr[i+1];
    if (val<minpixel) minpixel = val;
    if (val>maxpixel) maxpixel = val;
  }

  newi = 0;
  mult = 255.0/((double)(maxpixel-minpixel+1));
  add = (double)(-minpixel)*mult + 0.5;
  for (i=0; i<stop; i+=2, newi++) {
    val = swap_bytes?arr[i+1]*256+arr[i]:arr[i]*256+arr[i+1];
    val = (int)(((double)val)*mult + add);
    arr[i]=(unsigned char)val;
  }

  DEBUG_TRACE_OUT printf ( "Leaving convert_to_one_bpp\n" );
}

/* This is the old routine that simply uses only the high byte or the
   low byte and throws out the other
*/
/*void convert_to_one_bpp(unsigned char * arr, int w, int h, int swap_bytes) {
  int i, offset;

  if (swap_bytes) {
    offset = 1;
  } else {
    offset = 0;
  }
  
  for (i=0; i<w*h; i++) {
    arr[i] = arr[2*i+offset];
  }
  }*/

/* Fills in values for bounding box, etc. */
void fill_computed_uvh_values(image_matrix_type * image_matrix_ptr, geom_info_t * geom_ptr) {
    int i;
    int ilx, ily, ilz, ihx, ihy, ihz;
    double lx, ly, lz, hx, hy, hz;

    int type = image_matrix_ptr->orient_gui.sliceOrientationType;
    
    DEBUG_TRACE_IN printf ( "Entering fill_computed_uvh_values\n" );
    
    /* loop through each regionnum */
    for (i=0; i<256; i++) {
        if (geom_ptr->valid.uvval[i]) {
            if (get_bounds(geom_ptr->uvval[i], &ilx, &ily, &ilz, &ihx, &ihy, &ihz)) {
                /* Convert to WC and store */

                /* Values for low and high x */
                lx = image_matrix_ptr->img_arr[0].startx+
                    image_matrix_ptr->img_arr[0].pixel_size_x*ilx;

                hx = image_matrix_ptr->img_arr[0].startx+
                    image_matrix_ptr->img_arr[0].pixel_size_x*(ihx+1.0);

                /* Values for low and high y */
                ly = image_matrix_ptr->img_arr[0].starty+
                    image_matrix_ptr->img_arr[0].pixel_size_y*(image_matrix_ptr->img_arr[0].data_w-1.0-ihy);

                hy = image_matrix_ptr->img_arr[0].starty+
                    image_matrix_ptr->img_arr[0].pixel_size_y*(image_matrix_ptr->img_arr[0].data_w-ily);

                /* Values for low and high z */
                switch( type )
                {
                    case TRANSVERSE:
                    case AXIAL:
                        lz = geom_ptr->isaxismin+geom_ptr->pixelsizeslices*ilz;
                        hz = geom_ptr->isaxismin+geom_ptr->pixelsizeslices*(ihz+1.0);
                        break;

                    case CORONAL:
                        lz = geom_ptr->paaxismin+geom_ptr->pixelsizeslices*ilz;
                        hz = geom_ptr->paaxismin+geom_ptr->pixelsizeslices*(ihz+1.0);
                        break;

                    case SAGITTAL:
                        lz = geom_ptr->rlaxismin+geom_ptr->pixelsizeslices*ilz;
                        hz = geom_ptr->rlaxismin+geom_ptr->pixelsizeslices*(ihz+1.0);
                        break;
                }

                /*
                 * Set bounding boxes based on the type of orientation
                 */
                switch( type )
                {
                    case TRANSVERSE:
                    case AXIAL:

                        switch( image_matrix_ptr->orient_gui.elements[ROW].which_orientation )
                        {
                            /* Nose of patient at top or bottom of image */
                            case PA_PLUS:
                            case PA_MINUS:
                                geom_ptr->bboxpaaxismin[i] = ly;  
                                geom_ptr->bboxpaaxismax[i] = hy;
                                break;

                            /* Nose of patient at left of right of image */
                            case RL_PLUS:
                            case RL_MINUS:
                                geom_ptr->bboxpaaxismin[i] = lx;  
                                geom_ptr->bboxpaaxismax[i] = hx;
                                break;
                        }
                        
                        switch( image_matrix_ptr->orient_gui.elements[COLUMN].which_orientation )
                        {
                            /* Nose of patient at top or bottom of image */
                            case RL_PLUS:
                            case RL_MINUS:
                                geom_ptr->bboxrlaxismin[i] = lx;
                                geom_ptr->bboxrlaxismax[i] = hx;
                                break;

                            /* Nose of patient at left of right of image */
                            case PA_PLUS:
                            case PA_MINUS:
                                geom_ptr->bboxrlaxismin[i] = ly;
                                geom_ptr->bboxrlaxismax[i] = hy;
                                break;
                        }
                        
                        geom_ptr->bboxisaxismin[i] = lz;
                        geom_ptr->bboxisaxismax[i] = hz;
                        break;

                    case CORONAL:

                        switch( image_matrix_ptr->orient_gui.elements[ROW].which_orientation )
                        {
                            /* Top of head at top or bottom of image */ 
                            case IS_PLUS:
                            case IS_MINUS:
                                geom_ptr->bboxisaxismin[i] = ly;  
                                geom_ptr->bboxisaxismax[i] = hy;
                                break;

                            /* Top of head at left or right of image */    
                            case RL_PLUS:
                            case RL_MINUS:
                                geom_ptr->bboxisaxismin[i] = lx;  
                                geom_ptr->bboxisaxismax[i] = hx;
                                break;
                        }
                        
                        switch( image_matrix_ptr->orient_gui.elements[COLUMN].which_orientation )
                        {
                            /* Top of head at top or bottom of image */
                            case RL_PLUS:
                            case RL_MINUS:
                                geom_ptr->bboxrlaxismin[i] = lx;
                                geom_ptr->bboxrlaxismax[i] = hx;
                                break;

                            /* Top of head at left or right of image */    
                            case IS_PLUS:
                            case IS_MINUS:
                                geom_ptr->bboxrlaxismin[i] = ly;
                                geom_ptr->bboxrlaxismax[i] = hy;
                                break;
                        }
                        
                        geom_ptr->bboxpaaxismin[i] = lz;  
                        geom_ptr->bboxpaaxismax[i] = hz;
                        break;

                    case SAGITTAL:

                        switch( image_matrix_ptr->orient_gui.elements[ROW].which_orientation )
                        {
                            /* Top of head at top or bottom of image */
                            case IS_PLUS:
                            case IS_MINUS:
                                geom_ptr->bboxisaxismin[i] = ly;  
                                geom_ptr->bboxisaxismax[i] = hy;
                                break;

                            /* Top of head at left or right of image */
                            case PA_PLUS:
                            case PA_MINUS:
                                geom_ptr->bboxisaxismin[i] = lx;  
                                geom_ptr->bboxisaxismax[i] = hx;
                                break;
                        }
                        
                        switch( image_matrix_ptr->orient_gui.elements[COLUMN].which_orientation )
                        {
                            /* Top of head at top or bottom of image */
                            case PA_PLUS:
                            case PA_MINUS:
                                geom_ptr->bboxpaaxismin[i] = lx;
                                geom_ptr->bboxpaaxismax[i] = hx;
                                break;

                            /* Top of head at left or right of image */
                            case IS_PLUS:
                            case IS_MINUS:
                                geom_ptr->bboxpaaxismin[i] = ly;
                                geom_ptr->bboxpaaxismax[i] = hy;
                                break;
                        }
                        
                        geom_ptr->bboxrlaxismin[i] = lz;
                        geom_ptr->bboxrlaxismax[i] = hz;
                        break;
                }
                
                /* Mark all bounding boxes as valid */        
                geom_ptr->valid.bboxpaaxismin[i] = 1;
                geom_ptr->valid.bboxpaaxismax[i] = 1;
                geom_ptr->valid.bboxrlaxismin[i] = 1;
                geom_ptr->valid.bboxrlaxismax[i] = 1;
                geom_ptr->valid.bboxisaxismin[i] = 1;
                geom_ptr->valid.bboxisaxismax[i] = 1;
            }
        }
    }

    DEBUG_TRACE_OUT printf ( "Leaving fill_computed_uvh_values\n" );
}

/* returns 1 if found bounds, 0 if not */
int get_bounds(int index, int * lx, int * ly, int * lz, int * hx, int * hy, int * hz) {
    int i, j, k, height, width;
    image_matrix_type * image_matrix_ptr;

    DEBUG_TRACE_IN printf ( "Entering get_bounds\n" );

    image_matrix_ptr = get_image_matrix();
  
    /* If no images, nothing to do */
    if (image_matrix_ptr->num_pics<=0){
        DEBUG_TRACE_OUT printf ( "Leaving get_bounds\n" );
        return ( 0 );
    }

    * lx = image_matrix_ptr -> img_arr[0].data_w;
    * ly = image_matrix_ptr -> img_arr[0].data_h;
    * lz = image_matrix_ptr -> num_pics;
    * hx = -1;
    * hy = -1;
    * hz = -1;

    for (k = 0; k<image_matrix_ptr->num_pics; k++)
    {
        height = image_matrix_ptr->img_arr[k].data_h;
        width = image_matrix_ptr->img_arr[k].data_w;
        for (j=0; j<height; j++)
        {
            for (i=0; i<width; i++)
            {
                if ((image_matrix_ptr->img_arr[k].region_data[j*width+i]&REGION_MASK)==index)
                {
                    if (i>*hx) *hx = i;  /* This will be the largest x where index was found */
                    if (i<*lx) *lx = i;  /* This will be the smallest x where index was found */
                    if (j>*hy) *hy = j;  /* This will be the largest y where index was found */
                    if (j<*ly) *ly = j;  /* This will be the smallest y where index was found */
                    if (k>*hz) *hz = k;  /* This will be the last slice that index was found in */  
                    if (k<*lz) *lz = k;  /* This will be the first slice that index was found in */
                }
            }
        }
    }

    /* If no body, then check to see if any component out of bounds */
    if ((*hx) == -1){
        DEBUG_TRACE_OUT printf ( "Leaving get_bounds\n" );
        return(0);
    }
    else{
        DEBUG_TRACE_OUT printf ( "Leaving get_bounds\n" );
        return(1);
    }
}

void get_xyz(float xf, float yf, float zf, int * x, int * y, int * z) {
  image_matrix_type * image_matrix_ptr;
  int i, maxpics, best_z;
  float min_dist, cur_dist;
  img_arr_type * ir;

  image_matrix_ptr = get_image_matrix();

  DEBUG_TRACE_IN printf ( "Entering get_xyz\n" );

  maxpics = image_matrix_ptr->num_pics;

  /* Find the (integer) z _closest_ to the given zf */
  best_z = 0;
  if (maxpics>0) {
    min_dist = (float)fabs(image_matrix_ptr->img_arr[0].z_val - zf);
  } else {
    fprintf(stderr, "No slices in get_xyz.\n");
    DEBUG_TRACE_OUT printf ( "Leaving get_xyz\n" );
    return;
  }
  for (i=1; i<maxpics; i++) {
    cur_dist = (float)fabs(image_matrix_ptr->img_arr[i].z_val - zf);
    if (cur_dist<min_dist) {
      min_dist = cur_dist;
      best_z = i;
    }
  }
  *z = best_z;

  /* Now, need to get x and y */
  ir = &(image_matrix_ptr->img_arr[best_z]);

  if (ir->pixel_size_x!=0) {
    *x = (int)(((double)(xf - ir->startx))/ir->pixel_size_x);
  }
  if (ir->pixel_size_y!=0) {
    *y = (int)(((double)(yf - ir->starty))/ir->pixel_size_y);
    /* y is backwards! */
    *y = ir->data_h - *y - 1;
  }

  DEBUG_TRACE_OUT printf ( "Leaving get_xyz\n" );
}

void get_xyzf(int x, int y, int z, float * xf, float * yf, float * zf) {
  image_matrix_type * image_matrix_ptr;
  img_arr_type * ir;
  int maxpics;

  image_matrix_ptr = get_image_matrix();

  DEBUG_TRACE_IN printf ( "Entering get_xyzf\n" );

  maxpics = image_matrix_ptr->num_pics;

  if ((z<0)||(z>=maxpics)) {
    fprintf(stderr, "Out of range z in get_xyz\n");
    DEBUG_TRACE_OUT printf ( "Leaving get_xyzf\n" );
    return;
  }

  ir = &(image_matrix_ptr->img_arr[z]);

  /* y is backwards! */
  y = ir->data_h - y - 1;

  *zf = ir->z_val;
  *xf = ir->startx + (x + 0.5) * ir->pixel_size_x;
  *yf = ir->starty + (y + 0.5) * ir->pixel_size_y;

  DEBUG_TRACE_OUT printf ( "Leaving get_xyzf\n" );
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:      check_for_file_header()
%% 
%% Purpose:       Determine if the .txt file begins with a header line.
%%                The title is present IF AND ONLY IF an astrisk is in the 
%%                first row first column of the file.
%% 
%% Parameters:    fptr -> A FILE *, a ptr to a file opened for reading..
%%                title-> A char **, the address of a pointer which will
%%                        point to allocated memory containing the title
%%                        if it is present. If the title is not present,
%%                        title will point to the word "NULL". Should be
%%                        freed when no longer needed.
%% 
%% Return Value:  none
%% 
%% Written By:    Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void check_for_file_header( FILE * fptr, char ** title )
{

  char temp_title[256];
  int key;

  DEBUG_TRACE_IN printf("Entering check_for_file_header\n");
  /*
   * Look at the first character of the file. If it is an astrisk, we know
   * that the file has a title, and we can read it. Otherwise, there is no
   * title to read, so give title the value of "NULL".
   */

  key = fgetc( fptr );

  if( key == '*' )    /* there must be a title */
  {
    key = fgetc( fptr );                      /* get the next blank */
    fgets( temp_title, 81, fptr );            /* read the line */
    temp_title[ strlen(temp_title)-1 ] = '\0';/* overwrite the newline character */
    
    /*
     * Malloc memory for the incoming title, and copy temp_title into it.
     */

    *title = (char *)MT_malloc( sizeof(char)*(strlen(temp_title)+1) );
    strcpy( *title, temp_title );
  }
  else
  {
    ungetc( key, fptr );  /* put the character back */
    *title = (char *)MT_malloc( sizeof(char)*16 );
    strcpy( *title, "NULL" );
  }

  DEBUG_TRACE_OUT printf("Leaving check_for_file_header\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     make_scrolled_window_dialog()
%%  
%% Purpose:      Create a dialog that will contain a scrolled window for
%%               displaying a potentially long message string. The dialog
%%               will be configured to contain only an OK button.
%% 
%% Parameters:   parent ->The parent widget of the dialog.
%%               title  ->A char *, the title for the dialog.
%%               message->A char *, the message to display in the dialog.
%%               alignment->One of XmALIGNMENT_BEGINNING, XmALIGNMENT_CENTER,
%%                          or XmALIGNMENT_END
%% 
%% Return Value: none
%% 
%% Written By:   Mark Rossmeier
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void make_scrolled_window_dialog( Widget parent, char * title,
                                  char * message, int alignment )
{

  Widget shell;
  Widget form;
  Widget scrolled_window;
  Widget message_label;
  XmString xmstr;
  Dimension width;
  Dimension height;

  DEBUG_TRACE_IN printf("Entering make_scrolled_window_dialog\n");

  /*
   * Create a Message Dialog. 
   * Set the XmNdialogTitle using title, remove the Cancel and Help buttons,
   * and register a callback with the OK button.
   */

  shell = XmCreateMessageDialog( parent, "shell", NULL, 0 );
 
  xmstr = XmStringCreateLtoR( title , MY_CHARSET);
  XtVaSetValues( shell, XmNdialogTitle, xmstr, NULL );
  XmStringFree( xmstr );

  XtUnmanageChild( XmMessageBoxGetChild( shell, XmDIALOG_CANCEL_BUTTON ) );
  XtUnmanageChild( XmMessageBoxGetChild( shell, XmDIALOG_HELP_BUTTON ) );

  XtAddCallback( shell, XmNokCallback, Confirm_CB, NULL );

  /*
   * Create a form as a child of shell. 
   * This form will manage the scrolled_window.
   */

  form = XmCreateForm( shell, "form", NULL, 0 );

  /*
   * Now put the scrolled window inside of the form.
   */

  scrolled_window = XtVaCreateManagedWidget( "scrolled_window", xmScrolledWindowWidgetClass,
					     form, 
					     XmNscrollBarDisplayPolicy, XmAS_NEEDED,
					     XmNscrollingPolicy, XmAUTOMATIC,
					     XmNtopAttachment, XmATTACH_FORM,
					     XmNleftAttachment, XmATTACH_FORM,
					     XmNrightAttachment, XmATTACH_FORM,
					     XmNbottomAttachment, XmATTACH_FORM,
					     NULL );

  /*
   * Create a label widget to display the message
   */
  
  xmstr = XmStringCreateLtoR( message , MY_CHARSET);
  message_label = XtVaCreateManagedWidget( "message_label", xmLabelWidgetClass,
					   scrolled_window,
					   XmNlabelString, xmstr,
                                           XmNalignment, alignment,
					   NULL );
  XmStringFree( xmstr );

  /*
   * Find out the height and width of the label
   * and use these values to determine the height
   * and width of the scrolled window.
   */

  XtVaGetValues( message_label, 
		 XmNwidth, &width,
		 XmNheight, &height,
		 NULL );


  if( width < 450 )
    width += 30;   /* add a little for margins */
  else
    width = 450;   /* else, make default */

  if( height < 400 )
    height += 20;
  else
    height = 400;

  XtVaSetValues( scrolled_window, 
		 XmNwidth, width,
		 XmNheight, height,
		 NULL );
  
  /*
   * Manage the form and the dialog.
   */

  XtManageChild( form );
  XtManageChild( shell );

  DEBUG_TRACE_OUT printf("Entering make_scrolled_window_dialog\n");

}
