#ifndef COMMONFCNSHC
#define COMMONFCNSHC

#include "include.h"
#include "global.h"
#include "common.h"
#include "commonfcns.h"
#include "picshell.h"
/*#include "read_raw.h"*/
#include "color.h"

/****************************************************************/
void set_cursor(int type)
{
  static Cursor curs=(Cursor)NULL;
  static int turn_ons=0;

  DEBUG_TRACE_IN printf( "Entering set_cursor\n" );
  
  if ((type==0)&&(curs)) {
    if (turn_ons>0) turn_ons--;
    if (turn_ons==0) {
      XUndefineCursor(image_matrix.dpy, XtWindow(xcontoursTopLevelShell));
      XUndefineCursor(image_matrix.dpy, XtWindow(mainWindowDrawingArea));
    }
  } else if (type==1) {
    turn_ons++;
    if (!curs)
      curs=XCreateFontCursor(image_matrix.dpy, XC_watch);
    XDefineCursor(image_matrix.dpy, XtWindow(xcontoursTopLevelShell), curs);
    XDefineCursor(image_matrix.dpy, XtWindow(mainWindowDrawingArea), curs);
  }

  DEBUG_TRACE_OUT printf( "Leaving set_cursor\n" );
}


/****************************************************************/
Widget make_color_tool_shell(Widget parent)
{
  Arg al[10];
  int ac = 0;
  static int reg = 7;
  static int wi=5;    /* width */
  Widget shell, CT_title, CT_sep1, CT_sep2, CT_sep3, CT_done, /*CT_load,*/
    CT_reset, CT_form, cbar_window;
  
  DEBUG_TRACE_IN printf( "Entering make_color_tool_shell\n" );

  ac=0;
  XtSetArg( al[ac], XmNallowShellResize, True ); ac++;
  XtSetArg( al[ac], XmNautoUnmanage, False ); ac++; /* mwf changed this, don't think it helped */
  shell = XtCreatePopupShell("seraDose: Color Adjustments",
			     xmDialogShellWidgetClass,
			     parent,
			     al,
			     ac);
 
    /*    shell = XtAppCreateShell("Color Adjustments", "shell",
			     applicationShellWidgetClass,
			     XtDisplay(parent), al, ac);*/
/*****/
    ac=0;
    XtSetArg(al[ac], XmNfractionBase, wi*reg); ac++;
    CT_form = XmCreateForm(shell, "CT_form", al, ac);
    /*    XtManageChild(CT_form);*/
    
/*****/
    ac=0;
    CT_title = XmCreateLabel(CT_form, "CT_title", al, ac);
    XtVaSetValues(CT_title,
		  XmNlabelString, 
		  XmStringCreateLtoR("Set Color Map Properties", char_set),
		  NULL);
    XtManageChild(CT_title);
    
/*****/
    CT_sep1 = XmCreateSeparator(CT_form, "CT_sep1", al, ac);
    XtManageChild(CT_sep1);
    
/*****/
    CT_done = XmCreatePushButton(CT_form, "CT_done", NULL, 0);
    XtVaSetValues(CT_done,
		  XmNlabelString, XmStringCreateLtoR("Done", char_set),
		  NULL);
    XtManageChild(CT_done);
    XtAddCallback(CT_done,XmNactivateCallback,color_toolCB,(XtPointer) 0);
/*****/
    /*    CT_load = XmCreatePushButton(CT_form, "CT_load", NULL, 0);
    XtVaSetValues(CT_load,
		  XmNlabelString, XmStringCreateLtoR("Load\nCmap", char_set),
		  NULL);
    XtManageChild(CT_load);
    XtAddCallback(CT_load,XmNactivateCallback,CT_loadCB,NULL);*/
/*****/
    CT_reset = XmCreatePushButton(CT_form, "CT_reset", NULL, 0);
    XtVaSetValues(CT_reset,
		  XmNlabelString, XmStringCreateLtoR("Reset", char_set),
		  NULL);
    XtManageChild(CT_reset);
    XtAddCallback(CT_reset,XmNactivateCallback,color_tool_adjustCB,(XtPointer)(4+16));
    
/*****/
    CT_BACKGROUND = XmCreateScale(CT_form, "CT_BACKGROUND", NULL, 0);
    XtVaSetValues(CT_BACKGROUND,
		  XmNtitleString, XmStringCreateLtoR("Background", char_set),
		  XmNorientation, XmHORIZONTAL,
		  XmNshowValue, True,
		  XmNprocessingDirection, XmMAX_ON_RIGHT,
		  XmNmaximum, 511,
		  XmNminimum, -255,
		  XmNvalue, 0,
		  NULL);
    XtManageChild(CT_BACKGROUND);
/*****/
    CT_SATURATION = XmCreateScale(CT_form, "CT_SATURATION", NULL, 0);
    XtVaSetValues(CT_SATURATION,
		  XmNtitleString, XmStringCreateLtoR("Saturation", char_set),
		  XmNorientation, XmHORIZONTAL,
		  XmNshowValue, True,
		  XmNprocessingDirection, XmMAX_ON_RIGHT,
		  XmNmaximum, 511,
		  XmNminimum, -255,
		  XmNvalue, 255,
		  NULL);
    XtManageChild(CT_SATURATION);
/*****/
    CT_OFFSET = XmCreateScale(CT_form, "CT_OFFSET", NULL, 0);
    XtVaSetValues(CT_OFFSET,
		  XmNtitleString, XmStringCreateLtoR("Rotate Colormap", char_set),
		  XmNorientation, XmHORIZONTAL,
		  XmNshowValue, True,
		  XmNprocessingDirection, XmMAX_ON_RIGHT,
		  XmNmaximum, 256,
		  XmNminimum, 0,
		  XmNvalue, 0,
		  NULL);
    XtManageChild(CT_OFFSET);
/*****/
    CT_sep3 = XmCreateSeparator(CT_form, "CT_sep3", al, ac);
    XtManageChild(CT_sep3);
/*****/
    CT_GAMMA = XmCreateScale(CT_form, "CT_GAMMA", NULL, 0);
    XtVaSetValues(CT_GAMMA,
		  XmNtitleString, XmStringCreateLtoR("Load Gamma Colormap with specified Gamma", char_set),
		  XmNorientation, XmHORIZONTAL,
		  XmNshowValue, True,
		  XmNprocessingDirection, XmMAX_ON_RIGHT,
		  XmNmaximum, 30,
		  XmNminimum, 5,
		  XmNvalue, 20,
		  XmNdecimalPoints, 1,
		  NULL);
    XtManageChild(CT_GAMMA);
/*****/
    ac=0;
    XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
    cbar_window = XmCreateScrolledWindow(CT_form, "cbar_window", al, ac);

    XtVaSetValues(cbar_window, 
		  XmNwidth, /*165*/ 256+12,
		  XmNheight, /*150*/ 16+12,
		  NULL);
    XtManageChild(cbar_window);
/**/
    cbar_w=XtVaCreateManagedWidget("drawingarea",
				   xmDrawingAreaWidgetClass, cbar_window,
				   XmNresizable, False,
				   NULL);
    XtVaSetValues ( cbar_w, 
		    XtVaTypedArg, XmNborderColor, XtRString, "black", 6,
		    XmNborderWidth, 2,
		    XmNwidth, 256,
		    XmNheight, 34,
		    NULL );
    XtManageChild(cbar_w);
/*****/
    CT_sep2 = XmCreateSeparator(CT_form, "CT_sep2", NULL, 0);
    XtManageChild(CT_sep2);
/*****/
    SetFormPercent(CT_title, wi*0, wi*0+1, wi*reg-1, wi*1-1);
    SetFormPercent(CT_sep1, wi*1-1, wi*0+1, wi*reg-1, wi*1);
    SetFormPercent(CT_sep2, wi*(reg-1), wi*0+1, wi*reg-1, wi*(reg-1)+1);
    /*    SetFormPercent(CT_load, wi*(reg-1)+1, wi*(reg-3), wi*(reg-2)-1, wi*reg-1);*/
    SetFormPercent(CT_reset, wi*(reg-1)+1, wi*(reg-2), wi*(reg-1)-1, wi*reg-1);
    SetFormPercent(CT_done, wi*(reg-1)+1, wi*(reg-1), wi*reg-1, wi*reg-1);

    SetFormPercent(CT_BACKGROUND, wi*1, wi*0+1, wi*reg-1, wi*2);
    SetFormPercent(CT_SATURATION, wi*2, wi*0+1, wi*reg-1, wi*3);
    SetFormPercent(CT_OFFSET, wi*3, wi*0+1, wi*reg-1, wi*4);
    SetFormPercent(cbar_window, wi*4+2, wi*1+1, wi*(reg-1)-1, wi*5);
    SetFormPercent(CT_sep3, wi*5, wi*0+1, wi*reg-1, wi*5+1);
    SetFormPercent(CT_GAMMA, wi*5+1, wi*0+1, wi*reg-1, wi*6);
    
    /* Use +16 to indicate an extra call when the slider is released */
    XtAddCallback(CT_BACKGROUND,XmNdragCallback,color_tool_adjustCB,(XtPointer) 0);
    XtAddCallback(CT_BACKGROUND,XmNvalueChangedCallback,color_tool_adjustCB,(XtPointer) (0+16));
    XtAddCallback(CT_SATURATION,XmNdragCallback,color_tool_adjustCB,(XtPointer) 1);
    XtAddCallback(CT_SATURATION,XmNvalueChangedCallback,color_tool_adjustCB,(XtPointer) (1+16));
    XtAddCallback(CT_OFFSET,XmNdragCallback,color_tool_adjustCB,(XtPointer) 2);
    XtAddCallback(CT_OFFSET,XmNvalueChangedCallback,color_tool_adjustCB,(XtPointer) (2+16));
    XtAddCallback(CT_GAMMA,XmNdragCallback,color_tool_adjustCB,(XtPointer) 3);
    XtAddCallback(CT_GAMMA,XmNvalueChangedCallback,color_tool_adjustCB,(XtPointer) (3+16));


    DEBUG_TRACE_OUT printf( "Leaving make_color_tool_shell\n" );
    return(CT_form);
}
/****************************************************************/
void add_pixmap(unsigned char *raw_data, int pix_index, qhd_data_type qhd_data)
{
    unsigned char *new_data;
    unsigned int bpp;
    /*static char* str;*/

    DEBUG_TRACE_IN printf( "Entering add_pixmap\n" );

    new_data = NULL;

    if (verbose)
	printf("Entering add_pixmap\n");
    
    bpp=qhd_data.BPP;

    if (!(new_data=(unsigned char*)MT_malloc(image_matrix.pic_width*image_matrix.pic_height*bpp))) {
      printf("Malloc unsuccessful!\n");
      exit(13);
    }

    resize_and_recalc_pixels(raw_data, new_data, pix_index, qhd_data);
    /*use_new_color_depth(get_color_info()->depth, new_data, image_matrix.pic_width*image_matrix.pic_height);*/

    /**Copy the resized data back into the image_matrix. */
    memcpy(raw_data,new_data,
	   image_matrix.pic_width*image_matrix.pic_height*bpp);

    MT_free(new_data);

    image_matrix.img_arr[pix_index].image = 
      XCreateImage (image_matrix.dpy, DefaultVisual(image_matrix.dpy, image_matrix.screen), get_color_info()->depth, ZPixmap, 0, (char *) raw_data,
		    image_matrix.pic_width, image_matrix.pic_height, BitmapPad(image_matrix.dpy), image_matrix.pic_width*get_num_bytes());
    image_matrix.num_pics++;

    DEBUG_TRACE_OUT printf( "Entering add_pixmap\n" );
}

/****************************************************************/
void cbarExposedCB ( Widget w,
		     XtPointer clientData, 
		     XtPointer callData )
{
  DEBUG_TRACE_IN printf( "Entering cbarExposedCB\n" );

  if (XtIsRealized(cbar_w)) {
    XPutImageOneByteData(image_matrix.dpy, XtWindow(cbar_w), cbar_gc, cbar_image, 0,0,0,0,256,34);
    if (image_matrix.maxHWcmaps>1)
      make_colormap_window_children(cbar_w, get_color_info()->cmap);
  }

  DEBUG_TRACE_OUT printf( "Leaving cbarExposedCB\n" );
}
/****************************************************************/
void color_tool_adjustCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    static int i, release, mode;
    static int background;
    static int saturation;
    static int remanage_shell; /* mwf --> it automatically disappears... hmmm */

    DEBUG_TRACE_IN printf( "Entering color_tool_adjustCB\n" );

    mode = (int)clientData;

    /* This flag indicates when a slider or button has been released
       It will be used to redraw all images when not using pseudocolor */
    release = mode & 16;

    /* Get back to the "regular" mode */
    if ( release )
        mode -= 16;
    
    /* Gamma Adjust == 3 */
    if (mode==3)
      load_gamma_colormap();

    /* Reset == 4 */
    if (mode==4) {
	XtVaSetValues(CT_BACKGROUND, XmNvalue, 0, NULL);
	XtVaSetValues(CT_SATURATION, XmNvalue, 255, NULL);
	XtVaSetValues(CT_OFFSET, XmNvalue, 0, NULL);
	XtVaSetValues(CT_GAMMA, XmNvalue, 20, NULL);
	remanage_shell=1;
    } else {
      remanage_shell=0;
    }

    XtVaGetValues(CT_BACKGROUND, XmNvalue, &background, NULL);
    XtVaGetValues(CT_SATURATION, XmNvalue, &saturation, NULL);
    if ((mode==0)&&(background>=saturation)) {
	if (background==511) {
	    XtVaSetValues(CT_BACKGROUND, XmNvalue, 510, NULL);
	    XtVaSetValues(CT_SATURATION, XmNvalue, 511, NULL);
	} else
	    XtVaSetValues(CT_SATURATION, XmNvalue, background+1, NULL);
    }
    if ((mode==1)&&(background>=saturation)) {
	if (saturation==-255) {
	    XtVaSetValues(CT_BACKGROUND, XmNvalue, -255, NULL);
	    XtVaSetValues(CT_SATURATION, XmNvalue, -254, NULL);
	} else
	    XtVaSetValues(CT_BACKGROUND, XmNvalue, saturation-1, NULL);
    }
    colormap_loadrgb();

    if (image_matrix.maxHWcmaps>1) {
	for (i=0; i<image_matrix.num_pics; i++)
	    make_colormap_window_children(image_matrix.img_arr[i].draw_area, get_color_info()->cmap);
	make_colormap_window_children(cbar_w, get_color_info()->cmap);
    }
    if (remanage_shell==1)
      color_toolCB(NULL, (XtPointer) 1, NULL);

    /* If we are not using 8 bit, need to redraw the images
       with the new colormap */
    if ( get_color_info()->colortype!=PseudoColor && release )
    {
        for ( i = 0; i < image_matrix.num_pics; i++ )
            draw_preview ( i );
         
        draw_large_image();
    }
    
    DEBUG_TRACE_OUT printf( "Leaving color_tool_adjustCB\n" );
}


void refresh_all_images (void)
{
    XClearArea ( di, 
                 XtWindow ( mainWindowDrawingArea ),
                 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT,
                 False );
}


/****************************************************************/
void update_cbar(void) {
  int i, j;
  static float old_no_colors=256, new_no_colors, expansion_ratio, low_color_index;
  
  DEBUG_TRACE_IN printf( "Entering update_cbar\n" );

  new_no_colors = num_colors_avail + 1;
  expansion_ratio = new_no_colors / old_no_colors;
  low_color_index = MAX_GRAY_INDEX - new_no_colors + 1;
  
  if (!cbar_image) return;
  
  for (j=0; j<16; j++)
    for (i=0; i<256; i++) {
      (cbar_image->data)[i+j*256]=i*expansion_ratio+low_color_index+0.001;
    }

  /* Force an expose callback to redraw this area */
  cbarExposedCB(NULL, NULL, NULL);

  DEBUG_TRACE_OUT printf( "Leaving update_cbar\n" );
}
/****************************************************************/
void color_toolCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    static int first_call=1;
    static int i, j;
    static char *data;
    static XGCValues gcv;

    DEBUG_TRACE_IN printf( "Entering color_toolCB\n" );

    if ((int)clientData) {
      XtManageChild(color_tool_shell);
      /*XtPopup(XtParent(color_tool_shell), XtGrabNone);*/
      if (image_matrix.maxHWcmaps>1)
	make_colormap_window_children(cbar_w, get_color_info()->cmap);
    }
    else
    {
        /*XtPopdown(XtParent(color_tool_shell));*/
        XtUnmanageChild(color_tool_shell);
        /* If we are not using 8 bit, need to redraw the images
           with the new colormap */
        if ( get_color_info()->colortype!=PseudoColor )
        {
            for ( i = 0; i < image_matrix.num_pics; i++ )
                draw_preview ( i );
         
            draw_large_image();
        }
    }
    
    if (first_call) {
      first_call=0;
      if (!(data=(char*)MT_malloc(256*34*get_num_bytes())))
	exit(13);
      for (j=16; j<18; j++)
	for (i=0; i<256; i++) {
	  data[i+j*256]=(unsigned char)RESERVED_BLACK;
	}
      for (j=18; j<34; j++)
	for (i=0; i<256; i++) {
	  data[i+j*256]=(unsigned char)i;
	}
      /*use_new_color_depth(get_color_info()->depth, data, 256*34);*/
      cbar_image = XCreateImage (image_matrix.dpy, DefaultVisual(image_matrix.dpy, image_matrix.screen), get_color_info()->depth, ZPixmap, 0, data,
				 256, 34, BitmapPad(image_matrix.dpy), 256*get_num_bytes());
      gcv.function = GXcopy;
      cbar_gc = XCreateGC(image_matrix.dpy, XtWindow(cbar_w), GCFunction, &gcv);
      if (image_matrix.maxHWcmaps==1)
	make_colormap_window_children(color_tool_shell, get_color_info()->cmap);
      else
	make_colormap_window_children(cbar_w, get_color_info()->cmap);
      XPutImageOneByteData(image_matrix.dpy, XtWindow(cbar_w), cbar_gc, cbar_image, 0,0,0,0,256, 34);
      if (image_matrix.maxHWcmaps>1) /* do twice just in case */
	make_colormap_window_children(cbar_w, get_color_info()->cmap);
      XtAddCallback ( cbar_w, XmNexposeCallback, 
		      cbarExposedCB, NULL );
    }
    update_cbar();

    DEBUG_TRACE_OUT printf( "Leaving color_toolCB\n" );
}
/****************************************************************/
void CT_loadCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    DEBUG_TRACE_IN printf( "Entering CT_loadCB\n" );

    if (!CT_load_fsb) {
        char *ResourcePath;

        ResourcePath = (char *)getenv("SERA_RESOURCES");
	/* create the dialog box. */
	CT_load_fsb=XmCreateFileSelectionDialog(color_tool_shell,"Load Colormap", NULL, 0);
	XtVaSetValues(CT_load_fsb,
		      XmNdirMask, 
		      XmStringCreateLtoR("*.cmap", char_set),
		      NULL);
        {
	  if (!ResourcePath) {
	    printf("Expected to find environment variable SERA_RESOURCES for location of\n");
	    printf(".cmap (colormap) files.  Instead, current directory will be used.\n");
	  }
	  else /* set the directory to look in the resource directory by default */
	    XtVaSetValues(CT_load_fsb,
			  XmNdirectory, 
			  XmStringCreateLtoR(ResourcePath, char_set),
			  NULL);
        }
	XtAddCallback(CT_load_fsb,XmNokCallback,(XtCallbackProc)CT_load_fsb_doneCB,
		      (XtPointer) OK);
	XtAddCallback(CT_load_fsb,XmNcancelCallback,(XtCallbackProc)CT_load_fsb_doneCB,
		      (XtPointer) CANCEL);
	XtUnmanageChild(XmSelectionBoxGetChild(CT_load_fsb,
					       XmDIALOG_HELP_BUTTON));

	make_colormap_window_children(CT_load_fsb, get_color_info()->cmap); /* mwf 6-25-1996 */
    }

    XtManageChild(CT_load_fsb);

    DEBUG_TRACE_OUT printf( "Leaving CT_loadCB\n" );
}
/****************************************************************/
void CT_load_fsb_doneCB(Widget w,int client_data,XmSelectionBoxCallbackStruct *call_data)
{
    static char *filename;

    DEBUG_TRACE_IN printf( "Entering CT_load_fsb_doneCB\n" );

    switch (client_data)
	{
	case OK:
	    XtVaSetValues(CT_BACKGROUND, XmNvalue, 0, NULL);
	    XtVaSetValues(CT_SATURATION, XmNvalue, 255, NULL);
	    XtVaSetValues(CT_OFFSET, XmNvalue, 0, NULL);
	    /* get the string from the event structure. */
	    XmStringGetLtoR(call_data->value,char_set,&filename);
	    CT_load_cmap(filename, 1);
	    MT_free ((void*)filename);
	    break;
	case CANCEL:
	    break;
	}
    /* make the dialog box invisible */
    XtUnmanageChild(w);

    DEBUG_TRACE_OUT printf( "Leaving CT_load_fsb_doneCB\n" );
}

/****************************************************************/
void CT_load_cmap ( char* filename, int redraw )
{
    FILE *in_ptr;
    static unsigned int tmp_int;
    int i;

    DEBUG_TRACE_IN printf( "Entering CT_load_cmap\n" );

    if ((in_ptr = fopen (filename,"r"))==NULL) {
	printf("Could not open colormap file\n");
    } else {
	for (i=0; i<256*3; i++) {
	    if(fscanf(in_ptr, "%u", &tmp_int)!=EOF)
		cmap_vals[i]=(unsigned char)tmp_int;
	    else {
		printf("Warning:  Colormap file ended prematurely.\n");
		break;
	    }
	}
    }    
    fclose ( in_ptr );
    colormap_loadrgb();

    if ( get_color_info()->colortype != PseudoColor && redraw )
    {
        for ( i = 0; i < image_matrix.num_pics; i++ )
            draw_preview ( i );
         
        draw_large_image();
    }
    
    DEBUG_TRACE_OUT printf( "Leaving CT_load_cmap\n" );
}


void CT_reloadColormapCB ( Widget w, XtPointer clientData, XtPointer callData )
{
    char default_colormap_filename[256];
    Boolean view_contour_colorwash;
    /*int i;*/
    
    XtVaGetValues(view_contour_colorwash_button,
                  XmNset, &view_contour_colorwash,
                  NULL);
    
    if ( view_contour_colorwash && dosage_is_there )
    {
        strcpy ( default_colormap_filename, getenv ( "SERA_RESOURCES" ) );
        strcat ( default_colormap_filename, DoseDisplayResourceDir );
        strcat ( default_colormap_filename, DoseDisplayColorwashFile );
        CT_load_cmap ( default_colormap_filename, 1 );
    }
    else
    {
        strcpy ( default_colormap_filename, getenv ( "SERA_RESOURCES" ) );
        strcat ( default_colormap_filename, SharedColormapDir );
        strcat ( default_colormap_filename, DoseDisplayGammaFile );
        CT_load_cmap ( default_colormap_filename, 1 );
    }
}

            

/****************************************************************/
/*****************************************************************
 * this is an event handler that installs the colormap
 * on single colormap systems that is called whenever the
 * mouse cursor enters a drawable window - mwf 7-20-95
 * (make_colormap_window_children sets the event handler)
 */
void colormap_installCB(Widget w, XtPointer clientdata, XtPointer call_data)
{
    DEBUG_TRACE_IN printf( "Entering colormap_installCB\n" );

    if (get_color_info () -> depth == 8)
       XInstallColormap(XtDisplay(w), get_color_info() -> cmap);

    DEBUG_TRACE_OUT printf( "Leaving colormap_installCB\n" ); 
}
/****************************************************************/
void colormap_loadrgb(void)
{
    static int background;
    static int saturation;
    static int offset;
    static int i;
    static XColor temp_color_cell;
    static int num_colors;
    static float fincr, cumul;
    static int start, end;
    static int pixval;
    static int colors_avail;
    static int start_index;
    static int end_index;
    /*static int lastlow = 0;*/

    DEBUG_TRACE_IN printf( "Entering colormap_loadrgb\n" );

    colors_avail = num_colors_avail + 1;
    start_index = MAX_GRAY_INDEX - colors_avail + 1;
    end_index = MAX_GRAY_INDEX;

    XtVaGetValues(CT_BACKGROUND,
		  XmNvalue, &background,
		  NULL);
    XtVaGetValues(CT_SATURATION,
		  XmNvalue, &saturation,
		  NULL);
    XtVaGetValues(CT_OFFSET,
		  XmNvalue, &offset,
		  NULL);

    if (offset==256) offset = 0;
    offset=offset*colors_avail/256 + 0.5001; /* some number between 0 and (colors_avail-1) */
    background=background*colors_avail/256 + 0.5001;
    background+=start_index;
    saturation=saturation*colors_avail/256 + 0.5001;
    saturation+=start_index;

    if (background>=saturation)
	background=saturation-1;

    /* Restore to normal those outside of the range */
    /* no longer necessary
     * for (i=lastlow; (i<background)&&(i<start_index); i++) {
     *  temp_color_cell.pixel = i;
     *  myXQueryColor(image_matrix.dpy, default_colormap, &temp_color_cell);
     *  myXStoreColor(image_matrix.dpy, image_matrix.cmap, &temp_color_cell);
     * }
     * lastlow=background;
     * if (lastlow<0) lastlow=0;
     */

    /* All pixels in colormap to left of background are set to background */
    temp_color_cell.flags = DoRed | DoGreen | DoBlue;
    temp_color_cell.red = cmap_vals[0]<<8;
    temp_color_cell.green = cmap_vals[1]<<8;
    temp_color_cell.blue = cmap_vals[2]<<8;
    end=background;
    if (background>end_index)
	end=end_index+1;
    pixval=start_index+offset;
    for (i=start_index; i<end; i++) {
	if (pixval>end_index)
	    pixval-=(end_index-start_index+1);
	temp_color_cell.pixel = pixval;
	myXStoreColor(image_matrix.dpy, get_color_info()->cmap, &temp_color_cell);
	pixval++;
    }


    num_colors=saturation-background+1;
    if (num_colors>=1)
	fincr=255.0/(float)(num_colors-1);
    cumul=0.0;

    start=background;
    end=saturation;
    if (start<start_index) {
	cumul+=fincr*(start_index-start);
	start=start_index;
    }
    if (end>end_index)
	end=end_index;
    
    /* Now do the pixels between start and end, inclusive */
    pixval=start+offset;
    for (i=start; i<=end; i++) {
	if (pixval>end_index)
	    pixval-=(end_index-start_index+1);
	temp_color_cell.pixel = pixval;
	temp_color_cell.red = ((int)cmap_vals[3*(int)(cumul+.01)])<<8;
	temp_color_cell.green = ((int)cmap_vals[3*(int)(cumul+.01)+1])<<8;
	temp_color_cell.blue = ((int)cmap_vals[3*(int)(cumul+.01)+2])<<8;
	myXStoreColor(image_matrix.dpy, get_color_info()->cmap, &temp_color_cell);
	cumul+=fincr;
	pixval++;
    }


    /* All pixels in colormap to right of saturation are set to saturation */
    temp_color_cell.red = cmap_vals[255*3]<<8;
    temp_color_cell.green = cmap_vals[255*3+1]<<8;
    temp_color_cell.blue = cmap_vals[255*3+2]<<8;
    start=saturation+1;
    if (saturation<start_index)
	start=start_index;
    pixval=start+offset;
    for (i=start; i<=end_index; i++) {
	if (pixval>end_index)
	    pixval-=(end_index-start_index+1);
	temp_color_cell.pixel = pixval;
	myXStoreColor(image_matrix.dpy, get_color_info()->cmap, &temp_color_cell);
	pixval++;
    }

    DEBUG_TRACE_OUT printf( "Leaving colormap_loadrgb\n" );
}
/****************************************************************/
void load_gamma_colormap(void)
{
    static int i, j;
    static float intens;
    static int starting_index=0;
    static int delta=255;
    static float gamma_bnct, expo;
    static int tmp;

    DEBUG_TRACE_IN printf( "Entering load_gamma_colormap\n" );

    XtVaGetValues(CT_GAMMA,
		  XmNvalue, &tmp,
		  NULL);
    gamma_bnct=tmp/10.0;
    
    expo=1.0/gamma_bnct;

    for (i=0; i<256; i++) {
	intens = (int)(0.5 + 255.0 * (float)pow( (double)((float)(i - starting_index) / delta),
						 (double)expo ) );
	for (j=0; j<3; j++)
	    cmap_vals[3*i+j]=intens;
    }

    DEBUG_TRACE_OUT printf( "Leaving load_gamma_colormap\n" );
}
/****************************************************************/
void resize_and_recalc_pixels(unsigned char * data, unsigned char * output, unsigned int pix_index, qhd_data_type qhd_data)
{
    static unsigned int outwidth;
    static unsigned int outheight;
    static unsigned int inwidth;
    static unsigned int inheight;
    /*static unsigned int i;*/
    static unsigned int bpp;
    /*static unsigned int tmp;*/

    DEBUG_TRACE_IN printf( "Entering resize_and_recalc_pixels\n" );

    outwidth=image_matrix.pic_width;
    outheight=image_matrix.pic_height;

    inwidth=qhd_data.dimx;
    inheight=qhd_data.dimy;
    bpp=qhd_data.BPP;

    if (bpp==1)
      generic_resize_image(data, output, inwidth, inheight, outwidth, outheight);
    else {
      printf("Bytes per pixel must be 1.  Exiting.\n");
      exit(13);
    }
    recalc_one_image_pixels(output, output, outwidth, outheight);

    DEBUG_TRACE_OUT printf( "Leaving resize_and_recalc_pixels\n" );
}

/*******************************************************/
/* Widget placement function - the one I never modified*/
/* Sets position of widget on the form                 */
/*******************************************************/
void SetFormPercent(Widget widget, int top, int left, int right, int bottom)
{
    DEBUG_TRACE_IN printf( "Entering SetFormPercent\n" );

    XtVaSetValues(widget,
		  XmNtopAttachment, XmATTACH_POSITION,
		  XmNtopPosition, top,
		  XmNleftAttachment, XmATTACH_POSITION,
		  XmNleftPosition, left,
		  XmNrightAttachment, XmATTACH_POSITION,
		  XmNrightPosition, right,
		  XmNbottomAttachment, XmATTACH_POSITION,
		  XmNbottomPosition, bottom,
		  NULL);

    DEBUG_TRACE_OUT printf( "Leaving SetFormPercent\n" );
}

/******************************************/
/* updates ui if the image set has changed*/
/******************************************/
void reset_image_set_if_needed(int current_image_set){
   /* must be different from image_matrix.image_set initially */
   static int      image_set = -1; 

   /* Person has reset all images and is starting over.  
      Need to reset some values. */
   if (image_set!=current_image_set) {
     image_set = current_image_set;
     init_dose_factors();  /* reset values in dose factor widget */
   }
}


#endif
