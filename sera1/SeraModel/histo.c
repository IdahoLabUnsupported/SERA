#include "functions.h"
#include "sliderText.h"

void view_histogram_FCN(image_matrix_type * image_matrix_ptr) {
  static int first_call = 1;
  static Widget shell;

  DEBUG_TRACE_IN printf ( "Entering view_histogram_FCN\n");

  if (first_call) {
    first_call = 0;
    shell = make_histo_shell(image_matrix_ptr);
  }
  XtRealizeWidget(shell);

  DEBUG_TRACE_OUT printf ( "Leaving view_histogram_FCN\n");
}


Widget make_histo_shell(image_matrix_type * image_matrix_ptr) {
    Arg al[10];
    int ac = 0;
    Widget shell, histo_form, histo_title, histo_sep1;
    Widget histo_window, histo_scale, histo_sep2, histo_refresh;
    Widget histo_draw_area, histo_whichimage, histo_whichimage_textbox,
      histo_whichimage_form, toggle_all, histo_done;
    XmString xmstr;

    DEBUG_TRACE_IN printf ( "Entering make_histo_shell\n");

    /* Set some values first */
    HISTO_INFO.which_histo_image = 0;
    HISTO_INFO.see_overall_histo = 0;
    HISTO_INFO.histo_scaling = 50;
    /*************************/

    shell = XtAppCreateShell("Histogram", "shell",
			     applicationShellWidgetClass,
			     image_matrix_ptr->dpy, NULL, 0);

  /* Disable the window menu (closes program) */
  XtVaSetValues ( shell, 
		  XmNmwmDecorations, 
		  MWM_DECOR_ALL|MWM_DECOR_MENU, 
		  NULL );
/*****/
    ac=0;
    histo_form = XmCreateForm(shell, "histo_form", al, ac);
    XtManageChild(histo_form);
    
/*****/
    histo_title = XmCreateLabel(histo_form, "histo_title", NULL, 0);
    xmstr = XmStringCreateLtoR("Image Histogram", image_matrix_ptr->char_set);
    XtVaSetValues(histo_title,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNleftOffset, 5,
		  XmNrightOffset, 5,
		  XmNtopOffset, 5,
		  NULL);
    XmStringFree(xmstr);
    XtManageChild(histo_title);
    
/*****/
    histo_sep1 = XmCreateSeparator(histo_form, "histo_sep1", NULL, 0);
    XtVaSetValues(histo_sep1,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, histo_title,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNleftOffset, 5,
		  XmNrightOffset, 5,
		  XmNtopOffset, 5,
		  NULL);
    XtManageChild(histo_sep1);
    
/*****/
    ac=0;
    XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
    histo_window = XmCreateScrolledWindow(histo_form, "histo_window", al, ac);

    XtVaSetValues(histo_window,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, histo_sep1,
		  XmNleftAttachment, XmATTACH_FORM,
		  /*XmNrightAttachment, XmATTACH_FORM,*/
		  XmNwidth, 512+12,
		  XmNheight, 288+12,
		  XmNleftOffset, 5,
		  XmNtopOffset, 5,
		  NULL);
    XtManageChild(histo_window);
/**/
    histo_draw_area=XtVaCreateManagedWidget("drawingarea",
				   xmDrawingAreaWidgetClass, histo_window,
				   XmNresizable, False,
				   NULL);
    HISTO_INFO.hdraw_area = histo_draw_area;
    XtVaSetValues ( histo_draw_area, 
		    XmNborderColor, RESERVED_BLACK,
		    XmNborderWidth, 2,
		    XmNwidth, 512,
		    XmNheight, 288,
		    NULL );
    XtManageChild(histo_draw_area);
    XtAddCallback(histo_draw_area, XmNexposeCallback, histo_exposed_CB, NULL);
/*****/
    histo_scale = XmCreateScale(histo_form, "histo_scale", NULL, 0);
    xmstr = XmStringCreateLtoR("Scale", image_matrix_ptr->char_set);
    XtVaSetValues(histo_scale,
		  XmNtitleString, xmstr,
		  XmNorientation, XmVERTICAL,
		  XmNshowValue, False,
		  XmNmaximum, 100,
		  XmNminimum, 0,
		  XmNvalue, HISTO_INFO.histo_scaling,
		  XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		  XmNtopWidget, histo_window,
		  XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		  XmNbottomWidget, histo_window,
		  XmNleftAttachment, XmATTACH_WIDGET,
		  XmNleftWidget, histo_window,
		  XmNleftOffset, 10,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNrightOffset, 5,
		  NULL);
    XtManageChild(histo_scale);
    XmStringFree(xmstr);
    XtAddCallback(histo_scale,XmNvalueChangedCallback,histo_scale_CB,(XtPointer)0);
/*****/
    histo_whichimage_form = (Widget)
      CreateSliderText(&histo_whichimage, &histo_whichimage_textbox,
		       histo_form, "Select Image",
		       (int)True, 0,
		       0, 100, 0);
    XtVaSetValues(histo_whichimage_form,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, histo_window,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNleftOffset, 5,
		  XmNrightOffset, 5,
		  XmNtopOffset, 5,
		  NULL);
    XtManageChild(histo_whichimage_form);
/*****/
    toggle_all = XmCreateToggleButton(histo_form, "toggle_all", NULL, 0);
    xmstr = XmStringCreateLtoR("Histogram of Image Set", image_matrix_ptr->char_set);
    XtVaSetValues(toggle_all,
		  XmNset, False,
		  XmNlabelString, xmstr,
		  XmNleftAttachment, XmATTACH_FORM,
		  /*XmNrightAttachment, XmATTACH_FORM,*/
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, histo_whichimage_form,
		  XmNtopOffset, 5,
		  XmNleftOffset, 5,
		  NULL);
    XtAddCallback(toggle_all,XmNvalueChangedCallback,histo_all_CB,(XtPointer)0);
    XmStringFree(xmstr);
    XtManageChild(toggle_all);

/*****/
    histo_sep2 = XmCreateSeparator(histo_form, "histo_sep2", NULL, 0);
    XtVaSetValues(histo_sep2,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, toggle_all,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNleftOffset, 5,
		  XmNrightOffset, 5,
		  XmNtopOffset, 5,
		  NULL);
    XtManageChild(histo_sep2);
    
/*****/
    histo_done = XmCreatePushButton(histo_form, "histo_done", NULL, 0);
    xmstr = XmStringCreateLtoR("Dismiss", image_matrix_ptr->char_set);
    XtVaSetValues(histo_done,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, histo_sep2,
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  XmNbottomOffset, 5,
		  XmNrightOffset, 5,
		  XmNtopOffset, 5,
		  NULL);
    XtManageChild(histo_done);
    XmStringFree(xmstr);
    XtAddCallback(histo_done,XmNactivateCallback,histo_done_CB,(XtPointer) shell);
/*****/
    histo_refresh = XmCreatePushButton(histo_form, "histo_refresh", NULL, 0);
    xmstr = XmStringCreateLtoR("Refresh", image_matrix_ptr->char_set);
    XtVaSetValues(histo_refresh,
		  XmNlabelString, xmstr,
		  XmNtopAttachment, XmATTACH_WIDGET,
		  XmNtopWidget, histo_sep2,
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_WIDGET,
		  XmNrightWidget, histo_done,
		  XmNbottomOffset, 5,
		  XmNrightOffset, 5,
		  XmNtopOffset, 5,
		  NULL);
    XtManageChild(histo_refresh);
    XmStringFree(xmstr);
    XtAddCallback(histo_refresh,XmNactivateCallback,histo_refresh_CB,(XtPointer) 0);
/*****/
    /*XtAddCallback(histo_whichimage,XmNdragCallback,histo_whichimage_CB,(XtPointer) 0);*/
    XtAddCallback(histo_whichimage,XmNvalueChangedCallback,histo_whichimage_CB,(XtPointer) 0);

    DEBUG_TRACE_OUT printf ( "Leaving make_histo_shell\n");
    return(shell);
}


void histo_refresh_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering histo_refresh_CB\n");
  draw_histo();
  DEBUG_TRACE_OUT printf ( "Leaving histo_refresh_CB\n");
}

void histo_done_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering histo_done_CB\n");
  XtUnrealizeWidget((Widget)clientData);
  DEBUG_TRACE_OUT printf ( "Leaving histo_done_CB\n");
}

/* This is the function that determines if we are viewing the histogram
 * of the overall image set or the histogram of an individual image
 * in the set
 */
void histo_all_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  Boolean set;

  DEBUG_TRACE_IN printf ( "Entering histo_all_CB\n");

  XtVaGetValues(widget,
		XmNset, &set,
		NULL);
  if (set==True) {
    HISTO_INFO.see_overall_histo = 1;
  } else {
    HISTO_INFO.see_overall_histo = 0;
  }
  draw_histo();

  DEBUG_TRACE_OUT printf ( "Leaving histo_all_CB\n");
}


void histo_scale_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  DEBUG_TRACE_IN printf ( "Entering histo_scale_CB\n");
  XtVaGetValues(widget,
		XmNvalue, &HISTO_INFO.histo_scaling,
		NULL);
  draw_histo();
  DEBUG_TRACE_OUT printf ( "Leaving histo_scale_CB\n");
}

void histo_whichimage_CB(Widget widget, XtPointer clientData, XtPointer callData) {
  int whichimage;
  int numpics, top, max;
  image_matrix_type * image_matrix_ptr;

  DEBUG_TRACE_IN printf ( "Entering histo_whichimage_CB\n");

  image_matrix_ptr = get_image_matrix();
  numpics = image_matrix_ptr->num_pics;
  if (numpics<=1) top = 1;
  else top=numpics-1;

  XtVaGetValues(widget,
		XmNvalue, &whichimage,
		XmNmaximum, &max,
		NULL);
  if ((whichimage>=0)&&(whichimage<numpics)) {
  } else {
    whichimage = 0;
    XtVaSetValues(widget,
		  XmNvalue, whichimage,
		  NULL);
  }
  if (max!=top) {
    XtVaSetValues(widget,
		  XmNmaximum, top,
		  NULL);
  }

  if (whichimage!=HISTO_INFO.which_histo_image) {
    HISTO_INFO.which_histo_image = whichimage;
    if (!HISTO_INFO.see_overall_histo) {
      draw_histo();
    }
  }

  DEBUG_TRACE_OUT printf ( "Leaving histo_whichimage_CB\n");
}


void histo_exposed_CB(Widget draw_area, XtPointer clientData, XtPointer unused2) {
  DEBUG_TRACE_IN printf ( "Entering histo_exposed_CB\n");
  draw_histo();
  DEBUG_TRACE_OUT printf ( "Leaving histo_exposed_CB\n");
}

void draw_histo(void) {
  image_matrix_type * image_matrix_ptr;
  int i, j, bytes;
  static int first_call = 1;
  static unsigned char * data;
  static XImage * histo_image;
  static GC histo_gc;
  static XGCValues gcv;

  DEBUG_TRACE_IN printf ( "Entering draw_histo\n");

  image_matrix_ptr = get_image_matrix();

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
      DEBUG_TRACE_OUT printf ( "Leaving draw_histo\n");
      return;
      break;
    }
  
  /* On the first call, create the XImage and GC for the colorbar and
   * make sure its colors get set up correctly
   */
  if (first_call) {
    first_call = 0;
    if (!(data=(unsigned char*)MT_malloc(512*288*bytes)))
      exit(EXIT_FAILURE);
    histo_image = XCreateImage (image_matrix_ptr->dpy,
			       DefaultVisual(image_matrix_ptr->dpy,
					     image_matrix_ptr->screen),
			       get_color_info()->depth, ZPixmap, 0,
			       (char *) data,
			       512, 288, BitmapPad(image_matrix_ptr->dpy), 512*bytes);
    gcv.function = GXcopy;
    histo_gc = XCreateGC(image_matrix_ptr->dpy, XtWindow(HISTO_INFO.hdraw_area), GCFunction, &gcv);
    
    make_colormap_window_children(HISTO_INFO.hdraw_area, get_color_info()->cmap);
  }

  for (j=0; j<256; j++)
    for (i=0; i<512; i++) {
      data[i+j*512]=RESERVED_BLACK;
    }

  for (j=256; j<288; j++)
    for (i=0; i<512; i++) {
      data[i+j*512]=(unsigned char)(i/2);
    }

  make_histogram(data, 512, 256, HISTO_INFO.which_histo_image, HISTO_INFO.see_overall_histo);

  use_new_color_depth(get_color_info()->depth,
		      (unsigned char *)histo_image->data, 512*288);
  XPutImage(image_matrix_ptr->dpy, XtWindow(HISTO_INFO.hdraw_area), histo_gc, histo_image, 0,0,0,0,512,288);

  DEBUG_TRACE_OUT printf ( "Leaving draw_histo\n");
}


void make_histogram(unsigned char * data, int w, int h,
		    int whichimage, int see_all_images) {
  int histo[256], /*sorted_histo[256],*/ plot_data[256];
  int i, j, k;
  unsigned char * view_data;
  image_matrix_type * image_matrix_ptr;
  int numpics;
  int data_w, data_h, data_wh;
  float div_factor;
  int cur_height;
  int cur_max, cur_index, tmp;
  int total_pixels = 0;
  
  DEBUG_TRACE_IN printf ( "Entering make_histogram\n");

  image_matrix_ptr = get_image_matrix();
  numpics = image_matrix_ptr->num_pics;
  
  if ((whichimage<0)||(whichimage>=numpics)) {
    DEBUG_TRACE_OUT printf ( "Leaving make_histogram\n");
    return;
  }

  for (i=0; i<256; i++) {
    histo[i] = 0;
  }

  /* Loop through all images.  We need to do this in case we are looking
   * for the 'overall' image set histogram.  If not, we only look at
   * histogram of 'whichimage'
   */
  for (k=0; k<numpics; k++) {
    if ((k==whichimage)||(see_all_images)) {
      view_data = image_matrix_ptr->img_arr[k].pimage_data;
      data_w = image_matrix_ptr->img_arr[k].data_w;
      data_h = image_matrix_ptr->img_arr[k].data_h;
      data_wh = data_w*data_h;
      total_pixels+=data_wh;
      
      for (i=0; i<data_wh; i++) {
	histo[(int)(view_data[i])]++;
      }
    }
  }

  /* Actually, don't need to do this anymore... */
  /* Make copy of histogram for sorting */
  /*for (i=0; i<256; i++) {
    sorted_histo[i]=histo[i];
  }*/

  /* Sort the histogram */
  /*for (i=255; i>=1; i--) {
    cur_max = -1;
    for (j=0; j<=i; j++) {
      if (sorted_histo[j]>cur_max) {
	cur_max = sorted_histo[j];
	cur_index = j;
      }
    }
    tmp = sorted_histo[i];
    sorted_histo[i] = sorted_histo[cur_index];
    sorted_histo[cur_index] = tmp;
  }*/

  /*tmp = sorted_histo[250];*/
  /*div_factor = ((float)tmp)/((float)h)*1.2;*/

  /* Actually, don't need to do this anymore... */
  /* Need to do some good scaling --
   *   histo_scaling -- 50 should be good overall
   *                    0  should be 0 height
   *                   100 should be _very_ tall
   *   total_pixels  -- total number of pixels in histogram
   *   h             -- height (in pixels) of histogram
   *   256           -- max number of grey-levels in histogram
   *                    128 is more likely
   *   
   *   Want average height about:  h/4
   *   number pixels at arbitray grey-level is about:  total_pixels/128=N
   *   Want N/div_factor = h/4 --> div_factor = 4*N/h for histo_scaling=50
   *   Or, div_factor = total_pixels/(32*h)
   */
  div_factor = (float)total_pixels/((float)32*h);
  /* Now, we want a larger div_factor if histo_scaling is small
   * and a low div_factor if histo_scaling is large -- and stay the
   * same if histo_scaling is 50.
   * so, multiply by 51*51/(1+scaling)^2
   */
  div_factor = div_factor*(2601.0/((float)((1+HISTO_INFO.histo_scaling)*(1+HISTO_INFO.histo_scaling))));

  /* Now, the max height of any bar is h */
  /* The width is w (assumed 512) so we need to map all
   * 256 colors into these 512 -- so do each twice
   */
  for (i=0; i<256; i++) {
    plot_data[i] = (int)(((float)histo[i])/div_factor);
    if (plot_data[i]>h) plot_data[i]=h;
  }

  for (i=0; i<256; i++) {
    for (j=0; j<plot_data[i]; j++) {
      cur_height = (h-1-j);
      data[2*i+cur_height*w] = RESERVED_WHITE;
      data[2*i+1+cur_height*w] = RESERVED_WHITE;
    }
  }

  DEBUG_TRACE_OUT printf ( "Leaving make_histogram\n");
}

