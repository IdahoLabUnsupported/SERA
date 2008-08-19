#include "toqsh.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Mike Frandsen
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void myXStoreColor(main_gui_t *gui, Colormap cmap, XColor * color) {
  myXStoreColors(gui, cmap, color, 1);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Mike Frandsen modified by CLA
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void myXStoreColors(main_gui_t *gui, Colormap cmap, XColor * color,int num) {
  int i, index;
  Colormap default_cmap;

/*  DEBUG_TRACE_IN printf("Entered myXStoreColors\n");*/

  default_cmap = DefaultColormap(gui->display,gui->screen);

  for (i=0; i<num; i++) {
    index = color[i].pixel;
    gui->color_info.mycmap[index].pixel = color[i].pixel;
    gui->color_info.mycmap[index].flags = color[i].flags;
    gui->color_info.mycmap[index].red   = color[i].red;
    gui->color_info.mycmap[index].green = color[i].green;
    gui->color_info.mycmap[index].blue  = color[i].blue;

    if (gui->color_info.colortype!=PseudoColor) {
      XAllocColor(gui->display,default_cmap,&color[i]);
      gui->color_info.cmap_pixels[index] = color[i].pixel;
    } else {
      gui->color_info.cmap_pixels[index] = index;
    }
  }

  if (gui->color_info.colortype!=PseudoColor){ 
    /*DEBUG_TRACE_OUT printf("Done with myXStoreColors\n");*/
    return;
  }

  XStoreColors(gui->display, cmap, color, num);

/*  DEBUG_TRACE_OUT printf("Done with myXStoreColors\n");*/
}

/* Use this in place of XQueryColor
 * color->pixel is important
 * IF Pseudocolor, uses standard call to XQueryColor
 * IF NOT, queries color by using color_info.mycmap
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Mike Frandsen
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void myXQueryColor(main_gui_t *gui, Colormap cmap, XColor * color) {
  myXQueryColors(gui, cmap, color, 1);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Mike Frandsen
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void myXQueryColors(main_gui_t *gui, Colormap cmap, XColor * color, int num) {
  int index, i;

  DEBUG_TRACE_IN printf("Entered myXQueryColors\n");

  if (gui->color_info.colortype!=PseudoColor) {
    for (i=0; i<num; i++) {
      index = color[i].pixel; 
      color[i].pixel = gui->color_info.mycmap[index].pixel;
      color[i].flags = gui->color_info.mycmap[index].flags;
      color[i].red   = gui->color_info.mycmap[index].red;
      color[i].green = gui->color_info.mycmap[index].green;
      color[i].blue  = gui->color_info.mycmap[index].blue;
    }
  } else {
    XQueryColors(gui->display, cmap, color, num);
  }

  DEBUG_TRACE_OUT printf("Done with myXQueryColors\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Mike Frandsen, modified by Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_colors(main_gui_t *gui) {
  static Colormap default_cmap;
  static int first_call = 1;
  static XColor def_colors[256];
  XVisualInfo visual_info;
  Display *display;
  int screen_num;
  int default_depth;
  int i, success = 0;
  XColor temp_color_cell;
  int ncolors, num_desired=256;
  unsigned long colors[256], plane_masks[1]={0};

  DEBUG_TRACE_IN printf("Entered init_colors\n");


  gui->color_info.background = 0;
  gui->color_info.saturation = 255;
  gui->color_info.offset = 0;
  gui->color_info.gamma = 20;

  gui->color_info.maxHWcolormaps = MaxCmapsOfScreen(DefaultScreenOfDisplay(gui->display));
  printf("Your system has : %d colormaps\n",gui->color_info.maxHWcolormaps);

  for (i=0; i<256; i++) {
    gui->color_info.cmap_pixels[i]=i;
  }

  gui->color_info.cmap  = (Colormap)NULL;
  gui->color_info.depth = DefaultDepth(gui->display, gui->screen);

  switch(gui->color_info.depth)
    {
    /* supported depths */
    case 8:
    case 16:
    case 24:
    case 32:
      if ((gui->color_info.depth == 8 ) && 
	  (XMatchVisualInfo(gui->display, gui->screen, 
			    gui->color_info.depth, PseudoColor, &gui->visinfo))) {
	printf("Using PseudoColor of depth %d\n", gui->color_info.depth);
	gui->color_info.colortype=PseudoColor;
        gui->visual = gui->visinfo.visual;
	success = 1;
      } else if (XMatchVisualInfo(gui->display, gui->screen, gui->color_info.depth, TrueColor, &gui->visinfo)) {
	printf("Using TrueColor of depth %d\n", gui->color_info.depth);
	gui->color_info.colortype=TrueColor;
        gui->visual = gui->visinfo.visual;
	success = 1;
      }
      break;
    default:
      break;
    }

  if (!success) {
    fprintf(stderr, "Sorry, your default display is not one of the supported\n\
color modes -- PseudoColor of depth 8 or TrueColor of depth 8, 16, 24, or 32.\n");
    exit(EXIT_FAILURE);
  }

  if (first_call) {
    first_call = 0;
    if (gui->color_info.colortype==PseudoColor) {
      default_cmap = DefaultColormap(gui->display, gui->screen);
      for (i=0; i<256; i++) {
	def_colors[i].pixel = (unsigned long)i;
      }
      XQueryColors(gui->display, default_cmap, def_colors, num_desired);
      XFreeColormap(gui->display, default_cmap);
      gui->color_info.cmap =XCreateColormap(gui->display, RootWindow(gui->display, gui->screen),
					    gui->visual,
					    AllocAll);
      myXStoreColors(gui, gui->color_info.cmap, def_colors, num_desired);
      /*printf("the gui->colormap is : %d\n",gui->color_info.cmap);*/
    }
  }

  DEBUG_TRACE_OUT printf("Done with init_colors\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Mike Frandsen
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void use_new_color_depth(main_gui_t *gui, unsigned char * in_data,
			 unsigned int memsize) {
  static unsigned int i, top;
  static unsigned short * sdata;
  static unsigned long * ldata;
  static unsigned char * data;
  static unsigned long * truecolors;

  DEBUG_TRACE_IN printf("Entered use_new_color_depth\n");

  if (gui->color_info.depth==8){
    DEBUG_TRACE_OUT printf("Done with use_new_color_depth\n");
    return;
  }
  truecolors = gui->color_info.cmap_pixels;
  top = memsize-1;
  data = in_data+top;

  switch(gui->color_info.depth)
    {
    case 8:
      break;
    case 16:
      sdata = ((unsigned short *)in_data) + top;
      for (i=0; i<memsize; i++) {
	*sdata-- = (unsigned short)(truecolors[*data--]);
      }
      break;
    case 24:
    case 32:
      ldata = ((unsigned long *)in_data) + top;
      for (i=0; i<memsize; i++) {
	*ldata-- = truecolors[*data--];
      }
      break;
    default:
      break;
    }
  DEBUG_TRACE_OUT printf("Done with use_new_color_depth\n");
}


/* return the number of bytes for each pixel in image */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Mike Frandsen, modified by Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int get_num_bytes(main_gui_t *gui) {
  int bytes;

  switch(gui->color_info.depth)
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
      bytes = 1; /* really, this is undefined */
      break;
    }
  return(bytes);
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Mike Frandsen, modified by Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void myPutImage(main_gui_t *gui, Window wi, GC gc, XImage * ximage_src,
		int src_x, int src_y,
		int dst_x, int dst_y,
		int width, int height) {
  int i, j, src_width, src_height, dst_pos, src_pos, eoln_incr,
    bytes;
  unsigned long int memsize;
  XImage * ximage_dst;
  unsigned char * ximage_dst_data, * ximage_src_data;

  DEBUG_TRACE_IN printf("Entered myPutImage\n");
  
  /* If the depth wasn't set up for the screen, just do the
   * default XPutImage -- also, just do the default if the
   * color depth is 8 since we already have byte data
   */
  if (/*(ximage_src->depth != gui->color_info.depth) ||*/ (gui->color_info.depth==8)) {
    /* do nothing special in this case */
    /*
      printf("just calling XPutImage\n");
      
      printf("display: %d, wi : %d, gc : %d, ximage_src : %d, src_x : %d, src_y : %d, dst_x : %d, dst_y: %d, width : %d, height: %d\n",
      gui->display, wi, gc, ximage_src, src_x, src_y, dst_x, dst_y, width, height);
    */
    /*printf("ok here is the call\n");*/
    XPutImage(gui->display, wi, gc, ximage_src,
	      src_x, src_y, dst_x, dst_y, width, height);
    /*printf("done\n");*/
    DEBUG_TRACE_OUT printf("Done with myPutImage\n");
    return;
  }
  
  /*printf("have to do the special stuff now\n");*/
  ximage_src_data = (unsigned char *)ximage_src->data;
  src_width = ximage_src->width;
  src_height = ximage_src->height;
  
  /* Decrease width and height if they move OFF the image */
  if (src_x+width>src_width) width=src_width-src_x;
  if (src_y+height>src_height) height=src_height-src_y;
  
  /* don't do anything in this case */
  if ((width<=0)||(height<=0)){
    DEBUG_TRACE_OUT printf("Done with myPutImage\n");
    return;
  }
  memsize = width*height*sizeof(unsigned char);
  bytes = get_num_bytes(gui);
  
  if ((memsize*bytes<=0)||(height!=(int)((memsize*bytes)/(width*bytes)))||(width!=(int)((memsize*bytes)/(height*bytes)))) {
    printf("Memsize is 0 or negative.  Cannot draw!\n");
    DEBUG_TRACE_OUT printf("Done with myPutImage\n");
    return;
  }

  ximage_dst_data = (unsigned char *)XtMalloc(memsize*bytes);
  if (!ximage_dst_data) {
    printf("Out of memory.\n");
    DEBUG_TRACE_OUT printf("Done with myPutImage\n");
    return;
  }

  dst_pos = 0;
  src_pos = src_y * src_width + src_x;
  eoln_incr = src_width-width;
  for (j=0; j<height; j++) {
    for (i=0; i<width; i++) {
      ximage_dst_data[dst_pos++] = ximage_src_data[src_pos++];
    }
    src_pos+=eoln_incr;
  }

  use_new_color_depth(gui,ximage_dst_data, memsize);

  ximage_dst = XCreateImage (gui->display,
			     DefaultVisual(gui->display, DefaultScreen(gui->display)),
			     gui->color_info.depth,
			     ZPixmap, 0, (char *)ximage_dst_data,
			     width, height,
			     BitmapPad(gui->display), width*bytes);
  /*printf("calling XPutImage after using use_new_color_depth\n");*/
  XPutImage(gui->display, wi,
	    gc, ximage_dst,
	    0, 0, dst_x, dst_y, width, height);

  XDestroyImage(ximage_dst);

  /* Free the destination data.
   * This memory was actually freed in XDestroyImage, but
   * to make memory_tools know about it we'll call MT_free
   * with a NULL pointer
   */
  /*ximage_dst_data = NULL;
    MT_free( (void *) ximage_dst_data );*/

  DEBUG_TRACE_OUT printf("Done with myPutImage\n");
}


/* mwf:  7-12-95 - function added to install the RESERVED
 * colors (red, green, blue, etc.) into the passed colormap 
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Mike Frandsen
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void add_guaranteed_colors(main_gui_t *gui)
{
  XColor             color;
  int i;
  /*
  program_defaults_type * program_defaults_ptr;
  program_defaults_ptr = get_program_defaults();
  */

  color.flags = DoRed | DoGreen | DoBlue;

  /*  
  color.pixel=BORDER_COLOR;
  color.red = 65000;
  color.green = 65000;
  color.blue = 0;
  myXStoreColor(gui, gui->color_info.cmap, &color);
  */

  /* Add in the RESERVED colors. */
  color.pixel=RESERVED_RED;
  color.red = 65535;
  color.green = 0;
  color.blue = 0;
  myXStoreColor(gui, gui->color_info.cmap, &color);
  
  /*
  color.pixel = MAX_GRAY_INDEX+1; 
  myXStoreColor(gui, gui->color_info.cmap, &color);
  */  

  color.pixel=RESERVED_GREEN;
  color.red = 0;
  color.green = 65535;
  color.blue = 0;
  myXStoreColor(gui, gui->color_info.cmap, &color);
  
  color.pixel=RESERVED_BLUE;
  color.red = 0;
  color.green = 0;
  color.blue = 65000;
  myXStoreColor(gui, gui->color_info.cmap, &color);
  
  color.pixel=RESERVED_CYAN;
  color.red = 0;
  color.green = 65000;
  color.blue = 65000;
  myXStoreColor(gui, gui->color_info.cmap, &color);
  
  /*
  color.pixel = MAX_GRAY_INDEX+2; 
  myXStoreColor(gui, gui->color_info.cmap, &color);
  */  

  color.pixel=RESERVED_MAGENTA;
  color.red = 65000;
  color.green = 0;
  color.blue = 65000;
  myXStoreColor(gui, gui->color_info.cmap, &color);
  
  color.pixel=RESERVED_YELLOW;
  color.red = 65000;
  color.green = 65000;
  color.blue = 0;
  myXStoreColor(gui, gui->color_info.cmap, &color);
  /*
  color.pixel=RESERVED_BLACK;
  color.red = 0;
  color.green = 0;
  color.blue = 0;
  myXStoreColor(gui, gui->color_info.cmap, &color);
  
  color.pixel=RESERVED_WHITE;
  color.red = 65000;
  color.green = 65000;
  color.blue = 65000;
  myXStoreColor(gui, gui->color_info.cmap, &color);
  
  /*
  color.pixel = MAX_GRAY_INDEX+1; 
  myXStoreColor(gui, gui->color_in fo.cmap, &color);
  */
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Mike Frandsen
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void load_gamma_colormap(main_gui_t *gui, unsigned char *passed_cmap, float gamma_bnct)
{
  static int i, j;
  static float intens;
  static int starting_index=0;
  static int delta=255;
  static float expo;
  
  expo=1.0/gamma_bnct;
  
  for (i=0; i<256; i++) {
    intens = (int)(0.5 + 255.0 * (float)pow( (double)((float)(i - starting_index) / delta), (double)expo ) );
    for (j=0; j<3; j++)
      passed_cmap[3*i+j]=intens; /* RGB equal --> gray level map */
  }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Mike Frandsen
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void colormap_load_rgb(main_gui_t *gui)
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
    static unsigned char * cmap_vals;

    cmap_vals = gui->color_info.cmap_values;

    colors_avail = NUM_GRAYS;/*GRAYS_AVAILABLE;*/
    start_index = MIN_GRAY;
    end_index = MAX_GRAY;

    DEBUG_DATA printf("start index is %d\n", start_index);
    DEBUG_DATA printf("end index is %d\n", end_index);

    background = gui->color_info.background;
    saturation = gui->color_info.saturation;
    offset     = gui->color_info.offset;

    if (offset==256) offset = 0;
    offset=offset*colors_avail/256 + 0.5001; /* some number between 0 and (colors_avail-1) */
    background=background*colors_avail/256 + 0.5001;
    background+=start_index;
    saturation=saturation*colors_avail/256 + 0.5001;
    saturation+=start_index;

    if (background>=saturation)
	background=saturation-1;

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
      DEBUG_DATA printf("Going to allocate color %d\n", pixval);
      myXStoreColor(gui, gui->color_info.cmap, &temp_color_cell);
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
      myXStoreColor(gui, gui->color_info.cmap, &temp_color_cell);
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
      myXStoreColor(gui, gui->color_info.cmap, &temp_color_cell);
      pixval++;
    }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : make_color_tool_shell
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void Install_ColormapEH(Widget w, XtPointer clientdata,
			XEvent *event, Boolean *flag)
{
  main_gui_t *gui = (main_gui_t *)clientdata;
  
  DEBUG_TRACE_IN printf("Entered Install_ColormapEH\n");

  XInstallColormap(XtDisplay(w), gui->color_info.cmap);

  DEBUG_TRACE_OUT printf("Done with Install_ColormapEH\n");
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : register_colormap_ehs_for_widget
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: 
%%%
%%%  Purpose:
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void register_colormap_ehs_for_widget(main_gui_t *gui,Widget w, Colormap cmap)
{
    static int initial_call = 1;	
    static int maxHWcmaps;
    static Window wi;
    /*printf("entered register_colormap_ehs_for_widget\n");*/
    /* On the first call to this function, install the sent colormap
     * on systems that have only one hardware colormap.
     */

    if (gui->color_info.colortype != PseudoColor){
      DEBUG_DATA printf("The colortype is not PseudoColor, so just skipping out of the procedure\n");
      return;
    }
    if (initial_call) {
      /*maxHWcmaps = MaxCmapsOfScreen(DefaultScreenOfDisplay(gui->display));
      printf("your system has : %d colormaps\n",maxHWcmaps);*/
      if (gui->color_info.maxHWcolormaps == 1){
	DEBUG_DATA printf("INSTALLING the Colormap : %ld\n",gui->color_info.cmap);
	XInstallColormap(gui->display, cmap);
      }
      initial_call = 0;
    }
    
    if (XtIsRealized(w)) {
      if (gui->color_info.maxHWcolormaps == 1){
	DEBUG_DATA printf("adding the Event Handler for the widget\n");
	XtAddEventHandler(w, EnterWindowMask, False,
			  (XtEventHandler)Install_ColormapEH, (XtPointer)gui);
      }else {
	wi = XtWindow(w);

        DEBUG_DATA printf("calling XSetWMColormapWindows\n");
	XSetWMColormapWindows(gui->display, XtWindow(gui->toplevel), &wi, 1);
	XSetWindowColormap(gui->display, XtWindow(w), cmap);
      }
    }else
      DEBUG_DATA printf("The widget passed in was not Realized\n");
      
    
    if (XtIsComposite(w)) {
      WidgetList theList;
      Cardinal listCount;
      int i;
      
      DEBUG_DATA printf("The widget was composite going to register for all its children\n");

      XtVaGetValues(w, XmNnumChildren, &listCount,
		    XmNchildren, &theList, NULL); 
      for (i = 0; i < listCount; i ++ )
	if (XtIsWidget(theList[i])){ 
	  register_colormap_ehs_for_widget(gui,theList[i], cmap);
	}
    }
    
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : 
%%%
%%%  Written by: Mike Frandsen
%%%
%%%  Parameters: 
%%%
%%%  Purpose: 
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void print_supported_visuals(Display * display) {
  XVisualInfo *visualList;
  XSetWindowAttributes attributes;
  XVisualInfo vTemplate;
  int i, visualsMatched;

  DEBUG_TRACE_IN printf("Entered print_supported_visuals\n");

  visualList = XGetVisualInfo(display, 0, &vTemplate, &visualsMatched);

  printf("\n**************************************************\n");
  printf("Number of supported visuals is %d\n\n", visualsMatched);
  
  for (i=0; i<visualsMatched; i++) {
    printf("Visual %d/%d\n", i+1, visualsMatched);
    printf("  Type         :  ");
    switch(visualList[i].class) {
    case DirectColor:
      printf("DirectColor\n");
      break;
    case GrayScale:
      printf("GrayScale\n");
      break;
    case PseudoColor:
      printf("PseudoColor\n");
      break;
    case StaticColor:
      printf("StaticColor\n");
      break;
    case StaticGray:
      printf("StaticGray\n");
      break;
    case TrueColor:
      printf("TrueColor\n");
      break;
    default:
      printf("Unknown.\n");
      break;
    }
    printf("  Bits         :  %d\n", visualList[i].depth);
    printf("  colormap_size:  %d\n", visualList[i].colormap_size);
    printf("  bits_per_rgb :  %d\n", visualList[i].bits_per_rgb);
  }
  
  XFree(visualList);
  printf("\n**************************************************\n");

  DEBUG_TRACE_OUT printf("Done with print_supported_visuals\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%%  Procedure : init_color
%%%
%%%  Written by: Cory Albright
%%%
%%%  Parameters: r,g,b, and the Xcolor returned
%%%
%%%  Purpose: given a r,g,b values, an XColor matching the 
%%%           values is returned, if the system does not have 
%%%           the XColor, the closest one is returned (256 colors)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void init_color(main_gui_t *gui, int r, int g, int b, XColor *color)
{
  static int first_call = 1;
  static Colormap cmap;

  DEBUG_TRACE_IN printf("Entered init_color\n");

  if (first_call)
    {
      cmap = XDefaultColormap(gui->display, DefaultScreen(gui->display));
      first_call = 0;
    }
  color->red = r; 
  color->green = g;
  color->blue = b;
  color->flags = DoRed | DoGreen | DoBlue;
  XAllocColor(XtDisplay(gui->mainwindow),cmap, color);

  DEBUG_TRACE_OUT printf("Done with init_color\n");
}
