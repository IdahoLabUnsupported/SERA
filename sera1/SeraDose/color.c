#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <Xm/Xm.h>
#include "memory_tools.h"
#include "debug_tools.h"
#include "color.h"

color_t color_info;

/* Returns ptr to the color.c global variable:  color_info */
color_t * get_color_info(void) {
  return(&color_info);
}

/* Prints information about the supported visuals. */
void print_supported_visuals(Display * display) {
  XVisualInfo *visualList;
  XSetWindowAttributes attributes;
  XVisualInfo vTemplate;
  int i, visualsMatched;

  DEBUG_TRACE_IN printf( "Entering print_supported_visuals\n" );

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

  DEBUG_TRACE_OUT printf( "Leaving print_supported_visuals\n" );
}

/* Use this in place of XStoreColor
 * * This function just calls myXStoreColors
 */
void myXStoreColor(Display * display, Colormap cmap, XColor * color) {
  myXStoreColors(display, cmap, color, 1);
}

/* Use this in place of XStoreColors */
void myXStoreColors(Display * display, Colormap cmap, XColor * color,
		    int num) {
  int i, index;
  Colormap default_cmap;

  default_cmap = DefaultColormap(color_info.dpy,
				 color_info.screen);

  for (i=0; i<num; i++) {
    index = color[i].pixel;
    color_info.mycmap[index].pixel = color[i].pixel;
    color_info.mycmap[index].flags = color[i].flags;
    color_info.mycmap[index].red = color[i].red;
    color_info.mycmap[index].green = color[i].green;
    color_info.mycmap[index].blue = color[i].blue;
    if (color_info.colortype!=PseudoColor) {
      XAllocColor(color_info.dpy,
		  default_cmap,
		  &(color[i]));
      color_info.truecolors[index] = color[i].pixel;
    } else {
      color_info.truecolors[index] = index;
    }
  }

  if (color_info.colortype!=PseudoColor)
  {
     return;
  }

  XStoreColors(display, cmap, color, num);
}

/* Use this in place of XQueryColor
 * color->pixel is important
 * IF Pseudocolor, uses standard call to XQueryColor
 * IF NOT, queries color by using color_info.mycmap
 */
void myXQueryColor(Display * display, Colormap cmap, XColor * color) {
  myXQueryColors(display, cmap, color, 1);
}

void myXQueryColors(Display * display, Colormap cmap, XColor * color, int num) {
  int index, i;

  if (color_info.colortype!=PseudoColor) {
    for (i=0; i<num; i++) {
      index = color[i].pixel; 
      color[i].pixel = color_info.mycmap[index].pixel;
      color[i].flags = color_info.mycmap[index].flags;
      color[i].red = color_info.mycmap[index].red;
      color[i].green = color_info.mycmap[index].green;
      color[i].blue = color_info.mycmap[index].blue;
    }
  } else {
    XQueryColors(display, cmap, color, num);
  }
}

void init_colors(Widget toplevel) {
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

  DEBUG_TRACE_IN printf( "Entering init_colors\n" );

  for (i=0; i<256; i++) {
    color_info.truecolors[i]=i;
  }

  color_info.cmap = (Colormap)NULL;
  color_info.dpy = display = XtDisplay(toplevel);
  color_info.screen = screen_num = DefaultScreen(display);
  color_info.depth = default_depth = DefaultDepth(display, screen_num);

  switch(default_depth)
    {
    /* supported depths */
    case 8:
    case 16:
    case 24:
    case 32:
      if ((default_depth==8)&&(XMatchVisualInfo(display, screen_num, default_depth, PseudoColor, &visual_info))) {
	printf("Using PseudoColor of depth %d\n", default_depth);
	color_info.colortype=PseudoColor;
	success = 1;
      } else if (XMatchVisualInfo(display, screen_num, default_depth, TrueColor, &visual_info)) {
	printf("Using TrueColor of depth %d\n", default_depth);
	color_info.colortype=TrueColor;
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
    if (color_info.colortype==PseudoColor) {
      default_cmap = DefaultColormap(display, screen_num);
      for (i=0; i<256; i++) {
	def_colors[i].pixel = (unsigned long)i;
      }
      XQueryColors(display, default_cmap, def_colors, num_desired);
      XFreeColormap(display, default_cmap);
      color_info.cmap =
	XCreateColormap(display, RootWindow(display, screen_num),
			DefaultVisual(display, screen_num),
			AllocAll);
      myXStoreColors(display, color_info.cmap, def_colors, num_desired);
    }
  }

  DEBUG_TRACE_OUT printf( "Leaving init_colors\n" );
}

void use_new_color_depth(int depth, unsigned char * in_data,
			 unsigned int memsize) {
  static unsigned int i, top;
  static unsigned short * sdata;
  static unsigned long * ldata;
  static unsigned char * data;
  static unsigned long * truecolors;

  DEBUG_TRACE_IN printf( "Entering use_new_color_depth\n" );

  if (depth==8) return;

  truecolors = get_color_info()->truecolors;
  top = memsize-1;
  data = in_data+top;

  switch(depth)
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

  DEBUG_TRACE_OUT printf( "Leaving use_new_color_depth\n" );
}

/* return the number of bytes for each pixel in image */
int get_num_bytes(void) {
  int bytes;

  switch(color_info.depth)
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

void XPutImageOneByteData(Display * dpy,
		 Window wi,
		 GC gc,
		 XImage * ximage_src,
		 int src_x, int src_y,
		 int dst_x, int dst_y,
		 int width, int height) {
  int i, j, src_width, src_height, dst_pos, src_pos, eoln_incr,
    bytes;
  unsigned long int memsize;
  XImage * ximage_dst;
  unsigned char * ximage_dst_data, * ximage_src_data;
  
  DEBUG_TRACE_IN printf( "Entering XPutImageOneByteData\n" );

  if(ximage_src==NULL){
    printf("XPutImageOneByteData called with null pointer in ximage_src\n");
    DEBUG_TRACE_OUT printf( "Leaving XPutImageOneByteData\n" );
    return;
  }

  /* If the depth wasn't set up for the screen, just do the
   * default XPutImage -- also, just do the default if the
   * color depth is 8 since we already have byte data
   */
  if ((ximage_src->depth!=get_color_info()->depth)||(get_color_info()->depth==8)) {
    /* do nothing special in this case */
    XPutImage(dpy, wi, gc, ximage_src,
	      src_x, src_y, dst_x, dst_y, width, height);

    DEBUG_TRACE_OUT printf( "Leaving XPutImageOneByteData\n" );
    return;
  }

  ximage_src_data = (unsigned char *)ximage_src->data;
  src_width = ximage_src->width;
  src_height = ximage_src->height;
  

  /* Decrease width and height if they move OFF the image */
  if (src_x+width>src_width) width=src_width-src_x;
  if (src_y+height>src_height) height=src_height-src_y;
  
  /* don't do anything in this case */
  if ((width<=0)||(height<=0))
  {
    DEBUG_TRACE_OUT printf( "Leaving XPutImageOneByteData\n" );
    return;
  }
  
  memsize = width*height*sizeof(unsigned char);
  bytes = get_num_bytes();
  
  if ((memsize*bytes<=0)||(height!=(int)((memsize*bytes)/(width*bytes)))||(width!=(int)((memsize*bytes)/(height*bytes)))) {
    printf("Memsize is 0 or negative.  Cannot draw!\n");

    DEBUG_TRACE_OUT printf( "Leaving XPutImageOneByteData\n" );
    return;
  }
  ximage_dst_data = (unsigned char *)MT_malloc(memsize*bytes);
  if (!ximage_dst_data) {
    printf("Out of memory.\n");

    DEBUG_TRACE_OUT printf( "Leaving XPutImageOneByteData\n" );
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

  use_new_color_depth(get_color_info()->depth,
		      ximage_dst_data, memsize);

  ximage_dst = XCreateImage (dpy,
			     DefaultVisual(dpy, DefaultScreen(dpy)),
			     get_color_info()->depth,
			     ZPixmap, 0, (char *)ximage_dst_data,
			     width, height,
			     BitmapPad(dpy), width*bytes);
  XPutImage(dpy, wi,
	    gc, ximage_dst,
	    0, 0, dst_x, dst_y, width, height);

  MT_fake_free(ximage_dst->data);
  XDestroyImage(ximage_dst);
  /* Memory tools can no longer be faked. JJC
  MT_free( (void *) NULL ); fake memory tools into freeing memory that DestroyImage freed */
  
  DEBUG_TRACE_OUT printf( "Leaving XPutImageOneByteData\n" );
}

