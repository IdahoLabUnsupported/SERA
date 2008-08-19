#include <stdlib.h>
#include <time.h>
#include "include.h"
#include "iconCB.h"
#include "gen_resize.h"
#include "image_matrix.h"
#include "functions.h"

#ifndef max
#define max(x, y) ((x)>(y))?(x):(y)
#endif
#ifndef min
#define min(x, y) ((x)<(y))?(x):(y)
#endif

/* Added by MWF on 4-23-96
 * Used to update (redraw) the iconified version
 * of a slice
 */
void IconifyCB ( Widget    w,
		 XtPointer clientData, 
		 XmDrawingAreaCallbackStruct *cbs)
{
  static int first_call=1, width, height, icon_width=16, icon_height=16;
  static int i, j, bytes;
  static XGCValues gcv; 
  static XExposeEvent *event;
  static GC icon_gc;
  static XImage *icon_image;
  static unsigned char *real_data;
  static unsigned char *icon_byte_data;
  static int this_width, this_height;
  static int cur_pic = 0;
  static int cycle = 0;
  image_matrix_type * image_matrix_ptr;

  image_matrix_ptr = get_image_matrix();

  switch ((int)clientData)
    {
    case 0: /* Turn off cycling */
      debug("Turning cycling off.\n");
      cycle = 0;
      return;
      break;
    case 1: /* Turn on cycling */
      debug("Turning cycling on.\n");
      cycle = 1;
      break;
    default: /* redraw callback */
      event=(XExposeEvent*)(cbs->event);
      
      this_width = event->width + event->x;
      this_height = event->height + event->y;
      
      if ((first_call)||(this_width>icon_width)||(this_height>icon_height)) {
	
	if (!first_call) { /* Destroy what you created */
	  XDestroyImage(icon_image); /* also destroys malloced image */
	} else {
	  first_call=0;
	  gcv.function = GXcopy;
	  icon_gc = XCreateGC(XtDisplay(w), XtWindow(w), GCFunction, &gcv);
	}
	
	/* Set width and height to the biggest you've seen -- but don't go
	 * beyond 128 (note that the min was set to 16 above)
	 */
	icon_width = max(icon_width,this_width);
	icon_height = max(icon_height,this_height);
	icon_width = min(icon_width,128);
	icon_height = min(icon_height,128);
	
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
	    return;
	    break;
	  }
	icon_byte_data = (unsigned char *) MT_malloc(icon_width*icon_height*bytes);
	
	icon_image = XCreateImage (XtDisplay(w),
				   DefaultVisual(XtDisplay(w),
						 DefaultScreen(XtDisplay(w))),
				   get_color_info()->depth, ZPixmap, 0,
				   (char *)icon_byte_data,
				   icon_width, 
				   icon_height, 
				   BitmapPad(XtDisplay(w)),
				   icon_width*bytes);
      }
      
  
    }
  
  debug("Inside iconifyCB.\n");
    
  while (1) {
    
    if (image_matrix_ptr->num_pics<=0) {
      debug("No images.\n");
      return;
    } 

    
    /******************************************/
    real_data = image_matrix_ptr->img_arr[cur_pic].pimage_data;
    width=image_matrix_ptr->img_arr[cur_pic].data_w;
    height=image_matrix_ptr->img_arr[cur_pic].data_h;
    if ((width<=0)||(height<=0)) return;
    /******************************************/
    
    /* Don't resize and draw until the last expose event */
    /*if (event->count==0)*/ {
      generic_resize_image(real_data, (unsigned char *)icon_image->data,
			   width, height,
			   icon_width, icon_height, 1);
      debug("Icon calling use_new_color_depth.\n");
      use_new_color_depth(get_color_info()->depth,
			  (unsigned char *)icon_image->data, icon_width*icon_height);
      debug("Completed OK.\n");
      
      if (XtIsManaged(w)) {
	cur_pic = (cur_pic+image_matrix_ptr->num_pics-1)%(image_matrix_ptr->num_pics);
	{
	  int dummy, halfheight;
	  static int first_call=1, loops=10;
	  clock_t start, stop;
	  float elapse;
	  
	  halfheight = icon_height/2;

	  if (first_call) {
	    start = clock();
	    make_colormap_window_children(w, get_color_info()->cmap);
	  }

	  for (j=0; j<icon_height; j++) {
	    switch(cur_pic%2) {
	    case 0:
	      i=j;
	      break;
	    case 1:
	      i=(j+halfheight)%icon_height;
	      break;
	    }
	    for (dummy=0; dummy<loops; dummy++) {
	      XPutImage(XtDisplay(w), XtWindow(w), icon_gc, icon_image, 
			0, i, 0, i, icon_width, 1);
	      XPutImage(XtDisplay(w), XtWindow(w), icon_gc, icon_image, 
			0, icon_height-i-1,
			0, icon_height-i-1,
			icon_width, 1);
	      wait_on_xserver();
	    }
	  }
	  if (first_call) {
	    float frac;

	    first_call = 0;
	    stop = clock();
	    elapse = (float)(stop-start);
	    if (elapse>0.0) {
	      elapse/=(float)CLOCKS_PER_SEC;
	      /* Want elapsed time to be about 0.5 secs */

	      frac=0.5/elapse;
	      debug("The frac is %f\n", frac);
	      loops=(int)(((float)loops)*frac);
	      debug("loops currently %d\n", loops);
	      if (loops<=0) loops = 1;
	      debug("Elapsed time:  %f secs, loops set to %d\n",
		     elapse, loops);
	    }
	  }
	}
	if (!cycle) {
	  debug("Redraw complete.\n");
	  return;
	}
      } else {
	debug("Exiting callback!\n");
	exit(0);
	return;
      }
    }
  }
}

void MakeIconifyIcon ( Widget shell )
{
  Arg al[10];
  int ac;
  Display        *dpy = XtDisplay ( shell );
  Widget          icon_shell, icon_draw_area;
  static int icon_width, icon_height;

  /*
   * Create a shell widget, and set mappedWhenManaged to FALSE.
   * The window manager controls when this window is mapped.
   */

  debug("Making iconify icon.\n");

  icon_shell = XtVaAppCreateShell ( "icon_shell", "Icon_shell", 
				    topLevelShellWidgetClass, dpy, 
				    XmNmappedWhenManaged, FALSE,
				    NULL );
  
  icon_draw_area=
    XtVaCreateManagedWidget("drawingarea",
			    xmDrawingAreaWidgetClass, icon_shell,
			    NULL);
  XtAddEventHandler(icon_draw_area, LeaveWindowMask, False,
		    (XtEventHandler)IconifyCB, (XtPointer)0);
  XtAddEventHandler(icon_draw_area, EnterWindowMask, False,
		    (XtEventHandler)IconifyCB, (XtPointer)1);
  XtAddCallback ( icon_draw_area, XmNexposeCallback, 
		  (XtCallbackProc)IconifyCB, (XtPointer)2);
  
  XtManageChild(icon_draw_area);

  icon_width=128; /* These are basically maximums */
  icon_height=128;
  XtVaSetValues(icon_draw_area, 
		XmNwidth, icon_width,
		XmNheight, icon_height,
		NULL);

  /*
   * Realize the icon shell to force the shell's window to exist
   */
  
  XtRealizeWidget ( icon_shell );

  /*
   * Install the icon_shell's window as the icon window.
   */
  
  XtVaSetValues ( shell, XmNiconWindow, 
		  XtWindow ( icon_shell ), NULL );

  debug("Iconify icon made.\n");
}
