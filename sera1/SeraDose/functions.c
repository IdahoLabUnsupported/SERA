#include <stdlib.h>
#include <stdio.h>
#include "global.h"
#include "picshell.h"
#include "common.h"
#include "read_raw.h"
#include "include.h"
#include "contours.h"
#include "color.h"
#include "commonfcns.h"

XImage global_image;

/*
 * MAX_LENGTH_CONTOUR_LEVEL_STRING - used by check_contour_level_values.
 * This same constant must also be defined in callbacks.c
 */
#define MAX_LENGTH_CONTOUR_LEVEL_STRING 300

#define CONTOUR_LINES_LEGEND_MESSAGE "Contour Lines Legend\n\nThe following contour levels\nare to be drawn in\nthe specified colors."

#define CONTOUR_COLORWASH_LEGEND_MESSAGE "Contour Colorwash Legend\n\nThe following contour levels\nare to be colorwashed in\nthe specified colors."

/*
 * MAX_LENGTH_FILENAME - used by load_colorwash_colormap and load_gamma_colormap
 */
#define MAX_LENGTH_FILENAME 100


/*
 * These constants are used when attempting to read the contour level placement
 * values from the external file "contour_levels.txt."  MAX_FILENAME_LENGTH is the 
 * largest number of characters that the file at which the contour levels can be found
 * and its path can be found.  The FILE_NOT_FOUND_MESSAGE is displayed in a 
 * dialog window if the contour_levels.txt file is not found.  They are used in
 * load_contour_levels().
 *
 *    David Helzer  6-9-97
 */

#define MAX_FILENAME_LENGTH 256   
#define MAX_STRING_LENGTH 500
#define FILE_NOT_FOUND_MESSAGE "Error locating contour_levels.txt\nin the resources directory.\n\nThis file will be created\nwith default values."


void load_imageEH (Widget w, caddr_t client_data, XEvent *event)
{
    Boolean masks_replace_images;
    Boolean view_contour_colorwash, view_contour_lines;
    Boolean view_large_image_contour_labels;
    Boolean view_preview_image_contour_labels;
    Boolean scale_font;

    DEBUG_TRACE_IN printf( "Entering load_imageEH\n" );
     
    XtVaGetValues(masksReplaceImagesButton,
                  XmNset, &masks_replace_images,
                  NULL);
    XtVaGetValues(view_contour_lines_button, 
                  XmNset, &view_contour_lines,
                  NULL);                
    XtVaGetValues(view_contour_colorwash_button,
                  XmNset, &view_contour_colorwash,
                  NULL);
    XtVaGetValues(view_large_image_labels_toggle,
                  XmNset, &view_large_image_contour_labels,
                  NULL);
    XtVaGetValues(view_preview_image_labels_toggle,
                  XmNset, &view_preview_image_contour_labels,
                  NULL);
    XtVaGetValues(use_scalable_fonts_toggle,
                  XmNset, &scale_font,
                  NULL);                

    wi = XtWindow(mainWindowDrawingArea);
    if (event -> xexpose.count == 0){  
        int w, h, active;
    
        /*
         * Load the appropriate colormap
         */

        /*
         *  MTC commented this out.  The colormap is now loaded when switching
         *  between view contours and view colorwash.  Also the user can
         *  reset the default colormap now if the want
         */
        /*if ((view_contour_colorwash) && (dosage_is_there))
          set_colorwash_colormap();
          else 
          set_gamma_colormap();*/
    
        /* update the "active" small image */
        w=image_matrix.pic_width;
        h=image_matrix.pic_height;
        active=image_matrix.active_picture;
    
        /* out of bounds precaution */
        if ((active >= 0) && (active < XtNumber (image_matrix.img_arr))) {
            if ((image_matrix.img_arr[active].mask_in_use) && 
                (masks_replace_images))
                generic_resize_image (mask_pack.imagedata,
                                      image_matrix.img_arr[active].image->data,
                                      DATA_WIDTH, DATA_HEIGHT,
                                      w, h);
            else
                recalc_one_image_pixels (image_matrix.img_arr[active].raw_data,
                                         (*image_matrix.img_arr[active].image).data,
                                         w, h);   			      
      
            if ((dosage_is_there) && (!contours_are_current)) {
                draw_contours (dose_data, doseFlag, 1, view_contour_lines,
                               view_contour_colorwash, 
                               view_large_image_contour_labels,
                               view_preview_image_contour_labels, scale_font,
			       &global_image);
                contours_are_current = 1;
            }else {
                draw_preview (active);
                draw_large_image();
            }
        }
    }

    DEBUG_TRACE_OUT printf( "Leaving load_imageEH\n" );
}


/*
 * mask_imageEH -- draws the mask image to the mask window. 
 * This procedure should be called when the window is exposed, etc.
 */

void mask_imageEH (Widget w, caddr_t client_data, XEvent *event)
{
  XImage *theImage;
  unsigned char * tempimagedata;
  int scr;
  
  DEBUG_TRACE_IN printf( "Entering mask_imageEH\n" );
      
  if (event -> xexpose.count == 0) {
    scr = DefaultScreen(di);
    
    tempimagedata = 
      (unsigned char *)MT_malloc(DATA_WIDTH*DATA_HEIGHT*get_num_bytes());
    memcpy(tempimagedata, mask_pack.imagedata, 
	   DATA_WIDTH*DATA_HEIGHT*sizeof(char));
    /*use_new_color_depth(get_color_info()->depth, 
      tempimagedata, DATA_WIDTH*DATA_HEIGHT);*/
    theImage = XCreateImage(di, DefaultVisual(di, scr), 
			    get_color_info()->depth, ZPixmap, 0, 
			    (char *) tempimagedata, DATA_WIDTH, DATA_HEIGHT, 
			    BitmapPad(di), DATA_WIDTH*get_num_bytes());
    
    XSetFunction(di, gc, GXcopy);
    XClearWindow(di, XtWindow(w));
    XPutImageOneByteData(di, XtWindow(w), gc, 
			 theImage, 0, 0, 0, 0, DATA_WIDTH, DATA_HEIGHT);
    MT_fake_free(theImage->data);
    XDestroyImage(theImage);
  }

  DEBUG_TRACE_OUT printf( "Leaving mask_imageEH\n" );
}

void mask_region_search (Widget w, char *filename)
{
  int i /*, count = 1*/, scr;
   FILE *datafile;
   XImage *theImage;
   unsigned char * tempimagedata;


   /* constant cyclic colors used for displaying masks in contrasting colors */
   int mask_colors[]=
   {RESERVED_BLACK, RESERVED_RED, RESERVED_GREEN, RESERVED_BLUE, 
    RESERVED_MAGENTA, RESERVED_CYAN, RESERVED_WHITE,
    RESERVED_YELLOW}; 
   
   static int first_call = 1;
   static int indices_used;
   static int convert_table[MAXCOLORS];/* table of lookup values - 
					  if the raw image 
				      * specifies pixel value i then 
				      * the color displayed on the mask is the 
				      * pixel value in convert_table[i] 
				      */
   /*int cur_index;*/               /* counts colors found in the mask */
   unsigned char tmpchar;

   DEBUG_TRACE_IN printf( "Entering mask_region_search\n" );

   datafile = fopen(filename, "rb");
   /* read 256x256 raw data image */
   fread(mask_pack.maskdata, 1, DATA_WIDTH*DATA_HEIGHT, datafile);  
   
   fclose(datafile);
   
   
   /*
    * Make a copy of the maskdata and change pixel 
    * values so colors will contrast 
    */
   
   /* 
    * first, initialize the lookup table for converting 
    * from the raw value to the new value
    */
   if (first_call) {
     first_call = 0;
     indices_used = 0;
     for (i=0; i<XtNumber(convert_table); i++)
       convert_table[i]=0;
   }
   
   /* 
    * now, convert the raw image to an image with contrasting colors 
    */  
   
   for (i=0; i<XtNumber(mask_pack.maskdata); i++) {
     tmpchar = mask_pack.maskdata[i];
     if (!convert_table[(int)tmpchar]) {
       convert_table[(int)tmpchar] = 
	 mask_colors[indices_used%XtNumber(mask_colors)];
       indices_used++;
     }
     mask_pack.imagedata[i] = (unsigned char) convert_table[(int)tmpchar];
     /* mask_pack.imagedata[i]=tmpchar; */
     /* uncomment above line to view mask in the old mask colors */
   }
   
   scr = DefaultScreen(di);

   /* 
    * save theImage image mask in the new contrasting colors 
    */
   
   tempimagedata = (unsigned char *)MT_malloc(DATA_WIDTH*DATA_HEIGHT*
					   get_num_bytes());
   memcpy(tempimagedata, mask_pack.imagedata, 
	  DATA_WIDTH*DATA_HEIGHT*sizeof(char));
   /*use_new_color_depth(get_color_info()->depth, 
     tempimagedata, DATA_WIDTH*DATA_HEIGHT);*/
   theImage = XCreateImage(di, DefaultVisual(di, scr), 
			   get_color_info()->depth, ZPixmap, 0, 
			   (char *) tempimagedata, 
			   DATA_WIDTH, DATA_HEIGHT, BitmapPad(di), 
			   DATA_WIDTH*get_num_bytes());
   
   XSetFunction(di, gc, GXcopy);
   XClearWindow(di, XtWindow(w));
   XPutImageOneByteData(di, XtWindow(w), gc, 
			theImage, 0, 0, 0, 0, DATA_WIDTH, DATA_HEIGHT);
   /* theImage->data = NULL; now, want to get rid of this */
   MT_fake_free(theImage->data);
   XDestroyImage(theImage);
   process_region(0, mask_pack.masks[0]); /*mwf->method to activate new mask */

   DEBUG_TRACE_OUT printf( "Leaving mask_region_search\n" );
}


void process_region (unsigned int value, unsigned char do_mask)
{
   int i, j, bufrow, bufcol;
   XImage *theImage;
   int scr;
   int set;
   int divisor = 1;

   DEBUG_TRACE_IN printf( "Entering process_region\n" );   
   
   #ifdef BITWISE
      divisor = 8;
   #endif /* for IBM and SUN machines (solaris 2.5 and earlier) */


   if (do_mask == TRUE)
     set = 0;
   else
     set = 0xFF;
   
   for (i=0;i<DATA_HEIGHT;i++){
     for (j=0;j<DATA_WIDTH;j++){
       if (mask_pack.maskdata[i * DATA_WIDTH + j] == value){
	 bufrow = i * 2 * WINDOW_WIDTH;
	 bufcol = j * 2;
	 mask_pack.buffer[(bufrow + bufcol) / divisor] = set;
	 mask_pack.buffer[(bufrow + WINDOW_WIDTH + bufcol) / divisor] = set;
	 mask_pack.buffer[(bufrow + bufcol + 1) / divisor] = set;
	 mask_pack.buffer[(bufrow + WINDOW_WIDTH + bufcol + 1)/ divisor] = set;
       }   
     }
   }
   scr = DefaultScreen(di);
   theImage = XCreateImage(di, DefaultVisual(di,scr), 1, ZPixmap, 0, 
			   (char *) mask_pack.buffer, WINDOW_WIDTH, 
			   WINDOW_HEIGHT, BitmapPad(di), 
			   WINDOW_WIDTH / divisor);
   
   theImage->bits_per_pixel = DefaultDepth(di,scr);    
   XPutImageOneByteData(di, mask_pack.mask_region, newGC, 
			theImage, 0, 0, 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT);
   theImage->data = NULL;
   XDestroyImage(theImage);
   /*MT_free( (void *) NULL );*/

   DEBUG_TRACE_OUT printf( "Leaving process_region\n" );   
}   


void init_colors_old(void)
{  
  DEBUG_TRACE_IN printf( "Entering init_colors\n" );
  
  /* get_color_info()->cmap=
     XCreateColormap(image_matrix.dpy, RootWindow(image_matrix.dpy, 
     image_matrix.screen), 
     DefaultVisual(image_matrix.dpy, image_matrix.screen), AllocAll); */
  
  num_colors_avail = DEFAULT_NUM_GRAYS - 1; 
  
  /* moved out of create_contour_cm by mwf, 7-17-95 */
  contour_color.red = 65535;
  contour_color.green = 65535;
  contour_color.blue = 0;
  contour_color.flags = DoRed | DoGreen | DoBlue;
  contour_color.pixel = CONTOUR_INDEX;
  /* end move                                       */
    
  create_default_cm();  /* save the current Window Manager colors, etc. */
  /* These are saved into the following colormap:  default_colormap     */
  
  /* This copies the "low" colormap entries from 
     'default_colormap' to the current colormap */
  load_colormap( get_color_info()->cmap, 0, 
		 MAX_GRAY_INDEX - num_colors_avail - 1);
  
  /*This simply makes sure the passed colormap uses the correct contour color*/
  create_contour_cm( &(get_color_info()->cmap) );

  /*This adds the "12" reserved colors to the 
    "high" end of the passed colormap */

  /* mike:  we really probably don't want this twice
   * It's called at the beginning of the main program
   * BUT, that didn't work on single colormap systems
   * so it's back!
   */
  add_guaranteed_colors( &(get_color_info()->cmap) );
  
  /* This manipulates the part of the current colormap that 
     is used to display the image */
  /* The colormap for the image is stored in array; 
     based on this array along with the   */
  /* number of colors available, background, saturation, etc. 
     it sets up the colormap to */
  /* display the image so it looks appropriate according 
     to the user's selections.       */
  colormap_loadrgb();    

  DEBUG_TRACE_OUT printf( "Leaving init_colors\n" );
}


void setup_gc (Widget w)
{
  XGCValues vals;
  Arg wargs[10];
  int foreground, background;

  DEBUG_TRACE_IN printf( "Entering setup_gc\n" );

  XtSetArg (wargs[0], XmNforeground, &foreground);
  XtSetArg (wargs[1], XmNbackground, &background);
  XtGetValues (w, wargs, 2);
  vals.foreground = foreground;
  vals.background = background;
  vals.function = GXcopy;
  vals.line_width = 2;

  gc = XtGetGC (w,
	GCForeground | GCBackground | GCFunction | GCLineWidth, &vals);

  /* setup measure drag graphics context */
  vals.line_width = 2;
  vals.function = GXxor;
  vals.foreground = 0xFFFF;
  drag_gc = XCreateGC(di, XDefaultRootWindow(di),
                      GCFunction | GCForeground | GCLineWidth, &vals);

  DEBUG_TRACE_OUT printf( "Leaving setup_gc\n" );
}


void resize_image (unsigned char values[])        
{
  int i, j;

  DEBUG_TRACE_IN printf( "Entering resize_image\n" );

  /** Double the image from 256x256 to 512x512 **/
  for (i=WINDOW_HEIGHT-1;i>=0;i--)
      for (j=WINDOW_WIDTH-1;j>=0;j--)
      {
          values[i*WINDOW_HEIGHT+j] = j%2 ?
              (i%2 ? values[(i-1)/2*DATA_HEIGHT+(j-1)/2]
               : values[i/2*DATA_HEIGHT+(j-1)/2] )
              : (i%2 ? values[(i-1)/2*DATA_HEIGHT+j/2]
                 : values[i/2*DATA_HEIGHT+j/2] );
          /* mwf 7-17-95:  keep a copy */
          orig_values[i*WINDOW_HEIGHT+j]=values[i*WINDOW_HEIGHT+j]; 
      }
  recalc_pixels();
  
  global_image.width = WINDOW_WIDTH;
  global_image.height = WINDOW_HEIGHT;
  global_image.xoffset = 0;
  global_image.format = ZPixmap;
  global_image.data = (char *)values;
  global_image.byte_order = ImageByteOrder(di);
  global_image.bitmap_unit = BitmapUnit(di);
  global_image.bitmap_bit_order = BitmapBitOrder(di);
  global_image.bitmap_pad = BitmapPad(di);
  global_image.depth = get_color_info()->depth;
  /* image.depth = 8; */
  global_image.bytes_per_line = WINDOW_WIDTH*get_num_bytes();
  /* image.bytes_per_line = WINDOW_WIDTH; */
  global_image.bits_per_pixel = 8*get_num_bytes();
  /* image.bits_per_pixel = 8; */
  global_image.obdata = (char *)NULL;

  DEBUG_TRACE_OUT printf( "Leaving resize_image\n" );
}


/***** Loading image into memory from image file *****/

void load_image (FILE *file, Widget w)
{
  int num;

  DEBUG_TRACE_IN printf( "Entering load_image\n" );
    
  wi = XtWindow(w);  /* mwf -> why set this? */
  num = fread(values,1,65536,file);
  fclose(file);
  
  resize_image(values);

  DEBUG_TRACE_OUT printf( "Leaving load_image\n" );
}

/* reapply expansion / contraction algorithm to image data */

void recalc_pixels(void) {
  int i, r, c, w, h;
  static int lastnumcolors=0, offset;
  static unsigned char *tmp_copy;

  DEBUG_TRACE_IN printf( "Entering recalc_pixels\n" );

  w=image_matrix.pic_width;
  h=image_matrix.pic_height;
  
  recalc_one_image_pixels (orig_values, values, WINDOW_WIDTH, WINDOW_HEIGHT);
  
  if (num_colors_avail!=lastnumcolors) { 
    /* In this case, recalculate and draw the small images */
    lastnumcolors=num_colors_avail;
    for (i = 0; i < image_matrix.num_pics; i ++) {

      /* Save the contours in the current image before refreshing it */
      if (image_matrix.img_arr[i].contours_in_use) {
	tmp_copy = (unsigned char *) MT_malloc (w * h);
	for (r = 0; r < h; r ++) {
          offset = r*w;
	  for (c=0; c<w; c++)
	    if ((unsigned char)
		(image_matrix.img_arr[i].image->data[offset+c])==CONTOUR_INDEX)
	      tmp_copy[offset+c]=CONTOUR_INDEX;
	    else
	      tmp_copy[offset+c]=0;
	}
      }

      /* First, set up the raw_data appropriately */
      reload_image(i);

      /* Replace the saved contours, if present */
      if (image_matrix.img_arr[i].contours_in_use) {
	for (r=0; r<h; r++) {
	  offset = r*w;
	  for (c=0; c<w; c++)
	    if (tmp_copy[offset+c]==CONTOUR_INDEX)
	      image_matrix.img_arr[i].image->data[offset+c]=(char)CONTOUR_INDEX;
	}
	MT_free((void*)tmp_copy);
      }
      draw_preview(i);
    }
  }

  DEBUG_TRACE_OUT printf( "Leaving recalc_pixels\n" );
}



/* Generic resize routine */

void generic_resize_image(unsigned char *data, unsigned char *output, 
			  unsigned int inwidth, unsigned int inheight, 
			  unsigned int outwidth, unsigned int outheight) 
{
  static unsigned int xcoorout;
  static unsigned int ycoorout;
  /*static unsigned int i;*/
  static float xcoorin;
  static float ycoorin;
  static unsigned int tmp;
  static unsigned int out1, out2, out3, out4;
  static float p1, p2, p3, p4, tmpf;
  static float x1, y1, x2, y2;
  static unsigned int i1, i2, i3, i4;
  static float round=0.49;
  
  DEBUG_TRACE_IN printf( "Entering generic_resize_image\n" );  

  /* check for special case of integral shrink (or stay same size) */
  /* Same size */
  if ((inwidth==outwidth)&&(inheight==outheight)) {
    memcpy(output, data, (size_t)(outwidth*outheight));
    DEBUG_TRACE_OUT printf( "Leaving generic_resize_image\n" );  
    return;
  }
  
  /* Shrink integral in both width and height ** shrink may differ */
  if ((outwidth<=inwidth)&&(outheight<=inheight)&&(!(inwidth%outwidth))
      &&(!(inheight%outheight))) {
    unsigned int heightmult, widthmult, skipsize;
    
    heightmult=inheight/outheight;
    widthmult=inwidth/outwidth;
    skipsize=inwidth*heightmult;
    
    for (ycoorout=0; ycoorout<outheight; ycoorout++)
      for (xcoorout=0; xcoorout<outwidth; xcoorout++)
	output[ycoorout*outwidth+xcoorout]
	  =data[ycoorout*skipsize+xcoorout*widthmult];
    DEBUG_TRACE_OUT printf( "Leaving generic_resize_image\n" );  
    return;
  }
  
  for (ycoorout=0; ycoorout<outheight; ycoorout++) {
    for (xcoorout=0; xcoorout<outwidth; xcoorout++) {
      
      xcoorin=(float)(xcoorout*(inwidth-1))/(float)(outwidth-1);
      ycoorin=(float)(ycoorout*(inheight-1))/(float)(outheight-1);
      
      i1=((unsigned int)(ycoorin)*inwidth+(unsigned int)(xcoorin));
      i2=((unsigned int)(ycoorin)*inwidth+ceil_d(xcoorin));
      i3=(ceil_d(ycoorin)*inwidth+(unsigned int)(xcoorin));
      i4=(ceil_d(ycoorin)*inwidth+ceil_d(xcoorin));
      
      out1=data[i1];
      out2=data[i2];
      out3=data[i3];
      out4=data[i4];
      
      y1=ycoorin-(unsigned int)ycoorin;
      y2=1.0-y1;
      x1=xcoorin-(unsigned int)xcoorin;
      x2=1.0-x1;
      
      p1=1.0-square(y1)-square(x1);
      p2=1.0-square(y1)-square(x2);
      p3=1.0-square(y2)-square(x1);
      p4=1.0-square(y2)-square(x2);
      p1=max(0.0,p1);
      p2=max(0.0,p2);
      p3=max(0.0,p3);
      p4=max(0.0,p4);
      
      tmpf=p1+p2+p3+p4;
      tmp=(p1*out1+p2*out2+p3*out3+p4*out4)/tmpf+round;
      output[ycoorout*outwidth+xcoorout]
	=(unsigned char)tmp;
    }
  }
  DEBUG_TRACE_OUT printf( "Leaving generic_resize_image\n" );  
}

/* The actual recalc algorithm -- separated to make it generic */
void recalc_one_image_pixels(unsigned char *orig_values, 
			     unsigned char *values, 
			     unsigned int width, unsigned int height)
{
  int i;
  unsigned int tmp_val, maxpix, minpix;
  float new_no_colors, old_no_colors, low_color_index, expansion_ratio;
  unsigned char newmap[MAXCOLORS];

  DEBUG_TRACE_IN printf( "Entering recalc_one_image_pixels \n");
  
  minpix = maxpix = orig_values[0]; /* pick index 0 arbitrarily */
  for (i=0;i<height*width;i++) {
    tmp_val = orig_values[i];
    if (tmp_val > maxpix)
      maxpix = tmp_val;
    if (tmp_val < minpix)
      minpix = tmp_val;
  }
  
  new_no_colors = num_colors_avail + 1;
  old_no_colors = maxpix - minpix + 1;
  expansion_ratio = new_no_colors / old_no_colors;
  low_color_index = MAX_GRAY_INDEX - new_no_colors + 1;
  
  for (i=minpix; i<=maxpix; i++) {
    newmap[i]=(unsigned char)
      ((float)(i-minpix)*expansion_ratio + low_color_index + 0.001);
  }
  
  /** apply normalization to image **/
  for (i = 0; i < height * width; i ++)
    values[i] = newmap[orig_values[i]];

  DEBUG_TRACE_OUT printf( "Leaving recalc_one_image_pixels \n");
}



/* Draw the small preview image and accompanied contours */

void draw_preview (int ival) 
{    
  Boolean view_contour_lines;
  
  DEBUG_TRACE_IN printf( "Entering draw_preview \n");

  XtVaGetValues (view_contour_lines_button,
		 XmNset, &view_contour_lines,
		 NULL);
  
  /* make sure the value passed is legitimate */
  if ((ival>=0)&&(ival<image_matrix.num_pics))  {
    
    if ((dosage_is_there) && (view_contour_lines))
      XPutImageOneByteData(image_matrix.dpy, 
			   XtWindow(image_matrix.img_arr[ival].draw_area), 
			   image_matrix.img_arr[ival].gc, 
			   image_matrix.img_arr[ival].contoured_image, 
			   0,0,0,0,image_matrix.pic_width, 
			   image_matrix.pic_height);
    else if ((dosage_is_there) && (!view_contour_lines))
      XPutImageOneByteData(image_matrix.dpy, 
			   XtWindow(image_matrix.img_arr[ival].draw_area), 
			   image_matrix.img_arr[ival].gc, 
			   image_matrix.img_arr[ival].colorwashed_image, 
			   0,0,0,0,image_matrix.pic_width, 
			   image_matrix.pic_height);
    else 
      XPutImageOneByteData(image_matrix.dpy, 
			   XtWindow(image_matrix.img_arr[ival].draw_area),
			   image_matrix.img_arr[ival].gc, 
			   image_matrix.img_arr[ival].image,
			   0,0,0,0,image_matrix.pic_width, 
			   image_matrix.pic_height);
  }

  DEBUG_TRACE_OUT printf( "Leaving draw_preview \n");
}


void draw_large_image (void)
{
  Boolean view_contour_lines;
  
  DEBUG_TRACE_IN printf( "Entering draw_large_image \n");

  XtVaGetValues (view_contour_lines_button,
		 XmNset, &view_contour_lines, 
		 NULL);
  
  if ((dosage_is_there) && (view_contour_lines)) 
    XPutImageOneByteData (di, wi, gc, contoured_image, 0, 0, 0, 0, 
			  WINDOW_WIDTH, WINDOW_HEIGHT);
  else if ((dosage_is_there) && (!view_contour_lines))
    XPutImageOneByteData (di, wi, gc, colorwashed_image, 0, 0, 0, 0, 
			  WINDOW_WIDTH, WINDOW_HEIGHT);
  else if (slice_is_there) 
    XPutImageOneByteData (di, wi, gc, &global_image, 0, 0, 0, 0, 
			  WINDOW_WIDTH, WINDOW_HEIGHT);

  DEBUG_TRACE_OUT printf( "Leaving draw_large_image \n");
}

/*
 * load_mask - loads the mask data from the specified file and 
 * displays the mask image in the mask window.
 */ 
void load_mask ( char *local_image_file_name ) 
{
  FILE *mask_file;
  int i;

  DEBUG_TRACE_IN printf( "Entering load_mask\n" );

 /* If the mask is not currently in use, it needs memory allocated.  
    Do it now. */
  if (!(image_matrix.img_arr[image_matrix.active_picture].mask_in_use)) {
    if (!((image_matrix.img_arr[image_matrix.active_picture].mask_buffer = 
	   (unsigned char*)MT_malloc(WINDOW_WIDTH*WINDOW_HEIGHT*
				  sizeof(unsigned char)))&&    
	  (image_matrix.img_arr[image_matrix.active_picture].masked_array =
	   (unsigned char*)MT_malloc(256*sizeof(unsigned char))))) {
      printf("Malloc error.\n");
      exit(13);
    }
  }
  
  /* point the mask_pack.buffer and mask_pack.masks 
     to the above (or previous) memory */
  mask_pack.buffer = 
    image_matrix.img_arr[image_matrix.active_picture].mask_buffer;
  mask_pack.masks = 
    image_matrix.img_arr[image_matrix.active_picture].masked_array;
  
  /* Set up some defaults ONLY if the mask is not currently in use */
  if (!(image_matrix.img_arr[image_matrix.active_picture].mask_in_use)) {
    for (i = 0; i < WINDOW_WIDTH*WINDOW_HEIGHT; i ++)
      mask_pack.buffer[i] = 255;  /* default the masks to 1 */
    for (i = 0; i < 256; i ++)
      mask_pack.masks[i] = FALSE;
  }

  XSetFunction(di, newGC, GXset);
  XCopyArea(di, mask_pack.mask_region, mask_pack.mask_region, newGC, 0, 0, 
	    WINDOW_WIDTH, WINDOW_HEIGHT, 0, 0);  /* clear the mask */
  XSetFunction(di, newGC, GXcopy);
  
  if ((mask_file = fopen(local_image_file_name,"r")) == NULL) {
    printf ("Unable to open file!! %s\n", local_image_file_name);
    XBell(di,100);
    /* get rid of the mask shell, if present */
    if (mask_pack.in_use) {
      mask_pack.in_use = FALSE;
      XtPopdown(mask_pack.shell);
    }
    /* Now, free the memory for the mask and return */
    removefrom_mask_list(image_matrix.active_picture);

    /* mike --> hmm, do we even need the dummies? */
    mask_pack.buffer=dummy_buffer; 

    mask_pack.masks=dummy_masks;

    DEBUG_TRACE_OUT printf( "Leaving load_mask\n" );
    return;
  } else {
    /* add filename to the mask list if it's a new mask 
       (not yet marked as in use) */
    if (!(image_matrix.img_arr[image_matrix.active_picture].mask_in_use))
      addto_mask_list(local_image_file_name, image_matrix.active_picture);
    
    fclose ( mask_file ); /* save parsing for later */	    
    /*fclose(image_file);*/
    
    /* check to see if we have already popped up this box */
    if (!mask_pack.in_use) 
      {
	mask_pack.in_use = TRUE;
	XtPopup(mask_pack.shell, XtGrabNone);
      }
  }	  
  make_colormap_window_children (AppShell, get_color_info()->cmap);
  mask_region_search(mask_pack.maskScreen, local_image_file_name);

  DEBUG_TRACE_OUT printf( "Leaving load_mask\n" );
}

typedef struct {
  float z_value;
  floyd_data_ptr data;
  floyd_data_ptr original_data;
  int numpoints;
} Tdose_data;

Tdose_data GLOBAL_dose_data[256];

void add_dose_to_memory(int num, floyd_data_ptr data, 
			floyd_data_ptr original_data,
			int numpoints, float z_value){
  if(GLOBAL_dose_data[num].data)
    MT_free(GLOBAL_dose_data[num].original_data);
  GLOBAL_dose_data[num].data = 
    (floyd_data *)MT_calloc(numpoints, sizeof(floyd_data) );
  memcpy(GLOBAL_dose_data[num].data,data,sizeof(floyd_data)*numpoints);

  if(GLOBAL_dose_data[num].original_data)
    MT_free(GLOBAL_dose_data[num].original_data);
  GLOBAL_dose_data[num].original_data = 
    (floyd_data *)MT_calloc(numpoints, sizeof(floyd_data) );
  memcpy(GLOBAL_dose_data[num].original_data,
	 original_data,sizeof(floyd_data)*numpoints);
  GLOBAL_dose_data[num].numpoints = numpoints;
  GLOBAL_dose_data[num].z_value = z_value;

}

/***** load dosage data *****/

void load_dose (char *filename, floyd_data_ptr *data,
		floyd_data_ptr *original_data_ptr, float *z_value)
{
  int             /*file_length,*/ count, skipInt, versionFlag;
   int             Xcolumn, Ycolumn, SKIPPED_LINES ;
   char            line[LINESIZE], string[35];
   float           container[3];
   float           totalRef, boronRef, gammaRef, nitrogenRef, otherRef;
   float           fastRef, group1Ref, group2Ref, thermalFluenceRef;
   int             abscissa, ordinate;
   static int      LDfirstCall = 1;

   floyd_data_ptr  original_data;

   FILE *          file;

   /*char            z_value_string[100];*/

   DEBUG_TRACE_IN printf( "Entering load_dose\n" );

   /*
    * read number of points, number of lines skipped to get to dose data,
    *      xmin, xmax, ymin, ymax, zmin, and zmax
    */

   /* if filename[0] is '\b' then the data is stored in memory */
   if(filename[0]=='\b'){
    int i=(filename[1]-1)*128+(filename[2]-1);

    if (!LDfirstCall) {
      MT_free((void *)*data);
      /*MT_free((void *)original_data);*/
    }
    LDfirstCall = 0;
    
    NUMPOINTS = GLOBAL_dose_data[i].numpoints;
     
    *data = (floyd_data *)MT_calloc(NUMPOINTS, sizeof(floyd_data) );
    original_data = (floyd_data *)MT_calloc(NUMPOINTS, sizeof(floyd_data));

    memcpy(*data,GLOBAL_dose_data[i].data,
	   GLOBAL_dose_data[i].numpoints*sizeof(floyd_data));
    memcpy(original_data,GLOBAL_dose_data[i].original_data,
	   GLOBAL_dose_data[i].numpoints*sizeof(floyd_data));

    *z_value = GLOBAL_dose_data[i].z_value;

   } else {
     file = fopen(filename,"r");
     if(!file){
       printf("Unable to open file %s\n",filename);
       return;
     }
     versionFlag = 0;
     fgets(line, LINESIZE - 1, file); /* read version number */
     if( strstr(line, " new_v107") ) {
       DEBUG_LOADING printf(" ---- Version 1.07 Contour Data ----\n");
       versionFlag = 107;
     }
     
     fscanf(file,"%e %e %e %e %e %e %d %d %d %d %d", &FOV, &ZVAL,
	    &xmin, &xmax, &ymin, &ymax, &Xcolumn, &Ycolumn,
	    &NUM_X_POINTS, &NUM_Y_POINTS, &SKIPPED_LINES);
     abscissa = Xcolumn - 1;
     ordinate = Ycolumn - 1;
   
   
     /*
      * set up dynamic storage for all of the plane's dose points
      */
     if (!LDfirstCall) {
       MT_free((void *)*data);
       /*MT_free((void *)original_data);*/
     }
     LDfirstCall = 0;
     
     NUMPOINTS = NUM_X_POINTS  * NUM_Y_POINTS;
     

     *data = (floyd_data *)MT_calloc(NUMPOINTS, sizeof(floyd_data) );
     original_data = (floyd_data *)MT_calloc(NUMPOINTS, sizeof(floyd_data) );
     
     
     /*
      *
      * Skip past header information
      *
      */
     for (count=0; count<SKIPPED_LINES + 1; count++)
       fgets(line, LINESIZE - 1, file);
     
     /* read the concentrations */
     if(versionFlag == 107)
       fscanf(file,"%35c %e %e %e %e %e %e %e %e %e", string,
	      &container[0], &BoronConc, &GammaConc, &NitrogenConc,
	      &FastConc, &container[0], &container[0], &container[0], &OtherConc);
     else
       fscanf(file,"%35c %e %e %e %e %e %e %e %e", string,
	      &container[0], &BoronConc, &GammaConc, &NitrogenConc,
          &FastConc, &container[0], &container[0], &container[0]);
     
     CurConcs[0] = BoronConc, CurConcs[1] = GammaConc;
     CurConcs[2] = NitrogenConc, CurConcs[3] = FastConc;
     CurConcs[4] = OtherConc;
     
     /* read the dose factors */
     if(versionFlag == 107)
       fscanf(file,"%35c %e %e %e %e %e %e %e %e %e", string,
	      &container[0], &BoronFactor, &GammaFactor, &NitrogenFactor,
	      &FastFactor, &container[0], &container[0], &container[0], &OtherFactor);
     else
       fscanf(file,"%35c %e %e %e %e %e %e %e %e", string,
	      &container[0], &BoronFactor, &GammaFactor, &NitrogenFactor,
	      &FastFactor, &container[0], &container[0], &container[0]);
     
     CurFactors[0] = BoronFactor, CurFactors[1] = GammaFactor;
     CurFactors[2] = NitrogenFactor, CurFactors[3] = FastFactor;
     CurFactors[4] = OtherFactor;
     
     
     /* read the ref values at thermal peak */
     if(versionFlag == 107)
       fscanf(file,"%35c %e %e %e %e %e %e %e %e %e", string,
	      &totalRef, &BoronRef, &GammaRef, &NitrogenRef,
	      &FastRef, &group1Ref, &group2Ref, &thermalFluenceRef, &OtherRef);
     else
       fscanf(file,"%35c %e %e %e %e %e %e %e %e", string,
	    &totalRef, &BoronRef, &GammaRef, &NitrogenRef,
	      &FastRef, &group1Ref, &group2Ref, &thermalFluenceRef);
   
     CurRefs[0] = BoronRef, CurRefs[1] = GammaRef, CurRefs[2] = NitrogenRef;
     CurRefs[3] = FastRef, CurRefs[4] = group1Ref, CurRefs[5] = group2Ref;
     CurRefs[6] = thermalFluenceRef, CurRefs[7] = OtherRef;
     
     totalRef          = 100.0 / totalRef;
     boronRef          = 100.0 / BoronRef;
     gammaRef          = 100.0 / GammaRef;
     nitrogenRef       = 100.0 / NitrogenRef;
     fastRef           = 100.0 / FastRef;
     group1Ref         = 100.0 / group1Ref;
     group2Ref         = 100.0 / group2Ref;
     thermalFluenceRef = 100.0 / thermalFluenceRef; 
     otherRef          = 100.0 / OtherRef;
   
     
     for( count = 0; count < NUMPOINTS; count ++){
       if(versionFlag == 107)
	 fscanf(file,"%f %f %f %d %f %f %f %f %f %f %f %f %f",
		&container[0], &container[1], &container[2],
		&skipInt,
		&((original_data)[count].totalDose), 
		&((original_data)[count].boronDose),
		&((original_data)[count].gammaDose),
		&((original_data)[count].nitrogenDose), 
		&((original_data)[count].fastDose),
		&((original_data)[count].group1Fluence), 
		&((original_data)[count].group2Fluence),
		&((original_data)[count].thermalFluence), 
		&((original_data)[count].otherDose));
       else
	 fscanf(file,"%f %f %f %d %f %f %f %f %f %f %f %f",
		&container[0], &container[1], &container[2],
		&skipInt,
		&((original_data)[count].totalDose), 
		&((original_data)[count].boronDose),
		&((original_data)[count].gammaDose),
		&((original_data)[count].nitrogenDose), 
		&((original_data)[count].fastDose),
		&((original_data)[count].group1Fluence), 
		&((original_data)[count].group2Fluence),
		&((original_data)[count].thermalFluence));
     
     
       (*data)[count].x = container[abscissa];
       (*data)[count].y = container[ordinate];
       (*data)[count].totalDose      = (original_data)[count].totalDose * totalRef;
       (*data)[count].boronDose      = (original_data)[count].boronDose * boronRef;
       (*data)[count].gammaDose      = (original_data)[count].gammaDose * gammaRef;
       (*data)[count].nitrogenDose  = 
	 (original_data)[count].nitrogenDose * nitrogenRef;
       (*data)[count].fastDose      = (original_data)[count].fastDose * fastRef;
       (*data)[count].group1Fluence = 
	 (original_data)[count].group1Fluence * group1Ref;
       (*data)[count].group2Fluence = 
	 (original_data)[count].group2Fluence * group2Ref;
       (*data)[count].thermalFluence = 
	 (original_data)[count].thermalFluence * thermalFluenceRef;
       (*data)[count].otherDose      = (original_data)[count].otherDose * otherRef;

     }
     
     dosage_is_there = 1;
   
     *z_value = container[2];
     
     fclose(file);
   
   }

   reset_image_set_if_needed(image_matrix.image_set);
   
   /* This was added so the current_driver_data (which can be modified
    * by setting factors) will immediately replace any defaults loaded by
    * the file RATHER than having to click on the image first
    * Previously:
    *   toggle_on would call setfactors
    * Currently:
    *   toggle_on calls load_dose which calls setfactors
    *   ** advantage is that factors get immediately set up correctly
    *      as soon as a dose is loaded rather than having to click on
    *      an image to activate it
    */
   setfactors (&current_driver_data,original_data);

   if(original_data_ptr == NULL)
     MT_free(original_data);
   else 
     *original_data_ptr = original_data;

   DEBUG_TRACE_OUT printf( "Leaving load_dose\n" );
}


void load_colormap (Colormap whichColormap, int low_index, int high_index)
{
   XColor temp_color_cell;
   int   screen;
   int   i;

   /*
    *  Install the passed color map  -- we have tried to use XSetWindowColormap
    *  but due to problems with the server using only indices instead of
    *  names for colors we had to be bad citizens
    */

   DEBUG_TRACE_IN printf( "Entering load_colormap\n" );

   screen = DefaultScreen(di);

   for (i=low_index;i<=high_index;i++) {
      temp_color_cell.pixel = i;
      myXQueryColor(di, whichColormap, &temp_color_cell);
      myXStoreColor(di, get_color_info()->cmap, &temp_color_cell);
   }

   /*
    * block of code below will correctly
    * treat systems with multiple hardware
    * colormaps - DEW 8/16/94, remodified JLC 10/7/94
    */ 
    
    make_colormap_window_children(AppShell, get_color_info()->cmap); 

    
   #ifdef MORE_VERBOSE
      printf("load_colormap: pixel = %d\n", contour_color.pixel);
   #endif

   load_imageEH(mainWindowDrawingArea, 0, &SureEvent); 

   DEBUG_TRACE_OUT printf( "Leaving load_colormap\n" );
}


/* mwf:  7-12-95 - function added to install the RESERVED
 * colors (red, green, blue, etc.) into the passed colormap 
 */
void add_guaranteed_colors(Colormap *cmap)
{
    XColor             color;

    DEBUG_TRACE_IN printf( "Entering add_guaranteed_colors\n" );

    color.flags = DoRed | DoGreen | DoBlue;

    /* Add in the RESERVED colors. */
    color.pixel=RESERVED_RED;
    color.red = 65000;
    color.green = 0;
    color.blue = 0;
    myXStoreColor(di, *cmap, &color);

    color.pixel = MAX_GRAY_INDEX+3; /* mwf 7-20-95:  bnct_rtpe compatibility */
    myXStoreColor(di, *cmap, &color);

    color.pixel=RESERVED_GREEN;
    color.red = 0;
    color.green = 65000;
    color.blue = 0;
    myXStoreColor(di, *cmap, &color);

    color.pixel=RESERVED_BLUE;
    color.red = 0;
    color.green = 0;
    color.blue = 65000;
    myXStoreColor(di, *cmap, &color);

    color.pixel=RESERVED_CYAN;
    color.red = 0;
    color.green = 65000;
    color.blue = 65000;
    myXStoreColor(di, *cmap, &color);

    color.pixel=RESERVED_YELLOW;
    color.red = 65000;
    color.green = 65000;
    color.blue = 0;
    myXStoreColor(di, *cmap, &color);

    color.pixel = MAX_GRAY_INDEX+2; /* mwf 7-20-95:  bnct_rtpe compatibility */
    myXStoreColor(di, *cmap, &color);

    color.pixel=RESERVED_MAGENTA;
    color.red = 65000;
    color.green = 0;
    color.blue = 65000;
    myXStoreColor(di, *cmap, &color);

    color.pixel=RESERVED_BLACK;
    color.red = 0;
    color.green = 0;
    color.blue = 0;
    myXStoreColor(di, *cmap, &color);

    color.pixel=RESERVED_WHITE;
    color.red = 65000;
    color.green = 65000;
    color.blue = 65000;
    myXStoreColor(di, *cmap, &color);

    color.pixel = MAX_GRAY_INDEX+1; /* mwf 7-20-95:  bnct_rtpe compatibility */
    myXStoreColor(di, *cmap, &color);

    DEBUG_TRACE_OUT printf( "Leaving add_guaranteed_colors\n" );
}


void create_default_cm (void)
{
  Colormap colormap;
  int      i;
  XColor   colors[MAXCOLORS];

  DEBUG_TRACE_IN printf( "Entering create_default_cm\n" );

  colormap = DefaultColormap(di, DefaultScreen(di));

  for( i = 0; i < ncolors; i++ ) {
    colors[i].pixel = i;
    colors[i].flags = DoRed|DoGreen|DoBlue;
  }
  
  myXQueryColors(di, colormap, colors, ncolors);
  /* xyzzy get_color_info()->cmap = XCreateColormap(di, DefaultRootWindow(di),
                                     DefaultVisual(di, DefaultScreen(di)),
                                     AllocAll); */
  myXStoreColors(di, get_color_info()->cmap, colors, ncolors);

  DEBUG_TRACE_OUT printf( "Leaving create_default_cm\n" );
}


void create_contour_cm (Colormap *colormap)
{
   DEBUG_TRACE_IN printf( "Entering create_contour_cm\n" );

   #if defined (MORE_VERBOSE)
      printf( "   pixel = %d\n", contour_color.pixel );
   #endif /* MORE_VERBOSE */

   myXStoreColor(di,*colormap,&contour_color);

   DEBUG_TRACE_OUT printf( "Leaving create_contour_cm\n" );
}



void MeasureLineDrag  (Widget w, int x, int y, float *length, float *angle)
{
   int        xdist, ydist;

   DEBUG_TRACE_IN printf( "Entering MeasureLineDrag\n" );

   /*
    * Draw the last line to remove using an xor GC and then 
    * draw the new line.  Update the measure data structure
    * for the next pass.
    */

   MeasureDrawLine (w, measure_data . firstx, measure_data . firsty,
         measure_data . lastx, measure_data . lasty);

   measure_data . lastx = x;
   measure_data . lasty = y;
   
   MeasureDrawLine (w, measure_data . firstx, measure_data . firsty,
         measure_data . lastx, measure_data . lasty);

   /*
    * Calculate the values and build the string for the output
    * label.
    */

   xdist = measure_data . lastx - measure_data . firstx;
   ydist = -(measure_data . lasty - measure_data . firsty);
   *length = sqrt ((float) (xdist * xdist + ydist * ydist));

   if (ydist == 0)
      if (xdist == 0)
         *angle = 0.0;
      else
         if (xdist > 0.0)
            *angle = M_PI / 2.0;
         else
            *angle = 3 * M_PI / 2.0;
   else
   {
      *angle = atan (((float) xdist) / (float) ydist);

      /*
       * Get the output angle right for vertical = 0.0 degrees.  Each
       * quadrant is measured 0-90 from vertical to horizontal with the
       * signs alternating.
       */

      if (xdist > 0.0)
         if (ydist > 0.0)
            *angle = *angle;
         else
            *angle += M_PI;
      else
         if (ydist < 0.0)
            *angle += M_PI;
         else
            *angle += 2.0 * M_PI;
   }

   *angle *= 180.0 / M_PI;

   DEBUG_TRACE_OUT printf( "Leaving MeasureLineDrag\n" );

   return;
}



void MeasureDrawLine (Widget w, int x1, int y1, int x2, int y2)
{
   if ((x1 == x2) && (y1 == y2))
      return;

   XDrawLine (XtDisplay (w), XtWindow (w), drag_gc, x1, y1, x2, y2);

   return;
}



/*****************************************************************
 * this is an event handler that installs the xcontours colormap
 * on single colormap systems that is called whenever the
 * mouse cursor enters an xcontours window - mwf 7-20-95
 * (make_colormap_window_children sets the event handler)
 */
void colormap_install(Widget w, caddr_t client_data, XEvent *event)
{
    if (get_color_info() -> depth == 8)
       XInstallColormap(XtDisplay(w), get_color_info() -> cmap);
  }



/* 
 * Updates each small image in image matrix, returning to 'active' at end
 * New version only updates the individual image unless the user has specified to
 * update all images --> choice on OPTIONS menu
 */
 
void update_small_images (int active) 
{
     int i;
     Boolean apply_all;

     DEBUG_TRACE_IN printf( "Entering update_small_images\n" );

     XtVaGetValues(applyToAllButton, 
		   XmNset, &apply_all, 
		   NULL);

     set_cursor(1);

     if (apply_all)
       for (i = active + 1; i < active + 1 + image_matrix.num_pics; i ++)
	 toggle_on (i%image_matrix.num_pics, 1);
     else
	 toggle_on (active, 1);

     set_cursor(0);

     DEBUG_TRACE_OUT printf( "Leaving update_small_images\n" );
}



/* Similar to update_small_images in functions.c
 * --> Goal is to update all masks with one click to speed things
 *     (If automatic updates is on, does all images, else not)
 */
void update_masks (int active, unsigned int value, int set_to) 
{
  Boolean apply_all;
  int i;
  
  DEBUG_TRACE_IN printf( "Entering update_masks\n" );

  XtVaGetValues(applyToAllButton, 
		XmNset, &apply_all, 
		NULL);
  
  set_cursor(1);

  active=image_matrix.active_picture;
  if (apply_all) {
    for (i=active+1; i<active+1+image_matrix.num_pics; i++) {
      /* Only do this if the mask is in use */
      if (image_matrix.img_arr[i%image_matrix.num_pics].mask_in_use) {
	toggle_on(i%image_matrix.num_pics, 0);
	mask_pack.masks[value] = set_to;
	process_region(value, set_to);	/* mask or unmask the region */
	if (image_matrix.img_arr[i%image_matrix.num_pics].contours_in_use) {
	    contours_are_current = FALSE;
	    load_imageEH(mainWindowDrawingArea, 0, &SureEvent);
	}
      }
    }
  } else {
    mask_pack.masks[value] = set_to;
    process_region(value, set_to);	/* mask or unmask the region */
    if (image_matrix.img_arr[active].contours_in_use) {
        contours_are_current = FALSE;
	load_imageEH(mainWindowDrawingArea, 0, &SureEvent);
    }
  }
  /* Need to reactivate the active picture in this case */
  /* (it had no contours so above didn't reactivate it) */
  if (!image_matrix.img_arr[active].contours_in_use)
    toggle_on(active, 1);
  set_cursor(0);

  DEBUG_TRACE_OUT printf( "Leaving update_masks\n" );
}



/* Similar to update_small_images in functions.c
 * --> Goal is to update all masks with one click to speed things
 *     If automatic updates is on, does all images
 *     Sets all masks or clears all masks in an image based on set_to
 */
void set_unset_masks(int active, int set_to) 
{
  Boolean apply_all;
  int i, j, buffer_val;

  DEBUG_TRACE_IN printf( "Entering set_unset_masks\n" );
  
  XtVaGetValues(applyToAllButton, 
		XmNset, &apply_all, 
		NULL);
  
  set_cursor(1);

  if (set_to==FALSE) buffer_val=255;
  else buffer_val=FALSE;

  active=image_matrix.active_picture;
  if (apply_all) {
    for (i=active+1; i<active+1+image_matrix.num_pics; i++) {
      /* only do this if the mask and contours are in use */
      if ((image_matrix.img_arr[i%image_matrix.num_pics].contours_in_use)&&
	  (image_matrix.img_arr[i%image_matrix.num_pics].mask_in_use)) {
	toggle_on(i%image_matrix.num_pics, 0);
	for (j = 0; j < WINDOW_WIDTH*WINDOW_HEIGHT; j ++)
	  mask_pack.buffer[j] = buffer_val; /* hide or see all contours */
	for (j = 0; j < 256; j ++)
	  mask_pack.masks[j] = set_to;
	
	if (set_to==255)
	  XSetFunction(di, newGC, GXclear);
	else 
	  XSetFunction(di, newGC, GXset);
	XCopyArea(di, mask_pack.mask_region, 
		  mask_pack.mask_region, newGC, 
		  0, 0, 512, 512, 0, 0);  /* clear the mask */
	XSetFunction(di, newGC, GXcopy);
	if (image_matrix.img_arr[i%image_matrix.num_pics].contours_in_use) {
	  contours_are_current = FALSE;
	  load_imageEH(mainWindowDrawingArea, 0, &SureEvent);
	}
      }
    }
  } else {
    /* this is just the normal code for 1 image */
    for (j = 0; j < WINDOW_WIDTH*WINDOW_HEIGHT; j ++)
      mask_pack.buffer[j] = buffer_val; /* hide or see all contours */
    for (j = 0; j < 256; j ++)
      mask_pack.masks[j] = set_to;
    
    if (set_to==255)
      XSetFunction(di, newGC, GXclear);
    else 
      XSetFunction(di, newGC, GXset);
    XCopyArea(di, mask_pack.mask_region, 
	      mask_pack.mask_region, newGC, 
	      0, 0, 512, 512, 0, 0);  /* set or clear the mask */
    XSetFunction(di, newGC, GXcopy);
    if (image_matrix.img_arr[active].contours_in_use) {
      contours_are_current = FALSE;
      load_imageEH(mainWindowDrawingArea, 0, &SureEvent);
    }
  }
  /* Need to reactivate the active picture in this case */
  /* (it had no contours so above didn't reactivate it) */
  if (!image_matrix.img_arr[active].contours_in_use)
    toggle_on(active, 1);
  set_cursor(0);

  DEBUG_TRACE_OUT printf( "Leaving set_unset_masks\n" );
}



void labelContours (char * text) 
{
  static XmString xmscontourText,
                  defaultText;
  static int first_call = 1, 
             freeme = 0;

  DEBUG_TRACE_IN printf( "Entering labelContours\n" );

  if (first_call) {
    first_call = 0;
    defaultText = XmStringCreateSimple("(No Contours)");
  }

  if ((freeme)&&(xmscontourText!=NULL)) {
    XmStringFree(xmscontourText);
    freeme = 0;
  }

  if (strlen(text)==0) {
    XtVaSetValues(contourTextLabel, 
                  XmNlabelString, defaultText,
                  NULL);
  } 
  else {
    xmscontourText = XmStringCreateSimple(text);
    if (xmscontourText==NULL) {
      printf("No memory available in labelContours.  Exiting.\n");
      exit(13);
    }
    
    XtVaSetValues(contourTextLabel,
  		  XmNlabelString, xmscontourText,
		  NULL);
    freeme = 1;
  }

  DEBUG_TRACE_OUT printf( "Leaving labelContours\n" );
}



void reload_image (int picnum) 
{
  FILE *in_ptr;
  int retVal = 0;
  int items_read;
  int w, h;
  
  DEBUG_TRACE_IN printf( "Entering reload_image\n" );

  w = image_matrix.pic_width;
  h = image_matrix.pic_height;
  
  if ((in_ptr = fopen(image_matrix.img_arr[picnum].fname_data, "r"))!=NULL) {
    items_read = fread (image_matrix.img_arr[picnum].raw_data, 
			(size_t) 1, 256*256, in_ptr );
    if (items_read==256*256) retVal = 1;
    fclose(in_ptr);
    
    recalc_one_image_pixels(
	    image_matrix.img_arr[picnum].raw_data, 
	    (unsigned char *) image_matrix.img_arr[picnum].image->data, 
	    w, h);
  }

  DEBUG_TRACE_OUT printf( "Leaving reload_image\n" );
}


/***************************************************************************/
/* taken from bnct and added by mwf on 1-13-97
 * updated to return a value by mwf */
int confirm_popup(char *message)
{
  static XtInputMask m;
  
  DEBUG_TRACE_IN printf( "Entering confirm_popup\n" );

  CONFIRM = -99;
  /* Sound the bell */
  XBell (image_matrix.dpy, 100);
  XtVaSetValues(Confirm_dialog, XmNmessageString, 
		XmStringCreateLtoR(message,XmSTRING_DEFAULT_CHARSET), NULL);
  XtManageChild (Confirm_dialog);
  while (CONFIRM==-99) {
    if ((m=XtAppPending(context)))
      XtAppProcessEvent(context, m);
  }
  
  switch(CONFIRM){
  case OK:
    {
      DEBUG_TRACE_OUT printf( "Leaving confirm_popup\n" );
      return(1);
      break;
    }
  case CANCEL:
    {
      DEBUG_TRACE_OUT printf( "Leaving confirm_popup\n" );
      return(0);
      break;
    }
  default:
    {
      DEBUG_TRACE_OUT printf( "Leaving confirm_popup\n" );
      return(-1);
    }
  }
}


int is_image_file (char *str) 
{
  if (!((strstr (str, ",contour")) || (strstr (str, ",mask")))) {
    return (1);
  } else {
    return (0);
  }
}


int is_mask_file (char *str) 
{
  if (strstr (str, ",mask")) {
    return (1);
  }else {
    return (0);
  }
}


int is_dose_file (char *str) 
{
  if (strstr (str, ",contour")) {
    return (1);
  } else {
    return (0);
  }
}


/* 
 * load_contour_levels_from_string() - a procedure that reads in the contour
 * levels  from a string provided.  Values in the string must be separated
 * by some white space.  ALL error checking of the large string containing the 
 * contour level values should have been done prior to calling this procedure.
 * Any new contour levels are assigned the default contour level color 
 * that can be set in the preferences.  The new levels are put into a 
 * structure similar to the contour_levels structure.  After this, the 
 * levels are compared to those before the changes were made.  
 * Differences are then recorded in the contour_levels structure. 
 * Created by David Helzer on 6-13-97
 */

void load_contour_levels_from_string (char *contour_levels_to_load)
{
  char  current_char;
  int   count, count2;
  char  single_value_text[MAX_STRING_LENGTH];
  contour_levels_structure new_contour_levels;

  DEBUG_TRACE_IN printf( "Entering load_contour_levels_from_string\n" );
  
  new_contour_levels.number_levels = 0;
  
  /* 
   * Add a '\n' at the end of the string to allow this procedure to see
   * white space after each new contour level value
   */
  strcat (contour_levels_to_load, "\n");
  
  count2 = 0;
  
  /* 
   * In this procedure, all data is assumed to be valid.  Any error
   * checking should be done before calling this procedure.  
   */

  for (count = 0; count < strlen (contour_levels_to_load); count ++) {

    current_char = contour_levels_to_load[count];
    single_value_text[count2] = current_char;
    
    /* 
     * If a space has been read and there is a digit or a decimal in 
     * position 0, then a level has been read.
     */

    if ((isspace(current_char)) && (isdigit(single_value_text[0]))) {
      
      single_value_text[count2] = '\0';
      new_contour_levels.rlevel[new_contour_levels.number_levels] = 
	atoi (single_value_text);
      new_contour_levels.colors[new_contour_levels.number_levels] = 
	default_contour_color;
      new_contour_levels.number_levels ++;
      single_value_text[0] = '\0';
      count2 = 0;
    }
    else if (isspace(current_char)) {
      count2 = 0;
      single_value_text[0] = '\0';
    }
    else 
      count2 ++;
  }
  
  
  /* 
   * Compare these new levels to the previous levels.  Any levels missing are
   * removed from the contour_levels structure.  Any new levels are added with 
   * the default contour color.
   */
   
  for (count = 0; count < new_contour_levels.number_levels; count ++) {
    for (count2 = 0; count2 < contour_levels.number_levels; count2 ++) {
      if (new_contour_levels.rlevel[count] == contour_levels.rlevel[count2])
	new_contour_levels.colors[count] = contour_levels.colors[count2];
    }   
  }
  
  for (count = 0; count < new_contour_levels.number_levels; count ++) {
    contour_levels.rlevel[count] = new_contour_levels.rlevel[count];
    contour_levels.colors[count] = new_contour_levels.colors[count];
  }
  contour_levels.number_levels = new_contour_levels.number_levels;

  DEBUG_TRACE_OUT printf( "Leaving load_contour_levels_from_string\n" );
}


/* 
 * update_legend() -
 *
 * David Helzer 8/97
 */

void update_legend (void)
{
  Boolean view_contour_lines;
  
  DEBUG_TRACE_IN printf( "Entering update_legend\n" );

  XtVaGetValues (view_contour_lines_button,
		 XmNset, &view_contour_lines,
		 NULL);
  
  if (view_contour_lines)
    update_contour_lines_legend();
  else
    update_contour_colorwash_legend();

  DEBUG_TRACE_OUT printf( "Leaving update_legend\n" );
}


/*
 * update_contour_colorwash_legend() -
 *
 * David Helzer 8/97
 */
 
void update_contour_colorwash_legend (void)
{
  XmString message_to_display;
  XmString contour_levels_values_xmstring;
  char contour_levels_values_string[MAX_LENGTH_STRING];
  Widget color_canvas;
  short count;
  Boolean reveal_legend;
  
  DEBUG_TRACE_IN printf( "Entering update_contour_colorwash_legend\n" );

  /* 
   * If the legend is not being displayed, do nothing.
   */
  XtVaGetValues (reveal_legend_button,
		 XmNset, &reveal_legend,
		 NULL);
  
  if (reveal_legend) { 
    /*
     * If, for some reason, the legend is already displayed, 
     * remove it and build it again.
     */
    if (contour_colorwash_legend_rowcol) {
      XtDestroyWidget (contour_colorwash_legend_rowcol);
      contour_colorwash_legend_rowcol = NULL;
    }else if (contour_lines_legend_rowcol) {
      XtDestroyWidget (contour_lines_legend_rowcol);
      contour_lines_legend_rowcol = NULL;
    }
    
    if (XtIsManaged (contour_legend_widget))
      XtUnmanageChild (contour_legend_widget);
    
    message_to_display = XmStringCreateLtoR(CONTOUR_COLORWASH_LEGEND_MESSAGE, 
					    XmFONTLIST_DEFAULT_TAG);
    
    XtVaSetValues (contour_legend_label,
		   XmNlabelString, message_to_display,
		   NULL);
    
    XmStringFree (message_to_display);
    
    contour_colorwash_legend_rowcol = XtVaCreateManagedWidget(
			           "contour_colorwash_legend_rowcol",
				   xmRowColumnWidgetClass, contour_legend,
				   XmNorientation, XmVERTICAL,
				   XmNpacking, XmPACK_COLUMN,
				   XmNnumColumns, 2,
				   NULL);   
      
     /*
      * Add all of the contour levels and colors to the row-column
      * widget class.  The levels are in the left column and the colors 
      * are in the right.  Each level's value and color is its own label 
      * widget.  The label widgets for the colors will hold a small square
      * graphic containing that color.  
      */
     
     /*
      * Display the contour level values in the left column 
      */                         
    for (count = 0; count < colorwash_legend_values.number_regions - 1; 
	 count ++) {
      sprintf (contour_levels_values_string, " %d - %d ",
	       colorwash_legend_values.low_values[count],
	       colorwash_legend_values.high_values[count]);
      
       contour_levels_values_xmstring = 
	 XmStringCreateLtoR (contour_levels_values_string,
			     XmFONTLIST_DEFAULT_TAG);
       
       XtVaCreateManagedWidget (
		  "contour_level_value",
		  xmLabelWidgetClass, contour_colorwash_legend_rowcol,
		  XmNlabelString, contour_levels_values_xmstring,
		  NULL);
    }
     
    sprintf (contour_levels_values_string, " %d - Max ", 
 colorwash_legend_values.low_values[colorwash_legend_values.number_regions-1]);

    contour_levels_values_xmstring = 
      XmStringCreateLtoR (contour_levels_values_string,
			  XmFONTLIST_DEFAULT_TAG);
    XtVaCreateManagedWidget ("contour_level_value",
			     xmLabelWidgetClass,
			     contour_colorwash_legend_rowcol,
			     XmNlabelType, XmSTRING,
			     XmNlabelString, contour_levels_values_xmstring,
			     NULL);
    /*
     * Display each color associated with a contour level
     */ 
    
    for (count = 0; count < contour_levels.number_levels; count ++) {
      /* 
       * Display the contour level colors (as a square box filled with
       *  that color in the right column). 
       */
      
      color_canvas = XtVaCreateManagedWidget (
     "color_canvas", xmDrawingAreaWidgetClass,
     contour_colorwash_legend_rowcol,
     XmNbackground, get_color_info()->truecolors[contour_levels.colors[count]],
     XmNborderColor, get_color_info()->truecolors[RESERVED_BLACK],
     XmNborderWidth, 1,
     NULL);    
     }
      
     /* 
      * Display the color for the maximum contour values
      */
     color_canvas = XtVaCreateManagedWidget (
	  "color_canvas", xmDrawingAreaWidgetClass,
	  contour_colorwash_legend_rowcol,
	  XmNbackground, get_color_info()->truecolors[max_contour_value_color],
	  XmNborderColor, get_color_info()->truecolors[RESERVED_BLACK],
	  XmNborderWidth, 1,
	  NULL);
     
     make_colormap_window_children (contour_legend_widget, 
				    get_color_info()->cmap);      
     XtManageChild (contour_legend_widget);
   }  

  DEBUG_TRACE_OUT printf( "Leaving update_contour_colorwash_legend\n" );
}

/*
 * update_contour_lines_legend () - a procedure that causes the 
 * contour_levels_legend widget to be displayed (if it is not already
 * displayed) with the current contour levels and their colors that
 * will be displayed.
 *
 * David Helzer  7/17/97
 */
 
void update_contour_lines_legend (void)
{
   XmString       message_to_display;
   XmString       contour_level_value_xmstring;
   char           contour_level_value_string[MAX_STRING_LENGTH];
   Widget         color_canvas;
   short          count;
   Boolean        reveal_legend;
   
   DEBUG_TRACE_IN printf( "Entering update_contour_lines_legend\n" );

   /* 
    * If the legend is not being displayed, do nothing.
    */
   XtVaGetValues (reveal_legend_button,
		  XmNset, &reveal_legend,
		  NULL);
                   
   if (reveal_legend) {
     /*
      * If, for some reason, the legend is already displayed, 
      * remove it and build it again.
      */
     if (contour_lines_legend_rowcol) {
       XtDestroyWidget (contour_lines_legend_rowcol);
       contour_lines_legend_rowcol = NULL;
     }else if (contour_colorwash_legend_rowcol) {
       XtDestroyWidget (contour_colorwash_legend_rowcol);
       contour_colorwash_legend_rowcol = NULL;
     }
         
     if (XtIsManaged (contour_legend_widget))
       XtUnmanageChild (contour_legend_widget);
          
     message_to_display = XmStringCreateLtoR(CONTOUR_LINES_LEGEND_MESSAGE, 
					     XmFONTLIST_DEFAULT_TAG);
     
     XtVaSetValues (contour_legend_label,
		    XmNlabelString, message_to_display,
		    NULL);
     
     XmStringFree (message_to_display);
     
     contour_lines_legend_rowcol = XtVaCreateManagedWidget(
				   "contour_lines_legend_rowcol",
				   xmRowColumnWidgetClass, contour_legend,
				   XmNorientation, XmVERTICAL,
				   XmNpacking, XmPACK_COLUMN,
				   XmNnumColumns, 2,
				   NULL);   
      
     /*
      * Add all of the contour levels and colors to the row-column
      * widget class.  The levels are in the left column and the colors 
      * are in the right.  Each level's value and color is its own label 
      * widget.  The label widgets for the colors will hold a small square
      * graphic containing that color.
      */
     
     /*
      * Display the contour level values in the left column 
      */                         
     for (count = 0; count < contour_levels.number_levels; count ++) {
   
         /* Display the contour level values in the left column */
       sprintf (contour_level_value_string, "   %d  ", 
		contour_levels.rlevel[count]);
       contour_level_value_xmstring = XmStringCreateLtoR(
					  contour_level_value_string,
					  XmFONTLIST_DEFAULT_TAG);
       XtVaCreateManagedWidget ("contour_level_value",
				xmLabelWidgetClass,
				contour_lines_legend_rowcol,
				XmNlabelType, XmSTRING,
				XmNlabelString, contour_level_value_xmstring,
				NULL);
       
       XmStringFree (contour_level_value_xmstring);
     }
   
     /*
      * Display each color associated with a contour level
      */ 
     for (count = 0; count < contour_levels.number_levels; count ++) {
       
       /* 
	* Display the contour level colors (as a square box filled with
	*  that color in the right column. 
	*/
      
       color_canvas = XtVaCreateManagedWidget (
     "color_canvas", xmDrawingAreaWidgetClass,
     contour_lines_legend_rowcol,
     XmNbackground, get_color_info()->truecolors[contour_levels.colors[count]],
     XmNborderColor, get_color_info()->truecolors[RESERVED_BLACK],
     XmNborderWidth, 1,
     NULL);    
     }
     
     make_colormap_window_children (contour_legend_widget, 
				    get_color_info()->cmap);      
      XtManageChild (contour_legend_widget);
   }  

   DEBUG_TRACE_OUT printf( "Leaving update_contour_lines_legend\n" );
}

/*
 * sort_contour_levels() - A bubblesorting algorithm that sorts the 
 * contour levels stored in the global data structure "contour_levels" 
 * into ascending order.  Both the level values and the color values 
 * are sorted properly to ensure that the levels retain their chosen
 * colors.
 *
 * David Helzer 7/97
 */
 
void sort_contour_levels (void)
{
   short i, j;
   int temp_rlevel;
   int temp_color;
   
   DEBUG_TRACE_IN printf( "Entering sort_contour_levels\n" );

   for (i = 0; i < contour_levels.number_levels; i ++)
      for (j = contour_levels.number_levels - 1; j > i; j --)
         if (contour_levels.rlevel[j] < contour_levels.rlevel[j - 1]) {
            temp_rlevel = contour_levels.rlevel[j];
            temp_color = contour_levels.colors[j];
            contour_levels.rlevel[j] = contour_levels.rlevel[j - 1];
            contour_levels.colors[j] = contour_levels.colors[j - 1];
            contour_levels.rlevel[j - 1] = temp_rlevel;
            contour_levels.colors[j - 1] = temp_color;
         }        

   DEBUG_TRACE_OUT printf( "Leaving sort_contour_levels\n" );
}


/* 
 * update_colorwash_values() - a procedure that examines the current 
 * contour levels and modifies the colorwash_legend_values structure
 * to reflect the current contour levels.
 *
 * David Helzer 8/97
 */
 
void update_colorwash_values (void)
{
   short count;
   
   DEBUG_TRACE_IN printf( "Entering update_colorwash_values\n" );

   colorwash_legend_values.number_regions = contour_levels.number_levels + 1;
   colorwash_legend_values.low_values[0] = MINIMUM_POSSIBLE_CONTOUR_VALUE;
   colorwash_legend_values.high_values[0] = contour_levels.rlevel[0];
   colorwash_legend_values.colors[0] = contour_levels.colors[0];
   for (count = 1; count < contour_levels.number_levels; count ++) {
     colorwash_legend_values.low_values[count] = 
       contour_levels.rlevel[count - 1];
     colorwash_legend_values.high_values[count] = contour_levels.rlevel[count];
     colorwash_legend_values.colors[count] = contour_levels.colors[count];
   }
  colorwash_legend_values.low_values[colorwash_legend_values.number_regions-1]=
    contour_levels.rlevel[contour_levels.number_levels - 1];
 colorwash_legend_values.high_values[colorwash_legend_values.number_regions-1]=
   MAXIMUM_POSSIBLE_CONTOUR_VALUE;
 colorwash_legend_values.colors[colorwash_legend_values.number_regions - 1] = 
   max_contour_value_color;

   DEBUG_TRACE_OUT printf( "Leaving update_colorwash_values\n" );
}


/*
 * check_contour_level_values() - error checks a string containing
 * a set of contour level values that the user has inputted.  The procedure 
 * returns a value in the text_is_okay parameter.  It returns a 1 if the text 
 * inputted is okay and a 0 if it is not.  It also returns a new string 
 * containing the various contour level values with all extra white space 
 * removed.  
 * 
 * Created by David Helzer on 6-13-97 
 */
 
void check_contour_level_values (char *contour_levels, 
                                 char *all_levels_text, int *text_is_okay)
{
    char          single_value_text[MAX_LENGTH_CONTOUR_LEVEL_STRING];
    /*int           single_level_float;*/
    int           count, count2/*, count3*/;  
    char          current_char;
    int           good_character = 1;
    int           number_levels = 0;
        
    DEBUG_TRACE_IN printf( "Entering check_contour_level_values\n" );

    *text_is_okay = 1;
    
    /* 
     * Make sure there is white space before the null terminator in the 
     * string -- fscanf() works best with this space (I don't know why)
     */
    strcat (contour_levels, "\n");
    
    all_levels_text[0] = '\0';
    count2 = 0;  
    count = 0;
    
    while ((good_character) && (count < strlen (contour_levels))) 
      {
	current_char = contour_levels[count]; 
		
	if (!isdigit(current_char))
	  good_character = 0;
	if (isspace (current_char))   
	  good_character = 1;
	
	if (good_character) {	    
	  single_value_text[count2] = current_char;
	  count2 ++;
	  
	  /* 
	   * If the current character is a space or carriage return,
	   * a single contour level string has been read 
	   * (if the first character of
	   * the single_level_text is a digit or a dec. point). 
	   */
	  
	  if ((isspace(current_char)) && (isdigit(single_value_text[0]))){  
	    
	    single_value_text[count2] = '\0';
	    
	    /* Make sure that the number is positive */
	    if (atoi(single_value_text) < 0)
	      good_character = 0;
	    
	    /* put the contour level string in the string 
	       for the external file */
	    if (good_character) {
	      number_levels ++;
	      strcat (all_levels_text, single_value_text);
	    }	
	    
	    count2 = 0;		
	    single_value_text[0] = '\0';
	  
	  }else if (isspace(current_char)) {
	    count2 = 0;
	    single_value_text[0] = '\0';
	  }
	}
	
	/*
	 * If at the end of the string, make sure that between 1 and 
	 * 256 levels have been read. If not, set good_character to 0.
	 */
	
	if ((count + 1 == strlen(contour_levels)) && 
	    ((number_levels < 1) || (number_levels > 256)))
	  good_character = 0;	
	
	if (!good_character)
	  *text_is_okay = 0;
	
	count ++;
      }

    DEBUG_TRACE_OUT printf( "Leaving check_contour_level_values\n" );
}


void set_colorwash_colormap (void)
{
   char colorwash_colormap_filename[MAX_LENGTH_FILENAME];
   
   DEBUG_TRACE_IN printf( "Entering set_colorwash_colormap\n" );

   strcpy (colorwash_colormap_filename, getenv ("SERA_RESOURCES"));
   strcat (colorwash_colormap_filename, DoseDisplayResourceDir);
   strcat (colorwash_colormap_filename, DoseDisplayColorwashFile);
   CT_load_cmap (colorwash_colormap_filename, 0 );

   DEBUG_TRACE_OUT printf( "Leaving set_colorwash_colormap\n" );
}


void set_gamma_colormap (void)
{
   char gamma_colormap_filename[MAX_LENGTH_FILENAME];
   
   DEBUG_TRACE_IN printf( "Entering set_gamma_colormap\n" );

   strcpy (gamma_colormap_filename, getenv ("SERA_RESOURCES"));
   strcat (gamma_colormap_filename, SharedColormapDir);
   strcat (gamma_colormap_filename, DoseDisplayGammaFile);
   CT_load_cmap (gamma_colormap_filename, 0);

   DEBUG_TRACE_OUT printf( "Leaving set_gamma_colormap\n" );
}





/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: get_dose_value_under_mouse
%%
%%  Written by: Cory Albright
%%
%%  Parameters:(cb)
%%
%%  Purpose: maps the x,y mouse position intot the current dose grid
%%           does a double linear interpolation and returns the dose value
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
float get_dose_value_under_mouse(int orig_x, int orig_y, int ncols, int nrows)
{
  double value;
  double x,y;
  int  x1,x2;
  int y1,y2;
  int i1,i2,i3,i4;
  float dose1,dose2,dose3,dose4;
  /*double x_avg1,x_avg2,x_avg_factor;*/
 
  x = ((double)orig_x/(double)ncols) * (double)(NUM_X_POINTS-1);
  y = (double)(NUM_Y_POINTS-1) - ( ((double)orig_y/(double)ncols) * (double)(NUM_Y_POINTS-1) );
  
  /* printf("mapped x: %f, y: %f\n",x,y);*/
  
  x1 = (int)x;      
  y1 = (int)(y);
  x2 = (int)(x+1.0);  
  y2 = (int)(y+1.0);
  i1 = y1 * NUM_X_POINTS + x1;
  i2 = y1 * NUM_X_POINTS + x2;
  i3 = y2 * NUM_X_POINTS + x1;
  i4 = y2 * NUM_X_POINTS + x2;

  switch(doseFlag){
  case 0:
    /*printf("looking in the boronDose grid\n");*/
    dose1 = dose_data[i1].boronDose;
    dose2 = dose_data[i2].boronDose;
    dose3 = dose_data[i3].boronDose;
    dose4 = dose_data[i4].boronDose;
    break;
  case 1:
    /*printf("looking in the gammaDose grid\n");*/
    dose1 = dose_data[i1].gammaDose;
    dose2 = dose_data[i2].gammaDose;
    dose3 = dose_data[i3].gammaDose;
    dose4 = dose_data[i4].gammaDose;
    break;
  case 2:
    /*printf("looking in the nitrogenDose grid\n");*/
    dose1 = dose_data[i1].nitrogenDose;
    dose2 = dose_data[i2].nitrogenDose;
    dose3 = dose_data[i3].nitrogenDose;
    dose4 = dose_data[i4].nitrogenDose;
    break;
  case 3:
    /*printf("looking in the fastDose grid\n");*/
    dose1 = dose_data[i1].fastDose;
    dose2 = dose_data[i2].fastDose;
    dose3 = dose_data[i3].fastDose;
    dose4 = dose_data[i4].fastDose;
    break;
  case 4:
    dose1 = dose_data[i1].group1Fluence;
    dose2 = dose_data[i2].group1Fluence;
    dose3 = dose_data[i3].group1Fluence;
    dose4 = dose_data[i4].group1Fluence;
    break;
  case 5:
    dose1 = dose_data[i1].group2Fluence;
    dose2 = dose_data[i2].group2Fluence;
    dose3 = dose_data[i3].group2Fluence;
    dose4 = dose_data[i4].group2Fluence;
    break;
  case 6:
    dose1 = dose_data[i1].thermalFluence;
    dose2 = dose_data[i2].thermalFluence;
    dose3 = dose_data[i3].thermalFluence;
    dose4 = dose_data[i4].thermalFluence;
    break;
  case 7:
    dose1 = dose_data[i1].otherDose;
    dose2 = dose_data[i2].otherDose;
    dose3 = dose_data[i3].otherDose;
    dose4 = dose_data[i4].otherDose;
    break;
  case 8:
    dose1 = dose_data[i1].totalDose;
    dose2 = dose_data[i2].totalDose;
    dose3 = dose_data[i3].totalDose;
    dose4 = dose_data[i4].totalDose;
    break;
  default:
    printf("Unknown Dose Encountered %s:%d\n",__FILE__,__LINE__);
    dose1 = 0.0;
    dose2 = 0.0;
    dose3 = 0.0;
    dose4 = 0.0;
  }

#ifdef NOT_DEFINED
  x_avg_factor = (x-x1)/(x2-x1);
  x_avg1 = x_avg_factor * (dose2-dose1) + dose1;
  x_avg2 = x_avg_factor * (dose3-dose4) + dose4;
  
  /*
    printf("\n\n\n\nthe dose values i'm using are : \n");
    printf("\t%.2f\t\t%.2f\n\n",dose1,dose2);
    printf("\n\n");
    printf("\t%.2f\t\t%.2f\n\n",dose4,dose3);
  */

  /*** going to look in dose_data grid to find the grid ****/
  if (x > (double)(NUM_X_POINTS-1)){


    if (y > (double)(NUM_Y_POINTS-1)){
      /** tough case, for now just take the corner pixel **/
      /*printf("taking the corner pixel!\n");*/
      value = dose_data[NUM_X_POINTS * NUM_Y_POINTS -1].totalDose;
    }else{
      /** right side, take average of pixel above and below **/
      value = ((y-y1)/(y4-y1)) * (dose4 - dose1) + dose1;
      /*printf("on the right, mapping from above and below\n");*/
    }


  } else if (y > (double)(NUM_Y_POINTS-1)){
      /** bottom side, take average of pixel above and below **/
      value = x_avg1;
      /*printf("on the bottom , mapping from left and right\n");*/


  }else{
    value = ((y-y1)/(y4-y1)) * (x_avg2-x_avg1) + x_avg1;
  } 
#endif
  
  value = (dose1*(x2-x)+dose2*(x-x1))*(y2-y)+(dose3*(x2-x)+dose4*(x-x1))*(y-y1);

  return value;
}
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: toggle_dose_locatorCB
%%
%%  Written by: Cory Albright
%%
%%  Parameters:(cb)
%%
%%  Purpose:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void toggle_dose_locatorCB(Widget w, XtPointer clientdata, XtPointer calldata)
{
  Widget locate_window;
  char label_string[256];
  XmString xmstr;
  XmToggleButtonCallbackStruct *cbs = 
    (XmToggleButtonCallbackStruct *)calldata;

  locate_window = (Widget) clientdata;

  printf("in the toggle_dose_locaterCB, the window is : %d\n",locate_window);

  if (cbs->set){
    XtAddEventHandler(locate_window,PointerMotionMask,FALSE,
		      MeasureDoseEH, (XtPointer)w);
  }else{
    XtRemoveEventHandler(locate_window,PointerMotionMask,FALSE,
		         MeasureDoseEH,(XtPointer)w);

    sprintf(label_string,"Dose Value:");
    xmstr = XmStringCreateLocalized(label_string);
    
    XtVaSetValues(w,
		  XmNlabelString,xmstr,
		  NULL);
    XmStringFree(xmstr);

  }
}
*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Procedure: MeassureDoseEH
%%
%%  Written by: Cory Albright
%%
%%  Parameters:(eh), the window to measure the dose in
%%              is passed throught the clientdata
%%
%%  Purpose:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*
void MeasureDoseEH (Widget w, XtPointer clientdata,
		      XEvent *event, Boolean *flag)
{
  Widget label;
  int x,y;
  char label_string[256];
  XmString xmstr;
  
  label = (Widget) clientdata;
  
  x = event->xmotion.x;
  y = event->xmotion.y;
  
  sprintf(label_string,"Dose Value: %d,%d",x,y);
  xmstr = XmStringCreateLocalized(label_string);
  
  XtVaSetValues(label,
		XmNlabelString,xmstr,
		NULL);
  XmStringFree(xmstr);
}
*/
