/* ========================================================================
 *
 * Colormap support for the registration subsystem.
 *
 * History: Harkin, 7/99
 *
 * Notes:   The idea is that the registration subsystem should be able
 *          to stand on its own, so it needs its own colormap management
 *          tools.  These are modeled after the seraImage tools, but
 *          designed to be more capable of standing alone for other
 *          subsystems as well.
 *
 *          To avoid conflicts, the prefix Cs is used for all functions.
 *
 * =========================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "register_color.h"

void CsUseNewColorDepth (CsInfoType *, unsigned char *, unsigned long);

/* =========================================================================
 *
 * Functions to store 1 or more colors in a colormap.
 *
 *    CsStoreColor    - store 1 color in a colormap
 *    CsStoreColors   - store N colors in a colormap
 *
 * ==========================================================================
 */

void CsStoreColor 
(  CsInfoType  *info,
   Colormap    cmap,
   XColor      *color
)
{
   CsStoreColors (info, cmap, color, 1);
}

void CsStoreColors 
(  CsInfoType  *info,
   Colormap    cmap,
   XColor      *color,
   int         num
)
{
   int         ct, index;
   Colormap    default_cmap;

   default_cmap = DefaultColormap (info->display, info->screen); 

   /*
    * Get the color-
    */

   for (ct = 0; ct < num; ct ++)
   {
      /*
       * Get the index of the color and move the data to the colormap
       * for each color.
       */

      index = color[ct].pixel;
      info->cmap_colors[index].pixel = color[ct].pixel;
      info->cmap_colors[index].flags = color[ct].flags;
      info->cmap_colors[index].red   = color[ct].red;
      info->cmap_colors[index].green = color[ct].green;
      info->cmap_colors[index].blue  = color[ct].blue;

      if (info->colortype != PseudoColor)
      {
         XAllocColor (info->display, default_cmap, &color[ct]);
         info->cmap_pixels[index] = color[ct].pixel;
      }
      else
         info->cmap_pixels[index] = index;
   }

   /*
    * If TrueColor, we're done, if not, store the colors in the colormap.
    */

   if (info->colortype != PseudoColor)
      return;

   XStoreColors (info->display, cmap, color, num);
}

/* =========================================================================
 *
 * Functions to retrieve colors from the colormap.
 *
 *    CsQueryColor    - get 1 color in a colormap
 *    CsQueryColors   - get N colors in a colormap
 *
 * ==========================================================================
 */

void CsQueryColor
(  CsInfoType  *info,
   Colormap    cmap,
   XColor      *color
)
{
   CsQueryColors (info, cmap, color, 1);
}

void CsQueryColors
(  CsInfoType  *info,
   Colormap    cmap,
   XColor      *color,
   int         num
)
{
   int         ct, index;


   if (info->colortype != PseudoColor)
   {
      for (ct = 0; ct < num; ct ++)
      {
         index = color [ct].pixel;
         color [ct].pixel = info->cmap_colors[index].pixel;
         color [ct].flags = info->cmap_colors[index].flags;
         color [ct].red = info->cmap_colors[index].red;
         color [ct].green = info->cmap_colors[index].green;
         color [ct].blue = info->cmap_colors[index].blue;
      }
   }
   else
      XQueryColors (info->display, cmap, color, num);
}



void CsLoadGamma
(  CsInfoType         *info, 
   unsigned char      *passed_cmap, 
   float              gamma_bnct
)
{
   int i, j;
   float intens;
   int starting_index=0;
   int delta=255;
   float expo;
  
   expo=1.0/gamma_bnct;
  
   for (i=0; i<256; i++) 
   {
      intens = (int)(0.5 + 255.0 * 
         (float)pow
         (  (double)((float)(i - starting_index) / delta), 
            (double)expo )
         );
       for (j=0; j<3; j++)
          passed_cmap[3*i+j]=intens; /* RGB equal --> gray level map */
  }
}

void CsLoadRGB (CsInfoType *info)
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

    cmap_vals = info->cmap_values;

    colors_avail = REG_NUM_GRAYS;
    start_index = REG_MIN_GRAY;
    end_index = REG_MAX_GRAY;

    background = info->background;
    saturation = info->saturation;
    offset     = info->offset;

    if (offset == 256) offset = 0;

    /* some number between 0 and (colors_avail-1) */

    offset = offset*colors_avail/256 + 0.5001; 
    background = background*colors_avail/256 + 0.5001;
    background += start_index;
    saturation = saturation*colors_avail/256 + 0.5001;
    saturation += start_index;

    if (background >= saturation)
	background = saturation-1;

    /* All pixels in colormap to left of background are set to background */

    temp_color_cell.flags = DoRed | DoGreen | DoBlue;
    temp_color_cell.red = cmap_vals[0]<<8;
    temp_color_cell.green = cmap_vals[1]<<8;
    temp_color_cell.blue = cmap_vals[2]<<8;

    end=background;
    if (background > end_index)
	end = end_index+1;
    pixval = start_index+offset;
    for (i = start_index; i < end; i++) 
    {
       if (pixval>end_index)
          pixval-=(end_index-start_index+1);

       temp_color_cell.pixel = pixval;
       CsStoreColor (info, info->cmap, &temp_color_cell);
       pixval++;
    }


    num_colors = saturation-background+1;
    if (num_colors >= 1)
       fincr=255.0/(float)(num_colors-1);
    cumul =0.0;

    start = background;
    end = saturation;
    if (start < start_index) {
      cumul += fincr*(start_index-start);
      start = start_index;
    }
    if (end > end_index)
      end = end_index;
    
    /* Now do the pixels between start and end, inclusive */
    pixval = start+offset;
    for (i = start; i<= end; i++) {
      if (pixval > end_index)
	pixval -= (end_index-start_index+1);
      temp_color_cell.pixel = pixval;
      temp_color_cell.red = ((int)cmap_vals[3*(int)(cumul+.01)])<<8;
      temp_color_cell.green = ((int)cmap_vals[3*(int)(cumul+.01)+1])<<8;
      temp_color_cell.blue = ((int)cmap_vals[3*(int)(cumul+.01)+2])<<8;
      CsStoreColor(info, info->cmap, &temp_color_cell);
      cumul += fincr;
      pixval ++;
    }


    /* All pixels in colormap to right of saturation are set to saturation */

    temp_color_cell.red = cmap_vals[255*3]<<8;
    temp_color_cell.green = cmap_vals[255*3+1]<<8;
    temp_color_cell.blue = cmap_vals[255*3+2]<<8;
    start = saturation+1;
    if (saturation < start_index)
      start = start_index;
    pixval = start+offset;
    for (i = start; i <= end_index; i++) {
      if (pixval > end_index)
	pixval -= (end_index-start_index+1);
      temp_color_cell.pixel = pixval;
      CsStoreColor(info, info->cmap, &temp_color_cell);
      pixval++;
    }
}

void CsPutImage 
(  CsInfoType    *info, 
   Window        window, 
   GC            gc, 
   XImage        *ximage_src,
   int           src_x, 
   int           src_y, 
   int           dest_x, 
   int           dest_y, 
   int           width, 
   int           height
)
{
   int           row, col;
   int           src_width, src_height, dst_pos, src_pos, eoln_incr, bytes;
   unsigned long memsize;
   XImage        *ximage_dst;
   unsigned char *dst_data, *src_data;

 
   /* If the depth wasn't set up for the screen, just do the
    * default XPutImage -- also, just do the default if the
    * color depth is 8 since we already have byte data
    */

   if (info->depth == 8) 
   {
       XPutImage
       (  info->display, 
          window, gc, ximage_src,
          src_x, src_y, 
          dest_x, dest_y, 
          width, height
       );
     

     return;
   }
 
   src_data = (unsigned char *)ximage_src->data;
   src_width = ximage_src->width;
   src_height = ximage_src->height;
 
   /* 
    * Decrease width and height if they move OFF the image 
    */

   if (src_x + width > src_width) 
      width = src_width - src_x;
   if (src_y + height >src_height) 
      height = src_height - src_y;
 
   /* 
    * Don't do anything in this case 
    */

   if ((width<=0)||(height<=0))
   {
       return;
   }

   memsize = width * height * sizeof(unsigned char);
   switch (info->depth)
   {
      case 8:
         bytes = 1;
         break;

      case 16:
         bytes = 2;
         break;

      case 24:
         bytes = 4;
         break;

      case 32:
         bytes = 4;
         break;
 
      default:         /* Actually this is an error, but this might work. */
         bytes = 1;
   }

   if ((memsize * bytes <= 0) ||
       (height != (int)((memsize * bytes) / (width * bytes))) ||
       (width != (int)((memsize * bytes) / (height * bytes)))) 
   {
      printf("Memsize is 0 or negative.  Cannot draw!\n");
      return;
   }
 
   dst_data = (unsigned char *) malloc (memsize * bytes);
   if (!dst_data) 
   {
      printf("Out of memory.\n");
      return;
   }
 
   dst_pos = 0;
   src_pos = src_y * src_width + src_x;
   eoln_incr = src_width-width;
   for (row =0; row < height; row ++) 
   {
      for (col = 0; col < width; col ++) 
         dst_data [dst_pos++] = src_data [src_pos++];

      src_pos += eoln_incr;
   }
 
   CsUseNewColorDepth (info, dst_data, memsize);
 
   ximage_dst = XCreateImage 
   (  info -> display,
      DefaultVisual
      (  info->display, DefaultScreen(info->display)),
         info->depth,
         ZPixmap, 0, (char *) dst_data,
         width, height,
         BitmapPad(info->display), width*bytes
      );
   
   XPutImage 
   (  info->display, window, gc, 
      ximage_dst,
      0, 0, 
      dest_x, dest_y, 
      width, height
   );
 
   XDestroyImage(ximage_dst);
 
 
   return;
}

void CsUseNewColorDepth 
(  CsInfoType      *info, 
   unsigned char   *in_data, 
   unsigned long   memsize
)
{
   unsigned int         ct, top;
   unsigned short       *sdata;
   unsigned long        *ldata;
   unsigned char        *data;
   unsigned long        *truecolors;

   if (info->depth==8)
   {
    return;
   }

   truecolors = info->cmap_pixels;
   top = memsize - 1;
   data = in_data + top;
 
   switch(info -> depth)
   {
      case 8:
         break;
      case 16:
         sdata = ((unsigned short *)in_data) + top;
         for (ct = 0; ct < memsize; ct ++) 
         {
            *sdata-- = (unsigned short)(truecolors[*data--]);
         }
         break;

     case 24:
     case 32:
        ldata = ((unsigned long *)in_data) + top;
        for (ct = 0;  ct < memsize; ct ++) 
        {
           *ldata-- = truecolors[*data--];
        }
        break;

     default:
        break;
  }
 
}
