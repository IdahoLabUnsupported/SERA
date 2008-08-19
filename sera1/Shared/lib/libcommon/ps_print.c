/* =============================================================> pswrite.c
 *
 * Routines to output an image in postscript form plus support and
 * utility routines.
 *
 * Harkin, 8/97
 *    revised, 7/99 
 * =================================================================
 *     
 */

#include <stdio.h>
#include <errno.h>
#include <math.h>
#include "ps_print.h"

#define MASK   0x000000FF


/*
 * Array of conversions for pixel value to hexadecimal.
 */


static unsigned char *hexstring  = "\
000102030405060708090A0B0C0D0E0F\
101112131415161718191A1B1C1D1E1F\
202122232425262728292A2B2C2D2E2F\
303132333435363738393A3B3C3D3E3F\
404142434445464748494A4B4C4D4E4F\
505152535455565758595A5B5C5D5E5F\
606162636465666768696A6B6C6D6E6F\
707172737475767778797A7B7C7D7E7F\
808182838485868788898A8B8C8D8E8F\
909192939495969798999A9B9C9D9E9F\
A0A1A2A3A4A5A6A7A8A9AAABACADAEAF\
B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF\
C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF\
D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF\
E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF\
F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF";

static pagedata_T   pagedata;   /* static page data */


void puttitle (), zoomin ();
int  PsGray (ps_image_data_T *);
int  PsRGB (ps_image_data_T *);
void PsHeader (FILE *, int, int, int, int);
void PsFooter (FILE *, char *, int, int, float);


int PsStart 
(  FILE      *outfile,
   int       sheet_size,
   int       orientation
)
{
   /*
    * If no output sink is given or if the page is already
    * opened, nothing can proceed.
    */

   if (outfile == NULL)
      return (-1);

   if (pagedata.pa_pending) {
      if (pagedata.pa_written)
         return (-1);
      else
         ;
   }
   
   pagedata.pa_outfile = outfile;

   /*
    * We're going to start up a sheet for Postscript printing.  Set up
    * the page description for the given sheet size.  Postscript measures
    * everything is 72 dots per inch (ep's).  The logical location 
    * assumes a 1.0 inch margin, which could be changed if need be.
    */

   switch (sheet_size)
   {
      case STD_SS: /* 8.5 x 11.0 */
         if (orientation == PORTRAIT)
         {
             pagedata.pa_width = 612;
             pagedata.pa_height = 792;
             pagedata.pa_orientation = PORTRAIT;

             pagedata.pa_pending = True;
             pagedata.pa_written = False;
         }
         else
         {
             pagedata.pa_width = 792;
             pagedata.pa_height = 612;
             pagedata.pa_orientation = LANDSCAPE;

             pagedata.pa_pending = True;
             pagedata.pa_written = False;
         }
         break;

      case LEGAL_SS: /* 8.5 x 14.0 */
         if (orientation == PORTRAIT)
         {
             pagedata.pa_width = 612;
             pagedata.pa_height = 1008;
             pagedata.pa_orientation = PORTRAIT;

             pagedata.pa_pending = True;
             pagedata.pa_written = False;
         }
         else
         {
             pagedata.pa_width = 792;
             pagedata.pa_height = 612;
             pagedata.pa_orientation = LANDSCAPE;

             pagedata.pa_pending = True;
             pagedata.pa_written = False;
         }
         break;

      case BSIZE_SS:   /* 11.0 x 17.0 */
         if (orientation == PORTRAIT)
         {
             pagedata.pa_width = 792;
             pagedata.pa_height = 1224;
             pagedata.pa_orientation = PORTRAIT;

             pagedata.pa_pending = True;
             pagedata.pa_written = False;
         }
         else
         {
             pagedata.pa_width = 1224;
             pagedata.pa_height = 792;
             pagedata.pa_orientation = LANDSCAPE;

             pagedata.pa_pending = True;
             pagedata.pa_written = False;
         }
         break;

      default:
         return (-1);
   }

   /*
    * Set up the page margins
    */

   pagedata.pa_ps_minx = PS_LEFT_MARGIN; 
   pagedata.pa_ps_maxx = pagedata.pa_width - PS_RIGHT_MARGIN;
   pagedata.pa_ps_miny = PS_BOT_MARGIN; 
   pagedata.pa_ps_maxy = pagedata.pa_height - PS_TOP_MARGIN;
   pagedata.pa_ps_xlen = pagedata.pa_ps_maxx - pagedata.pa_ps_minx; 
   pagedata.pa_ps_ylen = pagedata.pa_ps_maxy - pagedata.pa_ps_miny; 

   PsHeader (outfile, pagedata.pa_ps_minx, pagedata.pa_ps_miny, 
             pagedata.pa_ps_xlen, pagedata.pa_ps_ylen);

   return (0);
}

void PsPrintPage ()
{
  /* call PsFooter to finish off page.  PsFooter currently dosen't use 
     the last four parameters */
   PsFooter(pagedata.pa_outfile,"",0,0,0.0);

   return;
}

void PsNewPage(){
  fprintf(pagedata.pa_outfile, "\nshowpage\n\n%%EndPage\n");
}

int PsWriteImage (ps_image_data_T *idata)
{
   int err;

   /*
    * Decide what the desired output type is, and call the routine.
    */

   if (idata->id_type == RGB_MAP)
      err = PsRGB (idata);
   else
      err = PsGray (idata);       /* default choice */

   return (err);
}

int PsWriteText (ps_text_data_T *tdata)
{
   
   /*
    * Place the text in the desired location and send it.
    */

   return (1);
}

int PsGray (ps_image_data_T *idata)   
{
   int           llx, lly, ct, r, scalex, scaley;
   unsigned int  img;
   unsigned char hex[65], *hp, *sp, *ip;
   int           xsize, ysize;

   /*
    * If xsize or ysize are less than 0.01, it will be treated 
    * as zero, indicating that full page is desired.  Make sure that
    * the expaned size is not larger than the page.
    */

   xsize = idata->id_xsize;
   ysize = idata->id_ysize;

   if (xsize < 0.01)
      xsize = pagedata.pa_width;
   if (ysize < 0.01)
      ysize = pagedata.pa_height;
   if (xsize > pagedata.pa_width)
      xsize = pagedata.pa_width;
   if (ysize > pagedata.pa_height)
      ysize = pagedata.pa_height;

   /* Calculate the lower left hand corner. */

   llx = pagedata.pa_ps_minx + 
      (int) (pagedata.pa_ps_xlen - xsize * 72.0) / 2.0;
   if (llx < pagedata.pa_ps_minx)
      llx = pagedata.pa_ps_minx;
   lly = pagedata.pa_ps_miny + 
      (int) (pagedata.pa_ps_ylen - ysize * 72.0) / 2.0;
   if (lly < pagedata.pa_ps_miny)
      lly = pagedata.pa_ps_miny;

   /*
    * Calculate the scale factors.
    */

   scalex = xsize * 72;
   scaley = ysize * 72;

  /*
   * Output the postscript program to stdout.
   */

   /* Output the definitions necessary for the image function. */

   fprintf (pagedata.pa_outfile, "%% Create a string to hold a line of data\n");
   fprintf (pagedata.pa_outfile, "/picstr %d string def\n\n", idata->id_width);

   fprintf (pagedata.pa_outfile, "%% Create a macro to dump the image lines.\n");
   fprintf (pagedata.pa_outfile, 
         "/imageline {currentfile picstr readhexstring pop } def\n\n");

   fprintf (pagedata.pa_outfile, "%% The lower left corner setting\n");
   fprintf (pagedata.pa_outfile, "%d %d translate\n\n", llx, lly);

   /* Calculate the scale to be applied to get the correct image size. */

   fprintf (pagedata.pa_outfile, "%% Size of the image in 1/72 inch scale\n");
   fprintf (pagedata.pa_outfile, "%d %d scale\n\n", scalex, scaley);

   /* Output the dimension and matrix parts of the postscript image
      function.

      xdim ydim bits/pix [xdim 0 0 -ydim 0 ydim]       */

   fprintf (pagedata.pa_outfile, "%% Dimensions of the data\n");
   fprintf 
   (  pagedata.pa_outfile, 
      "%d %d 8 [ %d 0 0 -%d 0 %d ]\n", 
      idata->id_width, idata->id_height, 
      idata->id_width, idata->id_height, 
      idata->id_height
   );
   fprintf (pagedata.pa_outfile, "{imageline} image\n");
   fprintf (pagedata.pa_outfile, "\n");

   /* Now output the image data.  First convert it to hex and then put
      it in the file. */

   ct = 0;
   hp = hex;
   ip = idata->id_image;
   for (r = 0; r < idata->id_width * idata->id_height; r++)
   {
      img = (unsigned int) *ip++;
      img &= MASK;
      sp = hexstring + (img << 1); /* times 2 for 2 hex digits per byte */
      *hp++ = *sp++;               /* First digit */
      *hp++ = *sp;                 /* Second digit */
      ct++;
      if (ct == 32)
      {
         *hp = '\0';
         fprintf (pagedata.pa_outfile, "%s\n", hex);
         ct = 0;
         hp = hex;
      }
   }

   *hp = '\0';
   fprintf (pagedata.pa_outfile, "%s\n", hex);
   fprintf (pagedata.pa_outfile, "\n");

   return (0);
}


int PsRGB (ps_image_data_T *idata)   
{
   int             llx, lly, ct, pct, r, scalex, scaley;
   int             xsize, ysize;
   unsigned int    img;
   char            hex[97], *hp, *sp;
   unsigned char   *ip;

   /*
    * If xsize or ysize are less than 0.01, it will be treated 
    * as zero, indicating that full page is desired.  Make sure that
    * the expaned size is not larger than the page.
    */
   
   xsize = idata->id_xsize;
   ysize = idata->id_ysize;

   if (xsize < 0.01)
      xsize = pagedata.pa_width;
   if (ysize < 0.01)
      ysize = pagedata.pa_height;
   if (xsize > pagedata.pa_width)
      xsize = pagedata.pa_width;
   if (ysize > pagedata.pa_height)
      ysize = pagedata.pa_height;


   /* Calculate the lower left hand corner. */

   llx = pagedata.pa_ps_minx +
      (int) (pagedata.pa_ps_xlen - xsize * 72.0) / 2.0;
   if (llx < pagedata.pa_ps_minx)
      llx = pagedata.pa_ps_minx;
   lly = pagedata.pa_ps_miny +
      (int) (pagedata.pa_ps_ylen - ysize * 72.0) / 2.0;
   if (lly < pagedata.pa_ps_miny)
      lly = pagedata.pa_ps_miny;


   /*
    * Calculate the scale factors.
    */

   scalex = xsize * 72;
   scaley = ysize * 72;

  /*
   * Output the postscript program to stdout.  
   */

   /* Output the definitions necessary for the image function. */

   fprintf (pagedata.pa_outfile, "%% Create a string to hold a line of data\n");
   fprintf (pagedata.pa_outfile, "/picstr %d string def\n\n", 
      3 * idata->id_width);

   fprintf (pagedata.pa_outfile, "%% Create defs for gray conversion\n");
   fprintf (pagedata.pa_outfile, "/gline %d string def\n\n", idata->id_width);
   fprintf (pagedata.pa_outfile, "/rgbindx %d def\n\n", 0);
   fprintf (pagedata.pa_outfile, "/npixls %d def\n\n", 0);

   fprintf (pagedata.pa_outfile, "%% Create a macro to dump the image lines.\n");
   fprintf (pagedata.pa_outfile, 
         "/imageline {currentfile picstr readhexstring pop } def\n\n");

   fprintf (pagedata.pa_outfile, "%% The lower left corner setting\n");
   fprintf (pagedata.pa_outfile, "%d %d translate\n\n", llx, lly);

   fprintf (pagedata.pa_outfile, "%% Size of the image in 1/72 inch scale\n");
   fprintf (pagedata.pa_outfile, "%d %d scale\n", scalex, scaley);
   fprintf (pagedata.pa_outfile, "\n");

   /*
    * Check to see if the printer handles color.  If so, continue,
    * otherwise, convert to gray scale.  "\colorimage" is the postscript
    * function.  The printer will answer true or false to a question
    * as to its local existance.
    */

   fprintf (pagedata.pa_outfile, "%% Check to see if colorimage is defined.  If not\n");
   fprintf (pagedata.pa_outfile, "%% define it. \n\n");
   fprintf (pagedata.pa_outfile, "%% Much of this comes from xv ans xwd2ps\n");
   
   fprintf (pagedata.pa_outfile, "/colorimage where\n");
   fprintf (pagedata.pa_outfile, "{ pop }            %% ignore the dict for colorimage\n");
   fprintf (pagedata.pa_outfile, "{                  %% convert to gray\n");
   fprintf (pagedata.pa_outfile, "   /colortogray\n");
   fprintf (pagedata.pa_outfile, "   {\n");
   fprintf (pagedata.pa_outfile, "      /rgbdata exch store    %% call input 'rgbdata'\n");
   fprintf (pagedata.pa_outfile, "      rgbdata length 3 idiv\n");
   fprintf (pagedata.pa_outfile, "      /npixls exch store\n");
   fprintf (pagedata.pa_outfile, "      /rgbindx 0 store\n");
   fprintf (pagedata.pa_outfile, "      0 1 npixls 1 sub\n");
   fprintf (pagedata.pa_outfile, "      {\n");
   fprintf (pagedata.pa_outfile, "         grays exch\n");
   fprintf (pagedata.pa_outfile, "         rgbdata rgbindx       get 20 mul    %% Red\n");
   fprintf (pagedata.pa_outfile, "         rgbdata rgbindx 1 add get 32 mul    %% Green\n");
   fprintf (pagedata.pa_outfile, "         rgbdata rgbindx 2 add get 12 mul    %% Blue\n");
   fprintf (pagedata.pa_outfile, "         add add 64 idiv      %% I = .5G + .31R + .18B\n");
   fprintf (pagedata.pa_outfile, "         put\n");
   fprintf (pagedata.pa_outfile, "         /rgbindx rgbindx 3 add store\n");
   fprintf (pagedata.pa_outfile, "      } for\n");
   fprintf (pagedata.pa_outfile, "      grays 0 npixls getinterval\n");
   fprintf (pagedata.pa_outfile, "   } bind def\n");
   fprintf (pagedata.pa_outfile, "\n");
   fprintf (pagedata.pa_outfile, "   %% Utility procedure for colorimage operator.\n");
   fprintf (pagedata.pa_outfile, "   %% This procedure takes two procedures off the\n");
   fprintf (pagedata.pa_outfile, "   %% stack and merges them into a single procedure.\n");

   fprintf (pagedata.pa_outfile, "   /mergeprocs def\n");
   fprintf (pagedata.pa_outfile, "   {\n");
   fprintf (pagedata.pa_outfile, "      dup length\n");
   fprintf (pagedata.pa_outfile, "      3 -1 roll\n");
   fprintf (pagedata.pa_outfile, "      dup\n");
   fprintf (pagedata.pa_outfile, "      length\n");
   fprintf (pagedata.pa_outfile, "      dup\n");
   fprintf (pagedata.pa_outfile, "      5 1 roll\n");
   fprintf (pagedata.pa_outfile, "      3 -1 roll\n");
   fprintf (pagedata.pa_outfile, "      add\n");
   fprintf (pagedata.pa_outfile, "      array cvx\n");
   fprintf (pagedata.pa_outfile, "      dup\n");
   fprintf (pagedata.pa_outfile, "      3 -1 roll\n");
   fprintf (pagedata.pa_outfile, "      0 exch\n");
   fprintf (pagedata.pa_outfile, "      putinterval\n");
   fprintf (pagedata.pa_outfile, "      dup\n");
   fprintf (pagedata.pa_outfile, "      4 2 roll\n");
   fprintf (pagedata.pa_outfile, "      putinterval\n");
   fprintf (pagedata.pa_outfile, "   } bind def\n");
   fprintf (pagedata.pa_outfile, "\n");

   fprintf (pagedata.pa_outfile, "   /colorimage   %% def\n");
   fprintf (pagedata.pa_outfile, "   {\n");
   fprintf (pagedata.pa_outfile, "      pop pop     %% remove 'false 3' operands\n");
   fprintf (pagedata.pa_outfile, "      {colortogray} mergeprocs\n");
   fprintf (pagedata.pa_outfile, "      image\n");
   fprintf (pagedata.pa_outfile, "    } bind def\n");
   fprintf (pagedata.pa_outfile, "} ifelse          %% end of 'false' case\n");
   fprintf (pagedata.pa_outfile, "\n");

   /* Output the dimension and matrix parts of the postscript image
    * function.
    *
    * xdim ydim bits/pix [xdim 0 0 -ydim 0 ydim]       
    */

   fprintf (pagedata.pa_outfile, "%% Dimensions of the data\n");
   fprintf 
   (  pagedata.pa_outfile, "%d %d 8 [ %d 0 0 -%d 0 %d ]\n", 
      idata->id_width, idata->id_height, 
      idata->id_width, idata->id_height, 
      idata->id_height
   );
   fprintf (pagedata.pa_outfile, "{imageline} false 3 colorimage\n");
   fprintf (pagedata.pa_outfile, "\n");

   /* Now output the image data.  First convert it to hex and then put
      it in the file. */

   ct = 0;
   hp = hex;
   ip = idata->id_image;
   for (r = 0; r < idata->id_width * idata->id_height; r++)
   {
       /*printf ( "width: %d  height: %d  r = %d\n", idata->id_width,idata->id_height, r );*/
      for (pct = 0; pct < 3; pct++)
      {
          /*printf ( "%p\n", ip );*/
          
          img = (unsigned int) *ip++;
          img &= MASK;
          sp = hexstring + (img << 1); /* times 2 for 2 hex digits per byte */
          *hp++ = *sp++;               /* First digit */
          *hp++ = *sp;                 /* Second digit */
      }

      /* 
       * Do 16 pixels per line.
       */

      ct++;
      if (ct == 16)
      {
         *hp = '\0';
         fprintf (pagedata.pa_outfile, "%s\n", hex);
         ct = 0;
         hp = hex;
      }
   }
   
   *hp = '\0';
   fprintf (pagedata.pa_outfile, "%s\n", hex);
   fprintf (pagedata.pa_outfile, "\n");

   fprintf (pagedata.pa_outfile, "\n");
   /*fprintf (pagedata.pa_outfile, "showpage\n");

   fprintf (pagedata.pa_outfile, "%% close temp dictionary and restore state\n\n");

   fprintf (pagedata.pa_outfile, "end\n");
   fprintf (pagedata.pa_outfile, "origstate restore\n");
   fprintf (pagedata.pa_outfile, "\n");
   fprintf (pagedata.pa_outfile, "%%---------- End of psdisplay ----------\n");
   */
   return(0);
}

/*
 * Output a title on the page.
 */

/*
void puttitle (outfile, title, xc, yc)
   FILE         *outfile;
   char         *title;
   int          xc, yc;
{  
   int fsize, min, yloc, xloc;

   fprintf (outfile, "%%     Output the label\n");
   fprintf (outfile, "initmatrix\n");

   fsize = 12;
   fprintf (outfile, "/Times-Bold findfont %d scalefont setfont\n", fsize);
   fprintf (outfile, "(%s) stringwidth\n", title);

   fprintf (outfile, "cvi neg %d add 10 sub exch\n", yc);
   fprintf (outfile, "cvi 2 idiv neg %d add exch\n", xc);

   fprintf (outfile, "moveto\n");
   fprintf (outfile, "(%s) show\n", title);

}
*/

void PsHeader 
(
   FILE        *outfile,
   int         startx, 
   int         starty, 
   int         width, 
   int         height
)
{
  /*
   * Output the header stuff.
   */

   fprintf (outfile, "%%!PS-Adobe-2.0 EPSF-2.0\n");
   fprintf (outfile, "%%%%Title: title \n");
   fprintf (outfile, "%%%%Creator: BNCT_rtpe - Rev. 14/02/97 - GJH\n");
   fprintf (outfile, "%%%%BoundingBox: %d %d %d %d\n",
         startx, starty, width, height);

   fprintf (outfile, "%%%%Pages: 1\n");
   fprintf (outfile, "%%%%DocumentFonts:\n");
   fprintf (outfile, "%%%%EndComments\n");
   fprintf (outfile, "%%%%EndProlog\n");
   fprintf (outfile, "\n%%%%Page: 1 1\n\n");

   /*
    * Remember the original state and build a temporary dictionary.
    */

   fprintf (outfile, "/origstate save def\n\n");
   fprintf (outfile, "20 dict begin\n\n");

   return;
}

void PsFooter 
(
   FILE     *outfile,
   char     *title,
   int      xstart,
   int      ystart,
   float    xwidth
)
{


   /*
    * Output the commands to dump the page, close the dictionary
    * and restore the previous state.
    */

   fprintf (outfile, "\nshowpage\n");

   /*fprintf (outfile, "\nend\n");
     fprintf (outfile, "\norigstate restore\n");*/
   fprintf (outfile, "\n%%%%Trailer\n");

   fprintf (outfile, "%% close temp dictionary and restore state\n\n");

   fprintf (outfile, "end\n");
   fprintf (outfile, "origstate restore\n");
   fprintf (outfile, "\n");
   fprintf (outfile, "%%---------- End of psdisplay ----------\n");
   fprintf (outfile, "%%EndPage\n%%EOF\n");

   return;
}


void PsCenter (int pagesize, ps_image_data_T *image_data)
{
   float     llx, lly;
   float     iwidth, iheight, xcent, ycent;

   switch (pagesize)
   {
      case STD_SS:
         iwidth = 8.5;
         iheight = 11.0;
         break;

      case LEGAL_SS:
         iwidth = 8.5;
         iheight = 14.0;
         break;

      case BSIZE_SS:
         iwidth = 11.0;
         iheight = 17.0;
         break;

      default:
         iwidth = 8.5;
         iheight = 11.0;
         break;
   }

   /*
    * Find the center and move half the distance of the picture size
    * to the left and down.
    */

   xcent = iwidth / 2.0; 
   ycent = iheight / 2.0; 

   image_data->id_xloc = xcent - image_data->id_xsize / 2.0; 
   image_data->id_yloc = ycent - image_data->id_ysize / 2.0; 

   return;
}

