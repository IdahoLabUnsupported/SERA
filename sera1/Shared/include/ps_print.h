/* ==========================================================================
 * 
 * Definitions for the print subsystem.
 *
 * 
 * Harkin, 7/99
 *
 * ==========================================================================
 */

#ifndef True
typedef unsigned int Boolean;
#define True         1
#define False        0
#endif

/*
 * Margin size definitions.  For Postscript, assume 1.0 inch, which is
 * 72 ep's in Postscript measure.
 */

#define PS_LEFT_MARGIN     72
#define PS_RIGHT_MARGIN    72
#define PS_TOP_MARGIN      72
#define PS_BOT_MARGIN      72

/*
 * Definitions of various parameters
 */

#define STD_SS             0           /* 8.5 x 11 */
#define LEGAL_SS           1           /* 8.5 x 14 */
#define BSIZE_SS           2           /* 11.0 x 17.0 */

#define PORTRAIT           0           /* Page orientations */          
#define LANDSCAPE          1          

#define GRAY_MAP           0
#define RGB_MAP            1

/* 
 * Definitions for the page printing facilities.  This includes both
 * image and text printing, although this facility is oriented towards
 * printing images and possibly some titles and short descriptive data,
 * not full text pages.
 */

/*
 * Definition of the data to be sent to the print routine.  All dimensions
 * are in inches.
 */

/*
 * Definition of an image for printing.
 */

typedef struct
{
   char     *id_image;          /* image data, single byte or 3 byte RGB     */
   int      id_type;            /* image type - 0 = GRAY, 1 = RGB            */

   float    id_xloc;            /* Location on the page (inches), where x is */
   float    id_yloc;            /* horz and y is vert, (0,0) is LOWER LEFT.  */

   int      id_width;           /* image width in pixels                     */
   int      id_height;          /* image height in pixels                    */

   float    id_xsize;           /* desired print width in inches             */
   float    id_ysize;           /* desired print height in inches            */
} ps_image_data_T;

typedef struct
{
   char      *td_data;          /* Text to write with embedded newlines      */
   float     td_xloc;           /* Location on page of center as per         */
   float     td_yloc;           /* pagedata_T                                */
   float     td_size;           /* height of text in eps (point size)        */
} ps_text_data_T;   

/* 
 * Internal structure for maintaining the data on the current page and
 * print object.
 */


typedef struct
{
   int           pa_width;         /* page size in eps */
   int           pa_height;        /* page size in eps */
   int           pa_orientation;   /* Landscape or Portrait */

   FILE          *pa_outfile;      /* output file (probably a pipe) */

   Boolean       pa_pending;       /* page data is current          */
   Boolean       pa_written;       /* current page has been written */

   /*
    * Definitions for Postscript with 72 dot/inch resolution.
    */

   int           pa_ps_minx, pa_ps_miny;   /* Logical (0,0) of page in eps  */
   int           pa_ps_maxx, pa_ps_maxy;   /* Logical extent of page in eps */
   int           pa_ps_xlen, pa_ps_ylen;   /* Length of page in eps         */

} pagedata_T; 

typedef struct
{
   int      od_llx, od_lly;       /* lower left corner */
   int      od_xlen, od_ylen;     /* size              */
} ps_objdata_T;


/*
 * Data needed by print setup routine.
 */

/* here is an example of use:
   PsStart( file, STD_SS, PORTRAIT)
   PsCenter( STD_SS, &ps_image_data );
   PsWriteImage( &ps_image_data);
   PsNewPage();

   PsWriteImage( &ps_image_data);
   //Note: no NewPage here 
   PsPrintPage();
*/

/*
 * Prototypes
 */
 
int PsWriteImage (ps_image_data_T *); /* Write nd image to a page         */
int PsWriteText  (ps_text_data_T *);  /* Write a piece of text to the page */
int PsStart (FILE *, int, int);    /* Start and new page for printing      */
void PsNewPage(); /* flush the previous page and go to a new blank page */
void PsPrintPage ();                /* Send the current page to the printer */

void PsCenter (int, ps_image_data_T *);
void PsLowerLeft (int, ps_image_data_T *);
void PsLowerRight (int, ps_image_data_T *);
void PsUpperLeft (int, ps_image_data_T *);
void PsUpperRight (int, ps_image_data_T *);
void PsUpperCenter (int, ps_image_data_T *);
void PsLowerCenter (int, ps_image_data_T *);
