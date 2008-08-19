/*
 * required motif include files
 */

#include <Xm/Xm.h>


/*
 * Some Convenience Routines
 */
#ifndef IGNORE_XPM_PIXMAP

/*
 * Copyright 1990 GROUPE BULL
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of GROUPE BULL not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  GROUPE BULL makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * GROUPE BULL disclaims all warranties with regard to this software,
 * including all implied warranties of merchantability and fitness,
 * in no event shall GROUPE BULL be liable for any special,
 * indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits,
 * whether in an action of contract, negligence or other tortious
 * action, arising out of or in connection with the use
 * or performance of this software.
 *
 */

#ifndef XPMP_h
#define XPMP_h

#include <stdio.h>
#include "memory_tools.h"

#ifndef XPM_h
#define XPM_h

#define PixmapSuccess          0
#define PixmapOpenFailed       1
#define PixmapFileInvalid      2
#define PixmapNoMemory         3
#define PixmapParseColorFailed 4
#define PixmapAllocColorFailed 5

typedef enum { CMUseDepth, CMForceMono, CMForceColor } ColorMode;

typedef struct {
    char *name;
    char *value;
    Pixel pixel;
} ColorSymbol;

typedef struct {
    char *type;
    int ncolors;
    char ***colorTable;
    Pixel *pixels;
    char *hints_cmt;
    char *colors_cmt;
    char *pixels_cmt;
    char *rgb_fname;
} XpmInfo;

typedef struct _MData
{
    unsigned int type;
    union
    {
        FILE *file;
        char **data;
    } stream;
    char         *cptr;
    unsigned int line;
    int CommentLength;
    char Comment[BUFSIZ];
    char *Bcmt, *Ecmt, Bos, Eos;
} MData;

/* minimal portability layer between ansi and KR C
 */

/* forward declaration of functions with prototypes */

#ifdef __cplusplus
extern "C" {
#endif

static int XCreatePixmapFromData(Display *display, Visual *visual, Drawable d, Colormap colormap, char **data, unsigned int depth, Pixmap *pixmap_return, unsigned int *width_return, unsigned int *height_return, Pixel **pixels_return, unsigned int *npixels_return, ColorSymbol *colorsymbols, unsigned int numsymbols, XpmInfo *infos);
static int CreatePixmap(Display *display, Visual *visual, Drawable d, Colormap colormap, MData *data, unsigned int depth, Pixmap *pixmap_return, unsigned int *width_return, unsigned int *height_return, Pixel **pixels_return, unsigned int *npixels_return, ColorSymbol *colorsymbols, unsigned int numsymbols, XpmInfo *infos, ColorMode cmode);
static unsigned int atoui(char *p);
static void mnextstring(MData *mdata);
static unsigned int mnextui(MData *mdata);
static char mgetc(MData *mdata);
static char mungetc(int c, MData *mdata);
static void mskipwhite(MData *mdata);
static unsigned int mnextw(MData *mdata, char *buf);
static void mgetcmt(MData *mdata, char **cmt);
static int mdataopen(char **data, MData **mdataptr);
static void mclose(MData *mdata);
static int visualType(Visual *visual);
static void freeColorTable(char ***colorTable, int ncolors);
static void XFreeXpmInfo(XpmInfo *infos);

#ifdef __cplusplus
}                                             /* for C++ V2.0 */
#endif

#endif

#ifdef SYSV
#define bcopy(source, dest, count) memcpy(dest, source, count)
#endif

#define MAXPRINTABLE 93                 /* number of printable ascii chars
                                         * minus \ and " for string compat.
                                         */

static char *printable =
" .XoO+@@#$%&*=-;:?>,<1234567890\
qwertyuipasdfghjklzxcvbnmMNBVCZ\
ASDFGHJKLPIUYTREWQ!~^/()_`'][{}|" ;
                                        /* printable begin with a space, so
                                           in most case, due to my algorythm,
                                           when the number of different colors
                                           is less than MAXPRINTABLE, it will
                                           give a char follow by "nothing"
                                           (a space) in the readable xpm file
                                         */

#define MARRAY 0
#define MFILE  1
#define MPIPE  2

typedef unsigned char  byte;      /* byte type */

#define EOL '\n'
#define TAB '\t'
#define SPC ' '

typedef struct
{
    char *type;                 /* key word */
    char *Bcmt;                 /* string begining comments */
    char *Ecmt;                 /* string ending comments */
    char  Bos;                  /* character begining strings */
    char  Eos;                  /* character ending strings */
    char *Strs;                 /* strings separator */
    char *Dec;                  /* data declaration string */
    char *Boa;                  /* string begining assignment */
    char *Eoa;                  /* string ending assignment */
} DataType;

extern DataType DataTypes[];

typedef struct
{                 /* rgb values and ascii names (from rgb text file) */
   int  r, g, b;  /* rgb values, range of 0 -> 65535 */
   char *name;    /* color mnemonic of rgb value */
} rgb_names;

/* Max number of rgb mnemonics allowed in rgb text file. */
#define MAX_RGBNAMES 1024

/*
 * removed by DEW 05/27/94 - see definition of ColorKeys below
extern char *ColorKeys[];
 *
 */
#define NKEYS 5                 /* number of ColorKeys */

#define MONO    2               /* key numbers for visual type, they must */
#define GRAY4   3               /* fit along with the number key of each */
#define GRAY    4               /* corresponding element in ColorKeys[] */
#define COLORK  5               /* defined in xpm.h */

#endif


/* Copyright 1990 GROUPE BULL -- See licence conditions in file COPYRIGHT */
/* XCrPFData.c:
 *
 *  XPM
 *  Create from Data utility for XPM file format
 *  Developed by Arnaud Le Hors
 */



/* ARGSUSED */
static int
XCreatePixmapFromData(display, visual, d, colormap, data, depth,
                      pixmap_return, width_return, height_return,
                      pixels_return, npixels_return, colorsymbols,
                      numsymbols, infos)
Display *display;
Visual *visual;
Drawable d;
Colormap colormap;
char **data;
unsigned int depth;
Pixmap *pixmap_return;
unsigned int *width_return;
unsigned int *height_return;
Pixel **pixels_return;
unsigned int *npixels_return;
ColorSymbol *colorsymbols;
unsigned int numsymbols;
XpmInfo *infos;
{
    MData *mdata;
    int ErrorStatus;

    /*
     * Initialize return values
     */
    if (pixmap_return) *pixmap_return = (Pixmap) NULL;
    if (width_return) *width_return = 0;
    if (height_return) *height_return = 0;
    if (pixels_return) *pixels_return = NULL;
    if (npixels_return) *npixels_return = 0;

    if ( (ErrorStatus = mdataopen(data, &mdata)) != PixmapSuccess )
        return(ErrorStatus);

    ErrorStatus = CreatePixmap(display, visual, d, colormap, mdata, depth,
                               pixmap_return, width_return, height_return,
                               pixels_return, npixels_return,
                               colorsymbols, numsymbols, infos, CMUseDepth);
    mclose(mdata);
    return(ErrorStatus);
}

/* Copyright 1990 GROUPE BULL -- See licence conditions in file COPYRIGHT
 *
 *  XPM
 *  Create utility for XPM file format
 *  Developed by Arnaud Le Hors
 */

static char *ColorKeys[] = {
    "s",                        /* key #1: symbol */
    "m",                        /* key #2: mono visual */
    "g4",                       /* key #3: 4 grays visual */
    "g",                        /* key #4: gray visual */
    "c",                        /* key #5: color visual */
};

#undef RETURN
#define RETURN(status) \
  { freeColorTable(colorTable, ncolors); \
    if (chars) MT_free((void *)chars); \
    if (pixelindex) MT_free((void *)pixelindex); \
    if (gc) XFreeGC(display, gc); \
    if (ximage) { MT_fake_free(ximage->data); XDestroyImage(ximage);} \
    if (pixels) MT_free((void *)pixels); \
    if (infos) XFreeXpmInfo(infos); \
    return(status); }

/* ARGSUSED */
static int
CreatePixmap(display, visual, d, colormap, data, depth, pixmap_return,
             width_return, height_return, pixels_return, npixels_return,
             colorsymbols, numsymbols, infos, cmode)
Display *display;
Visual *visual;
Drawable d;
Colormap colormap;
MData *data;
unsigned int depth;
Pixmap *pixmap_return;
unsigned int *width_return;
unsigned int *height_return;
Pixel **pixels_return;
unsigned int *npixels_return;
ColorSymbol *colorsymbols;
unsigned int numsymbols;
XpmInfo *infos;
ColorMode cmode;
{
    Pixmap              pixmap;
    unsigned int        cpp;                    /* chars per pixel */
    unsigned int        ncolors = 0;            /* number of colors */
    unsigned int        rncolors = 0;           /* read number of colors */
    char             ***colorTable = NULL;      /* color table */
    unsigned int        key;                    /* color key */
    Pixel              *pixels = NULL;          /* pixels colors */
    unsigned int        width, height;
    char               *chars = NULL, buf[BUFSIZ], *colorname;
    unsigned int       *iptr, *pixelindex = NULL;
    XColor              xcolor;
    XGCValues           gcv;
    GC                  gc = NULL;
    XImage             *ximage = NULL;
    unsigned int        a, b, x, y, l;
    Boolean             pixel_defined, defaultCase;
    unsigned int        virtual_depth;

    /*
     * parse the data
     */
    if ( cmode == CMForceMono )
    {
        virtual_depth = 1;
    }
    else if ( cmode == CMForceColor )
    {
        if ( depth > 1 )
        {
            virtual_depth = depth;
        }
        else
        {
            virtual_depth = 8;
        }
    }
    else
    {
        virtual_depth = depth;
    }

    /*
     * read hints: width, height, ncolors, chars_per_pixel
     */
    if (! ((width = mnextui(data)) && (height = mnextui(data))
           && (rncolors = mnextui(data)) && (cpp =  mnextui(data))))
        RETURN(PixmapFileInvalid);

    ncolors = rncolors;

    /*
     * store the hints comment line
     */
    if (infos) mgetcmt(data, &infos->hints_cmt);

    /*
     * read colors
     */
    colorTable = (char ***)MT_calloc(ncolors, sizeof(char **));
    if (!colorTable) RETURN(PixmapNoMemory);
    for(a = 0; a < ncolors; a++) {
        mnextstring(data);              /* skip the line */
        colorTable[a] = (char **)MT_calloc((NKEYS + 1), sizeof(char *));
        if (!colorTable[a]) RETURN(PixmapNoMemory);
        /*
         * read pixel value
         */
        *colorTable[a] = (char *)MT_malloc(cpp);
        if (!*colorTable[a]) RETURN(PixmapNoMemory);
        for (b = 0; b < cpp; b++)
            colorTable[a][0][b] = mgetc(data);

        /*
         * read color keys and values
         */
        while (l = mnextw(data, buf)) {
            for (key = 1; key < NKEYS + 1; key++)
                if (! strncmp(ColorKeys[key - 1], buf, l))
                    break;
            if (key <= NKEYS && (l = mnextw(data, buf))) {
                colorTable[a][key] = (char *)MT_malloc(l + 1);
                if (!colorTable[a][key])  RETURN(PixmapNoMemory);
                strncpy(colorTable[a][key], buf, l);
                colorTable[a][key][l] = '\0';
            } else
                RETURN(PixmapFileInvalid); /* key without value */
        }
    }

    /*
     * store the colors comment line
     */
    if (infos) mgetcmt(data, &infos->colors_cmt);

    /*
     * read pixels and index them on color number
     */
    pixelindex = (unsigned int *)MT_malloc(sizeof(unsigned int)*width*height);
    if (!pixelindex) RETURN(PixmapNoMemory);
    iptr = pixelindex;
    chars = (char *)MT_malloc(cpp);
    if (!chars) RETURN(PixmapNoMemory);
    for (y = 0; y < height; y++) {
        mnextstring(data);
        for (x = 0; x < width; x++, iptr++) {
            for (a = 0; a < cpp; a++)
                chars[a] = mgetc(data);
            for (a = 0; a < ncolors; a++)
                if (!strncmp(colorTable[a][0], chars, cpp))
                    break;
            if (a == ncolors) RETURN(PixmapFileInvalid); /* no color matches */
            *iptr = a;
        }
    }

    /*
     * store the pixels comment line
     */
    if (infos) mgetcmt(data, &infos->pixels_cmt);

    /*
     * build the image data
     */

    key = visualType(visual);
    pixels = (Pixel *)MT_malloc(sizeof(Pixel) * ncolors);
    if (!pixels) RETURN(PixmapNoMemory);

    /*
     * get pixel colors and index them
     */
    for (a = 0; a < ncolors; a++) {
        colorname = NULL;
        pixel_defined = False;

        /*
         * look for a defined symbol
         */
        if (numsymbols && colorTable[a][1]) {
            for (l = 0; l < numsymbols; l++)
                if (!strcmp(colorsymbols[l].name, colorTable[a][1]))
                  break;
            if (l != numsymbols) {
                if (colorsymbols[l].value)
                    colorname = colorsymbols[l].value;
                else
                    pixel_defined = True;
            }
        }
        if (! pixel_defined) {
            if (! colorname) {
                if (colorTable[a][key])
                    b = key;
                else {
                    for (b = key - 1; b > 1; b--)
                        if (colorTable[a][b])
                            break;
                    if (b == 1) {
                        for (b = key + 1; b < NKEYS + 1; b++)
                            if (colorTable[a][b])
                                break;
                        if (b == NKEYS + 1)
                            RETURN(PixmapFileInvalid);
                    }
                }
                colorname = colorTable[a][b];
            }
            if (! XParseColor(display, colormap, colorname, &xcolor))
                RETURN(PixmapParseColorFailed);
            if (XAllocColor(display, colormap, &xcolor)) {
/* too bad XAllocColorCells doesn't work on some visuals like monochrome...
              if (infos) {
                  for (b = 0; b < a; b++)
                      if (pixels[b] == xcolor.pixel)
                          break;
                  if (b != a) {
                      if (! XAllocColorCells(display, colormap, False, NULL, 0,
                                             pixels + a, 1))
                          RETURN(PixmapAllocColorFailed);
                      XFreeColors(display, colormap, &(xcolor.pixel), 1, 0);
                      xcolor.pixel = pixels[a];
                      XStoreColor(display, colormap, &xcolor);
                  }
              }
*/
            } else
                RETURN(PixmapAllocColorFailed);
            pixels[a] = xcolor.pixel;
        } else
            pixels[a] = colorsymbols[l].pixel;
    }

    /*
     * send the image to the server
     *
     * some of the algorithms implemented here come from the CreateXImage
     * function which is part of the xv-pl3 code written by
     * John Bradley, University of Pennsylvania (bradley@@cis.upenn.edu)
     *
     * In case depth is 1, 4, 6, 8, 24 or 32 build the XImage data,
     * otherwise build it by slowly but surely setting every pixel
     * with XPutPixel.
     */

    pixmap = XCreatePixmap(display, d, width, height, depth);
    gcv.function = GXcopy;
    gc = XCreateGC(display, pixmap, GCFunction, &gcv);
    defaultCase = False;
    switch (virtual_depth) {
    case 8:
    {
        byte *imagedata, *ip;
        imagedata = (byte *) MT_malloc(width*height);
        if (!imagedata) RETURN(PixmapNoMemory);
        ip = imagedata; iptr = pixelindex;
        for (y = 0; y < height; y++)
            for (x = 0; x < width; x++, iptr++)
                *ip++ = (byte) pixels[*iptr];
        ximage = XCreateImage(display, visual, depth, ZPixmap, 0,
                              (char *)imagedata, width, height, 8, 0);
        break;
    }
    case 1:
    {
        byte  *imagedata, *destline, *destptr, destmask;
        int  bperline;
        ximage = XCreateImage(display, visual, depth, XYPixmap, 0,
                              NULL, width, height, 8, 0);
        bperline = ximage->bytes_per_line;
        imagedata = (byte *) MT_malloc(bperline * height);
        if (!imagedata) RETURN(PixmapNoMemory);
        ximage->data = (char *) imagedata;
        iptr = pixelindex; destline = imagedata;
        if (ximage->bitmap_bit_order == MSBFirst)
            for (y = 0; y < height; y++, destline += bperline) {
                destmask = 0x80; destptr = destline;
                for (x = 0; x < width; x++, iptr++) {
                    if (pixels[*iptr] & 1)
                        *destptr |= destmask;
                    else
                        *destptr &= ~destmask;
                    if (!(destmask >>= 1)) {
                        destmask = 0x80;
                        destptr++;
                    }
                }
            }
        else
            for (y = 0; y < height; y++, destline += bperline) {
                destmask = 1; destptr = destline;
                for (x = 0; x < width; x++, iptr++) {
                    if (pixels[*iptr] & 1)
                        *destptr |= destmask;
                    else
                        *destptr &= ~destmask;
                    if (!(destmask <<= 1)) {
                        destmask = 1;
                        destptr++;
                    }
                }
            }
        break;
    }
    case 4:
    {
        byte  *imagedata, *ip, *lip;
        int  bperline, half;
        ximage = XCreateImage(display, visual, depth, ZPixmap, 0,
                              NULL, width, height, 8, 0);
        bperline = ximage->bytes_per_line;
        imagedata = (byte *) MT_malloc(bperline * height);
        if (!imagedata) RETURN(PixmapNoMemory);
        ximage->data = (char *) imagedata;
        if (ximage->bits_per_pixel == 4) {
            iptr = pixelindex; lip = imagedata;
            for (y = 0 ; y < height; y++, lip += bperline)
                for (x = 0, ip = lip, half = 0;
                     x < width; x++, iptr++, half++)
                    if (half & 1)
                        *ip++ |= (pixels[*iptr] & 0x0f)<<4;
                    else
                        *ip = pixels[*iptr] & 0x0f;
        } else if (ximage->bits_per_pixel == 8) {
            iptr = pixelindex; ip = imagedata;
            for (y = 0 ; y < height; y++)
                for (x = 0; x < width; x++, iptr++)
                    *ip++ = (byte) pixels[*iptr];
        } else {
	    MT_fake_free(ximage->data);
            XDestroyImage(ximage);
            defaultCase = True;
        }
        break;
    }
    case 6:
    {
        byte *imagedata, *ip;
        ximage = XCreateImage(display, visual, depth, ZPixmap, 0,
                              NULL, width, height, 8, 0);
        if (ximage->bits_per_pixel == 8) {
            imagedata = (byte *) MT_malloc(ximage->bytes_per_line*height);
            if (!imagedata) RETURN(PixmapNoMemory);
            ximage->data = (char *)imagedata;
            ip = imagedata; iptr = pixelindex;
            for (y = 0; y < height; y++)
                for (x = 0; x < width; x++, iptr++)
                    *ip++ = (byte) pixels[*iptr];
        } else {
	    MT_fake_free(ximage->data);
            XDestroyImage(ximage);
            defaultCase = True;
        }
        break;
    }
    case 24:
    case 32:
    {
        byte *imagedata, *ip;
        imagedata = (byte *) MT_malloc(4*width*height);
        if (!imagedata) RETURN(PixmapNoMemory);
        ximage = XCreateImage(display, visual, depth, ZPixmap, 0,
                              (char *)imagedata, width, height, 32, 0);
        ip = imagedata; iptr = pixelindex;
        if (ximage->byte_order == MSBFirst)
            for (y = 0; y < height; y++)
                for (x = 0; x < width; x++, iptr++) {
                    *ip++ = 0;
                    *ip++ = (pixels[*iptr]>>16) & 0xff;
                    *ip++ = (pixels[*iptr]>>8) & 0xff;
                    *ip++ =  pixels[*iptr] & 0xff;
                }
        else
            for (y = 0; y < height; y++)
                for (x = 0; x < width; x++, iptr++) {
                    *ip++ =  pixels[*iptr] & 0xff;
                    *ip++ = (pixels[*iptr]>>8) & 0xff;
                    *ip++ = (pixels[*iptr]>>16) & 0xff;
                    *ip++ = 0;
                }
        break;
    }
    default:
        defaultCase = True;
    }

    if (defaultCase) {
        ximage = XGetImage(display, pixmap, 0, 0,
                           width, height, AllPlanes, ZPixmap);
        if (!ximage) RETURN(PixmapNoMemory);
        iptr = pixelindex;
        for (y = 0; y < height; y++)
            for (x = 0; x < width; x++, iptr++)
                XPutPixel(ximage, x, y, pixels[*iptr]);
    }

    XPutImage(display, pixmap, gc, ximage, 0, 0, 0, 0, width, height);

    MT_free((void *)pixelindex);
    MT_free(ximage->data);
    XDestroyImage(ximage);
    XFreeGC(display, gc);
    MT_free((void *)chars);

    /*
     * store color table infos
     */
    if (infos) {
        infos->ncolors = ncolors;
        infos->colorTable = colorTable;
        if (pixels_return) {
            infos->pixels = (Pixel *)MT_malloc(sizeof(Pixel) * ncolors);
            bcopy(pixels, infos->pixels, sizeof(Pixel) * ncolors);
        } else {
            infos->pixels = pixels;
            pixels = NULL;
        }
    } else
        freeColorTable(colorTable, ncolors);

    if (pixmap_return) *pixmap_return = pixmap;
    if (width_return) *width_return = width;
    if (height_return) *height_return = height;
    if (pixels_return) *pixels_return = pixels; else if (pixels) MT_free((void *)pixels);
    if (npixels_return) *npixels_return = ncolors;

    return(PixmapSuccess);
}

/* Copyright 1990 GROUPE BULL -- See licence conditions in file COPYRIGHT
 *
 *  XPM
 *  I/O utility for XPM file format
 *  Developed by Arnaud Le Hors
 */

static unsigned int atoui(char *p)
{
    register int n;

    n = 0;
    while(*p >= '0' && *p <= '9')
        n = n*10 + *p++ - '0';
    return(n);
}

static void
mnextstring(MData *mdata)              /* skip to the end of the current string */
                                /* and the beginning of the next one */
{
    char  c;

    switch(mdata->type)
    {
    case MARRAY:
        mdata->cptr = (mdata->stream.data)[++mdata->line];
        break;
    case MFILE:
    case MPIPE:
        while ((c = mgetc(mdata)) != mdata->Eos && c != EOF);
        if (mdata->Bos)
            while ((c = mgetc(mdata)) != mdata->Bos && c != EOF);
        break;
    }
}

static unsigned int
mnextui(MData *mdata)          /* skip whitespace and return the  */
                        /* following unsigned int          */
{
    char buf[BUFSIZ];

    mnextw(mdata, buf);
    return(atoui(buf));
}


static char
mgetc(MData *mdata)            /* return the current character */
             
{
    char c;
    register unsigned int n = 1, a;
    unsigned int notend;

    switch(mdata->type)
    {
    case MARRAY:
        return(*mdata->cptr++);
    case MFILE:
    case MPIPE:
        c = getc(mdata->stream.file);
        if (mdata->Bcmt && c == mdata->Bcmt[0]) {
            mdata->Comment[0] = c;
            /*
             * skip the string begining comment
             */
            while ((mdata->Comment[n] = getc(mdata->stream.file))
                   == mdata->Bcmt[n]
                   && mdata->Bcmt[n] != '\0' && mdata->Comment[n] != EOF) n++;
            if (mdata->Bcmt[n] != '\0') {
                /* this wasn't the begining of a comment */
                for (a = 1; a < n; a++)
                    mungetc(mdata->Comment[a], mdata);
                return(c);
            }
            /*
             * store comment
             */
            mdata->Comment[0] = mdata->Comment[n];
            notend = 1;
            n = 0;
            while (notend) {
                while (mdata->Comment[n] != mdata->Ecmt[0]
                       && mdata->Comment[n] != EOF)
                    mdata->Comment[++n] = getc(mdata->stream.file);
                mdata->CommentLength = n++;
                a = 1;
                while ((mdata->Comment[n] = getc(mdata->stream.file))
                       == mdata->Ecmt[a]
                       && mdata->Ecmt[a] != '\0' && mdata->Comment[n] != EOF) {
                    a++; n++;
                }
                if (mdata->Ecmt[a] == '\0') {
                    /* this is the end of the comment */
                    notend = 0;
                    mungetc(mdata->Comment[n], mdata);
                }
            }
            c = mgetc(mdata);
        }
        return(c);
    }
}


static char
mungetc(int c, MData *mdata)               /* push the given character back */
      
             
{
    switch(mdata->type)
    {
    case MARRAY:
        return(*--mdata->cptr = c);
    case MFILE:
    case MPIPE:
        return(ungetc(c, mdata->stream.file));
    }
}


static void
mskipwhite(MData *mdata)               /* skip whitespace */
             
{
    char c;

    switch(mdata->type)
    {
    case MARRAY:
        while (*mdata->cptr == SPC || *mdata->cptr == TAB)
            mdata->cptr++;
        break;
    case MFILE:
    case MPIPE:
        while ((c = mgetc(mdata)) == SPC || c == TAB);
        mungetc(c, mdata);
        break;
    }
}


static unsigned int
mnextw(MData *mdata, char *buf)      /* skip whitespace and return the following char */
             
          
{
    register unsigned int n = 0;

    mskipwhite(mdata);
    switch(mdata->type)
    {
    case MARRAY:
        while ((buf[n] = *mdata->cptr++) != SPC
               && buf[n] != TAB && buf[n] != mdata->Eos && buf[n] != EOF) n++;
        mdata->cptr--;
        break;
    case MFILE:
    case MPIPE:
        while ((buf[n] = mgetc(mdata)) != SPC
               && buf[n] != TAB && buf[n] != mdata->Eos && buf[n] != EOF) n++;
        ungetc(buf[n],mdata->stream.file);
        break;
    }

    return(n);
}


static void
mgetcmt(MData *mdata, char **cmt)     /* get the current comment line */
             
           
{
    switch(mdata->type)
    {
    case MARRAY:
        break;
    case MFILE:
    case MPIPE:
        if (mdata->CommentLength) {
            *cmt = (char *) MT_malloc(mdata->CommentLength + 1);
            strncpy(*cmt, mdata->Comment, mdata->CommentLength);
            (*cmt)[mdata->CommentLength] = '\0';
            mdata->CommentLength = 0;
        } else
            *cmt = NULL;
        break;
    }
}


static int
mdataopen(char **data, MData **mdataptr)
{
    MData *mdata;

    if (! (mdata = (MData *)MT_malloc(sizeof(MData)))) {
        return(PixmapNoMemory);
    }
    mdata->type = MARRAY;
    mdata->stream.data = data;
    mdata->cptr = *data;
    mdata->line = 0;
    mdata->CommentLength = 0;
    mdata->Bcmt = mdata->Ecmt = NULL;
    mdata->Bos = mdata->Eos = '\0';
    *mdataptr = mdata;
    return(PixmapSuccess);
}


static void
mclose(MData *mdata)
{
    switch(mdata->type)
    {
    case MARRAY:
        break;
    case MFILE:
        if (mdata->stream.file != (stdout) && mdata->stream.file != (stdin))
            fclose(mdata->stream.file);
        break;
    case MPIPE:
        pclose(mdata->stream.file);
    }
    MT_free((void *)mdata);
    mdata = NULL;
}

/* Copyright 1990 GROUPE BULL -- See licence conditions in file COPYRIGHT
 *
 *  XPM
 *  Visual utility for XPM file format
 *  Developed by Arnaud Le Hors
 */

static int
visualType(Visual *visual)
{
    switch (visual->class)
    {
    case StaticGray:
    case GrayScale:
        switch(visual->map_entries)
        {
        case 2:
            return(MONO);
        case 4:
            return(GRAY4);
        default:
            return(GRAY);
        }
    default:
        return(COLORK);
    }
}

/* Copyright 1990 GROUPE BULL -- See licence conditions in file COPYRIGHT
 *
 *  XPM
 *  rgb file utility for XPM file format
 *  Developed by Arnaud Le Hors
 */

/* Part of this code has been taken from the ppmtoxpm.c file written by Mark
 * W. Snitily but has been modified for my special need
 */

static void
freeColorTable(char ***colorTable, int ncolors)
{
    int a, b;

    if (colorTable) {
        for (a = 0; a < ncolors; a++)
            if (colorTable[a]) {
                for (b = 0; b < (NKEYS + 1); b++)
                    if (colorTable[a][b])
                        MT_free((void *)colorTable[a][b]);
                MT_free((void *)colorTable[a]);
            }
        MT_free((void *)colorTable);
    }
}

static void
XFreeXpmInfo(XpmInfo *infos)
{
    if (infos) {
        if (infos->type) { MT_free((void *)infos->type); infos->type = NULL; }
        if (infos->colorTable) {
            freeColorTable(infos->colorTable, infos->ncolors);
            infos->colorTable = NULL; }
        if (infos->hints_cmt) { MT_free((void *)infos->hints_cmt);
                                infos->hints_cmt = NULL; }
        if (infos->colors_cmt) { MT_free((void *)infos->colors_cmt);
                                 infos->colors_cmt = NULL; }
        if (infos->pixels_cmt) { MT_free((void *)infos->pixels_cmt);
                                 infos->pixels_cmt = NULL; }
        if (infos->pixels) { MT_free((void *)infos->pixels); infos->pixels = NULL; }
    }
}

Pixmap XPM_PIXMAP(Widget w, char **pixmapName)
{
    Visual     *visual;
    Colormap    colormap;
    int         depth;
    int         argcnt;
    Arg         args[10];
    Pixmap      pixmap;
    int         returnValue;

    argcnt = 0;
    XtSetArg(args[argcnt], XmNdepth, &depth); argcnt++;
    XtSetArg(args[argcnt], XmNcolormap, &colormap); argcnt++;
    XtGetValues(w, args, argcnt);

    visual = DefaultVisual(XtDisplay(w), DefaultScreen(XtDisplay(w)));

    returnValue = XCreatePixmapFromData(XtDisplay(w), visual,
                                        DefaultRootWindow(XtDisplay(w)),
                                        colormap, pixmapName, depth, &pixmap,
                                        NULL, NULL, NULL, NULL, NULL,
                                        0, NULL);
    switch(returnValue)
    {
    case PixmapOpenFailed:
    case PixmapFileInvalid:
    case PixmapNoMemory:
    case PixmapParseColorFailed:
    case PixmapAllocColorFailed:
        XtWarning("Could not create pixmap.");
        return(XmUNSPECIFIED_PIXMAP);
    default:
        return(pixmap);
    }
}
#endif

#ifndef IGNORE_CONVERT
/*SUPRESS 592*/
caddr_t CONVERT(Widget w, char *from_string, char *to_type, int to_size, Boolean *success)
{
    XrmValue            fromVal, toVal; /* resource holders             */
    Boolean             convResult;     /* return value                 */
    unsigned char       oByte;          /* one byte result              */
    unsigned short      tByte;          /* two byte result              */
    caddr_t             fByte;          /* four byte result             */
    caddr_t             aByte;          /* allocated result             */

    /*
     * Zero it.
     */
    fByte = aByte = NULL;
    *success = False;

    /*
     * String to string.
     */
    if( strcmp(XmRString, to_type) == 0 )
    {
        *success = True;
        /*SUPPRESS 80*/
        return( from_string );
    }

    /*
     * Sometimes we do not know this at code output.
     */
    if(to_size == 0) to_size = strlen(from_string);

    /*
     * Motif String to StringTable converter
     * does not cache correctly and so
     * we need to special case this.
     */
    if( strcmp(to_type, "XmStringTable") == 0)
    {
        register int lcount, loop;
        register char tmp, *ptr, *e_ptr;
        XmStringTable table;

        ptr = (char *)from_string;
        if ((ptr == NULL) || (*ptr == '\0'))
        {
            return( NULL );
        }
        else
        {
            lcount = 1;
            while (*ptr != '\0')
            {
                if (*ptr++ == '\n')
                    lcount++;
            }
            table = (XmStringTable)
                MT_malloc(sizeof(XmString) * lcount);

            ptr = (char *)from_string;

            for (loop = 0; loop < lcount; loop++)
            {
                e_ptr = ptr;
		while((*e_ptr!='\0')&&(*e_ptr != '\n'))
                    e_ptr++;
                tmp = *e_ptr;
                *e_ptr = '\0';
                /* table[loop] = Str2MotifStr(ptr); */
                table[loop] = XmStringCreateSimple(ptr);
                *e_ptr = tmp;
                ptr = ++e_ptr;
            }

            fByte = (caddr_t)table;
        }
        *success = True;
        /*SUPPRESS 80*/
        return( fByte );
    }

    /*
     * Set up the list.
     */
    fromVal.size = strlen(from_string) + 1;
    fromVal.addr = from_string;

    switch( to_size )
    {
    case 1:
        toVal.size = sizeof(unsigned char);
        toVal.addr = (caddr_t)&oByte;
        break;
    case 2:
        toVal.size = sizeof(unsigned short);
        toVal.addr = (caddr_t)&tByte;
        break;
    default:
        toVal.size = sizeof(caddr_t);
        toVal.addr = (caddr_t)&fByte;
        break;
    }

    convResult = XtConvertAndStore(w,
                                   XmRString,
                                   &fromVal,
                                   to_type,
                                   &toVal);

    if( convResult )
    {
        switch( to_size )
        {
        case 1:
            fByte = (caddr_t)(int)oByte;
            break;
        case 2:
            fByte = (caddr_t)(int)tByte;
            break;
        default:
            break;
        }
    }


    /*
     * Conversion will fail if we need more than 4 bytes.
     * For strings it will fail always the first time.
     */
    if( !convResult && toVal.size != to_size )
    {
        /*
         * Need to allocate more space for this one.
         */
        toVal.addr = (caddr_t)MT_malloc(toVal.size);
        fByte = aByte = toVal.addr;
        convResult = XtConvertAndStore(w,
                                       XmRString,
                                       &fromVal,
                                       to_type,
                                       &toVal);
    }

    /*
     * Free any thing useless we may have allocated.
     */
    if( !convResult )
    {
        MT_free((void *)aByte);
        aByte = NULL;
    }

    /*
     * Return the result.
     */
    *success = convResult;
    /*SUPPRESS 80*/
    return( fByte );
}

#endif
