/* 
 * font functions.h 
 *
 * INEEL BNCT Research Project
 * Montana State University - Bozeman
 * 
 * Procedures modified from Adrian Nye's 'Xlib Programming Manual' (Volume One of "The
 * Definitive Guides to the X Window System."  
 * 
 * David Helzer  
 * June 23, 1997
 *
 * These two procedures are to be used by programs using scalable fonts.  The first 
 * procedures checks to ensure that a given font name is properly formed to be a
 * scaleable font.  The second generates the scaled font of a given size and name.  
 *
 */
 
 
#include "include.h"


#define MAX_NUM_FONTS 500



/*
 * This routine returns True if the passed name is a well-formed
 * XLFD style font name with a pixel size, point size, and average 
 * width (fields 7, 8, and 12) of "0".  These fields must be zero 
 * in a scaleable font.  
 */
 
Bool IsScalableFont (char *name);


/*
 * This routine is passed a scalable font name and a point size.  It returns
 * a ptr to a type Font for the given font scaled to the specified size and the
 * exact resolution of the screen.  The font name is assumed to be a 
 * well-formed XLFD name, and to have pixel size, point size, and avergae
 * width fields of "0" and arbitrary x-resolution and y-resolution fields. 
 * Size is specified in tenths of points.  Returns NULL if the name is 
 * malformed or no such font exists.  
 */
 
Font *LoadQueryScalableFont (Display *dpy, int screen, char *name, int size);



/*
 * fonts_exists() - a procedure that determines if the supplied font 
 * exists on the current X-Server.
 */
 
Bool font_exists (char *name, Display *dpy);

