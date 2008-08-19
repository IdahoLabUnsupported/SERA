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
 * procedure checks to ensure that a given font name is properly formed to be a
 * scalable font.  The second generates the scaled font of a given size and name.  
 *
 */


#include "font_functions.h"
#include "global.h"


/*
 * This routine returns True if the passed name is a well-formed
 * XLFD style font name with a pixel size, point size, and average
 * width (fields 7, 8, and 12) of "0".
 */

Bool IsScalableFont (char *name)
{
    int i, field;

    if ((name == NULL) || (name[0] != '-')) return False;
    
    for (i = field = 0; name[i] != '\0'; i ++) {
        if (name[i] == '-') {
            field ++;
            if ((field == 7) || (field == 8) || (field == 12))
                if ((name[i + 1] != '0') || (name[i + 2] != '-'))
                    return False;
        }
    }
    if (field != 14) 
       return False;
    else 
       return True; 
}




/*
 * This routine is passed a scalable font name and a point size.  It returns
 * a type Font for the given font scaled to the specified size and the
 * exact resolution of the screen.  The font name is assumed to be a 
 * well-formed XLFD name, and to have pixel size, point size, and avergae
 * width fields of "0" and arbitrary x-resolution and y-resolution fields. 
 * Size is specified in tenths of points.  Returns NULL if the name is 
 * malformed or no such font exists.  
 */
 
Font * LoadQueryScalableFont (Display *dpy, int screen, char *name, int size)
{
    int i, j, field;
    char newname[500];
    int res_x, res_y; 


    /* catch obvious errors */
    if ((name == NULL) || (name[0] != '-')) return NULL;
    
    /* calculate our screen resolution in dots per inch.  25.4mm = 1 inch */
    res_x = DisplayWidth(dpy, screen) / (DisplayWidthMM(dpy, screen) / 25.4);
    res_y = DisplayHeight (dpy, screen) / (DisplayHeightMM(dpy, screen) /25.4);

    /* copy the font name, changing the scalable fields as we do so */
    for (i = j = field = 0; name[i] != '\0' && field <= 14; i ++) {
        newname[j++] = name[i];
        if (name[i] == '-') {
            field ++;
            switch (field) {
            case 7:  /* pixel size */
            case 12: /* average width */
                /* change from "-0-" to "-*-" */
                newname[j] = '*';
                j++;
                if (name[i + 1] != '\0') i++;
                break;
            case 8:  /* point size */
                /* change from "-0-" to "-<size>-" */
                sprintf (&newname[j], "%d", size);
                while (newname[j] != '\0') j++;
                if (name[i + 1] != '\0') i++;
                break;
            case 9:  /* x-resolution */
            case 10:  /* y-resolution */
                /* change from an unspecified resolution to res_x or res_y */
                sprintf (&newname[j], "%d", (field == 9) ? res_x : res_y);
                while (newname[j] != '\0') j++;
                while ((name[i + 1] != '-') && (name[i + 1] != '\0')) i++;
                break;
            }
        }
    } 
    newname[j] = '\0';
    
    /* if there aren't 14 hyphens, it isn't a well formed name */
    if (field != 14) return NULL;

    new_font =XLoadFont(dpy, newname);
    return &new_font;
}



/*
 * font_exists() -- a procedure that checks to see if the supplied
 * font exists on the current X-Server.
 *
 * David Helzer
 * August, 1997
 */

Bool font_exists (char *name, Display *dpy)
{
   char **font_list = NULL;
   int number_fonts_returned = 0;
   
   
   font_list = XListFonts (dpy, name, MAX_NUM_FONTS, &number_fonts_returned);

   if ((number_fonts_returned == 0) || (font_list == NULL))
      return (0);
   else {
      XFreeFontNames (font_list);
      return (1);
   }
}
