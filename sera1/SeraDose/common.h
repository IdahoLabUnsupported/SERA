/*
 * common.h
 * 
 * Various variables, macros, etc. used by numberous XContours procedures.
 */
 
 
#ifndef COMMONH
#define COMMONH

#ifdef DECLARATION
#define externOrNot /* nil */
#else
#define externOrNot extern
#endif  /* DECLARATION */

#define ceil_d(x) ((unsigned int)(x+.99999))
#define square(x) ((x)*(x))

externOrNot int verbose;                 /* Set to true if you want the program to display tons and tons
					  * of (useful?) information while it runs
					  */

/* global colormap used by mwf -- stores the 256 rgb values of the "desired" colormap */
/* Assuming 128 colors are available, you'd get every other one, etc.                 */
externOrNot unsigned char cmap_vals[3*256];

externOrNot XmStringCharSet char_set;
externOrNot Widget color_tool_shell, CT_BACKGROUND, CT_SATURATION, CT_OFFSET, CT_GAMMA, 
             cbar_w, CT_load_fsb;        /* CT == color tool widgets */
externOrNot GC cbar_gc;                  /* graphics context for the color bar */
externOrNot XImage *cbar_image;          /* XImage to be displayed in the color bar -> see the pallette */

#endif

