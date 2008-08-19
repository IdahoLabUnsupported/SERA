#ifndef COMMONFCNSH
#define COMMONFCNSH

#ifdef DECLARATION
#define externOrNot /* nil */
#else
#define externOrNot extern
#endif  /* DECLARATION */

#include "read_raw.h"

/* Use normal cursor or turn on a watch while waiting */
externOrNot void set_cursor(int type);               

/* Sets up the color tool shell for colormap manipulation */
externOrNot Widget make_color_tool_shell(Widget parent);  

/* given a (unsigned char*, int) raw data buffer and index, 
 * it converts this into a pixmap used for previewing 
 * (resizing if necessary) and adds this to this list of 
 * pixmaps available for previewing
 */
externOrNot void add_pixmap(unsigned char *raw_data, int pix_index, qhd_data_type qhd_data);               

/* Things having to do with the color tool shell */

/* Redisplay the colorbar upon exposure              */
externOrNot void cbarExposedCB(Widget w, XtPointer clientData, XtPointer callData);

/* Callback for color tool knob adjustments          */
externOrNot void color_tool_adjustCB(Widget w, XtPointer clientData, XtPointer callData);

/* use after setting the number of colors available  */
externOrNot void update_cbar(void);

/* brings up or closes the colortool shell           */
externOrNot void color_toolCB(Widget w, XtPointer clientData, XtPointer callData);

/* callback to load a new colormap                   */
externOrNot void CT_loadCB(Widget w, XtPointer clientData, XtPointer callData);

externOrNot void CT_reloadColormapCB ( Widget, XtPointer, XtPointer );

/* colormap load fileselectionbox done               */
externOrNot void CT_load_fsb_doneCB(Widget w, int clientData, XmSelectionBoxCallbackStruct *callData);

/* function to load a colormap                       */
externOrNot void CT_load_cmap(char *filename, int redraw);


/* miscellaneous */

/* This is an enter notify callback procedure        */
externOrNot void colormap_installCB(Widget w, XtPointer clientdata, XtPointer callldata);


/* Based on the set background and saturation, loads the portion of the colormap  
 * dedicated to images into cmap
 */
externOrNot void colormap_loadrgb(void);
externOrNot void load_gamma_colormap(void);     /* Gets gamma from a slider and computes colormap accordingly */
externOrNot void resize_and_recalc_pixels(unsigned char * data, unsigned char * output, 
					  unsigned int pix_index, qhd_data_type qhd_data);

/* Places widgets on a form according to percent from top, left, etc. */
void SetFormPercent(Widget widget, int top, int left, int right, int bottom);

void reset_image_set_if_needed(int current_image_set);

#endif
