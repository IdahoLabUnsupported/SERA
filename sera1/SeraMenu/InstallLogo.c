/*#include "bnct.h" -- Commented by David Helzer */

#include "seramenu.h"
#include <X11/xpm.h>
#include "bitmaps/Shared/sera_logo_sm.xpm"
#include "bitmaps/Shared/seraLogo.xbm"

/*********************************************
 * InstallLogo.c: Display INEL/MSU logo
 *********************************************/
Widget InstallLogo (Widget parent)
{
    Widget       form,logo;
    Pixel        fg, bg;
    Pixmap       pix;
    Pixmap       mask;
    XpmAttributes attributes;
    int status;
    XpmColorSymbol symbols[1];
    Dimension form_width,logo_width;
    int offset;

    Display *dpy = XtDisplay(parent);
    int screen = DefaultScreen( dpy );
    int depth = DefaultDepth( dpy, screen );
    
    DEBUG_TRACE_IN  printf("Entering InstallLogo\n");

   /*
    * Create an XmLabel widget
    */
    form = XtCreateManagedWidget ( "logo_form", 
				   xmFormWidgetClass, parent, 
				   NULL, 0);
   
    logo = XtVaCreateManagedWidget ( "logo", 
				     xmDrawnButtonWidgetClass,form,
				     XmNshadowThickness, 0,
				     XmNleftAttachment, XmATTACH_FORM,
				     XmNleftOffset, 30,
                                     XmNlabelType, XmPIXMAP,
				     NULL);

   /* Get the logos foreground and background colors */

    XtVaGetValues ( logo, 
		    XmNdepth, &attributes.depth,
		    XmNcolormap, &attributes.colormap,
                    XmNforeground, &fg, 
                    XmNbackground, &bg, 
                    NULL );

    if( depth == 8 )
    {
        pix = XCreatePixmapFromBitmapData( dpy, DefaultRootWindow( dpy ),
                                           (char *)seraLogo_bits,
                                           seraLogo_width, seraLogo_height,
                                           fg, bg,
                                           depth );
        
        XtVaSetValues( logo, XmNlabelPixmap, pix, NULL );
    }
    else
    {
        symbols[0].name = "background";
        symbols[0].value = NULL;
        symbols[0].pixel = bg;
    
        attributes.colorsymbols = symbols;
        attributes.numsymbols = 1;
    
        attributes.visual = DefaultVisual (dpy, screen);
        attributes.valuemask = XpmColorSymbols | XpmDepth | XpmColormap | XpmVisual;
    

        status = XpmCreatePixmapFromData( dpy,
                                          DefaultRootWindow ( dpy ),
                                          Sera_logo_sm,&pix,&mask,
                                          &attributes);

        if (mask) XFreePixmap(dpy,mask); 

        if (status == XpmSuccess){
       
            XtVaSetValues (logo, 
                           XmNlabelPixmap, pix, 
                           NULL );

        }
        else{
            printf("error in creating the pixmap (XpmCreatePixmapFromData)\n");
        }
    }
    
    DEBUG_TRACE_OUT  printf("Leaving InstallLogo\n");
    /* Display the pixmap in the logo */
    return(form);
}
