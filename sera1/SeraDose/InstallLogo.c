/*********************************************
 * InstallLogo.c: Display INEL/MSU logo
 *********************************************/
 
 
#include <Xm/Xm.h>
#include <Xm/DrawnB.h>
#include <Xm/Form.h>
#include <stdlib.h>
#include "bitmaps/Shared/sera_logo_mini.xpm"
#include "bitmaps/Shared/seraLogoMini.xbm"
#include <X11/xpm.h>
#include "global.h"
#include "color.h"

/*
static char * sera_logo_mini[] = {
"16 16 3 1",
"a c red m black",
". c white m white s background",
"X c blue m black",
"aaaa...........X",
".aaaa..........X",
"..aaaa........X.",
"...aaaa......X..",
"...aaaa.....X...",
"....aaaa...X....",
".....aaaa..X....",
"......aaa.X.....",
"......aa.X......",
"......a.XXX.....",
".....a..XXXX....",
".....a...XXXX...",
"....a....XXXX...",
"...a......XXXX..",
"..a........XXXX.",
".a..........XXXX"};
*/

Widget InstallLogo ( Widget parent  )
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
    int screen   = DefaultScreen( dpy );
    int depth    = DefaultDepth( dpy, screen );
    
    DEBUG_TRACE_IN  printf("Entering InstallLogo\n");

    /*
     * Create an XmLabel widget
     */
    
    /*    form = XtCreateManagedWidget ( "logo_form", 
     *			   xmFormWidgetClass, parent, 
     *			   NULL, 0);
     */
   
    logo = XtVaCreateManagedWidget ( "logo", 
				     xmDrawnButtonWidgetClass,parent,
				     XmNshadowThickness, 0,
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
                                           (char *)seraLogoMini_bits,
                                           seraLogoMini_width, seraLogoMini_height,
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
    

        status = XpmCreatePixmapFromData(dpy,
                                         DefaultRootWindow (dpy),
                                         sera_logo_mini,&pix,&mask,
                                         &attributes);

        if (status == XpmSuccess){
            XtVaSetValues (logo, 
                           XmNlabelPixmap, pix, 
                           NULL );
        }else{
            printf("error creating the pixmap, going without\n");
        }
    }
    

    DEBUG_TRACE_OUT  printf("Leaving InstallLogo\n");
    /* Display the pixmap in the logo */
    return(logo);    
}  
