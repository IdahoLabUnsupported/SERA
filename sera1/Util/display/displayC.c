#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include <stdio.h>  

main()
{
     Display *display;
     int     maxHWcmaps;
     int     screen, default_depth;
     unsigned int  display_width, display_height;
     char    *display_name = NULL; /* Server to connect to; NULL means
                                          * connect to server specified in
                                          * environment variable DISPLAY */

   /* connect to X server */
   if( (display=XOpenDisplay(display_name)) == NULL )
   {
       (void) fprintf( stderr, "howMany: cannot connect to X server %s\n",
                      XDisplayName(display_name) ); 
       exit(-1);
   }

   /* get screen size from display structure macro */
   screen = DefaultScreen(display);
   display_width  = DisplayWidth(display, screen);
   display_height = DisplayHeight(display,screen);
   default_depth  = DefaultDepth(display,screen); 
   maxHWcmaps = MaxCmapsOfScreen(DefaultScreenOfDisplay(display));

   printf("Information for X server %s\n", XDisplayName(display_name) );
   printf("Display width                = %d \n",display_width);
   printf("Display height               = %d \n",display_height);
   printf("Number of hardware colormaps = %d \n",maxHWcmaps);
   printf("Default depth                = %d \n",default_depth);
}

