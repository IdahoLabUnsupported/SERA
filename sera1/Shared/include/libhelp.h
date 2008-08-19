/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   USAGE NOTES
%%%
%%%
%%%   You must first call set_preference_directory,
%%%   passing it a pointer to the preference directory
%%%   below the SERA_HELP. This is where the 
%%%   .dat help files are located for your program.
%%%
%%%   You need to register this callback with the 
%%%  help button from your program. For the help
%%%  to be able to find the correct widget to return
%%%  help for, you MUST pass your top level window
%%%  widget through the clientData parameter. 
%%%
%%%  EXAMPLE:  
%%% 
%%%  XtAddCallback(my_help_button, XmNhelpCallback,
%%%                ContextHelpCallback, (XtPointer)my_main_window);
%%%
%%%
%%%  A file named widget_help_URLs.txt MUST be in the directory that
%%%  you set with set_preference_directory. This file contains widget
%%%  names, and the corresponding section of the manual that should
%%%  be brought up if the user chooses to view the manual on the web.
%%%  The file should be in the following form:
%%%
%%%         some_widget_name   a section in the manual
%%%
%%%  for example
%%%
%%%         zoom_button   sec04.html#4.2
%%%         image_slider  sec04.html#4.1.2
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef LIBHELP_H
#define LIBHELP_H

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>    /* Motif Form widget. */
#include <Xm/Frame.h>   /* Motif Frame widget. */
#include <Xm/PushB.h>
#include <Xm/ScrolledW.h>
#include <Xm/MessageB.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/DialogS.h>
#include <Xm/LabelG.h>
#include <Xm/PanedW.h>
#include <Xm/PushBG.h>
#include <X11/cursorfont.h>

#include "memory_tools.h"
#include "debug_tools.h"
#include "dialog_tools.h"


void ContextHelpCallback( Widget w, XtPointer clientData, XtPointer callData );
void set_preference_directory( char * pref );

char pref_string[256];

#endif
