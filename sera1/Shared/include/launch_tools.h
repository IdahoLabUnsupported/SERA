/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% launch_tools.h 
%%
%% Purpose:  The purpose of this routine is to provide a simple way to
%%           create cross-launch capabilities for all the SERA modules.
%%              
%% Usage:    To create the launch button, and its submenu, simply call
%%           LT_make_launch_menu(). The parent parameter must be a widget
%%           either created with a call to XmCreatePulldownMenu or a 
%%           RowColumn widget with the XmNrowColumnType set to XmMENU_PULLDOWN.
%%           The current_application parameter should be the name of the
%%           application using the launch menu.
%%
%% Example Calls: 
%%           The call for seraModel would look like this:
%%
%%                  LT_make_launch_menu( pulldown_parent, "seraModel" );
%%
%%            This will create a Launch button on the pulldown_parent
%%            widget. The submenu will contain the buttons for the other
%%            SERA modules, and the button for Sera Model will be grayed
%%            out. If you do not want any of the buttons to be grayed out
%%            the call would look like this:
%% 
%%                  LT_make_launch_menu( pulldown_parent, "" );
%%
%% NOTES: debug_tools and dialog_tools are used in this routine, so they must
%%        be linked into the software module using launch_tools.        
%% 
%% Written By: Mark Rossmeier(01-07-99)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/


#ifndef LAUNCH_TOOLS_H
#define LAUNCH_TOOLS_H

#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>

#include "dialog_tools.h"
#include "debug_tools.h"

/* main structure for the launch menu */
typedef struct _launch_menu_t
{

  Widget launch_cascade_button;
  Widget launch_pulldown_menu;
  Widget seraImage_button;
  Widget seraModel_button;
  Widget sera3d_button;
  Widget seraDose_button;
  Widget seraPlot_button;
  Widget seraCalc_button;
  Widget seraPlan_button;

} launch_menu_t;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prototypes for functions in launch_tools.c
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void LT_make_launch_menu( Widget parent, char * current_application );

#endif
