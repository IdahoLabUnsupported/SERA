/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Purpose:  Create a way to reference widgets and their 
%%           corresponding help strings that will be displayed
%%           in the help bar on the MainMenu.
%%
%% IMPORTANT --> It is important that if a widget's name changes
%%               in create_main_menu.c that it also be changed here.
%%               If it is not changed, the help string will not
%%               get displayed.
%%
%% First Created:  November 24, 1998
%% Last Modified:  January 4, 1999
%%
%% Mark Rossmeier
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef _WIDGET_NAMES_H
#define _WIDGET_NAMES_H

#include "seramenu.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This is just a list of the widget names that will be
%%% allowed to have a help string.  They are in the order
%%% with which they apper on the Sera Menu.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

char * main_menu_widget_names[] = {
"helpbar",                  
"mode_toggle_button",     
"proj_dir_button",        
"global_prefs_button",
SERA_IMAGE,
SERA_MODEL,
SERA_3D,
SERA_DOSE,
SERA_PLOT,
SERA_CALC,
SERA_PLAN,
"close_apps_button",
"help_button",
"exit_button"
};

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% These are the messages that will be displayed in the
%%% help window when the cursor movees over the widgets
%%% in the main_menu_widget_names list.
%%%
%%% NOTE: Each string MUST contain two (2) newline characters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

char * widget_messages[] = {
"Sera Menu\nPointer Information\n",
"Advanced Mode\nSpecify Display and\nCommand Line Parameters",
"Set Project Directory\nCurrently Under Construction\n",
"Set Global Preferences\nCurrently Under Construction\n",
"Sera Image\nCommand Line:  seraImage\n",
"Sera Model\nCommand Line:  seraModel\n",
"Sera 3D\nCommand Line:  sera3d\n",
"Sera Dose\nCommand Line:  seraDose\n",
"Sera Plot\nCommand Line:  seraPlot\n",
"Sera Calc\nCommand Line:  seraCalc\n",
"Sera Plan\nCommand Line:  seraPlan\n",
"Get a list of currently running\nSera Menu applications\n",
"Open Context-Sensitive Help\nfor the Sera Menu\n",
"Exit the Sera Menu\n\n"
};

#endif
