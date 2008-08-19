/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% File Name:    seraplot.h
%% 
%% Purpose:      Define include files, structures, and prototypes used
%%               in seraplot.c
%% 
%% Written By:   Mark Rossmeier
%%
%% First Written: January 13, 1999
%% Last Modified: January 13, 1999
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef SERAPLOT_H
#define SERAPLOT_H

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   INCLUDES 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include <stdio.h>
#include <stdlib.h>

/*
 * Includes for Widgets
 */

#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/MessageB.h>
#include <Xm/Xm.h>

/*
 * Other includes
 */

#include "debug_tools.h"
#include "dialog_tools.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   STRUCTURE DEFINITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

typedef struct _main_gui_t
{

  XtAppContext app;
  Widget toplevel;
  Widget shell;          /* will be a message dialog */
  Widget form;
  Widget doseDepthButton;
  Widget doseVolumeButton;

} main_gui_t;


#endif



