/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  File:        overlay_info.h
%%%
%%%  Purpose:     These routines allow the user to switch between image
%%%               sets while keeping the same region information.
%%%
%%%  Notes:       Currently (10-06-99) not much checking is done to see
%%%               if two image sets are in fact registered, in which case
%%%               overlaying them would make sense. When the registration
%%%               process becomes reliable, some more checking needs to be
%%%               done to make sure that two sets are registered.
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef _OVERLAY_INFO_H
#define _OVERLAY_INFO_H

#include "include.h"
#include "image_matrix.h"
#include "functions.h"
#include "dialog_tools.h"
#include "debug_tools.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Defines
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#define MAX_OVERLAY_SETS 10


/* Structures */
typedef struct _overlayInfo_t
{
    Widget  switchSetsButton;
    Widget  switchSetsWindow;
    Widget  radioBox;
    Widget  toggles[MAX_OVERLAY_SETS];

    char filenames[MAX_OVERLAY_SETS][256];
    int  numFiles;
    
    int  activeFile;
    int  previousActiveFile;
    
} overlayInfo_t;


/*%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%  GLOBALS
%%%%
%%%%%%%%%%%%%%%%%%%%%%%*/
overlayInfo_t overlayInfo;


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Prototypes
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void initializeOverlayInfo( void );
void switchImageSetsCB( Widget w, XtPointer clientData, XtPointer callData );

#endif
