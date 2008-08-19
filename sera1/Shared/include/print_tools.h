/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  File:       print_tools.h
%%%
%%%  Purpose:    print_tools allows widgets to be printed either to a
%%%              printer or to a post script file.  A Print button should
%%%              be added to the widgets which will be printable.  The widget
%%%              ID of the widget to print should be passed as clientData
%%%              through the callback.  For example:
%%%
%%%              ...
%%%              someWidget = XtCreateManagedWidget( ... );
%%%              printButton = XtCreateManagedWidget( "Print", ... );
%%%
%%%              XtAddCallback( printButton, XmNactivateCallback,
%%%                             PT_printCallback, (XtPointer) someWidget );
%%%
%%%              After the user specifies some information in a popup dialog,
%%%              someWidget will be the widget printed.
%%%
%%%  Notes:      Not implemented yet...but something to think about.
%%%              How about add a feature to let the user select an area of
%%%              the screen to print (similar to xv for example), or let the
%%%              user click on a widget to print?
%%%
%%%  Written By: Mark Rossmeier & Gary Harkin  (July 1999)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef _PRINT_TOOLS_H
#define _PRINT_TOOLS_H

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Standard Includes and Includes for Widgets
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>

#include <Xm/Xm.h>
#include <Xm/ArrowB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/MessageB.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <X11/cursorfont.h>

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Local Includes
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "debug_tools.h"
#include "dialog_tools.h"
#include "memory_tools.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Defines
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifdef SYSV
#define PRINT_COMMAND    "lp"
#define PRINT_OPTION     "-d"
#else
#define PRINT_COMMAND    "lpr"
#define PRINT_OPTION     "-P"
#endif

#define TO_PRINTER       "Print to Printer"
#define TO_FILE          "Print to File"

#define PRINTER          0
#define FILES            1

#define NOT_PROPORTIONAL 0
#define PROPORTIONAL     1

#define GRAY             0
#define COLOR            1


#define PRINTER_STR      "Printer:"
#define FILE_STR         "   File:"

#define FILE_SUFFIX      ".ps"

#define MIN_DIM          0.25
#define MAX_DIM          8.00
#define INCREMENT        0.25

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Structure definitions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

typedef struct _widgetToPrint_t
{
    Widget self;
    char name[256];

    Dimension width;
    Dimension height;
    Position x;
    Position y;

    float factor;
    
} widgetToPrint_t;

typedef struct _printInfo_t
{
    char printerName[256];
    char pathname[256];
    char filename[256];
    
    int destination;
    int proportional;
    int color;

    float printedWidth;
    float printedHeight;
    
    XImage *ximage;
    Colormap colormap;

} printInfo_t;

typedef struct _printGui_t
{
    Display * display;
    int      screen;
    XtAppContext app;
    
    Widget shell;
    Widget form;
    Widget rowcol;

    Widget destinationFrame;
    Widget dimensionsFrame;
    Widget optionsFrame;

    Widget destinationForm;
    Widget dimensionsForm;
    Widget optionsForm;

    Widget mainLabel;
    Widget destinationLabel;
    Widget printerLabel;
    Widget dimensionsLabel;
    Widget widthLabel;
    Widget heightLabel;
    Widget optionsLabel;
    
    Widget destinationMenu;
    Widget destinationPane;
    Widget destinations[2];

    Widget printerText;
    Widget widthText;
    Widget heightText;
    Dimension textFieldHeight;

    Widget increaseWidth;
    Widget decreaseWidth;
    Widget increaseHeight;
    Widget decreaseHeight;
    
    Widget colorToggle;
    Widget proportionalToggle;

    Widget browseButton;
    Widget selectWidgetButton;
    
    widgetToPrint_t widgetToPrint;
    printInfo_t     printInfo;
    
} printGui_t;


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Prototypes
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

void PT_print (Widget, Widget);


#endif
