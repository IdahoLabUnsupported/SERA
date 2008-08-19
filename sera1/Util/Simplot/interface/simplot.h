#ifndef  _SIMPLOT_H
#define  _SIMPLOT_H

#ifdef  GLOBALS
#define Externornot
#else
#define Externornot extern
#endif


#include <stdio.h>
#include <errno.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <Xm/MessageB.h>
#include <Xm/Frame.h>

#include "debug_tools.h"
#include "dialog_tools.h"
#include "memory_tools.h"
#include "file_tools.h"

#define NUM_SIMPLOT_TOGGLES 13

typedef struct 
{
    Widget SimplotInputFile,
           SimplotOutputFile,
           SimplotSampleRate,
           SimplotNormFactor,
           SimplotBoronConc;
    int    options[NUM_SIMPLOT_TOGGLES];
    char * inputfile;
    char * outputfile;
    char * sample_rate;
    char * norm_factor;
    char * boron_conc;
    char * bnct_home;
} simplot_struct;


void SimplotToggleCB( Widget w, XtPointer clientData, XtPointer callData );
void StartSimplotCB ( Widget w, XtPointer clientData, XtPointer callData );
void FileSelect     ( Widget w, Widget parent, XtPointer callData );
void SetCursorPos   ( Widget w );

#endif
