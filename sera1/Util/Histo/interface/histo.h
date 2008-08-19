#ifndef  _HISTO_H
#define  _HISTO_H

#ifdef  GLOBALS
#define Externornot
#else
#define Externornot  extern
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

#define NUM_HISTO_TOGGLES 8

typedef struct 
{
   Widget HistoInputFile;
   Widget HistoOutputFile;
   int  options[NUM_HISTO_TOGGLES];
   char * inputfile;
   char * outputfile;
   char * normalizedose;
   char * sera_home;
} histo_struct;

/*Externornot struct histo_struct histo_struct_var;*/
       
void HistoToggleCB  ( Widget w, XtPointer clientData, XtPointer callData );
void StartHistoCB   ( Widget w, XtPointer clientData, XtPointer callData );
void DestroyShell   ( Widget shell );
void CancelCB       ( Widget w, XtPointer clientData, XtPointer callData );
void RadioCB        ( Widget w, XtPointer clientData, XtPointer callData );

char * GetMemory  ( Cardinal size, char * calling_function );
Widget GetTopShell( Widget w );
void SetCursorPos ( Widget w );

Externornot char * SERA_HOME;


#endif
