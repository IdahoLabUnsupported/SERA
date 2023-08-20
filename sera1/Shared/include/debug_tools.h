/************************************************************************
 * debug_tools.h
 *
 * AUTHOR:  Matt Cohen
 *
 * USAGE:   1. Include debug_tools.h in the files you want to use 
 *             debug_tools and edit the Makefile for debug_tools.o.
 * 
 *          2. Change XtVaAppInitialize() or XtAppInitialize() in main 
 *             to use the options array. 
 *             example:
 *
 *             before: 
 *             shell = XtVaAppInitialize ( &app "Program", 
 *                                         NULL, 0,
 *                                         &argc, argv,
 *                                         NULL, NULL );
 *             after:
 *             shell = XtVaAppInitialize ( &app, "Program", 
 *                                         options, XtNumber (options),
 *                                         &argc, argv,
 *                                         NULL, NULL );
 *
 *          3. After the call to XtVaAppInitialize, call 
 *             set_debug_values(), sending the main shell you created,
 *             and the name of the executable program.
 *             example: 
 *                       set_debug_values ( argv[0], shell );
 *          
 *          4. If you want debug_tools to check the syntax of command
 *             options use debug_syntax() after your call to
 *             set_debug_values().
 *             example:
 *                       if (argc > 1)
 *                           debug_syntax (argc, argv);
 *
 *          5. Use the defined debugging comparisons in your code whenever
 *             you want to place a debugging statement.  The comparisons
 *             are:                           flag to use:
 *                   DEBUG_TRACE_IN            -debug_trace
 *                   DEBUG_TRACE_OUT           -debug_trace
 *                   DEBUG_GUI                 -debug_gui
 *                   DEBUG_LOADING             -debug_loading
 *                   DEBUG_DATA                -debug_data
 *                   DEBUG_IO                  -debug_io
 *                   DEBUG_LIBQSH              -debug_libqsh
 *                   DEBUG_LIBQSH_KEYS         -debug_libqshkeys
 *                   DEBUG_MEMORY_ALLOC        -debug_memory
 *                   DEBUG_MEMORY_FREE         -debug_memory
 *                   DEBUG_MEMORY_REALLOC      -debug_memory
 *                   DEBUG_MEMORY              -debug_memory
 *
 *             example:
 *                       DEBUG_GUI printf("Entering build_something()\n");
 *
 *          6. To use the flags just type the flag after the executable.
 *             example:
 *                      > yourProg -debug_gui
 *         
 *             You can set multiple flags or use -debug_all to set 
 *             all flags.
 *
 * debug_trace usage
 *          1. At the beginning of each function use:
 *                DEBUG_TRACE_IN printf("Entering some funtion\n");
 *          
 *          2. At the end of each function use:
 *                DEBUG_TRACE_OUT printf("Leaving some function\n");
 *
 *          3. Then when the -debug_trace flag is used, a count is
 *             kept for number of tabs before each print statement  
 *             and the program can be traced though it's execution.
 ************************************************************************/

#ifndef DEBUGTOOLS_H
#define DEBUGTOOLS_H

#include <Xm/Xm.h>

/* defined debugging statements */
#define DEBUG_TRACE_IN     if (debugData.debugTrace) debug_push(); if (debugData.debugTrace)
#define DEBUG_TRACE_OUT    if (debugData.debugTrace) debug_pop(); if (debugData.debugTrace)        
#define DEBUG_GUI          if (debugData.debugGui)
#define DEBUG_LOADING      if (debugData.debugLoading)
#define DEBUG_DATA         if (debugData.debugData)
#define DEBUG_IO           if (debugData.debugIO)
#define DEBUG_LIBQSH       if (debugData.debugLibqsh)
#define DEBUG_LIBQSH_KEYS  if (debugData.debugLibqshKeys)
#define DEBUG_MEMORY_ALLOC if (debugData.debugMemory) debug_memory_push(); if (debugData.debugMemory)
#define DEBUG_MEMORY_FREE  if (debugData.debugMemory) debug_memory_pop(); if (debugData.debugMemory) 
#define DEBUG_MEMORY_REALLOC if (debugData.debugMemory) debug_memory_noop(); if (debugData.debugMemory)
#define DEBUG_MEMORY       if (debugData.debugMemory)

/*
 * this structure contains the values (true/false) for each
 * debug flag 
 */
typedef struct {
    Boolean version;
    Boolean debugTrace;
    Boolean debugAll;
    Boolean debugGui;
    Boolean debugLoading;
    Boolean debugData;
    Boolean debugIO; 
    Boolean debugLibqsh;
    Boolean debugLibqshKeys;
    Boolean debugMemory;

} DebugOptionsStruct;
extern DebugOptionsStruct debugData;

/* Create the debugging resources */
static XtResource resources[] = 
{
    {
        "version", 
        "Version", 
        XmRBoolean, 
        sizeof ( Boolean ),
        XtOffset (DebugOptionsStruct *, version),
        XmRString,  
        "FALSE",
    },
    {
        "debugTrace", 
        "DebugTrace", 
        XmRBoolean, 
        sizeof ( Boolean ),
        XtOffset (DebugOptionsStruct *, debugTrace),
        XmRString,  
        "FALSE",
    },
    {
        "debugAll", 
        "DebugAll", 
        XmRBoolean, 
        sizeof ( Boolean ),
        XtOffset (DebugOptionsStruct *, debugAll),
        XmRString,  
        "FALSE",
    },
    {
        "debugGui", 
        "DebugGui", 
        XmRBoolean, 
        sizeof ( Boolean ),
        XtOffset (DebugOptionsStruct *, debugGui),
        XmRString,  
        "FALSE",
    },
    {
        "debugLoading", 
        "DebugLoading", 
        XmRBoolean, 
        sizeof ( Boolean ),
        XtOffset (DebugOptionsStruct *, debugLoading),
        XmRString,  
        "FALSE",
    },
    {
        "debugData", 
        "DebugData", 
        XmRBoolean, 
        sizeof ( Boolean ),
        XtOffset (DebugOptionsStruct *, debugData),
        XmRString,  
        "FALSE",
    },
    {
        "debugIO", 
        "DebugIO", 
        XmRBoolean, 
        sizeof ( Boolean ),
        XtOffset (DebugOptionsStruct *, debugIO),
        XmRString,  
        "FALSE",
    },
    {
        "debugLibqsh",
        "DebugLibqsh", 
        XmRBoolean, 
        sizeof ( Boolean ),
        XtOffset (DebugOptionsStruct *, debugLibqsh),
        XmRString,  
        "FALSE",
    },
    {
        "debugLibqshKeys",
        "DebugLibqshKeys", 
        XmRBoolean, 
        sizeof ( Boolean ),
        XtOffset (DebugOptionsStruct *, debugLibqshKeys),
        XmRString,  
        "FALSE",
    },
    {
        "debugMemory",
	"DebugMemory",
	XmRBoolean,
	sizeof( Boolean ),
	XtOffset (DebugOptionsStruct *, debugMemory),
	XmRString,
	"FALSE",
    },
};

/* Set up the flags */
static XrmOptionDescRec options[] = {
  { "-version",          "*version",         XrmoptionNoArg, "TRUE" },
  { "-debug_trace",      "*debugTrace",      XrmoptionNoArg, "TRUE" },
  { "-debug_all",        "*debugAll",        XrmoptionNoArg, "TRUE" },
  { "-debug_gui",        "*debugGui",        XrmoptionNoArg, "TRUE" },
  { "-debug_loading",    "*debugLoading",    XrmoptionNoArg, "TRUE" },
  { "-debug_data",       "*debugData",       XrmoptionNoArg, "TRUE" },
  { "-debug_io",         "*debugIO",         XrmoptionNoArg, "TRUE" },
  { "-debug_libqsh",     "*debugLibqsh",     XrmoptionNoArg, "TRUE" },
  { "-debug_libqshkeys", "*debugLibqshKeys", XrmoptionNoArg, "TRUE" },
  { "-debug_memory",     "*debugMemory",     XrmoptionNoArg, "TRUE" },
};

/* Counter for debug_trace tabs */
static int number_of_tabs = 0;

/* Counter for debug_memory tabs */
static int number_of_memory_tabs = 0;

/* Function prototypes */
void output_version    ( char * );
int  get_version_string( char *, char * );
void set_debug_values  ( char *, Widget );
void debug_syntax      ( int, char ** );
void debug_push        ( void );
void debug_pop         ( void );
void debug_memory_push ( void );
void debug_memory_noop ( void );
void debug_memory_pop  ( void );

#endif

