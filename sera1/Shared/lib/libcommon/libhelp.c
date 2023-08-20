/*************************************************
 * libhelp.c
 *
 * INEEL:  BNCT Research Project
 * Montana State Univerity - Bozeman
 *
 * Last Modified by David Helzer on 7/10/97
 * Most recent revision by Mark Rossmeier on 5/4/99
 *
 * This callback provides the procedures used
 * to display dialog help messages and 
 * the SERA manual from Netscape.
 *
 * Netscape must be in the user's path.  
 * The directory in which the SERA manual
 * .html files and widget_help_URLs.txt can be found 
 * must be specified in the .bnctrc file.  The 
 * prefix Web address at which the files can be 
 * found on the Internet must also be specified 
 * there. 
 *************************************************/

#include "libhelp.h"

char pref_string[256];
/*** Bitmap Question Mark ***/
#define question_width 60
#define question_height 60
static unsigned char question_bits[] = {
    0xff, 0x8f, 0xff, 0xc7, 0xff, 0xe3, 0xff, 0x01, 0xff, 0x8f, 0xff, 0xc7,
    0xff, 0xe3, 0xff, 0x01, 0xff, 0x8f, 0xff, 0xc7, 0xff, 0xe3, 0xff, 0x01,
    0x07, 0x80, 0x03, 0xc0, 0x81, 0xe3, 0xc0, 0x01, 0x07, 0x80, 0x03, 0xc0,
    0x81, 0xe3, 0xc0, 0x01, 0x07, 0x80, 0x03, 0xc0, 0x81, 0xe3, 0xc0, 0x01,
    0x07, 0x80, 0x03, 0xc0, 0x81, 0xe3, 0xc0, 0x01, 0x07, 0x80, 0x03, 0xc0,
    0x81, 0xe3, 0xc0, 0x01, 0x07, 0x80, 0x03, 0xc0, 0x81, 0xe3, 0xc0, 0x01,
    0xff, 0x8f, 0xff, 0xc7, 0xff, 0xe3, 0xff, 0x01, 0xff, 0x8f, 0xff, 0xc7,
    0xff, 0xe3, 0xff, 0x01, 0xff, 0x8f, 0xff, 0xc7, 0xff, 0xe3, 0xff, 0x01,
    0x00, 0x8e, 0x03, 0xc0, 0x1f, 0xe0, 0xc0, 0x01, 0x00, 0x8e, 0x03, 0xc0,
    0x1f, 0xe0, 0xc0, 0x01, 0x00, 0x8e, 0x03, 0xc0, 0x3f, 0xe0, 0xc0, 0x01,
    0x00, 0x8e, 0x03, 0xc0, 0x71, 0xe0, 0xc0, 0x01, 0x00, 0x8e, 0x03, 0xc0,
    0xf1, 0xe0, 0xc0, 0x01, 0x00, 0x8e, 0x03, 0xc0, 0xf1, 0xe1, 0xc0, 0x01,
    0xff, 0x8f, 0xff, 0xc7, 0xe1, 0xe3, 0xc0, 0x01, 0xff, 0x8f, 0xff, 0xc7,
    0xc1, 0xe3, 0xc0, 0x01, 0xff, 0x8f, 0xff, 0xc7, 0x81, 0xe3, 0xc0, 0x01,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0xff, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0xff,
    0x3f, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x01, 0x70, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xf0, 0x00, 0xe0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x78, 0x00,
    0xc0, 0x01, 0x00, 0x00, 0x00, 0x00, 0x3c, 0x00, 0xc0, 0x03, 0x00, 0x00,
    0x00, 0x00, 0x1c, 0x00, 0x80, 0x03, 0x00, 0x00, 0x00, 0x00, 0x1c, 0x00,
    0x80, 0x03, 0x00, 0x00, 0x00, 0x00, 0x1c, 0x00, 0x80, 0x03, 0x00, 0x00,
    0x00, 0x00, 0x1c, 0x00, 0x80, 0x03, 0x00, 0x00, 0x00, 0x00, 0x1c, 0x00,
    0x80, 0x03, 0x00, 0x00, 0x00, 0x00, 0x1c, 0x00, 0xc0, 0x03, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xc0, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xe0, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x03, 0x00, 0x00,
    0x00, 0x00, 0x00, 0xf0, 0xff, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8,
    0xff, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0xff, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0xfc, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc,
    0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0x01, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0xfc, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc,
    0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0x01, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x70, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc,
    0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0x01, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0xfc, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x70, 0x00, 0x00, 0x00, 0x00};


/**** Bitmap Help Logo ****/
#define SERA_help_width 64
#define SERA_help_height 100
static unsigned char SERA_help_bits[] = {
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0a, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50,
    0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50,
    0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50, 0x0a, 0x80, 0x3f, 0x00,
    0x00, 0x00, 0x00, 0x50, 0x0a, 0x40, 0x40, 0x00, 0x00, 0xfc, 0x1f, 0x50,
    0x0a, 0x20, 0x80, 0x00, 0x00, 0x80, 0x00, 0x50, 0x0a, 0x20, 0x00, 0x00,
    0x00, 0x80, 0x00, 0x50, 0x0a, 0x20, 0x00, 0x00, 0x00, 0x80, 0x00, 0x50,
    0x0a, 0x20, 0x00, 0x00, 0x00, 0x80, 0x00, 0x50, 0x0a, 0x20, 0x00, 0x00,
    0x00, 0x80, 0x00, 0x50, 0x0a, 0x40, 0x00, 0x00, 0x00, 0x80, 0x00, 0x50,
    0x0a, 0x80, 0x3f, 0x00, 0x00, 0x80, 0x00, 0x50, 0x0a, 0x00, 0x40, 0x00,
    0x00, 0x80, 0x00, 0x50, 0x0a, 0x00, 0x80, 0x00, 0x00, 0x80, 0x00, 0x50,
    0x0a, 0x00, 0x80, 0x00, 0x00, 0x80, 0x00, 0x50, 0x0a, 0x00, 0x80, 0x00,
    0x00, 0x80, 0x00, 0x50, 0x0a, 0x00, 0x80, 0x00, 0x00, 0x80, 0x00, 0x50,
    0x0a, 0x00, 0x80, 0x00, 0x00, 0x80, 0x00, 0x50, 0x0a, 0x00, 0x80, 0x00,
    0x00, 0x80, 0x00, 0x50, 0x0a, 0x20, 0x40, 0x00, 0x00, 0xfc, 0x1f, 0x50,
    0x0a, 0xc0, 0x3f, 0x00, 0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50,
    0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50,
    0x0a, 0x80, 0x7f, 0x00, 0x00, 0x02, 0x10, 0x50, 0x0a, 0x40, 0x00, 0x00,
    0x00, 0x06, 0x10, 0x50, 0x0a, 0x20, 0x00, 0x00, 0x00, 0x0a, 0x10, 0x50,
    0x0a, 0x20, 0x00, 0x00, 0x00, 0x12, 0x10, 0x50, 0x0a, 0x20, 0x00, 0x00,
    0x00, 0x12, 0x10, 0x50, 0x0a, 0x20, 0x00, 0x00, 0x00, 0x22, 0x10, 0x50,
    0x0a, 0x40, 0x00, 0x00, 0x00, 0x42, 0x10, 0x50, 0x0a, 0x80, 0x7f, 0x00,
    0x00, 0x82, 0x10, 0x50, 0x0a, 0x40, 0x00, 0x00, 0x00, 0x02, 0x11, 0x50,
    0x0a, 0x20, 0x00, 0x00, 0x00, 0x02, 0x12, 0x50, 0x0a, 0x20, 0x00, 0x00,
    0x00, 0x02, 0x12, 0x50, 0x0a, 0x20, 0x00, 0x00, 0x00, 0x02, 0x14, 0x50,
    0x0a, 0x20, 0x00, 0x00, 0x00, 0x02, 0x18, 0x50, 0x0a, 0x40, 0x00, 0x00,
    0x00, 0x02, 0x10, 0x50, 0x0a, 0x80, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x50,
    0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50,
    0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00,
    0x00, 0xfc, 0x0f, 0x50, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x50,
    0x0a, 0xc0, 0x3f, 0x00, 0x00, 0x04, 0x00, 0x50, 0x0a, 0x40, 0x40, 0x00,
    0x00, 0x04, 0x00, 0x50, 0x0a, 0x40, 0x80, 0x00, 0x00, 0x04, 0x00, 0x50,
    0x0a, 0x40, 0x80, 0x00, 0x00, 0x04, 0x00, 0x50, 0x0a, 0x40, 0x80, 0x00,
    0x00, 0x04, 0x00, 0x50, 0x0a, 0x40, 0x80, 0x00, 0x00, 0xfc, 0x01, 0x50,
    0x0a, 0x40, 0x40, 0x00, 0x00, 0x04, 0x00, 0x50, 0x0a, 0xc0, 0x3f, 0x00,
    0x00, 0x04, 0x00, 0x50, 0x0a, 0x40, 0x10, 0x00, 0x00, 0x04, 0x00, 0x50,
    0x0a, 0x40, 0x10, 0x00, 0x00, 0x04, 0x00, 0x50, 0x0a, 0x40, 0x20, 0x00,
    0x00, 0x04, 0x00, 0x50, 0x0a, 0x40, 0x20, 0x00, 0x00, 0x04, 0x00, 0x50,
    0x0a, 0x40, 0x40, 0x00, 0x00, 0x04, 0x00, 0x50, 0x0a, 0x40, 0x40, 0x00,
    0x00, 0x04, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50,
    0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50,
    0x0a, 0x00, 0x08, 0x00, 0x00, 0xf0, 0x03, 0x50, 0x0a, 0x00, 0x14, 0x00,
    0x00, 0x08, 0x04, 0x50, 0x0a, 0x00, 0x22, 0x00, 0x00, 0x04, 0x08, 0x50,
    0x0a, 0x00, 0x41, 0x00, 0x00, 0x02, 0x10, 0x50, 0x0a, 0x00, 0x41, 0x00,
    0x00, 0x02, 0x10, 0x50, 0x0a, 0x80, 0x80, 0x00, 0x00, 0x02, 0x10, 0x50,
    0x0a, 0x80, 0x80, 0x00, 0x00, 0x02, 0x10, 0x50, 0x0a, 0x80, 0x80, 0x00,
    0x00, 0x02, 0x10, 0x50, 0x0a, 0x40, 0x00, 0x01, 0x00, 0x02, 0x10, 0x50,
    0x0a, 0x40, 0x00, 0x01, 0x00, 0x02, 0x10, 0x50, 0x0a, 0xc0, 0xff, 0x01,
    0x00, 0x02, 0x10, 0x50, 0x0a, 0x40, 0x00, 0x01, 0x00, 0x02, 0x10, 0x50,
    0x0a, 0x40, 0x00, 0x01, 0x00, 0x02, 0x10, 0x50, 0x0a, 0x40, 0x00, 0x01,
    0x00, 0x04, 0x08, 0x50, 0x0a, 0x40, 0x00, 0x01, 0x00, 0x08, 0x04, 0x50,
    0x0a, 0x40, 0x00, 0x01, 0x00, 0xf0, 0x03, 0x50, 0x0a, 0x40, 0x00, 0x01,
    0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50,
    0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50,
    0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50, 0x0a, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};


/*
 * The length of all strings used here
 */
#define MAX_STRING_LENGTH 300


/*
 * Messages to be displayed in the popup dialog boxes
 */

#define HELP_METHOD_MESSAGE "Would you like to load a standard\nhelp dialog box, the SERA manual\nfrom the Internet, or the SERA manual\nfrom the files on your computer?\n\nNote: Loading either SERA\nmanual requires Netscape Navigator."

#define URL_FILE_NOT_FOUND_MESSAGE "The file widget_help_URLs.txt\nwas not found in the specified directory\ncontaining the SERA manual.\n\nPlease place the file\nthere and try again."


/* Declarations of callback functions */
static void Dialog_Help_Callback     ( Widget w, XtPointer clientData, XtPointer callData );
static void Internet_Manual_Callback ( Widget w, XtPointer clientData, XtPointer callData );
static void Files_Manual_Callback    ( Widget w, XtPointer clientData, XtPointer callData );
static void Help_Method_Close_CB     ( Widget w, XtPointer clientData, XtPointer callData );
static void Destroy_Shell_CB         ( Widget w, XtPointer clientData, XtPointer callData );

/* Declarations of the help methods */
static char * OpenHelpText  ( Widget w );
static void   dialog_help   ( Widget w );
static void   load_netscape ( char * address_to_load );
static Widget GetTopShell   ( Widget w );


void ContextHelpCallback ( Widget w, XtPointer clientData, XtPointer callData )
{
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) callData;
    Widget        selectedWidget = NULL;
    static char  *helpString = NULL;
    static char   helpURL_file_name[MAX_STRING_LENGTH];/* A single file name (no path) */

    /**** Widgets for the help method dialog box ****/
    Widget        HelpMethodShell;
    Widget        HelpMethodForm;
    Widget        help_method_close_button;
    Widget        help_method_dialog_button;
    Widget        help_method_internet_button;
    Widget        help_method_files_button;
    Widget        help_method_message;
    Widget        help_method_separator;
    Widget        question_label;
    Pixmap        question_pixmap;
    Pixel         fg, bg;
    XmString      xmstr;
    /************************************************/
  
    Widget        URL_File_Not_Found_Dialog;
    static Cursor cursor = (Cursor) NULL;
    XEvent        event;
    Widget        parent = XtParent ( w );                /* In case w is a gadget */
    Widget	  main_window = GetTopShell (w);
    Widget        main_window_of_selected;                /* top shell of the selected widget */
    FILE         *URL_Addresses;
    char          current_widget_name[MAX_STRING_LENGTH];
    char          file_with_URLs[MAX_STRING_LENGTH];       /* The file containing all URL's */
    int           URL_Match;
    Widget        current_comparison_widget;
    char         *dialog_help_path;
  

    DEBUG_TRACE_IN  printf("Entering ContextHelpCallback\n");
    
    /*
     * If called from an XmNhelpCallback (invoked with <F1>))
     * the reason will be XmCR_HELP. Extract the smallest
     * window in which the help request occurred, and find
     * the corresponding widget.
     */
    if ( cbs->reason == XmCR_HELP )
    {
        if ( cbs->event && cbs->event->xany.window )
            selectedWidget = XtWindowToWidget ( XtDisplay ( parent ),
                                                cbs->event->xany.window );
    }

    else
    {
        /*
         * If the reason is not XmCR_HELP, this callback must have been
         * invoked a help menu item.
         * Allow the user to interactively select a widget on which
         * help is required.
         */
        if ( !cursor )
            cursor = XCreateFontCursor ( XtDisplay ( parent ),
                                         XC_question_arrow );
      
        selectedWidget = XmTrackingEvent ( main_window, cursor,
                                           FALSE,  &event );
    }
  
  
    /*
     * Check for a valid widget.  If none found, don't do anything, just return.
     */
    if (!selectedWidget) {
        DEBUG_TRACE_OUT  printf("Leaving ContextHelpCB, no valid widget found\n");
        return;
    }
  
  
    /*
     * Retrieve the text of a help message from the  
     * file containing the help corresponding to the 
     * selected widget or its first ancestor that has
     * a help string corresponding to it.
     */
    helpString = OpenHelpText ( selectedWidget );
    if( helpString == NULL ) return;
  
    /*
     * Retrieve the URL address of the help message from the
     * external file "widget_help_URLs.txt" corresponding to the
     * selected widget or its first ancestor that has a help
     * string corresponding to it.
     */

    dialog_help_path = (char *) getenv("SERA_HELP");
    if( dialog_help_path == NULL || strstr( dialog_help_path, "/Docs/dialog_help" ) == NULL )
    {
        MT_free( (void *) helpString );
        DT_error( w, "Your environment variable SERA_HELP\nhas not been set correctly! Help will not\nwork until this is set correctly.",
                  NULL, NULL );
        return;
    }

    strcpy (file_with_URLs, dialog_help_path);
    strcat (file_with_URLs, pref_string);
    strcat (file_with_URLs, "widget_help_URLs.txt");
  
    URL_Addresses = fopen(file_with_URLs, "r");
  
    helpURL_file_name[0] = '\0';
  
  
    /* 
     * Check to make sure that the external file was found. 
     * If not found, create an error popup dialog box.  
     */
    if (!URL_Addresses) 
    {
        /* 
         * Create an error dialog box -- the external file was not in the 
         * same directory as the SERA executable.
         */
        URL_File_Not_Found_Dialog = XmCreateErrorDialog ( w,
                                                          "URL_File_Not_Found_Dialog",
                                                          NULL, 0);
        /* 
         * Remove the Cancel and Help buttons.
         * Also, declare the string to be displayed and center it
         */
        XtUnmanageChild ( XmMessageBoxGetChild ( URL_File_Not_Found_Dialog,
                                                 XmDIALOG_CANCEL_BUTTON ) );
    
        XtUnmanageChild ( XmMessageBoxGetChild ( URL_File_Not_Found_Dialog, 
                                                 XmDIALOG_HELP_BUTTON ) );
    
        XtVaSetValues (URL_File_Not_Found_Dialog, 
                       XtVaTypedArg, XmNmessageString, XmRString,
                       URL_FILE_NOT_FOUND_MESSAGE, strlen (URL_FILE_NOT_FOUND_MESSAGE)+1,
                       XmNmessageAlignment, XmALIGNMENT_CENTER,
                       XmNnoResize, TRUE,
                       NULL );
    
        XtManageChild (URL_File_Not_Found_Dialog);
    }
  
  
    /* 
     * If the file was opened, proceed to find a URL file name (not the path) to 
     * match the selected Widget.
     */
    else 
    {
        main_window_of_selected = GetTopShell (selectedWidget);
        current_comparison_widget = selectedWidget;
        helpURL_file_name[0] = '\0';
        URL_Match = 0;
    
    
        fscanf (URL_Addresses, "%s", current_widget_name);
    
        while ((!feof (URL_Addresses)) & (!URL_Match)) 
        {
            if (strcmp (current_widget_name, XtName (current_comparison_widget)) == 0) 
            {
                URL_Match = 1;
                fscanf (URL_Addresses, "%s", helpURL_file_name);
            }
            fscanf (URL_Addresses, "%s", current_widget_name);
        }
    
        /*
         * The entire ext. file has been cycled through once.  To do this again, it 
         * must be closed and reopened.  
         */
    
        fclose (URL_Addresses);
    
        if (!URL_Match) 
        {  
            do 
            {
                current_comparison_widget = XtParent (current_comparison_widget);
                URL_Addresses = fopen (file_with_URLs, "r");
                fscanf (URL_Addresses, "%s", current_widget_name);
                while ((!feof (URL_Addresses)) && (!URL_Match)) 
                { 
                    if (strcmp(current_widget_name, XtName(current_comparison_widget)) == 0) 
                    {
                        URL_Match = 1;
                        fscanf (URL_Addresses, "%s", helpURL_file_name);
                    }
                    fscanf (URL_Addresses, "%s", current_widget_name);
                }
                fclose (URL_Addresses);
            }
            while (XtName(current_comparison_widget) != XtName(main_window_of_selected));
        }
    
        /*
         * If both a helpString and a helpURL_file_name have been found, 
         * create a dialog to ask the user if a standard help dialog
         * or the Netscape help manual should be loaded.
         */
        if ((helpURL_file_name[0] != '\0')) 
        {
      
            /* Free the helpString, it will be retrieved again in dialog_help()*/
            MT_free( (void *) helpString );
      
      
            /*
             * Create a popup shell to hold the main form for the window
             */
            HelpMethodShell = XtVaCreatePopupShell("help_method_shell",
                                                   xmDialogShellWidgetClass, w,
                                                   XmNtitle, "Which Help Method",
                                                   NULL);
      
            HelpMethodForm  = XtVaCreateWidget("help_method_form",
                                               xmFormWidgetClass, HelpMethodShell,
                                               XmNnoResize, TRUE,     /* cannot be resized */
                                               XmNhorizontalSpacing, 10,
                                               XmNverticalSpacing,   10,
                                               NULL);
      
            /*
             * Get the foreground and background values of the form, and
             * then create a pixmap with these colors.
             */
            XtVaGetValues( HelpMethodForm, XmNforeground, &fg, XmNbackground, &bg, NULL );
      
            question_pixmap = XCreatePixmapFromBitmapData( XtDisplay(HelpMethodForm), 
                                                           RootWindowOfScreen(XtScreen(HelpMethodForm)),
                                                           (char *) question_bits, 
                                                           question_width, question_height,
                                                           fg, bg, 
                                                           DefaultDepthOfScreen(XtScreen(HelpMethodForm)));
      
            /*
             * Create a label widget to display the pixmap we just created.
             */
            question_label = XtVaCreateManagedWidget("question",
                                                     xmLabelWidgetClass, HelpMethodForm,
                                                     XmNlabelType, XmPIXMAP,
                                                     XmNlabelPixmap, question_pixmap,
                                                     XmNleftAttachment, XmATTACH_FORM,
                                                     XmNleftOffset,     15,
                                                     XmNtopAttachment,  XmATTACH_FORM,
                                                     XmNtopOffset,      35,
                                                     NULL);
      
      
            /*
             * Create a label widget to display the help message
             */
            help_method_message = XtVaCreateManagedWidget("help_method_message",
                                                          xmLabelWidgetClass, HelpMethodForm,
                                                          XtVaTypedArg, XmNlabelString, XmRString,
                                                          HELP_METHOD_MESSAGE, strlen(HELP_METHOD_MESSAGE),
                                                          XmNalignment, XmALIGNMENT_CENTER,
                                                          XmNleftAttachment,  XmATTACH_WIDGET,
                                                          XmNleftWidget,      question_label,
                                                          XmNleftOffset,      5,
                                                          XmNrightAttachment, XmATTACH_FORM,
                                                          XmNtopAttachment,   XmATTACH_FORM,
                                                          XmNtopOffset,       10,
                                                          NULL);
            /*
             * Create a separator between the labels and the four buttons.
             */
            help_method_separator = XtVaCreateManagedWidget("help_method_sep",
                                                            xmSeparatorWidgetClass, HelpMethodForm,
                                                            XmNleftAttachment, XmATTACH_FORM,
                                                            XmNrightAttachment,XmATTACH_FORM,
                                                            XmNtopAttachment,  XmATTACH_WIDGET,
                                                            XmNtopWidget,      help_method_message, 
                                                            NULL);
      
      
            /*
             * Add the four buttons to the bottom of the window
             */
            xmstr = XmStringCreateLtoR("Close", XmFONTLIST_DEFAULT_TAG);
            help_method_close_button = XtVaCreateManagedWidget("Close", xmPushButtonWidgetClass,
                                                               HelpMethodForm,
                                                               XmNwidth, 150,
                                                               XmNlabelString,      xmstr,
                                                               XmNalignment,        XmALIGNMENT_CENTER,
                                                               XmNtopAttachment,    XmATTACH_WIDGET,
                                                               XmNtopWidget,        help_method_separator,
                                                               XmNleftAttachment,   XmATTACH_FORM,
                                                               XmNbottomAttachment, XmATTACH_FORM,
                                                               NULL);
            XmStringFree( xmstr );
      
            xmstr = XmStringCreateLtoR("Dialog Help",XmFONTLIST_DEFAULT_TAG);
            help_method_dialog_button = XtVaCreateManagedWidget("Dialog", xmPushButtonWidgetClass,
                                                                HelpMethodForm,
                                                                XmNwidth,  150,
                                                                XmNlabelString, xmstr,
                                                                XmNalignment, XmALIGNMENT_CENTER,
                                                                XmNtopAttachment,    XmATTACH_WIDGET,
                                                                XmNtopWidget,        help_method_separator,
                                                                XmNleftAttachment,   XmATTACH_WIDGET,
                                                                XmNleftWidget,       help_method_close_button,
                                                                XmNbottomAttachment, XmATTACH_FORM,
                                                                NULL);
            XmStringFree( xmstr );
      
            xmstr = XmStringCreateLtoR("Internet Manual",XmFONTLIST_DEFAULT_TAG);
            help_method_internet_button = XtVaCreateManagedWidget("Internet", xmPushButtonWidgetClass,
                                                                  HelpMethodForm,
                                                                  XmNwidth,  150,
                                                                  XmNlabelString,    xmstr,
                                                                  XmNalignment,      XmALIGNMENT_CENTER,
                                                                  XmNtopAttachment,    XmATTACH_WIDGET,
                                                                  XmNtopWidget,        help_method_separator,
                                                                  XmNleftAttachment,   XmATTACH_WIDGET,
                                                                  XmNleftWidget,       help_method_dialog_button,
                                                                  XmNbottomAttachment, XmATTACH_FORM,
                                                                  NULL);
            XmStringFree( xmstr );
      
            xmstr = XmStringCreateLtoR("Files Manual",XmFONTLIST_DEFAULT_TAG);
            help_method_files_button = XtVaCreateManagedWidget("Files", xmPushButtonWidgetClass,
                                                               HelpMethodForm,
                                                               XmNwidth,  150,
                                                               XmNlabelString,    xmstr,
                                                               XmNalignment,      XmALIGNMENT_CENTER,
                                                               XmNtopAttachment,    XmATTACH_WIDGET,
                                                               XmNtopWidget,        help_method_separator,
                                                               XmNleftAttachment,   XmATTACH_WIDGET,
                                                               XmNleftWidget,       help_method_internet_button,
                                                               XmNbottomAttachment, XmATTACH_FORM,
                                                               XmNrightAttachment,  XmATTACH_FORM,
                                                               NULL);
            XmStringFree( xmstr );
      
            /*
             * Add callbacks for the four buttons
             */
      
            XtAddCallback( help_method_close_button, XmNactivateCallback,
                           (XtCallbackProc) Help_Method_Close_CB, (XtPointer) HelpMethodShell );
      
            XtAddCallback( help_method_dialog_button, XmNactivateCallback,
                           (XtCallbackProc) Dialog_Help_Callback, (XtPointer) selectedWidget );
      
      
            XtAddCallback( help_method_internet_button, XmNactivateCallback,
                           (XtCallbackProc) Internet_Manual_Callback, (XtPointer) helpURL_file_name );
      
            XtAddCallback( help_method_files_button, XmNactivateCallback,
                           (XtCallbackProc) Files_Manual_Callback, (XtPointer) helpURL_file_name );    
      
      
            /*
             * Manage the form inside the shell
             * and popup the shell
             */
      
            XtManageChild( HelpMethodForm );
            XtPopup( HelpMethodShell, XtGrabNone );
      
        }
    
    
        /*
         * If only the helpString contains valid data or if neither
         * helpString nor helpURL contains valid data, load the popup dialog
         */
        else
            dialog_help ( selectedWidget );
    
    }  
    DEBUG_TRACE_OUT  printf("Leaving ContextHelpCB\n");
}


/*
 * The Help_Method_Close_CB is called when the close button is
 * pressed from the HelpMethod popup shell.
 * Simply pops down the shell and then destroys it.
 */
void Help_Method_Close_CB( Widget w, XtPointer clientData, XtPointer callData )
{
    Widget shell = (Widget) clientData;
    DEBUG_TRACE_IN  printf("Entering Help_Method_Close_CB\n");
  
    XtPopdown( shell );
    XtDestroyWidget( shell );
  
    DEBUG_TRACE_OUT  printf("Leaving Help_Method_Close_CB\n");
}


/* 
 * The Dialog_Help_Callback is called when the "Dialog Help" button is pressed in 
 * response to the question of which help method to load.
 */
void Dialog_Help_Callback ( Widget w, XtPointer clientData, XtPointer callData )
{
    Widget selectedWidget = (Widget) clientData;
    DEBUG_TRACE_IN  printf("Entering Dialog_Help_Callback\n");
  
    dialog_help (selectedWidget);  
  
    DEBUG_TRACE_OUT  printf("Leaving Dialog_Help_Callback\n");
}


/* 
 * the Internet_Manual_Callback is called when the "Internet_Manual" button is pressed in
 * response to the question of which help method to load.
 */
void Internet_Manual_Callback ( Widget w, XtPointer clientData, XtPointer callData )
{
    char URL_to_load[MAX_STRING_LENGTH];
    char *helpURL_file_name = (char *) clientData;
    char *net_manual_path;

    DEBUG_TRACE_IN  printf("Entering Internet_Manual_Callback\n");

    net_manual_path = (char *) getenv("NET_MANUAL_PATH");
    if( net_manual_path == NULL || strstr(net_manual_path,"http://www.cs.montana.edu/~bnct/manual/") == NULL )
    {
        DT_error(w, "Your environment variable NET_MANUAL_PATH\nhas not been set correctly! The Internet manual\ncannot be loaded until this is set correctly.",
                 NULL, NULL);
        return;
    }
  
    strcpy (URL_to_load, net_manual_path);  /* Adds URL prefix to file name */
    strcat (URL_to_load, helpURL_file_name);
    load_netscape (URL_to_load);
  
    DEBUG_TRACE_OUT  printf("Leaving Internet_Manual_Callback\n");
  
}




/*
 * Files_Manual_Callback is called when the "Files Manual" button is pressed in
 * response to the question of which help method to load.
 */

void Files_Manual_Callback (Widget w, XtPointer clientData, XtPointer callData)
{
    char file_to_load[MAX_STRING_LENGTH];
    char *helpURL_file_name = (char *) clientData;
    char *files_manual_path;

    DEBUG_TRACE_IN  printf("Entering Files_Manual_Callback\n");
  
    files_manual_path = (char *) getenv("FILES_MANUAL_PATH");
    if( files_manual_path == NULL || strstr(files_manual_path,"/Docs/Manuals/sera1/") == NULL )
    {
        DT_error(w, "Your environment variable FILES_MANUAL_PATH\nhas not been set correctly! The Files manual\ncannot be loaded until this is set correctly.",
                 NULL, NULL);
        return;
    }

    strcpy (file_to_load, files_manual_path);
    strcat (file_to_load, helpURL_file_name);
    load_netscape (file_to_load);
  
    DEBUG_TRACE_OUT  printf("Leaving Files_Manual_Callback\n");
}


/*
 * dialog_help () is a procedure that is called when a standard
 * dialog popup box is to be used to display the help file.
 *    ('w' is the selected widget to display help on.)
 *
 * This code not modified by David Helzer      
 */

void dialog_help (Widget w)
{
  
    Widget   help_dialog, pane, form, label, text_w, cancel_widget;
    Pixmap   pixmap;
    Pixel    fg, bg;
    Arg      args[12];
    char    *helpString;
  
    DEBUG_TRACE_IN  printf("Entering dialog_help\n");
  
  
    /*
     * Retrieve the text of a help message from the  
     * file containing the help corresponding to the 
     * selected widget or its first ancestor that has
     * a help string corresponding to it.
     */
    helpString =  OpenHelpText ( w );
    if( helpString == NULL ) return;
  
    /*
     * Create a dialog to display the help information
     */
    help_dialog = XtVaCreatePopupShell("Help",
                                       xmDialogShellWidgetClass, GetTopShell(w),
                                       XmNdeleteResponse, XmDESTROY,
                                       NULL);
  
    /* Create a PanedWindow to manage the widgets in this dialog. */
    pane = XtVaCreateWidget("pane", xmPanedWindowWidgetClass, help_dialog,
                            XmNsashWidth,  1,   /* PanedWindow won't let us set these to 0! */
                            XmNsashHeight, 1,   /* Make small so user doesn't try to resize */
                            NULL);
  
    /* Create a RowColumn in the form for Label and Text widgets.
     * This is the control area.
     */
    form = XtVaCreateWidget("form1", xmFormWidgetClass, pane, XmNheight, 230, XmNwidth, 520, NULL);
    XtVaGetValues(form,   /* once created, we can get its colors */
                  XmNforeground, &fg,
                  XmNbackground, &bg,
                  NULL);
  
    /* create the pixmap of the appropriate depth using the colors
     * that will be used by the parent (form).
     */
    pixmap = XCreatePixmapFromBitmapData(XtDisplay(form),
                                         RootWindowOfScreen(XtScreen(form)),
                                         (char *)SERA_help_bits, SERA_help_width, SERA_help_height,
                                         fg, bg, DefaultDepthOfScreen(XtScreen(form)));
  
    /* Create a label gadget using this pixmap */
    label = XtVaCreateManagedWidget("label", xmLabelGadgetClass, form,
                                    XmNlabelType,        XmPIXMAP,
                                    XmNlabelPixmap,      pixmap,
                                    XmNleftAttachment,   XmATTACH_FORM,
                                    XmNtopAttachment,    XmATTACH_FORM,
                                    XmNbottomAttachment, XmATTACH_FORM,
                                    XmNrightAttachment,  XmATTACH_NONE,
                                    NULL);
  
    XtSetArg(args[0], XmNvalue,                 helpString);
    XtSetArg(args[1], XmNscrollHorizontal,      False);
    XtSetArg(args[2], XmNeditMode,              XmMULTI_LINE_EDIT);
    XtSetArg(args[3], XmNeditable,              False);
    XtSetArg(args[4], XmNcursorPositionVisible, False);
    XtSetArg(args[5], XmNwordWrap,              True);
    XtSetArg(args[6], XmNscrollVertical,        True);
    XtSetArg(args[7], XmNrows,                  15);
    XtSetArg(args[8], XmNcolumns,               60);
    /* The XmNrows and the XmNcolumns should be adjusted according to 
     * the users preference as to what the size the help popup should be.
     */
  
    text_w = XmCreateScrolledText(form, "help_text", args, 9);
    /* Attachment values must be set on the Text widget's PARENT,
     * the ScrolledWindow. This is the object that is positioned.
     */
    XtVaSetValues(XtParent(text_w),
                  XmNleftAttachment,   XmATTACH_WIDGET,
                  XmNleftWidget,       label,
                  XmNtopAttachment,    XmATTACH_FORM,
                  XmNrightAttachment,  XmATTACH_FORM,
                  XmNbottomAttachment, XmATTACH_FORM,
                  NULL);
    XtManageChild(text_w);
    XtManageChild(form);
  
    /* Create another form to act as the action area for the dialog */
    form = XtVaCreateWidget("form2", xmFormWidgetClass, pane,
                            XmNfractionBase,    3,
                            NULL);
  
    /* The Cancel button is under the pane's separator and is
     * attached to the left edge of the form.  It spreads from
     * position 1 to 2 along the bottom (the form is split into
     * 3 separate grids ala XmNfractionBase upon creation).
     */
    cancel_widget = XtVaCreateManagedWidget("Cancel",
                                            xmPushButtonGadgetClass, form,
                                            XmNtopAttachment,        XmATTACH_FORM,
                                            XmNbottomAttachment,     XmATTACH_FORM,
                                            XmNleftAttachment,       XmATTACH_POSITION,
                                            XmNleftPosition,         1,
                                            XmNrightAttachment,      XmATTACH_POSITION,
                                            XmNrightPosition,        2,
                                            XmNshowAsDefault,        True,
                                            XmNdefaultButtonShadowThickness, 1,
                                            NULL);
    XtAddCallback(cancel_widget, XmNactivateCallback, (XtCallbackProc)Destroy_Shell_CB, (XtPointer) help_dialog);
  
    /* Fix the action area pane to its current height 
     * never let it resize 
     */
    XtManageChild(form);
    {
        Dimension h;
        XtVaGetValues(cancel_widget, XmNheight, &h, NULL);
        XtVaSetValues(form, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
    }
  
    XtManageChild(pane);
    XtPopup(help_dialog, XtGrabNone);
  
    MT_free ( (void *) helpString);    
  
    DEBUG_TRACE_OUT  printf("Leaving dialog_help\n");
}



/*
 * netscape_help (char *helpURL) is a procedure that executes 
 * netscape and loads the specified URL.  The netscape directory must
 * be in the user's path.  
 */

void load_netscape (char *address_to_load)
{
    Widget     Netscape_Load_Error;
    int        netscape_pid;
    char       netscape_remote_command [MAX_STRING_LENGTH];
    char       system_call_argument[MAX_STRING_LENGTH];
    int        URL_Match = 0;
    int        netscape_system_return;
  
    DEBUG_TRACE_IN  printf("Entering load_netscape\n");
  
    /*
     * If Netscape is already running, use a system -remote call to 
     * change the current URL.  If Netscape is not running,
     * use a 'fork()' and an 'execlp' to load it with the 
     * appropriate URL.
     *
     * The -remote flag is provided by Netscape.  To find out more 
     * about Netscape flags, type "netscape -help" at a UNIX prompt.
     */
  
    strcpy (system_call_argument, "netscape -remote 'openURL(");
    strcat (system_call_argument, address_to_load);
    strcat (system_call_argument, ")'");
    netscape_system_return = system (system_call_argument);
  
  
    if (netscape_system_return != 0) { 
        printf ("SERA is attempting to load Netscape.\n");
    
        netscape_pid = fork();
        switch (netscape_pid)
        { 
            case 0: 
                /* execlp() is used to avoid locking SERA while running Netscape */
                execlp ("netscape", "netscape", address_to_load, NULL);
	
            case -1:
                printf ("Error loading Netscape!  Make sure Netscape is on your computer and in your path.\n");
                exit (0);
                break;
	
            default:
                break;
        }
    }    
    DEBUG_TRACE_OUT  printf("Leaving load_netscape\n");
}

void set_preference_directory(char *pref)
{
    DEBUG_TRACE_IN  printf("Entering set_preference_directory\n");
    strcpy(pref_string,pref);
    DEBUG_TRACE_OUT printf("Leaving set_preference_directory\n");
}


void Destroy_Shell_CB( Widget w, XtPointer clientData, XtPointer callData )
{
  
    DEBUG_TRACE_IN  printf("Entering DestroyShellCB\n");
  
    XtDestroyWidget( (Widget) clientData );
  
    DEBUG_TRACE_OUT printf("Leaving DestroyShellCB\n");
  
}


Widget GetTopShell( Widget w )
{
    DEBUG_TRACE_IN  printf("Entering GetTopShell\n");
  
    while (w && !XtIsWMShell(w))
    {
        w = XtParent(w);
    }
  
    DEBUG_TRACE_OUT  printf("Leaving GetTopShell\n");
    return w;
}


char * OpenHelpText( Widget w )
{
    char    HelpPath[MAX_STRING_LENGTH];
    char   *string1 = pref_string;     
    char   *string2 = ".dat";       /* extension added on to widget name */
    char   *widgetname = XtName(w); /* name of widget looking for help on */
    char   *savename = XtName(w);   /* save first name for error message */
    Widget  parent = XtParent (w);

    FILE   *fp;                    /* pointer to the file where help text is located */
    long   length;                 /* length of the input file in bytes */
    char   *msg_buffer;            /* buffer to build Sorry message in */
    char   *returnValue = NULL;    /* string containing the actual help text */
    char   *BnctHelp;

    DEBUG_TRACE_IN  printf("Entering OpenHelpText\n");

    BnctHelp = (char *) getenv("SERA_HELP");
    if( BnctHelp == NULL || strstr(BnctHelp, "/Docs/dialog_help") == NULL )
    {
        DT_error(w, "Your environment variable SERA_HELP\nhas not been set correctly! Dialog Help\nwill not work until this is set correctly.",
                 NULL, NULL);
        return( (char *) NULL );
    }

    /*
     * While no help text is found keep searching up widget tree
     */
    while( returnValue == NULL ) 
    {
        /*
         * Forming the directory path and file to look for
         * help text in.
         */
     
        strcpy (HelpPath, BnctHelp);
        strcat (HelpPath, string1);
        strcat (HelpPath, widgetname);
        strcat (HelpPath, string2);

        if (( fp = fopen (HelpPath, "r")) != NULL)
        {

            /*
             * checking to see how many bytes the file contains
             * and storing that value in length
             */
            fseek ( fp, (long)0, 2);
            length = ftell (fp);
            fseek ( fp, (long)0, 0);

            /*
             * allocating enough memory to hold entire file in 1 string
             * the memory is freed at the end of the ContextHelpCB
             */
            returnValue = (char*) MT_malloc( length + 1 ); 

            /* 
             * copying file into the char * variable named returnValue
             */
            fread (returnValue, (size_t) 1, length, fp);
            /*
             * adding on a string terminator to the end of the newly read in string.
             */
            * (returnValue + length) = '\0';

            if (fclose (fp) == EOF)
                fprintf (stderr, "The file couldn't be closed.\n");
            /* This should probably produce an error popup. */
        }
        else
	{

            /*
             * changing the widgets name to its parent's name for
             * help string lookup purposes and the parent widget 
             * to what was the grandparent
             */
            widgetname = XtName(parent);
            parent = XtParent(parent);
         
            /* 
             * If climbed widget tree up to top and still never found
             * any help text, make help text proclaiming no help.
             */
            if (parent == NULL)
            {
                msg_buffer = (char *) MT_malloc( (40+strlen(savename)+1)*sizeof(char) );
                sprintf(msg_buffer,"Sorry no help available. File: %s\n",savename);
	   
                returnValue = msg_buffer;

            }
	}
    }
    DEBUG_TRACE_OUT  printf("Leaving OpenHelpText\n");
    return (returnValue);
} 
