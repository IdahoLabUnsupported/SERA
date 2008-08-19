/******************************************************************************
 * multi_file_select.c                                                        *
 *                                                                            *
 * INEEL BNCT Research Project                                                *
 * Montana State University - Bozeman                                         *
 *                                                                            *
 * Allows the selection of multiple files at once.                            *
 * Call get_multiple_files().  Sending a topLevelWidget, the XtAppContext,    *
 *   and the address of a multi_select_type structure.                        *
 *                                                                            *
 * Matt Cohen 7/28/98                                                         *
 *****************************************************************************/
#include <Xm/Xm.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/List.h>
#include <Xm/Label.h>
#include <Xm/ArrowB.h>
#include <Xm/Separator.h>
#include <Xm/ScrollBar.h>
#include <Xm/Frame.h>
#include <Xm/ScrolledW.h>
#include <Xm/MainW.h>
#include <Xm/Text.h>
#include "multi_file_select.h"
#include "debug_tools.h"
#include "memory_tools.h"

/* Call this to create the multiple file selection widget 
 * or just to pop it up.
 */
int get_multiple_files ( Widget w, XtAppContext app,
			 multi_select_type *multi )
{
  static int first_time = 1;
    DEBUG_TRACE_IN printf ( "Entering get_multiple_files\n" );

    multi->user_done = 0;


    if ( first_time )
    {
      build_MFS_gui ( w, multi );
      first_time = 0; 
    }

    XtPopup ( multi->shell, XtGrabNone );

    while ( !multi->user_done )
    {
        XtAppProcessEvent (app, XtIMAll );
    }

    DEBUG_TRACE_OUT printf ( "Leaving get_multiple_files\n" );

    return ( multi->got_files );
}


/* This function builds the multiple selection widget */
void build_MFS_gui ( Widget w, multi_select_type *multi )
{
    XmString buttonLabel;
    Arg al[10];      
    int ac = 0;

    DEBUG_TRACE_IN printf ( "Entering build_MFS_gui\n" );

    DEBUG_GUI printf ( "Building shell\n");
    multi->shell 
        = XtCreatePopupShell ( "Select Files", topLevelShellWidgetClass,
			       w, NULL, 0 );

    /* Allow resizing */
    XtVaSetValues ( multi->shell, XmNallowShellResize, TRUE, NULL );

    DEBUG_GUI printf ( "Building form\n");
    multi->form 
        = XtVaCreateManagedWidget ( "form", xmFormWidgetClass,
				    multi->shell, NULL);

    DEBUG_GUI printf ( "Building file selection box\n");
    /* Build File Selection Box */
    multi->fsb  
        = XtVaCreateManagedWidget ( "fsb", xmFileSelectionBoxWidgetClass,
                                    multi->form,
                                    XmNleftAttachment,   XmATTACH_FORM,
				    XmNleftOffset,       5,
                                    XmNtopAttachment,    XmATTACH_FORM,
				    XmNtopOffset,        5,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNbottomOffset,     5,
                                    NULL );   
    
    /* Remove annoying callback from the list widget! */
    XtRemoveAllCallbacks ( XmFileSelectionBoxGetChild (multi->fsb,
						   XmDIALOG_FILE_LIST ),
                           XmNdefaultActionCallback );

    XtAddCallback ( XmFileSelectionBoxGetChild (multi->fsb,
						XmDIALOG_FILE_LIST ),
                    XmNdefaultActionCallback, addMFSCallback, 
		    ( XtPointer ) multi );

    /* Get rid of ok button */
    XtUnmanageChild ( XmFileSelectionBoxGetChild ( multi->fsb,
						   XmDIALOG_OK_BUTTON ) );

    /* Redefine the cancel button */
    buttonLabel = XmStringCreateLocalized("Select All");
    XtVaSetValues ( multi->fsb, XmNcancelLabelString, buttonLabel, NULL );
    XmStringFree(buttonLabel);
    
    /* Add callback for Select All button */
    XtAddCallback( multi->fsb, XmNcancelCallback,
                   selectAllMFSCallback, ( XtPointer ) multi->fsb );

    /* Redefine the help button */
    buttonLabel = XmStringCreateLocalized("Deselect All");
    XtVaSetValues ( multi->fsb, XmNhelpLabelString, buttonLabel, NULL );
    XmStringFree(buttonLabel);
    
    /* Add callback for Deselect All button */
    XtAddCallback ( multi->fsb, XmNhelpCallback,
                    deselectAllMFSCallback, ( XtPointer ) multi->fsb );
    
    /* Allow multiple selections */
    XtVaSetValues ( XmFileSelectionBoxGetChild ( multi->fsb,
					       XmDIALOG_FILE_LIST ),
		    XmNselectionPolicy, XmEXTENDED_SELECT,
		    NULL );

    DEBUG_GUI printf ( "Building Remove all button\n");
    multi->remove_all_button
        = XtVaCreateManagedWidget ( "Remove All", xmPushButtonWidgetClass,
				    multi->form,
				    XmNleftAttachment,  XmATTACH_WIDGET,
				    XmNleftWidget,      multi->fsb,
				    XmNleftOffset,      5,
				    XmNtopAttachment,   XmATTACH_FORM,
				    XmNtopOffset,       200,
				    NULL );

    XtAddCallback ( multi->remove_all_button, XmNactivateCallback,
		    removeAllMFSCallback, ( XtPointer ) multi );

    DEBUG_GUI printf ( "Building move file buttons\n");
    multi->move_up_button
        = XtVaCreateManagedWidget ( "up", xmArrowButtonWidgetClass,
				    multi->form,
				    XmNarrowDirection,  XmARROW_UP,
				    XmNtopAttachment,   XmATTACH_FORM,
				    XmNtopOffset,       150,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNrightOffset,     15,
                                    NULL );

    XtAddCallback ( multi->move_up_button, XmNactivateCallback,
		    moveFileUpMFSCallback, ( XtPointer ) multi );

    multi->move_down_button
        = XtVaCreateManagedWidget ( "up", xmArrowButtonWidgetClass,
				    multi->form,
				    XmNarrowDirection,  XmARROW_DOWN,
				    XmNtopAttachment,   XmATTACH_WIDGET,
				    XmNtopWidget,       multi->move_up_button,
				    XmNtopOffset,       15,
				    XmNrightAttachment, XmATTACH_FORM,
				    XmNrightOffset,     15,
                                    NULL );

    XtAddCallback ( multi->move_down_button, XmNactivateCallback,
		    moveFileDownMFSCallback, ( XtPointer ) multi );

    DEBUG_GUI printf ( "Building help button\n");
    multi->help_button
        = XtVaCreateManagedWidget ( "  Help  ", xmPushButtonWidgetClass,
				    multi->form,
				    XmNrightAttachment,  XmATTACH_WIDGET,
				    XmNrightWidget,   multi->move_down_button,
				    XmNrightOffset,      15,
				    XmNbottomAttachment, XmATTACH_FORM,
	   			    XmNbottomOffset,     20,
				    XmNheight,           30,
				    NULL );      

    XtAddCallback ( multi->help_button, XmNactivateCallback,
		    helpButtonMFSCallback, ( XtPointer ) multi );

    DEBUG_GUI printf ( "Building apply button\n");
    multi->apply_button
        = XtVaCreateManagedWidget ( " Apply ", xmPushButtonWidgetClass,
				    multi->form,
				    XmNrightAttachment,  XmATTACH_WIDGET,
				    XmNrightWidget,      multi->help_button,
				    XmNrightOffset,      10,
				    XmNbottomAttachment, XmATTACH_FORM,
	   			    XmNbottomOffset,     20,
				    XmNheight,           30,
				    NULL );

    XtAddCallback ( multi->apply_button, XmNactivateCallback,
		    applyMFSCallback, ( XtPointer ) multi );

    DEBUG_GUI printf ( "Building cancel button\n");
    multi->cancel_button
        = XtVaCreateManagedWidget ( " Cancel ", xmPushButtonWidgetClass,
				    multi->form,
				    XmNrightAttachment,  XmATTACH_WIDGET,
				    XmNrightWidget,      multi->apply_button,
				    XmNrightOffset,      10,
				    XmNbottomAttachment, XmATTACH_FORM,
	   			    XmNbottomOffset,     20,
				    XmNheight,           30,
				    NULL );   

    XtAddCallback ( multi->cancel_button, XmNactivateCallback,
		    cancelMFSCallback, ( XtPointer ) multi );

    DEBUG_GUI printf ( "Building separator\n");
    multi->separator
        = XtVaCreateManagedWidget ( "separator", xmSeparatorWidgetClass,
				    multi->form,
				    XmNrightAttachment,  XmATTACH_FORM,
				    XmNrightOffset,      5,
				    XmNleftAttachment,   XmATTACH_WIDGET,
				    XmNleftWidget,       multi->fsb,
				    XmNbottomAttachment, XmATTACH_WIDGET,
				    XmNbottomWidget,     multi->apply_button,
				    XmNbottomOffset,     16,
				    NULL );

    DEBUG_GUI printf ( "Building files to load label\n");
    multi->list_label
        = XtVaCreateManagedWidget ( "Files To Load", xmLabelWidgetClass,
				    multi->form,
				    XmNleftAttachment,  XmATTACH_WIDGET,
				    XmNleftWidget,   multi->remove_all_button,
				    XmNleftOffset,      15,
				    XmNtopAttachment,   XmATTACH_FORM,
				    XmNtopOffset,       13,
				    NULL );

    multi->invisible_text
        = XtVaCreateManagedWidget ( "invisible", xmTextWidgetClass,
				    multi->form,
				    XmNleftAttachment,  XmATTACH_WIDGET,
				    XmNleftWidget,   multi->remove_all_button,
				    XmNleftOffset,      15,
				    XmNrightAttachment, XmATTACH_WIDGET,
				    XmNrightWidget,     multi->move_up_button,
				    XmNrightOffset,     15,
				    XmNtopAttachment,   XmATTACH_WIDGET,
				    XmNtopWidget,       multi->list_label,
                                    XmNtopOffset,       0,
				    XmNheight,          1,
				    NULL );
    
    DEBUG_GUI printf ( "Building list window\n");
    ac=0;
    XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmSTATIC); ac++;    
    multi->list_window
        = XmCreateScrolledWindow  ( multi->form, "list_window", al, ac );

    XtVaSetValues                 ( multi->list_window,
				    XmNshadowThickness, 0,
				    XmNleftAttachment,  XmATTACH_WIDGET,
				    XmNleftWidget,   multi->remove_all_button,
				    XmNleftOffset,      15,
				    XmNrightAttachment, XmATTACH_WIDGET,
				    XmNrightWidget,     multi->move_up_button,
				    XmNrightOffset,     15,
				    XmNtopAttachment,   XmATTACH_WIDGET,
				    XmNtopWidget,       multi->list_label,
				    XmNtopOffset,       2,
				    XmNbottomAttachment, XmATTACH_WIDGET,
				    XmNbottomWidget,    multi->separator,
				    XmNbottomOffset,    10,
				    NULL );

    DEBUG_GUI printf ( "Building file list\n");
    multi->file_list
        = XtVaCreateManagedWidget ( "file_list", xmListWidgetClass,
				    multi->list_window,
				    XmNselectionPolicy,  XmEXTENDED_SELECT,
				    XmNvisibleItemCount, 14,
				    XmNwidth,            195,
				    NULL );

    XtManageChild ( multi->list_window );


    DEBUG_GUI printf ( "Building remove button\n");
    multi->remove_button
        = XtVaCreateManagedWidget ( "Remove", xmPushButtonWidgetClass,
				    multi->form,
				    XmNleftAttachment,  XmATTACH_WIDGET,
				    XmNleftWidget,      multi->fsb,
				    XmNleftOffset,      5,
				    XmNrightAttachment, XmATTACH_WIDGET,
				    XmNrightWidget,     multi->list_window,
				    XmNrightOffset,     15,
				    XmNbottomAttachment, XmATTACH_WIDGET,
				    XmNbottomWidget, multi->remove_all_button,
				    XmNbottomOffset,       10,
				    NULL );

    XtAddCallback ( multi->remove_button, XmNactivateCallback,
		    removeMFSCallback, ( XtPointer ) multi );

    DEBUG_GUI printf ( "Building >>> button\n");
    multi->add_button
        = XtVaCreateManagedWidget ( " >>> ", xmPushButtonWidgetClass,
				    multi->form,
				    XmNleftAttachment,   XmATTACH_WIDGET,
				    XmNleftWidget,       multi->fsb,
				    XmNleftOffset,       5,
				    XmNrightAttachment,  XmATTACH_WIDGET,
				    XmNrightWidget,      multi->list_window,
				    XmNrightOffset,      15,
				    XmNbottomAttachment, XmATTACH_WIDGET,
				    XmNbottomWidget,     multi->remove_button,
				    XmNbottomOffset,     10,
				    NULL );

    XtAddCallback ( multi->add_button, XmNactivateCallback,
		    addMFSCallback, ( XtPointer ) multi );

    DEBUG_TRACE_OUT printf ( "Leaving multi_file_select\n" );
}


/* Callback for the select all button in the file selection box */
void selectAllMFSCallback ( Widget w, XtPointer clientData, 
			    XtPointer callData )
{
    Widget     fsb = ( Widget ) clientData;
    Widget     list;
    int        i, num_files, num_selected_files;
    int        j = 0;
    int        *selected_file;
    Boolean temp;

    DEBUG_TRACE_IN printf ( "Entering selectAllCallback\n" );

    list = XmFileSelectionBoxGetChild ( fsb, XmDIALOG_FILE_LIST );

    XtVaGetValues ( list, XmNitemCount, &num_files, NULL );

    /* Temporlarily change list's selection policy */
    XtVaSetValues ( list, XmNselectionPolicy, XmMULTIPLE_SELECT, NULL );

    /* First deselect all items */
    XmListDeselectAllItems ( list );

    /* Then select all */
    for (i = 0; i < num_files; i++)
        XmListSelectPos ( list, i + 1, FALSE );

    /* Restore selection policy */
    XtVaSetValues ( list, XmNselectionPolicy, XmEXTENDED_SELECT, NULL );

    DEBUG_TRACE_OUT printf ( "Leaving selectAllCallback\n" );
} 


/* Callback for the deselect all button in the file selection box */
void deselectAllMFSCallback ( Widget w, XtPointer clientData, 
			      XtPointer callData )
{
    Widget fsb = ( Widget ) clientData;
    Widget list;
    XmString *filenames;
    int i, num_files;

    DEBUG_TRACE_IN printf ( "Entering deselectAllCallback\n" );

    list = XmFileSelectionBoxGetChild ( fsb, XmDIALOG_FILE_LIST );

    XmListDeselectAllItems( list );
    
    DEBUG_TRACE_OUT printf ( "Leaving deselectAllCallback\n" );
} 


/* Call back for the >>> button */
void addMFSCallback ( Widget w, XtPointer clientData, XtPointer callData )
{
    multi_select_type *multi = ( multi_select_type * ) clientData;
    XmString          *filenames, *to_filenames;
    Widget            fsb_list;
    int               i, j, num_files, to_num;
    Dimension         width, x;
    int               matching_file, columns;
    int               position, increment, page_inc, max, size;
    char              *string;
    Widget            scroller, work_window, clip_window;

    DEBUG_TRACE_IN printf ( "Entering addCallback\n" );

    fsb_list = XmFileSelectionBoxGetChild ( multi->fsb, XmDIALOG_FILE_LIST );

    XtVaGetValues ( fsb_list, XmNselectedItemCount, &num_files, NULL );   
    XtVaGetValues ( fsb_list, XmNselectedItems, &filenames, NULL );
    /*XtVaGetValues ( multi->file_list, XmNitemCount, &to_num, NULL );
    XtVaGetValues ( multi->file_list, XmNitems, &to_filenames, NULL );*/

    /* only insert filenames that aren't already inserted */
    for ( i = 0; i < num_files; i++ )
    {
        DEBUG_LOADING 
        {
            if ( XmStringGetLtoR ( filenames[i], XmSTRING_DEFAULT_CHARSET, 
				   &string ) )
	        printf ( "Attempting to add file: %s\n", string );
	}

        if ( !XmListItemExists ( multi->file_list, filenames[i] ) )
        {
            /* zero means just add to the end of the list   | */
            /*                                              V */ 
            XmListAddItem ( multi->file_list, filenames[i], 0 );
        }
    }

    /* Deselect all items in fsb */
    XmListDeselectAllItems( fsb_list );
    
    columns = get_list_window_width ( multi->file_list );
    if ( columns < 20 )
        columns = 20;
    
    XtVaGetValues ( multi->file_list, XmNitemCount, &to_num, NULL );

    if ( to_num == 0 )
    {
        XtVaSetValues ( multi->file_list,
			XmNwidth, 195,
			NULL );
    }

    if ( to_num < 14 )
        XtVaSetValues ( multi->file_list, XmNvisibleItemCount, 14, NULL );
    else
        XtVaSetValues ( multi->file_list, XmNvisibleItemCount, 
			to_num, NULL );

    XtVaSetValues ( multi->invisible_text, XmNcolumns, columns, NULL );
    XtVaSetValues ( multi->invisible_text, XmNheight,  1, NULL );

    XtVaGetValues ( multi->list_window,
		    XmNhorizontalScrollBar, &scroller,
		    NULL );
    XtVaGetValues ( scroller, XmNmaximum, &max, NULL );
    XmScrollBarGetValues ( scroller, &position, &size, &increment,
			   &page_inc );
			  
    /* Move scrollbar */
    XmScrollBarSetValues ( scroller, max-size, size, increment, page_inc,
			   TRUE );
			   
    DEBUG_TRACE_OUT printf ( "Leaving addCallback\n" );
}


/* Determine the number of columns to show in the list widget */
int get_list_window_width ( Widget list )
{
    XmString *xfiles;
    char     *file, *filename;
    int      num_files, i;
    int      shortest, longest, file_length, temp_length;

    DEBUG_TRACE_IN printf ( "Entering get_list_window_width\n" );

    XtVaGetValues ( list, 
		    XmNitems, &xfiles,
		    XmNitemCount, &num_files,
                    NULL );

    if ( num_files > 0 )
    {
        if ( !(XmStringGetLtoR ( xfiles[0], XmFONTLIST_DEFAULT_TAG, &file ) ) )
        {
            DEBUG_TRACE_OUT printf ( "Leaving get_list_window_width\n" );
	    return ( 0 );
	}
    }
    else
    {
        DEBUG_TRACE_OUT printf ( "Leaving get_list_window_width\n" );
	return ( 0 );
    }

    shortest = longest = strlen ( file );

    for ( i = 0; i < num_files; i++ )
    {
        if ( !(XmStringGetLtoR ( xfiles[i], XmFONTLIST_DEFAULT_TAG, &file ) ) )
	{
	    DEBUG_TRACE_OUT printf ( "Leaving get_list_window_width\n" );
	    return ( 0 );
	}

        temp_length = strlen ( file );  
      
        if ( temp_length > longest )
            longest = temp_length;
        else if ( temp_length <= shortest )
	{
            shortest = temp_length;
            filename = strrchr ( file, '/' );
            file_length = strlen ( filename ) - 1;
	}
    }

    /* Subtract the 2 to try to get the columns to work correctly */
    DEBUG_TRACE_OUT printf ( "Leaving get_list_window_width\n" );
    return ( longest - shortest + file_length - 2 );
}

/* Callback for the apply button */
void applyMFSCallback ( Widget w, XtPointer clientData, XtPointer callData )
{
    multi_select_type *multi = ( multi_select_type * ) clientData;
    XmString          *filenames;
    int               i;
    static int        file_memory_allocated = 0;
    static int        last_number_of_files = 0;

    DEBUG_TRACE_IN printf ( "Entering applyMFSCallback\n" );

    multi->got_files = 1;

    XtVaGetValues ( multi->file_list, XmNitemCount, &multi->num_files, NULL );
    XtVaGetValues ( multi->file_list, XmNitems, &filenames, NULL );

    /* Free previously allocated memory if neccessary */
    if ( file_memory_allocated )
    {
        for ( i = 0; i < last_number_of_files; i++ )
	    XtFree ( (char *) multi->files[i] );

        MT_free ( (void *) multi->files );
    }
    
    /* Allocate memory for array of character arrays */
    multi->files = ( char ** ) MT_calloc ( multi->num_files, 
					   sizeof ( char * ) );

    DEBUG_LOADING printf ("Returning with the following files:\n" );

    /* Convert XmStrings to character arrays */
    for ( i = 0; i < multi->num_files; i++ )
    {
        if ( !XmStringGetLtoR ( filenames[i], XmSTRING_DEFAULT_CHARSET, 
			        &multi->files[i] ) )
        {
	    printf ( "Unable to create filenames from XmStrings.\n" );
            multi->got_files = 0;
        }
        else DEBUG_LOADING printf ( "   %d. %s\n", i + 1, multi->files[i] );
    }

    if ( multi->num_files == 0 )
    {
        multi->got_files = 0;
        file_memory_allocated = 0;
    }
    else
        file_memory_allocated = 1;
    
    last_number_of_files = multi->num_files;
   
    XmListDeleteAllItems ( multi->file_list );

    XtPopdown ( multi->shell );

    multi->user_done = 1; 

    DEBUG_TRACE_OUT printf ( "Leaving applyMFSCallback\n" );
}


/* Callback for the cancel button */
void cancelMFSCallback ( Widget w, XtPointer clientData, XtPointer callData )
{
    multi_select_type *multi = ( multi_select_type * ) clientData;

    DEBUG_TRACE_IN printf ( "Entering cancelMFSCallback\n" );

    multi->got_files = 0;
    multi->user_done = 1;

    XmListDeleteAllItems ( multi->file_list );

    XtPopdown ( multi->shell );

    DEBUG_TRACE_OUT printf ( "Leaving cancelMFSCallback\n" );
}


/* Callback for the remove all button */
void removeAllMFSCallback ( Widget w, XtPointer clientData, 
			    XtPointer callData )
{
    multi_select_type * multi = ( multi_select_type * ) clientData;

    DEBUG_TRACE_IN printf ( "Entering removeAllCallback\n" );
    XmListDeleteAllItems ( multi->file_list );

    XtVaSetValues ( multi->file_list, XmNwidth, 195, NULL );
    XtVaSetValues ( multi->file_list, XmNvisibleItemCount, 14, NULL );
    XtVaSetValues ( multi->invisible_text, XmNcolumns, 20, NULL );
    XtVaSetValues ( multi->invisible_text, XmNheight, 1, NULL );    

    DEBUG_TRACE_OUT printf ( "Leaving removeAllCallback\n" );
}


/* Callback for the remove button */
void removeMFSCallback ( Widget w, XtPointer clientData, XtPointer callData )
{
    multi_select_type *multi = ( multi_select_type * ) clientData;
    XmString          *filenames;
    Widget            scroller;
    int               num_files, total_num, columns;
    int               size, max, increment, page_inc, position;
    int               num_selected;
    int               *positions_selected;
    int i;
    
    
    DEBUG_TRACE_IN printf ( "Entering removeCallback\n" );

    /* Get the selected positions and delete them */
    if( XmListGetSelectedPos( multi->file_list, &positions_selected, &num_selected ) )
    {
        /* Have to start at the end of the list and work backwards */
        /* or else we may try to delete a position that no longer exists */
        for( i = num_selected - 1; i >= 0; i-- )
            XmListDeletePos( multi->file_list, positions_selected[i] );
        
        XtFree( (char *) positions_selected );

        XtVaGetValues ( multi->file_list, XmNitemCount, &total_num, NULL );   

        columns = get_list_window_width ( multi->file_list );
        if ( columns < 20 )
            columns = 20;

        /* Set the width of the list if there are no files */
        if ( total_num == 0 )
            XtVaSetValues ( multi->file_list,
                            XmNwidth, 195,
                            NULL );
    
        /* Set the height of the list */
        if ( total_num < 14 )
            XtVaSetValues ( multi->file_list, XmNvisibleItemCount, 14, NULL );
        else
            XtVaSetValues ( multi->file_list, XmNvisibleItemCount, 
                            total_num, NULL );

        /* Set the width of the window by setting the number of columns 
         * in the invisible text box */
        XtVaSetValues ( multi->invisible_text, XmNcolumns, columns, NULL );
        XtVaSetValues ( multi->invisible_text, XmNheight, 1, NULL );

        /* Get the scrollbar and move it to show the filenames */
        XtVaGetValues ( multi->list_window,
                        XmNhorizontalScrollBar, &scroller,
                        NULL );
        XtVaGetValues ( scroller, XmNmaximum, &max, NULL );
        XmScrollBarGetValues ( scroller, &position, &size, &increment,
                               &page_inc );
			  
        /* Move scrollbar */
        XmScrollBarSetValues ( scroller, max-size, size, increment, page_inc,
                               TRUE );
    }
    
    DEBUG_TRACE_OUT printf ( "Leaving removeCallback\n" );    
}


/* Callback for the up button */
void moveFileUpMFSCallback ( Widget w, XtPointer clientData, 
			     XtPointer callData )
{
    multi_select_type *multi = ( multi_select_type * ) clientData;

    DEBUG_TRACE_IN printf ( "Entering moveFileUpCallback\n" );
    move_MFS_files ( multi, 1 );
    DEBUG_TRACE_OUT printf ( "Leaving moveFileUpCallback\n" );
}


/* Callback for the down button */
void moveFileDownMFSCallback ( Widget w, XtPointer clientData, 
			       XtPointer callData )
{
    multi_select_type *multi = ( multi_select_type * ) clientData;

    DEBUG_TRACE_IN printf ( "Entering moveFileDownCallback\n" );
    move_MFS_files ( multi, 0 );
    DEBUG_TRACE_OUT printf ( "Leaving moveFileDownCallback\n" );
}


/* Moves the selected files up or down */
void move_MFS_files ( multi_select_type *multi, int move_up )
{
    int               *file_location;
    int               i, pos, num_files, total_num;
    char              *string;
    XmString          *filenames;
    XmString          *files_to_move;
    Widget            fsb_list;

    DEBUG_TRACE_IN printf ( "Entering moveFiles\n" );

    fsb_list = XmFileSelectionBoxGetChild ( multi->fsb, XmDIALOG_FILE_LIST );

    /* Temporlarily change list's selection policy */
    XtVaSetValues ( multi->file_list, XmNselectionPolicy, 
		    XmMULTIPLE_SELECT, NULL );

    XtVaGetValues ( multi->file_list, XmNselectedItems, &filenames, NULL );
    XmListGetSelectedPos ( multi->file_list, &file_location, &num_files );
    XtVaGetValues ( multi->file_list, XmNitemCount, &total_num, NULL );

    files_to_move = ( XmString * ) MT_calloc ( num_files, sizeof ( XmString ) );

    for ( i = 0; i < num_files; i++ )
        files_to_move[i] = XmStringCopy ( filenames[i] );

    /* If there are some selected files */
    if ( num_files > 0 )
    {
        /* If moving up and not at the top */ 
        if ( move_up && (file_location[0] > 1) )
        {
	    for ( i = 0; i < num_files; i++ )
            {
	        pos = file_location[i];
		XmListDeleteItem ( multi->file_list, files_to_move[i] );
		XmListAddItem ( multi->file_list, files_to_move[i], pos - 1 );
	    }
	}
        /* IF moving down and not at the bottom */
	else if ( !move_up && (file_location[num_files - 1] < total_num) )
        {
	    for ( i = num_files - 1; i >= 0; i-- )
            {
	        pos = file_location[i];
		XmListDeleteItem ( multi->file_list, files_to_move[i] );
		XmListAddItem ( multi->file_list, files_to_move[i], pos + 1 );
	    }
	}
    }

    /* de-select ALL files first */
    XmListDeselectAllItems ( multi->file_list );

    /* re-select previous files */
    for ( i = 0; i < num_files; i++ )
    {
        DEBUG_LOADING
        {
            if ( XmStringGetLtoR ( files_to_move[i], XmSTRING_DEFAULT_CHARSET, 
				   &string ) )
	        printf ( "Attempting to select file: %s\n", string );
	}

        XmListSelectItem ( multi->file_list,
			   files_to_move[i], FALSE );
    }

    /* Restore selection policy */
    XtVaSetValues ( multi->file_list, XmNselectionPolicy, 
		    XmEXTENDED_SELECT, NULL );

    /* Free the memory */
    if ( files_to_move )
    {
        for ( i = 0; i < num_files; i++ )
            XmStringFree ( files_to_move[i] );

        MT_free ( (void *) files_to_move );
    }

    DEBUG_TRACE_OUT printf ( "Leaving moveFiles\n" );
}
 

/* Callback for help button */
void helpButtonMFSCallback ( Widget w, XtPointer clientData, 
			     XtPointer callData )
{
    multi_select_type *multi = ( multi_select_type * ) clientData;
    static int first_time = 1;

    DEBUG_TRACE_IN printf ( "Entering helpButtonMFSCallback\n" );
  
    if ( first_time )
    {
        make_MFS_help_window ( multi );
        first_time = 0;
    }

    XtPopup ( multi->help_shell, XtGrabNone );

    DEBUG_TRACE_OUT printf ( "Leaving helpButtonMFSCallback\n" );
}


/* Makes the help window */
void make_MFS_help_window ( multi_select_type *multi )
{
    DEBUG_TRACE_IN printf ( "Entering make_MFS_help_window\n" );

    DEBUG_GUI printf ( "Building help shell\n" );
    multi->help_shell
        = XtCreatePopupShell ( "Multiple File Selection Help", 
			       topLevelShellWidgetClass,
			       multi->shell, NULL, 0 );

    DEBUG_GUI printf ( "Building help form\n" );
    multi->help_form
        = XtVaCreateManagedWidget ( "help_form", xmFormWidgetClass,
				    multi->help_shell, NULL );

    DEBUG_GUI printf ( "Building help text box\n" );
    multi->help_text
        = XtVaCreateManagedWidget ( "help_text", xmTextWidgetClass,
				    multi->help_form,
				    XmNtopAttachment,     XmATTACH_FORM,
				    XmNrightAttachment,   XmATTACH_FORM,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNwidth,             725,
				    XmNheight,            475,
                                    XmNcursorPositionVisible, False,
                                    XmNeditable, False,
				    NULL );

    /* Fill in the text */
    add_MFS_help_text ( multi->help_text );

    DEBUG_GUI printf ( "Building help dismiss button\n" );
    multi->help_dismiss
        = XtVaCreateManagedWidget ( "Dismiss", xmPushButtonWidgetClass,
				    multi->help_form,
				    XmNtopAttachment,     XmATTACH_WIDGET,
				    XmNtopWidget,         multi->help_text,
				    XmNtopOffset,         10,
				    XmNrightAttachment,   XmATTACH_FORM,
				    XmNrightOffset,       320,
				    XmNleftAttachment,    XmATTACH_FORM,
				    XmNleftOffset,        320,
				    XmNbottomAttachment,  XmATTACH_FORM,
				    XmNbottomOffset,      10,
				    NULL );

    XtAddCallback ( multi->help_dismiss, XmNactivateCallback,
		    dismissHelpMFSCallback, ( XtPointer ) multi );

    DEBUG_TRACE_OUT printf ( "Leaving make_MFS_help_window\n" );
}


/* callback for the dismiss button in the help window */
void dismissHelpMFSCallback ( Widget w, XtPointer clientData, 
			      XtPointer callData )
{
    multi_select_type *multi = ( multi_select_type * ) clientData;

    DEBUG_TRACE_IN printf ( "Entering dismissHelpMFSCallback\n" );
    XtPopdown ( multi->help_shell );
    DEBUG_TRACE_OUT printf ( "Leaving dismissHelpMFSCallback\n" );
}
 

/* Adds the text to the text box in the help window */
void add_MFS_help_text ( Widget w )
{
    int pos;

    DEBUG_TRACE_IN printf ( "Entering add_MFS_help_text\n" );

    XmTextInsert ( w, 0, MFS_BUTTONS );

    pos = XmTextGetInsertionPosition ( w );
    XmTextInsert ( w, pos, FILTER );

    pos = XmTextGetInsertionPosition ( w );
    XmTextInsert ( w, pos, SELECT_ALL );

    pos = XmTextGetInsertionPosition ( w );
    XmTextInsert ( w, pos, DESELECT_ALL );

    pos = XmTextGetInsertionPosition ( w );
    XmTextInsert ( w, pos, ADD_FILES );

    pos = XmTextGetInsertionPosition ( w );
    XmTextInsert ( w, pos, REMOVE_FILES );

    pos = XmTextGetInsertionPosition ( w );
    XmTextInsert ( w, pos, CANCEL_BUTTON );

    pos = XmTextGetInsertionPosition ( w );
    XmTextInsert ( w, pos, APPLY_BUTTON );

    pos = XmTextGetInsertionPosition ( w );
    XmTextInsert ( w, pos, HELP_BUTTON );

    pos = XmTextGetInsertionPosition ( w );
    XmTextInsert ( w, pos, UP_ARROW );

    pos = XmTextGetInsertionPosition ( w );
    XmTextInsert ( w, pos, DOWN_ARROW );

    pos = XmTextGetInsertionPosition ( w );
    XmTextInsert ( w, pos, SELECTING_FILES );

    DEBUG_TRACE_OUT printf ( "Leaving add_MFS_help_text\n" );
}
