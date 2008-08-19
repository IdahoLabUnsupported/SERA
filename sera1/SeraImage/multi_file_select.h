/******************************************************************************
 * multi_file_select.h                                                        *
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
#ifndef MULTI_FILE_SELECT_H
#define MULTI_FILE_SELECT_H


#define MFS_BUTTONS "Multiple File Selection Buttons:\n\n"
#define FILTER "   Filter         - Apply the filter to files in a directory.\n\n"
#define SELECT_ALL "   Select All     - Select all the files in a directory.\n\n"
#define DESELECT_ALL "   Deselect All   - Deselect all the files in a directory.\n\n"
#define ADD_FILES "   >>>            - Add all the selected files to the load list.\n\n"
#define REMOVE_FILES "   Remove         - Remove selected files in the load list.\n\n"
#define REMOVE_ALL "   Remove All     - Remove all the files in the load list.\n\n"
#define CANCEL_BUTTON "   Cancel         - Return without loading any files.\n\n"
#define APPLY_BUTTON "   Apply          - Return to load the files in the load list.\n\n"
#define HELP_BUTTON "   Help           - Get this window.\n\n"
#define UP_ARROW "   Up Arrow       - Move selected files up in the load list.\n\n"
#define DOWN_ARROW "   Down Arrow     - Move selected files down in the load list.\n\n"
#define SELECTING_FILES "Selecting Files:  Use the mouse to click on files you want to select.\n                  Hold <Shift> and click to select adjacent files.\n                  Hold <Ctrl> and click to select multiple files."


typedef struct _multi_select_type
{
    Widget shell;              /* The main popup shell                      */
    Widget form;               /* Manager widget                            */
    Widget fsb;                /* The File Selection Box                    */
    Widget add_button;         /* Add one or more files to file_list        */
    Widget remove_button;      /* Remove on or more files from file_list    */
    Widget remove_all_button;  /* Remove all files from file_list           */
    Widget list_label;         /* Label for the list                        */
    Widget invisible_text;     /* Used to resize list window                */
    Widget list_window;        /* Window for the file list                  */
    Widget file_list;          /* List widget containing selected files     */
    Widget move_up_button;     /* Move a file up in the list                */
    Widget move_down_button;   /* Move a file down in the list              */
    Widget separator;          /* Separates last two buttons from rest      */
    Widget apply_button;       /* Return the files to the calling program   */
    Widget cancel_button;      /* Return without any files                  */

    Widget help_button;        /* Help for multiple file selection          */
    Widget help_shell;         /* Help popup shell                          */
    Widget help_form;          /* Form widget for help popup                */
    Widget help_text;          /* Text box for directions                   */
    Widget help_dismiss;       /* Dismiss button                            */

    char   **files;            /* Actual list of filenames                  */
    int    num_files;          /* Number of files being returned            */
    int    got_files;          /* 1 if returning with files, 0 if cancelled */

    int    user_done;          /* Indicates when user is finished           */
} multi_select_type;

/* Function prototypes */
int  get_multiple_files      ( Widget, XtAppContext, multi_select_type * );
void build_MFS_gui           ( Widget, multi_select_type * );
void move_MFS_files          ( multi_select_type *, int );
void make_MFS_help_window    ( multi_select_type * );
void add_MFS_help_text       ( Widget );
int get_list_window_width    ( Widget );


/* Callbacks */
void applyMFSCallback        ( Widget, XtPointer, XtPointer );
void cancelMFSCallback       ( Widget, XtPointer, XtPointer );
void selectAllMFSCallback    ( Widget, XtPointer, XtPointer );
void deselectAllMFSCallback  ( Widget, XtPointer, XtPointer );
void removeMFSCallback       ( Widget, XtPointer, XtPointer );
void removeAllMFSCallback    ( Widget, XtPointer, XtPointer );
void addMFSCallback          ( Widget, XtPointer, XtPointer );
void moveFileUpMFSCallback   ( Widget, XtPointer, XtPointer );
void moveFileDownMFSCallback ( Widget, XtPointer, XtPointer );
void helpButtonMFSCallback   ( Widget, XtPointer, XtPointer );
void dismissHelpMFSCallback  ( Widget, XtPointer, XtPointer );


#endif


