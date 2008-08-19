/* rtt_mc.c - A collection of functions to support the rtt_mc interface
 *            popup control.  This file includes:
 *  create_rtt_run_button : A function to create the run text selection
 *  rtt_runCB : A program to run the rtt_mc code
 *  
 *  by: Ray S. Babcock, Montana State University
 *      November 30, 1993
 */

#include "rtt.h"
#include "rtt_mc.h"
#include "connection_tools.h"
#include "file_tools.h"
#include <errno.h>
#include <sys/types.h>

#include "libhelp.h"

/* External definitions for version checking */
extern void build_check_version_shell( main_checker_t * check );
extern void get_recent_version       ( main_checker_t * check );


/* External definitions for input/output */
extern void rtt_input();
extern int rtt_output();

/* added 2-1-99 mbr */
static void rtt_CheckVersionCB( Widget w, XtPointer clientData, XtPointer callData );
void MC_check_version ( Widget w, char *module, char *version );


/* Shell to display the entire text value when button 3 is pressed */
static Widget popupShell = NULL;

/*---------------------------------------------------------run.c-----*/
/* runs the rtt_mc application with the user selected filenames.     */
/*-------------------------------------------------------------------*/



/* rtt_mc function called from menu              
 *
 * October, 1993, Ray Babcock
 */

void
rtt_mc()
{
    DEBUG_TRACE_IN printf("Entering rtt_mc\n");
    XtPopup(rtt_file_data->rtt_shell, XtGrabNone);
    DEBUG_TRACE_OUT printf("Leaving rtt_mc\n");
    
} /* end rtt_mc */


void RttSelectCB(Widget w, char * fname, 
                 XmFileSelectionBoxCallbackStruct *cbs)
{
    char *s;

    DEBUG_TRACE_IN printf("Entering RttSelectCB\n");
    
    /* the input variable fname is fname_in, imp_input, fname_out, or fname_del;
     */
    XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, &s);
    strcpy(fname,s);

    CONFIRM = 0; /* we got a file */
    XtUnmanageChild(w);
    XtFree( (char *) s );

    DEBUG_TRACE_OUT printf("Leaving RttSelectCB\n");
}

void RttCancelCB(Widget w, char * fname, 
                 XmFileSelectionBoxCallbackStruct *cbs)
{
    DEBUG_TRACE_IN printf("Entering RttCancelCB\n");
    
    CONFIRM = 2; /* didn't get a file Cancel was pressed */
    XtUnmanageChild(w);

    DEBUG_TRACE_OUT printf("Leaving RttCancelCB\n");
}


void rtt_KillCB(Widget w,
                char     *data,
                XmAnyCallbackStruct *call_data)

{
    FILE *fptr, *jptr;
    char saveDirectory[256];
    char rmRunInputCommand[256];
    char seraMonPath[256];
    char seraJobsPath[256];
    char *buffer;
    char s1[10], s2[10], s3[10], s4[10];
    int  num, pid;
    

    DEBUG_TRACE_IN printf("Entering rtt_KillCB\n");
    
    if( rtt_file_data->rttRunInProgress )
    {
        XtVaSetValues(rtt_run_widgets->RunW, XmNsensitive, TRUE, NULL);
        XtVaSetValues(rtt_run_widgets->TestW, XmNsensitive, TRUE, NULL);

        /* First get the save directory */
        strcpy( saveDirectory, rtt_file_data->saveDirectory );

/* Execute ps command on system to get process IDs
 * Then, read this file to find the process ID to kill,
 * and submit kill command
 */
        sprintf( seraJobsPath, "%s%s", saveDirectory, "sera.jobs" );
        sprintf( rmRunInputCommand, "ps > %s", seraJobsPath );
        system ( rmRunInputCommand );

        jptr = fopen ( seraJobsPath, "r" );
        pid = 0;
        num = fscanf(jptr, "%s %s %s %s\n", s1, s2, s3, s4);
        num = fscanf(jptr, "%s %s %s %s\n", s1, s2, s3, s4);
        while ( (num != EOF) && (pid == 0) ) {
           if ( strncmp(s4, "seraMC", 6) == NULL ) {
              sscanf ( s1, "%d", &pid );
           }
           else {
              num = fscanf(jptr, "%s %s %s %s\n", s1, s2, s3, s4);
           }
        }
        fclose (jptr);

        sprintf( rmRunInputCommand, "kill -9 %d", pid );
        system ( rmRunInputCommand );
 
        sprintf( rmRunInputCommand, "rm %s", seraJobsPath );
        system ( rmRunInputCommand );

        /* Build the command to delete RUN.INPUT */
        sprintf( rmRunInputCommand, "rm tmp.* %s%s", saveDirectory, "RUN.INPUT" );
        system( rmRunInputCommand );

        XtRemoveTimeOut(rtt_file_data->interval_id);
        XtVaSetValues(rtt_file_data->rtt_status_text,
                      XmNlabelString, XmStringCreateLtoR("KILLED",XmSTRING_DEFAULT_CHARSET),
                      NULL);

        /* Build the full path to the sera.mon file */
        sprintf( seraMonPath, "%s%s", saveDirectory, "sera.mon" );
        
        if ( (fptr = fopen( seraMonPath, "r+") ) == NULL)
        {
            fprintf(stderr, "No sera.mon file to kill.\n");
            DEBUG_TRACE_OUT printf("Leaving rtt_KillCB\n");
            return;
        }

        fprintf(fptr, "KILLED\n");
        fclose(fptr);

        rtt_file_data->rttRunInProgress = 0;
    }

    DEBUG_TRACE_OUT printf("Leaving rtt_KillCB\n");
} /* end of kill.c */


void rtt_selectCB(Widget w,
                  char          *data,
                  XmAnyCallbackStruct *call_data)
{
    char                         *s;
    XmSelectionBoxCallbackStruct *selection;
    
    char * ptr;
    int whichButton;       /* which text box to update */
    char storeString[256]; /* what gets put in the text box */
    char copy[256];        /* copy of what we got from the FSB */
    int valid = 0;
    
    DEBUG_TRACE_IN printf("Entering rtt_selectCB\n");
    
    selection=(XmSelectionBoxCallbackStruct *) call_data;
    
    /*
     * extract file name string from widget dialog box
     */
    if( XmStringGetLtoR(selection->value, XmSTRING_DEFAULT_CHARSET, &s) )
    {
        if( strlen( s ) > 0 )
        {
            strcpy( copy, s ); /* get a copy   */
            XtFree( s );       /* free the old */

            whichButton = rtt_file_data->buttonNumber;
            /*
             * Slightly different things must be done based on
             * the button number.
             */

            switch( whichButton )
            {
                    /* Save Directory */
                case 0:
                    
                    if( isADirectory( copy ) )
                    {
                        strcpy( storeString, copy );
                        valid = 1;

                        strcpy( rtt_file_data->saveDirectory, storeString );
                        rtt_file_data->saveDirectoryValid = 1;
                    }
                    else
                        DT_error( rtt_file_data->rtt_shell, "You must select a directory.", "File Error", NULL );
                    break;

                    /* CG Geom File */ 
                case 1:

                    if( FT_filenameEndsIn( copy, ".geom" ) && FT_fileExists( copy ) )
                    {
                        strcpy( storeString, copy );
                        valid = 1;
                    }
                    else
                        DT_error( rtt_file_data->rtt_shell, "You must select an existing .geom file.", "File Error", NULL );
                    break;

                    /* Patient Geom File */
                case 2:

                    if( FT_fileExists( copy ) && ( FT_filenameEndsIn(copy, ".uv") || FT_filenameEndsIn(copy, ".uv.gz") ) ) 
                    {
                        strcpy( storeString, copy );
                        valid = 1;
                    }
                    else
                        DT_error( rtt_file_data->rtt_shell, "You must select an existing .uv file.", "File Error", NULL );
                    break;

                    /* Old RST File */
                case 3:

                    if( strstr( copy, ".rst" ) && FT_fileExists( copy ) )
                    {
                        strcpy( storeString, copy );
                        valid = 1;
                    }
                    else
                        DT_error( rtt_file_data->rtt_shell, "You must select an existing .rst file.", "File Error", NULL );
                    break;
                    
                    /* New RST File */
                case 4:

                    if( FT_filenameEndsIn( copy, ".rst" ) )
                    {
                        /* Just store the filename, append it to the save directory later */
                        ptr = strrchr( copy, '/' );
                        if( ptr != NULL )
                            ptr++;
                        else
                            ptr = &copy[0];
                        
                        strcpy( storeString, ptr );
                        valid = 1;
                    }
                    else
                        DT_error( rtt_file_data->rtt_shell, "You must specify a .rst file.", "File Error", NULL );
                    break;

                    /* Rest of the cases, just make sure the files exist */
                case 5:
                case 6:
                case 7:
                case 8:

                    if( FT_fileExists( copy ) )
                    {
                        strcpy( storeString, copy );
                        valid = 1;
                    }
                    else
                        DT_error( rtt_file_data->rtt_shell, "That file does not exist.", "File Error", NULL );
                    break;

                default:
                    break;
            }
    
            if( valid )
            {
                /*
                 * copy select string into filename
                 */
                strcpy(rtt_file_data->stringName[ whichButton ], storeString);
                
                /*
                 * put selected file back into text widget
                 * and unmanage file selection widget
                 */
                XmTextSetString(rtt_file_data->textinfo[ whichButton ],
                                rtt_file_data->stringName[ whichButton ] );
                XtUnmanageChild(w);
            }
        }
    }
    else
        DT_error( rtt_file_data->rtt_shell, "Error retrieving filename.", NULL, NULL );

    DEBUG_TRACE_OUT printf("Leaving rtt_selectCB\n");
}

void
rtt_cancelCB (Widget              w,
              char           *data,
              XmAnyCallbackStruct *call_data )
{
    DEBUG_TRACE_IN printf("Entering rtt_cancelCB\n");
    XtUnmanageChild(w);
    DEBUG_TRACE_OUT printf("Leaving rtt_cancelCB\n");
}

void
rtt_RunCB(Widget              w,
          char           *data,
          XmAnyCallbackStruct *call_data)
{
    char  buffer[256];
    char  inputFilename[256];
    char  * ptr;
    int i;
    char newRstFilename[256];
    char fileToDelete[256];
    char fileToCopy[256];

    DEBUG_TRACE_IN printf("Entering rtt_RunCB\n");

    if( checkRttFiles( ) == 1 ) /* all files are valid */
    {
        /*
         *  Before doing anything else, check to see if the input file information has
         *  been previously saved to a file;  if so, save again;  if not, error
         */
        rtt_file_data->ntrk = 0;
        if ( rtt_file_data->originalInputFileValid ) {
            if ( !rtt_output() ) {
               DEBUG_TRACE_OUT printf("Leaving rtt_RunCB\n");
               return;
            }
        }
        else
        {
            DT_error ( rtt_file_data->rtt_shell,
                       "No Save file has been previously defined.\nPlease use Save As before attempting Run again.",
                       "Save error", NULL );
            DEBUG_TRACE_OUT printf("Leaving rtt_RunCB\n");
            return;
        }

        /*
         * Create the filename that seraMC is looking for.
         * In other words strip off the .input suffix.
         */
        sprintf ( fileToCopy, "cp %s %s", rtt_file_data->originalInputFile,
                                      rtt_file_data->saveDirectory );
        system ( fileToCopy );
        ptr = strrchr ( rtt_file_data->originalInputFile, '/' );
        strcpy( inputFilename, ptr+1 );
        if( FT_filenameEndsIn( inputFilename, ".input" ) )
        {
            ptr = strstr( inputFilename, ".input" );
            if( ptr != NULL )
                *ptr = '\0';

            /* generate command */
            ptr = XmTextGetString(rtt_file_data->textinfo[8]);
            if ( strstr(ptr,"run_seraMC") == ptr )
            {
                sprintf(buffer,"$SERA_HOME/Target/bin/%s %s %s &", 
                        XmTextGetString(rtt_file_data->textinfo[8]),
                        inputFilename,
                        rtt_file_data->saveDirectory);
            }
            else
            {
                sprintf(buffer,"%s %s %s &", 
                        XmTextGetString(rtt_file_data->textinfo[8]),
                        inputFilename,
                        rtt_file_data->saveDirectory);

            }

            rtt_file_data->interval_id = XtAppAddTimeOut(rtt_app, 
                                                         RTT_RUN_TIMEOUT, read_and_display, NULL);

            /* seraMC will crash if a specified new .rst file already exists */
            /* So, delete the file before we begin */
            (void) getValueFromTextBox( rtt_file_data->textinfo[4], newRstFilename, NO_EXPAND );
            if( strlen( newRstFilename ) > 0 )
            {
                sprintf( fileToDelete, "%s%s", rtt_file_data->saveDirectory, newRstFilename );
                FT_deleteFile( fileToDelete );
            }

            /* Delete any previous view.* files */
            sprintf( fileToDelete, "rm -f %sview.*", rtt_file_data->saveDirectory );
            system( fileToDelete );
            
            /*printf ( "Command is %s\n", buffer );*/
            system(buffer);

            if( viewGui->displayViewWindow )
            {
                /*
                 * The views will get displayed at this point. If the view window
                 * is already up destroy the old ximages first.
                 */
                if( XtIsManaged( viewGui->shell ) )
                    destroyViewXImages( );
                
                /* Add a timeout to look for the view.* files from seraMC */
                viewGui->viewIntervalId = XtAppAddTimeOut( rtt_app, VIEW_TIMEOUT,
                                                           lookForViewFiles, NULL );
            }
            else
            {
                /*
                 * The views won't get displayed, if the view window is currently up
                 * destroy the ximages and close the window.
                 */
                if( XtIsManaged( viewGui->shell ) )
                {
                    XtUnmanageChild( viewGui->shell );
                    destroyViewXImages( );
                }
            }
            
            
            XtVaSetValues(rtt_run_widgets->RunW, XmNsensitive, False, NULL);
            XtVaSetValues(rtt_run_widgets->TestW, XmNsensitive, False, NULL);

            /* Now we're running */
            rtt_file_data->rttRunInProgress = 1;
        }
        else
            DT_error( rtt_file_data->rtt_shell, "A valid .input file could not be found.", NULL, NULL );
    
    }
    
    DEBUG_TRACE_OUT printf("Leaving rtt_RunCB\n");
}

void
rtt_TestCB(Widget              w,
           char           *data,
           XmAnyCallbackStruct *call_data)
{
    char  buffer[256];
    char  inputFilename[256];
    char  * ptr;
    int i;
    char newRstFilename[256];
    char fileToDelete[256];
    char fileToCopy[256];

    DEBUG_TRACE_IN printf("Entering rtt_TestCB\n");
    
    if( checkRttFiles( ) == 1 ) /* all files are valid */
    {
        /*
         *  Before doing anything else, check to see if the input file information has
         *  been previously saved to a file;  if so, save again;  if not, error
         */
        rtt_file_data->ntrk = 1;
        if ( rtt_file_data->originalInputFileValid ) {
            if ( !rtt_output() ) {
               DEBUG_TRACE_OUT printf("Leaving rtt_TestCB\n");
               return;
            }
        }
        else
        {
            DT_error ( rtt_file_data->rtt_shell,
                       "No Save file has been previously defined.\nPlease use Save As before attempting Run again.",
                       "Save error", NULL );
            DEBUG_TRACE_OUT printf("Leaving rtt_TestCB\n");
            return;
        }

        /*
         * Create the filename that seraMC is looking for.
         * In other words strip off the .input suffix.
         */
        sprintf ( fileToCopy, "cp %s %s", rtt_file_data->originalInputFile,
                                      rtt_file_data->saveDirectory );
        system ( fileToCopy );
        ptr = strrchr ( rtt_file_data->originalInputFile, '/' );
        strcpy( inputFilename, ptr+1 );
        if( FT_filenameEndsIn( inputFilename, ".input" ) )
        {
            ptr = strstr( inputFilename, ".input" );
            if( ptr != NULL )
                *ptr = '\0';

            /* generate command */
            ptr = XmTextGetString(rtt_file_data->textinfo[8]);
            if ( strstr(ptr,"run_seraMC") == ptr )
            {
                sprintf(buffer,"$SERA_HOME/Target/bin/%s %s %s &", 
                        XmTextGetString(rtt_file_data->textinfo[8]),
                        inputFilename,
                        rtt_file_data->saveDirectory);
            }
            else
            {
                sprintf(buffer,"%s %s %s &", 
                        XmTextGetString(rtt_file_data->textinfo[8]),
                        inputFilename,
                        rtt_file_data->saveDirectory);

            }

            rtt_file_data->interval_id = XtAppAddTimeOut(rtt_app, 
                                                         RTT_RUN_TIMEOUT, read_and_display, NULL);

            /* seraMC will crash if a specified new .rst file already exists */
            /* So, delete the file before we begin */
            (void) getValueFromTextBox( rtt_file_data->textinfo[4], newRstFilename, NO_EXPAND );
            if( strlen( newRstFilename ) > 0 )
            {
                sprintf( fileToDelete, "%s%s", rtt_file_data->saveDirectory, newRstFilename );
                FT_deleteFile( fileToDelete );
            }

            /* Delete any previous view.* files */
            sprintf( fileToDelete, "rm -f %sview.*", rtt_file_data->saveDirectory );
            system( fileToDelete );            
            
            /*printf ( "Command is %s\n", buffer );*/
            system(buffer);

            if( viewGui->displayViewWindow )
            {
                /*
                 * The views will get displayed at this point. If the view window
                 * is already up destroy the old ximages first.
                 */
                if( XtIsManaged( viewGui->shell ) )
                    destroyViewXImages( );
                
                /* Add a timeout to look for the view.* files from seraMC */
                viewGui->viewIntervalId = XtAppAddTimeOut( rtt_app, VIEW_TIMEOUT,
                                                           lookForViewFiles, NULL );
            }
            else
            {
                /*
                 * The views won't get displayed, if the view window is currently up
                 * destroy the ximages and close the window.
                 */
                if( XtIsManaged( viewGui->shell ) )
                {
                    XtUnmanageChild( viewGui->shell );
                    destroyViewXImages( );
                }
            }            
            
        
            XtVaSetValues(rtt_run_widgets->RunW, XmNsensitive, False, NULL);
            XtVaSetValues(rtt_run_widgets->TestW, XmNsensitive, False, NULL);

            /* Now we're running */
            rtt_file_data->rttRunInProgress = 1;
        }
        else
            DT_error( rtt_file_data->rtt_shell, "A valid .input file could not be found.", NULL, NULL );
    
    }

    DEBUG_TRACE_OUT printf("Leaving rtt_TestCB\n");
}


void 
rtt_OpenCB(Widget              w,
           char           *data,
           XmAnyCallbackStruct *call_data)
{
    DEBUG_TRACE_IN printf("Entering rtt_OpenCB\n");
    rtt_input();
    DEBUG_TRACE_OUT printf("Leaving rtt_OpenCB\n");
}

void 
rtt_SaveCB(Widget              w,
           char           *data,
           XmAnyCallbackStruct *call_data)
{

    DEBUG_TRACE_IN printf("Entering rtt_SaveCB\n");
    
    if ( rtt_file_data->originalInputFileValid )
    {
        if ( !rtt_output() ) {
           DEBUG_TRACE_OUT printf("Leaving rtt_SaveCB\n");
           return;
        }
    }
    else
        DT_error ( rtt_file_data->rtt_shell, "No Save file has been specified - use Save As",
                   "Save error", NULL );

    DEBUG_TRACE_OUT printf("Leaving rtt_SaveCB\n");
}

void 
rtt_SaveAsCB(Widget              w,
             char           *data,
             XmAnyCallbackStruct *call_data)
{
    char * dot_location;
    char * slashLocation;
    char saveWarning[256];
    int  okToSave = 1;
    int    i;

    DEBUG_TRACE_IN printf("Entering rtt_SaveAsCB\n");
    
    /* Get file name with file selection popup */
    CONFIRM = 1;
    XtManageChild(rtt_file_data->rtt_file_select);

    while (CONFIRM == 1)
        XtAppProcessEvent(rtt_app, XtIMAll);

    if( CONFIRM == 0 ) /* FSB closed with a file selected */
    {
        if( FT_filenameEndsIn( rtt_file_data->fname_in, ".input" ) )
        {
            /* See if the file exists first */
            if( FT_fileExists( rtt_file_data->fname_in ) )
            {
                sprintf( saveWarning, "Ok to overwrite file %s?", rtt_file_data->fname_in );
                if( DT_decide( rtt_file_data->rtt_shell, rtt_app, saveWarning, "Overwrite?", "Yes", "No" ) == 0 )
                    okToSave = 0;
            }

            if( okToSave )
            {
                slashLocation = strrchr( rtt_file_data->fname_in, '/' );

                if( slashLocation != NULL )
                {
                    /* Store this filename as the original for subsequent saves */
                    strcpy( rtt_file_data->originalInputFile, rtt_file_data->fname_in );
                    rtt_file_data->originalInputFileValid = 1;

                    /* output file to this file */
                    rtt_output();
                    if ( !rtt_output() ) {
                       DEBUG_TRACE_OUT printf("Leaving rtt_SaveAsCB\n");
                       return;
                    }
                }
                else
                    DT_error( rtt_file_data->rtt_shell, "That does not appear to be a valid file.", NULL, NULL );
            }
        }
        else
            DT_error( rtt_file_data->rtt_shell, "You must specify a .input file.", NULL, NULL );
    }

    DEBUG_TRACE_OUT printf("Leaving rtt_SaveAsCB\n");
}

void
rtt_ExitEditCB(Widget              w,
               char           *data,
               XmAnyCallbackStruct *call_data)
{
    DEBUG_TRACE_IN printf("Entering rtt_ExitEditCB\n");

    if( XtIsManaged( viewGui->shell ) )
        destroyViewXImages;

    MT_free( (void *) viewGui );
    MT_free( (void *) rtt_file_data );
    MT_free( (void *) rtt_run_widgets );
    
    /* print allocated memory */
    MT_onexit( );

    DEBUG_TRACE_OUT printf("Leaving rtt_ExitEditCB\n");
    
    XtAppSetExitFlag(rtt_app);
}

void 
rtt_edit_PreferencesCB(Widget              w,
                       char           *data,
                       XmAnyCallbackStruct *call_data)
{
}

/* rtt_directive_add_buttonCB */
void rtt_directive_add_buttonCB(Widget              w,
                                char           *data,
                                XmAnyCallbackStruct *call_data)
{
    DEBUG_TRACE_IN printf("Entering rtt_directive_add_buttonCB\n");
    XtPopup(rtt_editpu_popup, XtGrabNone);
    DEBUG_TRACE_OUT printf("Leaving rtt_directive_add_buttonCB\n");
}

/* rtt_directive_clear_buttonCB */
void rtt_directive_clear_buttonCB(Widget              w,
                                  char           *data,
                                  XmAnyCallbackStruct *call_data)
{
    int ac;
    Arg al[20];
    Widget text;
    XmString XmSlabel;
    char *label;

    DEBUG_TRACE_IN printf("Entering rtt_directive_clear_buttonCB\n");
    
    text = rtt_file_data->rtt_directive_text1;
    ac=0;
    XtSetArg(al[ac], XmNlabelString, &XmSlabel); ac++;
    XtGetValues(w, al, ac);
    label = (char *) MT_malloc(120*sizeof(char));
    XmStringGetLtoR(XmSlabel, XmSTRING_DEFAULT_CHARSET, &label);

    if(strncmp(label,"Clear",5)==0)
    {
        XmTextSetSelection(text, 0, XmTextGetLastPosition(text), 0);
        XmTextCut(text,(Time)0);
        ac = 0;
        XtSetArg(al[ac], XmNlabelString, XmStringCreateSimple("Restore")); ac++;
        XtSetValues(w, al, ac);
    } else {
        XmTextPaste(text);
        ac = 0;
        XtSetArg(al[ac], XmNlabelString, XmStringCreateSimple("Clear")); ac++;
        XtSetValues(w, al, ac);
    };
    
    MT_free((void *)label);

    DEBUG_TRACE_OUT printf("Leaving rtt_directive_clear_buttonCB\n");

}

void rtt_editdir_buttonCB(Widget              w,
                          char           *data,
                          XmAnyCallbackStruct *call_data)
{
    Widget w_id;
    int local_select;
    Arg al[20];
    int ac=0;

    DEBUG_TRACE_IN printf("Entering rtt_editdir_buttonCB\n");
    
    ac=0;
    XtSetArg(al[ac], XmNuserData, &local_select); ac++;
    XtGetValues(w, al, ac);

    if(local_select == -1) 
    {
        XtPopdown(rtt_editpu_popup);
    } else {
        w_id = rtt_file_data->rtt_directive_text1;

        XmTextInsert(w_id, XmTextGetLastPosition(w_id), rtt_opt[local_select]);
        XmTextInsert(w_id, XmTextGetLastPosition(w_id), "\n");
    };

    DEBUG_TRACE_OUT printf("Leaving rtt_editdir_buttonCB\n");
}


/* rtt_edit_buttonCB()
 *
 * manage the FileSelectionDialog widget
 * after file selection is chosen by rtt_edit_buttonCB callback
 *
 */
void rtt_edit_buttonCB(Widget              w,
                       char           *data,
                       XmAnyCallbackStruct *call_data)
{
    int buttonNumber;
    Arg        arg_list[1];
    int        arg_count;

    DEBUG_TRACE_IN printf("Entering rtt_edit_buttonCB\n");
    
    /*
     * extract which button was selected so we know
     * which file to use. Store in buttonNumber member
     * of data structure so file selection widget can
     * match filename with textinfo widget.
     */
    XtSetArg(arg_list[0], XmNuserData, &buttonNumber);
    XtGetValues(w, arg_list, 1);
    rtt_file_data->buttonNumber = buttonNumber;
    if ( rtt_file_data->buttonNumber ) {
        XtVaSetValues ( rtt_file_data->select_file, XmNfileTypeMask, XmFILE_REGULAR, NULL );
    }
    else {
        XtVaSetValues ( rtt_file_data->select_file, XmNfileTypeMask, XmFILE_DIRECTORY, NULL );
    }
    /*
     * manage file selection widget
     */
    XtManageChild(rtt_file_data->select_file);

    DEBUG_TRACE_OUT printf("Leaving rtt_edit_buttonCB\n");
}

/* change_textCB
 *
 *  propogate typed in text changes to the data structure
 *
 */
void change_textCB(Widget              w,
                       char           *data,
                       XmAnyCallbackStruct *call_data)
{
    int buttonNumber;
    char *ptr;

    DEBUG_TRACE_IN printf("Entering change_textCB\n");
    
    /*
     * extract which button was selected so we know
     * which file to use. Store in buttonNumber member
     * of data structure so file selection widget can
     * match filename with textinfo widget.
     */
    XtVaGetValues(w, XmNuserData, &buttonNumber, NULL );
    rtt_file_data->buttonNumber = buttonNumber;
    if ( buttonNumber )
       rtt_file_data->fileName[buttonNumber] = XmTextGetString(rtt_file_data->textinfo[buttonNumber]);
    else {
       ptr = XmTextGetString(rtt_file_data->textinfo[buttonNumber]);
       strcpy ( rtt_file_data->saveDirectory, ptr );
    }

    DEBUG_TRACE_OUT printf("Leaving change_textCB\n");
}

void rtt_excluded_buttonCB(Widget              w,
                           char           *data,
                           XmAnyCallbackStruct *call_data)
{
    DEBUG_TRACE_IN printf("Entering rtt_excluded_buttonCB\n");
    XtPopup(rtt_excluded_popup, XtGrabNone);
    DEBUG_TRACE_OUT printf("Leaving rtt_excluded_buttonCB\n");
}

void rtt_excluded_saveCB(Widget              w,
                         char           *data,
                         XmAnyCallbackStruct *call_data)
{
    RttEditPopupSt * local_data;

    DEBUG_TRACE_IN printf("Entering rtt_excluded_saveCB\n");
    
    local_data = (RttEditPopupSt *) data;
    local_data->rtt_excluded_saveflag = TRUE; 
    XtPopdown(rtt_excluded_popup);

    DEBUG_TRACE_OUT printf("Leaving rtt_excluded_saveCB\n");
}

void rtt_excluded_resetCB(Widget              w,
                          char           *data,
                          XmAnyCallbackStruct *call_data)
{
    RttEditPopupSt * local_data;
    Widget t;

    DEBUG_TRACE_IN printf("Entering rtt_excluded_resetCB\n");
    
    local_data = (RttEditPopupSt *) data;
    t = local_data->rtt_excluded_text1;
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);

    DEBUG_TRACE_OUT printf("Leaving rtt_excluded_resetCB\n");
}

void rtt_excluded_cancelCB(Widget              w,
                           char           *data,
                           XmAnyCallbackStruct *call_data)
{
    RttEditPopupSt * local_data;

    DEBUG_TRACE_IN printf("Entering rtt_excluded_cancelCB\n");
    
    local_data = (RttEditPopupSt *) data;
    local_data->rtt_excluded_saveflag = FALSE; 
    XtPopdown(rtt_excluded_popup);

    DEBUG_TRACE_OUT printf("Leaving rtt_excluded_cancelCB\n");
}

void rtt_transbs_buttonCB(Widget              w,
                          char           *data,
                          XmAnyCallbackStruct *call_data)
{
    DEBUG_TRACE_IN printf("Entering rtt_transbs_buttonCB\n");
    XtPopup(rtt_transbs_popup, XtGrabNone);
    DEBUG_TRACE_OUT printf("Leaving rtt_transbs_buttonCB\n");
}

void rtt_transbs_saveCB(Widget              w,
                        char           *data,
                        XmAnyCallbackStruct *call_data)
{
    RttEditPopupSt * local_data;

    DEBUG_TRACE_IN printf("Entering rtt_transbs_saveCB\n");
    
    local_data = (RttEditPopupSt *) data;
    local_data->rtt_transbs_saveflag = TRUE; 
    XtPopdown(rtt_transbs_popup);

    DEBUG_TRACE_OUT printf("Leaving rtt_transbs_saveCB\n");
}

void rtt_transbs_resetCB(Widget              w,
                         char           *data,
                         XmAnyCallbackStruct *call_data)
{
    RttEditPopupSt * local_data;
    Widget t;

    DEBUG_TRACE_IN printf("Entering rtt_transbs_resetCB\n");
    
    local_data = (RttEditPopupSt *) data;
    t = local_data->rtt_transbs_matrix[0];
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "0.0");

    t = local_data->rtt_transbs_matrix[1];
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "0.0");

    t = local_data->rtt_transbs_matrix[2];
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "0.0");

    t = local_data->rtt_transbs_matrix[3];
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "1.0");

    t = local_data->rtt_transbs_matrix[4];
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "1.0");

    t = local_data->rtt_transbs_matrix[5];
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "1.0");

    t = local_data->rtt_transbs_matrix[6];
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "0.0");

    t = local_data->rtt_transbs_matrix[7];
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "0.0");

    t = local_data->rtt_transbs_matrix[8];
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "0.0");

    t = local_data->rtt_transbs_scale;
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "1.0");

    t = local_data->rtt_transbs_reindex;
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "2 1 3");

    DEBUG_TRACE_OUT printf("Leaving rtt_transbs_resetCB\n"); 
}

void rtt_transbs_cancelCB(Widget              w,
                          char           *data,
                          XmAnyCallbackStruct *call_data)
{
    RttEditPopupSt * local_data;

    DEBUG_TRACE_IN printf("Entering rtt_transbs_cancelCB\n");
    
    local_data = (RttEditPopupSt *) data;
    local_data->rtt_transbs_saveflag = FALSE; 
    XtPopdown(rtt_transbs_popup);

    DEBUG_TRACE_OUT printf("Leaving rtt_transbs_cancelCB\n");
}

void rtt_iop_buttonCB(Widget w, XtPointer clientData, XtPointer callData)
{
    RttEditPopupSt * local_data = (RttEditPopupSt *) clientData;
    char buffer[256];
    int button;
    Widget t;
    
    DEBUG_TRACE_IN printf("Entering rtt_iop_buttonCB\n");

    local_data->rtt_iop_prompt_for_apply = 0;

    /* Fill in the values if we have them */
    if( local_data->iopInfoInFile )
    {
        button = local_data->rtt_iop_button;

        /* activate the button */
        XmToggleButtonSetState( local_data->rtt_iop_buttons[button-1], True, True );

        if( button != 1 )
        {
            t = local_data->rtt_iop_text2;
            XmTextFieldSetSelection( t, 0, XmTextFieldGetLastPosition( t ), (Time) 0 );
            XmTextFieldRemove( t );
            XmTextFieldInsert( t, 0, local_data->rtt_iop_regions[0] );

            t = local_data->rtt_iop_text3;
            XmTextFieldSetSelection( t, 0, XmTextFieldGetLastPosition( t ), (Time) 0 );
            XmTextFieldRemove( t );
            XmTextFieldInsert( t, 0, local_data->rtt_iop_regions[1] );

            t = local_data->rtt_iop_text4;
            sprintf( buffer, "%.1f", local_data->rtt_iop_zsep );
            XmTextFieldSetSelection( t, 0, XmTextFieldGetLastPosition( t ), (Time) 0 );
            XmTextFieldRemove( t );
            XmTextFieldInsert( t, 0, buffer);

            if( button == 2 )
            {
                t = local_data->rtt_iop_text5;
                sprintf( buffer, "%-.1f %-.1f %-.1f",
                         local_data->rtt_iop_beamline[0],
                         local_data->rtt_iop_beamline[1],
                         local_data->rtt_iop_beamline[2] );

                XmTextFieldSetSelection( t, 0, XmTextFieldGetLastPosition( t ), (Time) 0 );
                XmTextFieldRemove( t );
                XmTextFieldInsert( t, 0, buffer);                
            }

            if( local_data->rtt_iop_plusflag )
                XmToggleButtonSetState( local_data->rtt_iop_plus, True, False );
            else
                XmToggleButtonSetState( local_data->rtt_iop_plus, False, False );
        }
    }
    
    XtPopup(rtt_iop_popup, XtGrabNone);
    DEBUG_TRACE_OUT printf("Leaving rtt_iop_buttonCB\n");
}


void rtt_iop_changedCB( Widget w, XtPointer clientData, XtPointer callData )
{
    int buttonNumber = 0;
    RttEditPopupSt * local_data;
    XmToggleButtonCallbackStruct * cbs = (XmToggleButtonCallbackStruct *) callData;
    
    DEBUG_TRACE_IN printf("Entering rtt_iop_changedCB\n");

    if( cbs->set )
    {
        local_data = (RttEditPopupSt *) clientData;
        local_data->rtt_iop_prompt_for_apply = 1;   /* the user made a change */
        
        buttonNumber = atoi( XtName( w ) );
        switch( buttonNumber )
        {
            case 1:
                
                XtVaSetValues(local_data->rtt_iop_text2, XmNsensitive, FALSE, NULL);
                XtVaSetValues(local_data->rtt_iop_text3, XmNsensitive, FALSE, NULL);
                XtVaSetValues(local_data->rtt_iop_text4, XmNsensitive, FALSE, NULL);
                XtVaSetValues(local_data->rtt_iop_text5, XmNsensitive, FALSE, NULL);

                XmToggleButtonSetState( local_data->rtt_iop_plus, False, False );
                XtVaSetValues(local_data->rtt_iop_plus,  XmNsensitive, FALSE, NULL);
                break;

            case 2:
            case 3:
            case 4:
            case 5:
                
                XtVaSetValues(local_data->rtt_iop_text2, XmNsensitive, TRUE, NULL);
                XtVaSetValues(local_data->rtt_iop_text3, XmNsensitive, TRUE, NULL);
                XtVaSetValues(local_data->rtt_iop_text4, XmNsensitive, TRUE, NULL);
                XtVaSetValues(local_data->rtt_iop_plus,  XmNsensitive, TRUE, NULL);

                if( buttonNumber == 2 )
                    XtVaSetValues(local_data->rtt_iop_text5, XmNsensitive, TRUE, NULL);
                else
                    XtVaSetValues(local_data->rtt_iop_text5, XmNsensitive, FALSE, NULL);
                break;

            default:
                break;
        }
    }
    DEBUG_TRACE_OUT printf("Leaving rtt_iop_changedCB\n");
}


void rtt_iop_plusCB(Widget w, XtPointer clientData, XtPointer callData)
{

    RttEditPopupSt *local_data;
    
    DEBUG_TRACE_IN printf("Entering rtt_iop_plusCB\n");

    local_data = (RttEditPopupSt *) clientData;

    local_data->rtt_iop_prompt_for_apply = 1;   

    DEBUG_TRACE_OUT printf("Leaving rtt_iop_plusCB\n");

}

void rtt_iop_saveCB(Widget w, XtPointer clientData, XtPointer callData)
{
    RttEditPopupSt * local_data;
    int conversions;
    float tempFloats[3];
    char buffers[2][256];
    char errorString[1024];
    char error[256];
    int buttonNumber = 0;
    int allValid = 1;
    int i = 0;
    
    DEBUG_TRACE_IN printf("Entering rtt_iop_saveCB\n");

    local_data = (RttEditPopupSt *) clientData;
    
    /* find which button is toggled */
    while( i < 5 && buttonNumber == 0 )
    {
        if( XmToggleButtonGetState( local_data->rtt_iop_buttons[i] ) )
            buttonNumber = i + 1;
        else
            i++;
    }

    strcpy( errorString, "You must first fill in the following values:\n\n" );

    /* Do different things based on the button */
    switch( buttonNumber )
    {
        case 1:
            /* iop is not being used */
            local_data->rtt_iop_saveflag = FALSE;
            local_data->rtt_iop_plusflag = FALSE;
            local_data->rtt_iop_button   = buttonNumber;
            local_data->rtt_iop_prompt_for_apply = 0;
            local_data->iopInfoInFile = 1;  /* we can revert to these values */
            break;
            
        case 2:
        case 3:
        case 4:
        case 5:
            
            /* make sure the appropriate fields are filled */
            (void) getValueFromTextBox( local_data->rtt_iop_text2, buffers[0], NO_EXPAND );
            (void) getValueFromTextBox( local_data->rtt_iop_text3, buffers[1], NO_EXPAND );

            if( strlen( buffers[0] ) == 0 || strlen( buffers[1] ) == 0 )
            {
                allValid = 0;
                sprintf( error, "\tregions\n" );
                strcat( errorString, error );
            }
            else
            {
                strcpy( local_data->rtt_iop_regions[0], buffers[0] );
                strcpy( local_data->rtt_iop_regions[1], buffers[1] );
            }

            (void) getValueFromTextBox( local_data->rtt_iop_text4, buffers[0], NO_EXPAND );
            conversions = sscanf( buffers[0], "%f", &tempFloats[0] );

            if( conversions != 1 )
            {
                allValid = 0;
                sprintf( error, "\tzsep\n" );
                strcat( errorString, error );
            }
            else
                local_data->rtt_iop_zsep = tempFloats[0];

            if( buttonNumber == 2 )
            {
                (void) getValueFromTextBox( local_data->rtt_iop_text5, buffers[0], NO_EXPAND );
                conversions = sscanf( buffers[0], "%f %f %f",
                                      &tempFloats[0], &tempFloats[1], &tempFloats[2] );

                if( conversions != 3 )
                {
                    allValid = 0;
                    sprintf( error, "\tbeamline\n" );
                    strcat( errorString, error );
                }
                else
                {
                    local_data->rtt_iop_beamline[0] = tempFloats[0];
                    local_data->rtt_iop_beamline[1] = tempFloats[1];
                    local_data->rtt_iop_beamline[2] = tempFloats[2];
                }
            }

            if( allValid == 0 )
                DT_error( w, errorString, "Missing Values", NULL );
            else
            {
                /* save the current setup */
                local_data->rtt_iop_saveflag = TRUE;

                local_data->iopInfoInFile = 1;  /* we can revert to these values */

                /* check for iop+ */
                if( XmToggleButtonGetState( local_data->rtt_iop_plus ) )
                    local_data->rtt_iop_plusflag = 1;
                else
                    local_data->rtt_iop_plusflag = 0;

                local_data->rtt_iop_button = buttonNumber;
                local_data->rtt_iop_prompt_for_apply = 0;   
            }

            break;

        default:
            break;
    }
    
    DEBUG_TRACE_OUT printf("Leaving rtt_iop_saveCB\n");
}

void rtt_iop_resetCB(Widget w, XtPointer clientData, XtPointer callData)
{
    RttEditPopupSt * local_data;
    char regions[2][256];
    float zsep;
    float beamline[3];
    int button;
    int iopPlus = 0;
    char buffer[256];
    Widget t;

    DEBUG_TRACE_IN printf("Entering rtt_iop_resetCB\n");
    
    local_data = (RttEditPopupSt *) clientData;

    if( local_data->iopInfoInFile ) /* we have values to revert to */
    {
        button = local_data->rtt_iop_button;
        if( button != 1 )
        {
            iopPlus = local_data->rtt_iop_plusflag;
            
            strcpy( regions[0], local_data->rtt_iop_regions[0] );
            strcpy( regions[1], local_data->rtt_iop_regions[1] );
            
            zsep = local_data->rtt_iop_zsep;
            
            if( button == 2 )
            {
                beamline[0] = local_data->rtt_iop_beamline[0];
                beamline[1] = local_data->rtt_iop_beamline[1];
                beamline[2] = local_data->rtt_iop_beamline[2];
            }
        }
    }
    else
    {
        button = 2;
        iopPlus = 0;

        strcpy( regions[0], "" );
        strcpy( regions[1], "" );

        zsep = 0.0;

        beamline[0] = 0.0;
        beamline[1] = 0.0;
        beamline[2] = 0.0;
    }
            
    /* reset toggle button */
    XmToggleButtonSetState( local_data->rtt_iop_buttons[button-1], True, True );

    if( button != 1 )
    {
        /* reset regions */
        t = local_data->rtt_iop_text2;
        XmTextFieldSetSelection( t, 0, XmTextFieldGetLastPosition( t ), (Time) 0 );
        XmTextFieldRemove( t );
        XmTextFieldInsert( t, 0, regions[0] );
        
        t = local_data->rtt_iop_text3;
        XmTextFieldSetSelection( t, 0, XmTextFieldGetLastPosition( t ), (Time) 0 );
        XmTextFieldRemove( t );
        XmTextFieldInsert( t, 0, regions[1] );

        /* zsep */
        t = local_data->rtt_iop_text4;
        sprintf( buffer, "%-.1f", zsep );
        XmTextFieldSetSelection( t, 0, XmTextFieldGetLastPosition( t ), (Time) 0 );
        XmTextFieldRemove( t );
        XmTextFieldInsert( t, 0, buffer );

        if( button == 2 )
        {
            t = local_data->rtt_iop_text5;
            sprintf( buffer, "%-.1f %-.1f %-.1f",
                     beamline[0], beamline[1], beamline[2] );
            XmTextFieldSetSelection( t, 0, XmTextFieldGetLastPosition( t ), (Time) 0 );
            XmTextFieldRemove( t );
            XmTextFieldInsert( t, 0, buffer );
        }

        if( iopPlus )
            XmToggleButtonSetState( local_data->rtt_iop_plus, True, False );
        else
            XmToggleButtonSetState( local_data->rtt_iop_plus, False, False );
    }
    
    DEBUG_TRACE_OUT printf("Leaving rtt_iop_resetCB\n");
}

void rtt_iop_cancelCB(Widget w, XtPointer clientData, XtPointer callData)
{
    RttEditPopupSt * local_data;
    
    DEBUG_TRACE_IN printf("Entering rtt_iop_cancelCB\n");
    
    local_data = (RttEditPopupSt *) clientData;

    if( local_data->rtt_iop_prompt_for_apply )
    {
        if( DT_decide( w, rtt_app,
                       "You have not saved your changes.\nWould you like to close anyway?",
                       "Changes not Saved", "Yes", "No" ) )
        {
            XtPopdown(rtt_iop_popup);
        }
    }
    else
        XtPopdown( rtt_iop_popup );

    DEBUG_TRACE_OUT printf("Leaving rtt_iop_cancelCB\n");
}

void rtt_isotopes_buttonCB(Widget              w,
                           char           *data,
                           XmAnyCallbackStruct *call_data)
{
    DEBUG_TRACE_IN printf("Entering rtt_isotopes_buttonCB\n");
    XtPopup(rtt_isotopes_popup, XtGrabNone);
    DEBUG_TRACE_OUT printf("Leaving rtt_isotopes_buttonCB\n");
}

void rtt_isotopes_saveCB(Widget              w,
                         char         *data,
                         XmAnyCallbackStruct *call_data)
{
    RttEditPopupSt * local_data;

    DEBUG_TRACE_IN printf("Entering rtt_isotopes_saveCB\n");
    
    local_data = (RttEditPopupSt *) data;
    local_data->rtt_isotopes_saveflag = TRUE; 
    XtPopdown(rtt_isotopes_popup);
    sscanf (XmTextGetString(local_data->rtt_isotopes_text1a), "%d", &local_data->nuclide_id[0]);
    sscanf (XmTextGetString(local_data->rtt_isotopes_text2a), "%d", &local_data->nuclide_id[1]);
    sscanf (XmTextGetString(local_data->rtt_isotopes_text3a), "%d", &local_data->nuclide_id[2]);
    sscanf (XmTextGetString(local_data->rtt_isotopes_text4a), "%d", &local_data->nuclide_id[3]);
    sscanf (XmTextGetString(local_data->rtt_isotopes_text5a), "%d", &local_data->nuclide_id[4]);
    sscanf (XmTextGetString(local_data->rtt_isotopes_text6a), "%d", &local_data->nuclide_id[5]);
    sscanf (XmTextGetString(local_data->rtt_isotopes_text7a), "%d", &local_data->nuclide_id[6]);

    sscanf ( XmTextGetString(local_data->rtt_isotopes_text1b), "%lf",
             &local_data->nuclide_density[0] );
    sscanf ( XmTextGetString(local_data->rtt_isotopes_text2b), "%lf",
             &local_data->nuclide_density[1] );
    sscanf ( XmTextGetString(local_data->rtt_isotopes_text3b), "%lf",
             &local_data->nuclide_density[2] );
    sscanf ( XmTextGetString(local_data->rtt_isotopes_text4b), "%lf",
             &local_data->nuclide_density[3] );
    sscanf ( XmTextGetString(local_data->rtt_isotopes_text5b), "%lf",
             &local_data->nuclide_density[4] );
    sscanf ( XmTextGetString(local_data->rtt_isotopes_text6b), "%lf",
             &local_data->nuclide_density[5] );
    sscanf ( XmTextGetString(local_data->rtt_isotopes_text7b), "%lf",
             &local_data->nuclide_density[6] );

    DEBUG_TRACE_OUT printf("Leaving rtt_isotopes_saveCB\n");
}

void rtt_isotopes_resetCB(Widget              w,
                          char           *data,
                          XmAnyCallbackStruct *call_data)
{
    RttEditPopupSt * local_data;
    Widget t;

    DEBUG_TRACE_IN printf("Entering rtt_isotopes_resetCB\n");
    
    local_data = (RttEditPopupSt *) data;
    t = local_data->rtt_isotopes_text1a;
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "521");

    t = local_data->rtt_isotopes_text1b;
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "6.01426E-8");

    t = local_data->rtt_isotopes_text2a;
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "508");

    t = local_data->rtt_isotopes_text2b;
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "6.315E-2");

    t = local_data->rtt_isotopes_text3a;
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "513");

    t = local_data->rtt_isotopes_text3b;
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "7.911E-4");

    t = local_data->rtt_isotopes_text4a;
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "512");

    t = local_data->rtt_isotopes_text4b;
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "7.270E-3");

    t = local_data->rtt_isotopes_text5a;
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "505");

    t = local_data->rtt_isotopes_text5b;
    XmTextFieldSetSelection(t, 0, XmTextFieldGetLastPosition(t), (Time)0);
    XmTextFieldRemove(t);
    XmTextFieldInsert(t, 0, "2.680E-2");

    DEBUG_TRACE_OUT printf("Leaving rtt_isotopes_resetCB\n");
}

void rtt_isotopes_cancelCB(Widget              w,
                           char           *data,
                           XmAnyCallbackStruct *call_data)
{
    RttEditPopupSt * local_data;

    DEBUG_TRACE_IN printf("Entering rtt_isotopes_cancelCB\n");
    
    local_data = (RttEditPopupSt *) data;
    local_data->rtt_isotopes_saveflag = FALSE; 
    XtPopdown(rtt_isotopes_popup);

    DEBUG_TRACE_OUT printf("Leaving rtt_isotopes_cancelCB\n");
}

/* 
 * create_rtt_edit_menus ()   Ray S. Babcock   5/18/94
 *
 * Creates the menus for the rtt edit widget.  
 *
 * Input parameter, the MenuBar (locally called bar) for attaching menus.
 */
void create_rtt_edit_menus(Widget bar,Widget popup)
{

#define MAXBUTTONS 8
#define MAXMENUS 8

    XmString xmstr;
    
    static Widget menubarBtn[MAXBUTTONS];
    static Widget pulldowns[MAXMENUS];
    static Widget buttons[MAXMENUS];
    static Widget filePulldownSep;

    Arg al[20];
    int ac=0;
    static int pullindex = 0;
    static int buttonindex = 0;
    Widget RunW, TestW, KillW;

    DEBUG_TRACE_IN printf("Entering create_rtt_edit_menus\n");
    
    /* Create the Help button */
    ac = 0;
    XtSetArg(al[ac], XmNlabelString, XmStringCreate("Help",
                                                    XmSTRING_DEFAULT_CHARSET)); ac++;
    XtSetArg(al[ac], XmNmnemonic, 'H');
    menubarBtn[MAXBUTTONS] = XmCreateCascadeButton(bar, "help", al, ac);

    set_preference_directory("/SeraCalc/");
    XtAddCallback (menubarBtn[MAXBUTTONS], XmNactivateCallback,
                   (XtCallbackProc)ContextHelpCallback, menubarBtn[MAXBUTTONS]);

    ac = 0;
    XtSetArg(al[ac], XmNmenuHelpWidget, (XtArgVal)menubarBtn[MAXBUTTONS]); ac++;
    XtSetValues(bar, al, ac);
    XtManageChild(menubarBtn[MAXBUTTONS]);

    /* Create the pull down panes and buttons */
    pullindex=0;

    /* FILE button and pulldown pane ******************/
    ac = 0;
    pulldowns[pullindex] = XmCreatePulldownMenu(bar, "pulldownv0", al, ac);

    ac = 0;
    XtSetArg(al[ac], XmNsubMenuId, pulldowns[pullindex]); ac++;
    XtSetArg(al[ac], XmNlabelString, XmStringCreate("File",
                                                    XmSTRING_DEFAULT_CHARSET)); ac++;
    menubarBtn[pullindex] = XmCreateCascadeButton(bar, "Rtt_mc_edit_buttonv0", al, ac);

    /* FILE menu selections */
    buttonindex = 0;

    /* Open selection */
    ac = 0;
    XtSetArg(al[ac], XmNlabelString, XmStringCreate("Open",
                                                    XmSTRING_DEFAULT_CHARSET)); ac++;
    buttons[buttonindex] =  XmCreatePushButtonGadget(pulldowns[pullindex],
                                                     "buttonv01", al, ac);
    XtAddCallback(buttons[buttonindex],XmNactivateCallback,
                  (XtCallbackProc)rtt_OpenCB, NULL);
    buttonindex++;

    /* Save selection */
    ac = 0;
    XtSetArg(al[ac], XmNlabelString, XmStringCreate("Save",
                                                    XmSTRING_DEFAULT_CHARSET)); ac++;
    buttons[buttonindex] =  XmCreatePushButtonGadget(pulldowns[pullindex],
                                                     "buttonv02", al, ac);
    XtAddCallback(buttons[buttonindex],XmNactivateCallback,
                  (XtCallbackProc)rtt_SaveCB, NULL);
    buttonindex++;

    /* Save As selection */
    ac = 0;
    XtSetArg(al[ac], XmNlabelString, XmStringCreate("Save As",
                                                    XmSTRING_DEFAULT_CHARSET)); ac++;
    buttons[buttonindex] =  XmCreatePushButtonGadget(pulldowns[pullindex],
                                                     "buttonv03", al, ac);
    XtAddCallback(buttons[buttonindex],XmNactivateCallback,
                  (XtCallbackProc)rtt_SaveAsCB, NULL);
    buttonindex++;

    /* Separator and Launch button, added 1-7-99 mbr */

    filePulldownSep = XmCreateSeparator( pulldowns[pullindex], "sep", NULL, 0 );
    XtManageChild( filePulldownSep );

    LT_make_launch_menu( pulldowns[pullindex], "seraCalc" );
    
    /* Check Version selection */
    ac = 0;
    XtSetArg(al[ac], XmNlabelString, XmStringCreate("Check Version",
                                                    XmSTRING_DEFAULT_CHARSET)); ac++;
    buttons[buttonindex] = XmCreatePushButtonGadget(pulldowns[pullindex],
                                                    "button04", al, ac);
    XtAddCallback(buttons[buttonindex], XmNactivateCallback,
		  (XtCallbackProc)rtt_CheckVersionCB, NULL);
    buttonindex++;

    

    /* Exit selection */
    ac = 0;
    XtSetArg(al[ac], XmNlabelString, XmStringCreate("Exit",
                                                    XmSTRING_DEFAULT_CHARSET)); ac++;
    buttons[buttonindex] =  XmCreatePushButtonGadget(pulldowns[pullindex],
                                                     "buttonv05", al, ac);
    XtAddCallback(buttons[buttonindex], XmNactivateCallback, 
                  (XtCallbackProc)rtt_ExitEditCB, popup);
    buttonindex++;
    XtManageChildren(buttons, buttonindex);

    /* Run button and pulldown pane *******************************/
    pullindex++;
    ac = 0;
    pulldowns[pullindex] = XmCreatePulldownMenu(bar, "pulldownv1", al, ac);
    ac = 0;
    XtSetArg(al[ac], XmNsubMenuId, pulldowns[pullindex]); ac++;
    XtSetArg(al[ac], XmNlabelString, XmStringCreate("Run",
                                                    XmSTRING_DEFAULT_CHARSET)); ac++;
    menubarBtn[pullindex] = XmCreateCascadeButton(bar, "Rtt_mc_edit_buttonv1", al, ac);

    /* Run menu selections */
    buttonindex = 0;

    /* RUN RTT selection */
    ac = 0;
    XtSetArg(al[ac], XmNlabelString, XmStringCreate("Run",
                                                    XmSTRING_DEFAULT_CHARSET)); ac++;
    buttons[buttonindex] =  XmCreatePushButtonGadget(pulldowns[pullindex],
                                                     "buttonv30two", al, ac);
    RunW = buttons[buttonindex];
    buttonindex++;

    /* TEST RTT selection */
    ac = 0;
    XtSetArg(al[ac], XmNlabelString, XmStringCreate("Test",
                                                    XmSTRING_DEFAULT_CHARSET)); ac++;
    XtSetArg(al[ac], XmNsensitive, TRUE); ac++;
    buttons[buttonindex] =  XmCreatePushButtonGadget(pulldowns[pullindex],
                                                     "buttonv31two", al, ac);
    TestW = buttons[buttonindex];
    buttonindex++;

    /* KILL RTT selection */
    ac = 0;
    XtSetArg(al[ac], XmNlabelString, XmStringCreate("Kill",
                                                    XmSTRING_DEFAULT_CHARSET)); ac++;
    buttons[buttonindex] =  XmCreatePushButtonGadget(pulldowns[pullindex],
                                                     "buttonv34two", al, ac);
    KillW = buttons[buttonindex];
    buttonindex++;

    /* set up a structure to pass to callbacks */
    rtt_run_widgets->RunW = RunW;
    rtt_run_widgets->TestW = TestW;
    rtt_run_widgets->KillW = KillW;
    XtAddCallback(RunW,XmNactivateCallback,
                  (XtCallbackProc)rtt_RunCB, rtt_run_widgets);
    XtAddCallback(TestW,XmNactivateCallback,
                  (XtCallbackProc)rtt_TestCB, rtt_run_widgets);
    XtAddCallback(KillW,XmNactivateCallback,
                  (XtCallbackProc)rtt_KillCB, rtt_run_widgets);
    /*XtManageChildren(buttons, buttonindex);*/

    XtManageChildren(buttons, buttonindex);

    /* Options menu selection */
    pullindex++;
    ac = 0;
    pulldowns[pullindex] = XmCreatePulldownMenu( bar, "pulldownv1", al, ac );

    ac = 0;
    XtSetArg( al[ac], XmNsubMenuId, pulldowns[pullindex] ); ac++;
    XtSetArg( al[ac], XmNlabelString, XmStringCreate( "Options", XmSTRING_DEFAULT_CHARSET ) ); ac++;
    menubarBtn[pullindex] = XmCreateCascadeButton( bar, "optionsButton", al, ac );

    /* A toggle button for displaying the view files */
    buttonindex = 0;
    xmstr = XmStringCreateLtoR( "Display View Files", XmSTRING_DEFAULT_CHARSET );
    ac = 0;
    XtSetArg( al[ac], XmNlabelString, xmstr ); ac++;
    buttons[buttonindex] = XmCreateToggleButton( pulldowns[pullindex], "viewToggle", al, ac );
    viewGui->viewToggle = buttons[buttonindex];
    XmStringFree( xmstr );

    buttonindex++;
    XtManageChildren( buttons, buttonindex );

    XtAddCallback( viewGui->viewToggle, XmNvalueChangedCallback, viewToggleCallback, NULL );
    
    /* manage all menu related widgets */
    pullindex++;
    XtManageChildren(menubarBtn, pullindex);

    DEBUG_TRACE_OUT printf("Leaving create_rtt_edit_menus\n");
} /* end of create_rtt_edit_menus */


void rtt_CheckVersionCB( Widget w, XtPointer clientData, XtPointer callData )
{
    char seramc_version[32];

    /* SERACALC and SERAMC defined in rtt_define.h */
    /* MODULE_VERSION and SERAMC_VERSION defined in module_version.h */
    DEBUG_TRACE_IN printf("Entering rtt_CheckVersionCB\n");

    if( get_version_string( SERAMC, seramc_version ) == 0 )
        strcpy( seramc_version, "Unavailable" );
    
    CT_check_version( rtt_top, SERACALC );
    MC_check_version( rtt_top, SERAMC, seramc_version );

    DEBUG_TRACE_OUT printf("Leaving rtt_CheckVersionCB\n");
}


void MC_check_version ( Widget w, char *module, char *version )
{
    static main_checker_t   check;               /* main structure used */
    static int              first_call = 1;      /* First call flag     */
    XmString                xmstr;               /* String for widgets  */

    DEBUG_TRACE_IN printf ( "Entering MC_check_version\n" );

    /* Copy parent widget and module and version strings into structure */
    strcpy ( check.module, module );
    strcpy ( check.version_number, version );
    check.parent = w;

    /* if this is the first call, build the shell */
    if ( first_call )
    {
        build_check_version_shell ( &check );
        first_call = 0;
    }

    /* Manage the dialog */
    XtManageChild ( check.shell );

    /* Check the version of module */
    get_recent_version ( &check );

    /* Set the string of most resent software version */
    xmstr = XmStringCreateLocalized ( check.version_string );
    XtVaSetValues ( check.recent_version, XmNlabelString, xmstr, NULL );
    XmStringFree ( xmstr );

    DEBUG_TRACE_OUT printf ( "Leaving MC_check_version\n" );
}


void buttonPressedEH( Widget w, XtPointer clientData, XEvent * event, Boolean * flag )
{
    char value[256];
    int success;
    int button = (int) clientData;
    Widget form;
    Widget label;
    Arg al[10];
    int ac = 0;
    XmString xmstr;
    
    if( event->xbutton.button == 3 )
    {
        success = getValueFromTextBox( rtt_file_data->textinfo[button], value, EXPAND );
        if( success && strlen( value ) > 15 )
        {
            ac = 0;
            XtSetArg( al[ac], XmNmwmDecorations, MWM_DECOR_ALL|MWM_DECOR_TITLE|MWM_DECOR_RESIZEH|MWM_DECOR_MENU ); ac++;
                        
            popupShell = XtCreatePopupShell( "Full Text Value", xmDialogShellWidgetClass, w, al, ac );

            ac = 0;
            XtSetArg( al[ac], XmNverticalSpacing, 5 ); ac++;
            XtSetArg( al[ac], XmNhorizontalSpacing, 5 ); ac++;
            form = XmCreateForm( popupShell, "form", al, ac );
            
            ac = 0;
            xmstr = XmStringCreateLtoR( value, XmSTRING_DEFAULT_CHARSET );
            XtSetArg( al[ac], XmNlabelString, xmstr ); ac++;
            XtSetArg( al[ac], XmNleftAttachment, XmATTACH_FORM ); ac++;
            XtSetArg( al[ac], XmNtopAttachment, XmATTACH_FORM ); ac++;
            XtSetArg( al[ac], XmNrightAttachment, XmATTACH_FORM ); ac++;
            XtSetArg( al[ac], XmNbottomAttachment, XmATTACH_FORM ); ac++;
            
            label = XmCreateLabel( form, "label", al, ac );
            XmStringFree( xmstr );
            
            XtManageChild( label );
            XtManageChild( form );

            ac = 0;
            XtSetArg( al[ac], XmNx, event->xbutton.x_root ); ac++;
            XtSetArg( al[ac], XmNy, event->xbutton.y_root ); ac++;
            XtSetValues( popupShell, al, ac );
            
            XtPopup( popupShell, XtGrabNone );
        }
        else
            popupShell = NULL;
        
    }
}

void buttonReleasedEH( Widget w, XtPointer clientData, XEvent * event, Boolean * flag )
{
    int button = (int) clientData;

    if( event->xbutton.button == 3 )
    {
        if( popupShell != NULL )
        {
            XtPopdown( popupShell );
            XtDestroyWidget( popupShell );
            popupShell = NULL;
        }
    }
}
