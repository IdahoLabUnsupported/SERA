#include "rtt.h"
#include "file_tools.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     checkRttFiles()
%%%
%%%  Purpose:      Go through the seraCalc interface and check for valid
%%%                files. The file names are gotten from the text fields in
%%%                the widget. Some files are not required, but if they are
%%%                in the widget, they will be checked for existence.
%%%
%%%  Parameters:   None. The global structure rtt_file_data is used.
%%%
%%%  Returns:      1 if a successful check is completed.
%%%                If unsuccessful, 0 is returned, and a warning dialog appears.
%%%
%%%  Notes:
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int checkRttFiles( void )
{
    char buffer[256], cptr[256], *ptr;
    int success;
    char warningString[1024];  /* entire warning string */
    char warning[256];         /* warning for individual fields */
    int returnValue = 1;
    
    DEBUG_TRACE_IN printf("Entering checkRttFiles\n");

    strcpy( warningString, "The following errors occurred while checking your files:\n" );
    strcat( warningString, "========================================================\n\n" );

    /* Save Directory */ 
    success = getValueFromTextBox( rtt_file_data->textinfo[0], buffer, EXPAND );
    if( success == 0 || isADirectory( buffer ) == 0 )
    {
        returnValue = 0;
        sprintf( warning, "\tYou have not entered a valid Save Directory\n" );
        strcat( warningString, warning );
    }

    /* CG Geom File Name */
    success = getValueFromTextBox( rtt_file_data->textinfo[1], buffer, EXPAND );
    sscanf ( buffer, "%s", cptr );
    if( success == 0 || FT_fileExists( cptr ) == 0 || FT_filenameEndsIn( cptr, ".geom" ) == 0 )
    {
        returnValue = 0;
        sprintf( warning, "\tYou have not entered a valid CG Geom File\n" );
        strcat( warningString, warning );
    }

    /* Patient Geom File Name */
    success = getValueFromTextBox( rtt_file_data->textinfo[2], buffer, EXPAND );
    sscanf ( buffer, "%s", cptr );
    
    if( ( success == 0 || FT_fileExists( cptr ) == 0 ||
          !( FT_filenameEndsIn( cptr, ".uv" ) || FT_filenameEndsIn( cptr, ".uv.gz" ) ) ) &&
        strcmp(cptr, "none")  )
    {
        returnValue = 0;
        sprintf( warning, "\tYou have not entered a valid Patient Geom File\n" );
        strcat( warningString, warning );
    }
    /* Make sure there is a .uvh file to go with the .uv file. */
    if( success == 1 && (FT_filenameEndsIn(cptr,".uv") || FT_filenameEndsIn(cptr,".uv.gz")) ) 
    {
        ptr = strrchr( cptr, 'v' );
        if( ptr != NULL )
        {
            *( ptr + 1 ) = 'h';
            *( ptr + 2 ) = '\0';
        }
        if( FT_fileExists( cptr ) == 0 )
        {
            returnValue = 0;
            sprintf( warning, "\tThere is no .uvh file corresponding to your .uv file\n" );
            strcat( warningString, warning );
        }
    }

    /* Old RST File Name */
    success = getValueFromTextBox( rtt_file_data->textinfo[3], buffer, EXPAND );
    sscanf ( buffer, "%s", cptr );
    if( success == 0 )
    {
        returnValue = 0;
        sprintf( warning, "\tYou have not entered a valid Old RST File\n" );
        strcat( warningString, warning );
    }
    else
    {
        if( strlen( cptr ) > 0 )
        {
            if( FT_fileExists( cptr ) == 0 )
            {
                if( strcmp( cptr, "none" ) != 0 )
                {
                    returnValue = 0;
                    sprintf( warning, "\tYou have not entered a valid Old RST File\n" );
                    strcat( warningString, warning );
                }
            }
        }
    }

    /* New RST File Name */
    success = getValueFromTextBox( rtt_file_data->textinfo[4], buffer, EXPAND );
    sscanf ( buffer, "%s", cptr );
    if( success == 0 )
    {
        returnValue = 0;
        sprintf( warning, "\tYou have not entered a valid New RST File\n" );
        strcat( warningString, warning );
    }
    else
    {
        if( strlen( cptr ) > 0 )
        {
            if( strstr( cptr, ".rst" ) == 0 )
            {
                if( strcmp( cptr, "none" ) != 0 )
                {
                    returnValue = 0;
                    sprintf( warning, "\tYou have not entered a valid New RST File\n" );
                    strcat( warningString, warning );
                }
            }
        }
    }    

    /* Source File Name */
    success = getValueFromTextBox( rtt_file_data->textinfo[5], buffer, EXPAND );
    sscanf ( buffer, "%s", cptr );
    if( success == 0 || FT_fileExists( cptr ) == 0 )
    {
        returnValue = 0;
        sprintf( warning, "\tYou have not entered a valid Source File\n" );
        strcat( warningString, warning );
    }

    /* Material File Name */
    success = getValueFromTextBox( rtt_file_data->textinfo[6], buffer, EXPAND );
    sscanf ( buffer, "%s", cptr );
    if( success == 0 || FT_fileExists( cptr ) == 0 )
    {
        returnValue = 0;
        sprintf( warning, "\tYou have not entered a valid Material File\n" );
        strcat( warningString, warning );
    }

    /* Cross Section File Name */
    success = getValueFromTextBox( rtt_file_data->textinfo[7], buffer, EXPAND );
    sscanf ( buffer, "%s", cptr );
    if( success == 0 || FT_fileExists( cptr ) == 0 )
    {
        returnValue = 0;
        sprintf( warning, "\tYou have not entered a valid Cross Section File\n" );
        strcat( warningString, warning );
    }

    /* seraMC Run Script */
    success = getValueFromTextBox( rtt_file_data->textinfo[8], buffer, EXPAND );
    sscanf ( buffer, "%s", cptr );
    if ( strstr(cptr, "run_seraMC") != cptr ) {
       if( success == 0 || FT_fileExists( cptr ) == 0 )
       {
           returnValue = 0;
           sprintf( warning, "\tYou have not entered a valid seraMC Run Script\n" );
           strcat( warningString, warning );
       }
    }

    /* Popup a warning if we found something wrong */
    if( returnValue == 0 )
        DT_error( rtt_file_data->rtt_shell, warningString, NULL, NULL );
    
    DEBUG_TRACE_OUT printf("Leaving checkRttFiles\n");
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:      isADirectory
%%%
%%%  Purpose:       Determine if a given filename is a directory currently
%%%                 on the system.
%%%
%%%  Parameters:    filename -> A char *, the name of the file.
%%%
%%%  Returns:       1 if filename is a current directory, 0 otherwise
%%%
%%%  Notes:         This particular function requires that the last
%%%                 character of filename must be a '/' to be a directory.
%%%
%%%  Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int isADirectory( char * filename )
{
    int returnValue = 0;
    
    DEBUG_TRACE_IN printf("Entering isADirectory with %s\n", filename);

    if( filename != NULL )
    {
        if( FT_isADirectory( filename ) && FT_filenameEndsIn( filename, "/" ) )
            returnValue = 1;
    }
    
    DEBUG_TRACE_OUT printf("Leaving isADirectory\n");
    return( returnValue );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     getValueFromTextBox
%%%
%%%  Purpose:      Get the string value from a text box.  If the string
%%%                begins with a $, it is assumed to be an environment
%%%                variable and will be expanded and returned in value.
%%%
%%%  Parameters:   textbox -> A text widget.
%%%                value   -> A char *, a  buffer to store the return value.
%%%                expand  -> Determines if environment variables should
%%%                           be expanded or not, 1 = yes, 0 = don't expand
%%%
%%%  Returns:      1 on success, 0 otherwise.
%%%                The string in the text widget is returned in value.
%%%
%%%  Notes:        A return value of 0 means that an invalid environment
%%%                variable was found. The data in value should not be used.
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int getValueFromTextBox( Widget textbox, char * value, int expand )
{
    char * textValue = NULL;
    char * rightPtr  = NULL;
    char * leftPtr   = NULL;
    
    char   buffer[256];
    char   localCopy[256];

    char * environmentPtr = NULL;
    char   environmentBuffer[256];

    int returnValue = 1;   /* assume success */

    DEBUG_TRACE_IN printf("Entering getValueFromTextBox\n");

    /* initialize buffer */
    buffer[ 0 ] = '\0';
    
    /* Get the value from the text widget */
    textValue = XmTextGetString( textbox );

    if( strlen( textValue ) > 0 )
    {
        /* keep a copy */
        strcpy( localCopy, textValue );

        if( expand == 1 )
        {
            /* Check for an environment variable */
            leftPtr = strchr( localCopy, '$' );

            if( leftPtr != NULL ) /* need to expand an env variable */
            {
                /* Now find where the environment variable "ends" */
                rightPtr = strchr( localCopy, '/' );

                if( rightPtr != NULL )
                {
                    *rightPtr = '\0';
                    rightPtr++;       /* move past the '/' */
                }
            
                leftPtr++;  /* move past the $ */

                /* store the name of the environment variable */
                strcpy( environmentBuffer, leftPtr );
            
                /* and find its value */
                environmentPtr = getenv( environmentBuffer );

                if( environmentPtr != NULL )
                {
                    sprintf( buffer, "%s/", environmentPtr );

                    /* now get the rest of the textvalue */
                    if( *rightPtr != '\0' )
                        strcat( buffer, rightPtr );
                }
                else
                    returnValue = 0;
            }
            else  /* no environment variables found */
            {
                strcpy( buffer, textValue );
            }
        }
        else     /* not expanding environment variables */
        {
            strcpy( buffer, textValue );
        }

        XtFree( textValue );
    }

    /* return what we found */
    strcpy( value, buffer );

    DEBUG_TRACE_OUT printf("Leaving getValueFromTextBox\n");
    return( returnValue );
}
