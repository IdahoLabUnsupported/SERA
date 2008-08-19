/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  File:        file_tools.h
%%%
%%%  Purpose:     Common routines used on files.
%%%
%%%  Notes:       All function names are prefixed with FT_ for "File Tools"
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#ifndef __FILE_TOOLS_H__
#define __FILE_TOOLS_H__

/*%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Includes
%%%
%%%%%%%%%%%%%%%%%%%%%%%%*/
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>

#include "debug_tools.h"


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Prototypes
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     FT_fileExists()
%%%
%%%  Purpose:      Determine if a given filename currently exists.
%%%
%%%  Parameters:   filename -> A char *, the name of the file
%%%
%%%  Returns:      1 if the file exists, 0 otherwise
%%%
%%%  Notes:        Returns 0 if a directory is given as input.
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int FT_fileExists( char * filename );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:      FT_isADirectory
%%%
%%%  Purpose:       Determine if a given filename is a directory currently
%%%                 on the system.
%%%
%%%  Parameters:    filename -> A char *, the name of the file.
%%%
%%%  Returns:       1 if filename is a current directory, 0 otherwise
%%%
%%%  Notes:         
%%%
%%%  Written By:    Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int FT_isADirectory( char * filename );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     FT_filenameEndsIn
%%%
%%%  Purpose:      Determine if a given filename ends with a given string.
%%%
%%%  Parameters:   filename -> A char *, the original filename.
%%%                suffix   -> A char *, the suffix to test for in filename.
%%%
%%%  Returns:      1 if filename ends in suffix, 0 otherwise
%%%
%%%  Notes:        
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int FT_filenameEndsIn( char * filename, char * suffix );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    FT_deleteFile
%%%
%%%  Purpose:     Delete the given filename from the system.
%%%
%%%  Parameters:  filename -> A char *, the filename to delete.
%%%
%%%  Returns:     nothing
%%%
%%%  Notes:       This function will not attempt to remove a directory.
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void FT_deleteFile( char * filename );


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Function:     FT_sizeOfFile
%%% 
%%% Purpose:      Determine the size of a file.
%%% 
%%% Parameters:   filename -> A char *, must be null-terminated.
%%% 
%%% Return Value: The size of the file on success. Zero on error.
%%% 
%%% Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int FT_sizeOfFile( char * filename );

#endif
