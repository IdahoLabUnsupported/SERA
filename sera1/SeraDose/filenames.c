#include "global.h"
#include "filenames.h"
#include "debug_tools.h"
#include "memory_tools.h"
#include "file_tools.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <glob.h>
#include <dirent.h>
#include <unistd.h>

int pd[2];        /* pd is a pipe */
int dd;
int pid;

void get_filenames (char *directory, char *files)
{
  int bytes_desired=4096;
  int rdsize;
  int status;
  char buf[8192];

  DEBUG_TRACE_IN printf( "Entering get_filenames\n" );

  /* pd[0] is for reading (parent does the reading),
   * pd[1] is for writing (child does the writing)
   */
  if (pipe(pd)<0) {
    fprintf(stderr, "Couldn't open pipe.\n");
    exit(0);
  }
  
  switch(pid=fork())
    {
    case -1:
      /* fork didn't happen */
      break;
    case 0:  /* Child sees this */
      close(pd[0]);                  /* pd[0] is for the parent    */
      close(1);                      /* child closes stdout!       */
      dd=dup(pd[1]);                 /* child stdout goes to pd[1] */

      execlp("ls", "ls", directory, "-a", NULL);
      break;
    default: /* Parent sees this */
      close(pd[1]);                  /* pd[1] is for the child     */
      break;
    }

   files[0] = '\0';

  /* Now, we just need to read the pipe */
  do {
    rdsize = read(pd[0], buf, bytes_desired);
    buf[rdsize]='\0';
    strcat (files, buf);
  } while (rdsize>0);
  
  waitpid(pid, &status, WNOHANG);
  close(pd[0]);

  DEBUG_TRACE_OUT printf( "Leaving get_filenames\n" );
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     get_filenames2
%%%
%%%  Purpose:      Given a directory and a filename suffix, return all the
%%%                files in that directory that match that suffix.
%%%
%%%  Parameters:   directory -> A char *, the directory to look in.
%%%                suffix    -> A char *, the filename suffix to look for.
%%%                files     -> A char ***, the list of filenames is
%%%                             returned in this parameter.
%%%                numFiles  -> An int, the number of matching files found.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:        Memory is dynamically allocated for files each time this
%%%                gets called. A call to free_filenames should be used when
%%%                the list of filenames is no longer needed. You must also
%%%                pass to free_filenames the number of files in the list,
%%%                which is returned in numFiles here.
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void get_filenames2( char * directory, char * suffix, char *** files, int * numfiles )
{
    int length;
    DIR * dirPtr;
    glob_t globbuf;
    char * pattern;
    int i;
    
    DEBUG_TRACE_IN printf("Entering get_filenames2\n");

    *numfiles = 0;
    *files = NULL;
    
    /* Make sure directory is valid */
    dirPtr = opendir( directory );

    if( dirPtr != NULL )
    {
        closedir( dirPtr );

        /* Make room for the pattern to search for. */
        length = strlen( directory );
        pattern = (char *) MT_malloc( (length + 25) * sizeof( char ) );

        if( directory[ length - 1 ] != '/' )
            sprintf( pattern, "%s/*%s", directory, suffix );
        else
            sprintf( pattern, "%s*%s", directory, suffix );

        /* Now get the files matching the pattern */
        if( glob( pattern, 0, NULL, &globbuf ) == 0 )
        {
            *numfiles = globbuf.gl_pathc; /* return the number of files */
            *files = (char **) MT_malloc( (*numfiles) * sizeof( char * ) );
            
            for( i = 0; i < *numfiles; i++ )
            {
                length = strlen( globbuf.gl_pathv[i] );
                (*files)[i] = (char *) MT_malloc( (length + 1) * sizeof( char ) );
                strcpy( (*files)[i], globbuf.gl_pathv[i] );
            }

            globfree( &globbuf );
        }

        MT_free( (void *) pattern );
    }
    DEBUG_TRACE_OUT printf("Leaving get_filenames2\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:     free_filenames
%%%
%%%  Purpose:      Free the memory allocated by a previous call to
%%%                get_filenames.
%%%
%%%  Parameters:   files    -> A char **, the pointer to the list to free.
%%%                numFiles -> The number of filenames in the list.
%%%
%%%  Returns:      nothing
%%%
%%%  Notes:        numFiles must be the same as was returned when
%%%                get_filenames2 was called.
%%%
%%%  Written By:   Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void free_filenames( char ** files, int numfiles )
{
    int i;
    
    DEBUG_TRACE_IN printf("Entering free_filenames\n");

    for( i = 0; i < numfiles; i++ )
        MT_free( (void *) files[i] );

    MT_free( (void *) files );

    DEBUG_TRACE_OUT printf("Leaving free_filenames\n");
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    breakIntoDirAndFilename
%%%
%%%  Purpose:     Given a path, return the directory portion and the
%%%               filename portion.
%%%
%%%  Parameters:  input     -> A char *, the path name to break apart.
%%%               directory -> A char *, the directory part of input.
%%%               filename  -> A char *, the file part of input.
%%%
%%%  Returns:     1 on success, 0 otherwise
%%%
%%%  Notes:       The name is kind of misleading. You would expect that if
%%%               input contained something like directory/file, then
%%%               directory would contain directory/ on return and filename
%%%               would hold file. However, on return, directory does hold
%%%               the directory part, but filename is basically a copy of
%%%               input, (i.e. it contains the whole path of the file.)
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int breakIntoDirAndFilename( char * input, char * directory, char * filename )
{
    char * ptr;
    int returnValue = 0;
    char localCopy[MAX_LENGTH_NAME];
    
    DEBUG_TRACE_IN printf("Entering breakIntoDirAndFilename\n");

    /* First make a local copy that we can manipulate */
    strcpy( localCopy, input );
    
    /* Find the / in input */
    ptr = strrchr( localCopy, '/' );
    if( ptr != NULL )
    {
        ptr++;
        
        /* get the filename */
        strcpy( filename, localCopy );

        /* now get the directory */
        *ptr = '\0';
        strcpy( directory, localCopy );
        
        returnValue = 1; /* success */
    }

    DEBUG_TRACE_OUT printf("Leaving breakIntoDirAndFilename\n");
    return( returnValue );
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%  Function:    getDirAndFilename
%%%
%%%  Purpose:     Retrieve a file from a file selection widget. Return
%%%               the entire filename selected, and the directory it is in.
%%%
%%%  Parameters:  title     -> A char *, the title for the FSB
%%%               directory -> A char *, the directory where the selected
%%%                            file resides.
%%%               filename  -> A char *, the user's selected file (name and path)
%%%               suffix    -> A char *, a suffix to check against the
%%%                            selected file.
%%%
%%%  Returns:     1 on success, 0 otherwise
%%%
%%%  Notes:       Failure occurs if the selected file doesn't exist, or
%%%               it doesn't end in the specified suffix.
%%%
%%%  Written By:  Mark Rossmeier
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
int getDirAndFilename( char * title, char * directory, char * filename, char * suffix )
{
    char buffer[MAX_LENGTH_NAME];
    int success;
    int returnValue = 0;

    DEBUG_TRACE_IN printf("Entering getDirAndFilename\n");

    success = DT_select_file( xcontoursTopLevelShell, context, buffer, title );
    if( success )
    {
        if( FT_fileExists( buffer ) && FT_filenameEndsIn( buffer, suffix ) )
        {
            if( breakIntoDirAndFilename( buffer, directory, filename ) )
            {
                returnValue = 1;
            }
        }
        else
        {
            sprintf( buffer, "That is not an existing '%s' file!", suffix );
            DT_error( xcontoursTopLevelShell, buffer, NULL, NULL );
        }
    }

    DEBUG_TRACE_OUT printf("Leaving getDirAndFilename\n");
    return( returnValue );
}
