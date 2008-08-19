#include <stdlib.h>
#include <sys/wait.h>
#include "gz_tools.h"

/* These two routines are prototyped here as these routines are only
 * used internally and not by programs using the gz_tools library
 */
int open_gz_pipe(char *, gz_FILE *);
int gz_fread_pipe(int *, void *, int);


/**************************************************************************
 * gz_FILE * gz_fopen(char * name, char * how_open)                       *
 **************************************************************************
 * Routine similar to fopen but will also allow user to open gzipped      *
 * files and read them transparently.                                     *
 * Variables:                                                             *
 *   name:      Path and name to file you wish to open                    *   
 *   how_open:  Only supports "r" for read                                *
 *                                                                        *
 * Notes:                                                                 *
 *   > A gzipped file is recognized by 'name' ending in lowercase .gz     *
 *   > Return value is ptr to a malloced gz_FILE structure                *
 *     (Nothing malloced if return value is NULL)                         *
 **************************************************************************/
gz_FILE * gz_fopen(char * name, char * how_open) {
  gz_FILE * retval = (gz_FILE *) NULL;
  FILE * fptr;

  if (strcmp(how_open, "r")) {
    fprintf(stderr, "Sorry, cannot use gz_fopen except with \"r\"\n");
    fprintf(stderr, "as only READING files is allowed.\n");
  } else {
    retval = (gz_FILE *)MT_malloc(sizeof(gz_FILE));
    if (retval) {
      if ((strlen(name)<3)||(strcmp(name+(strlen(name)-3), ".gz"))) {
	/* not gzipped */
	fptr = fopen(name, how_open);
	if (fptr) {
	  /* Opened file successfully */
	  retval->whichtype = 1;   /* Indicate non-gzipped file */
	  retval->fptr=fptr;
	} else {
	  /* Could not open file */
	  MT_free((void*)retval);
	  retval = NULL;
	}
      } else {
	/* gzipped file -- ended in ".gz" */
	retval->whichtype = 2;     /* Indicate gzipped file */
	if (!(open_gz_pipe(name, retval))) {
	  /* Not opened successfully */
	  MT_free((void*)retval);
	  retval = NULL;
	}
      }
    }
  }
  return(retval);
}


/**************************************************************************
 * void gz_fclose(gz_FILE * gz_fptr)                                      *
 **************************************************************************
 * Routine similar to fclose but operates on gz_FILES                     *
 * Variables:                                                             *
 *   gz_fptr:   ptr to open gz_FILE structure
 *                                                                        *
 * Notes:                                                                 *
 *   > You MUST close gz_FILEs after opening and reading them             *
 *   > No need to close files from failed gz_opens                        *
 **************************************************************************/
void gz_fclose(gz_FILE * gz_fptr) {
  int status;
  char buf[256];

  if (!gz_fptr) return; /* Possible result of failure to open the file */
  else {
    switch(gz_fptr->whichtype)
      {
      case 1:   /* Indicates non-gzipped file */
	/* Simply close the opened file the normal way */
	if (gz_fptr->fptr) {
	  fclose(gz_fptr->fptr);
	}
	break;
      case 2:   /* Indicates gzipped file */
	/* In this case, need to do some special things to close a gz file */
	
	/* First, empty the entire pipe */
	while (gz_fread_pipe(gz_fptr->pd, buf, 256)>0);
	
	/* gz reading process should have died -- do cleanup */
	waitpid(gz_fptr->pid, &status, WNOHANG);
	close(gz_fptr->pd[0]);   /* We're done reading the pipe    */
        break;
      }
    MT_free((void*)gz_fptr);        /* Free the gz_FILE structure     */
  }
}


/**************************************************************************
 * int gz_fread(void * data_ptr, size_t chunksize, size_t numchunks,      *
 *	        gz_FILE * gz_fptr)                                        *
 **************************************************************************
 * Routine similar to fread but operates on gz_FILES                      *
 * Variables:                                                             *
 *   data_ptr:  ptr to buffer to place data that is read                  *
 *   chunksize: number of bytes to be read as a single unit (usually 1)   *
 *   numchunks: number of 'chunksize' units to read                       *
 *   gz_fptr:   ptr to open gz_FILE structure                             *
 *                                                                        *
 * Notes:                                                                 *
 *   > You must successfully open the gz file before reading it           *
 *   > -1 return value indicate an error                                  *
 *   > Otherwise, return value is number of BYTES successfully read       *
 **************************************************************************/
int gz_fread(void * data_ptr, size_t chunksize, size_t numchunks,
	     gz_FILE * gz_fptr) {
  int size;

  if ((chunksize<=0)||(numchunks<=0)) return(0);
  if (!data_ptr) return(-1);
  else {
    switch (gz_fptr->whichtype)
      {
      case 1:
	/* standard file */
	size = fread(data_ptr, chunksize, numchunks, gz_fptr->fptr);
	if (size>=0) {
	  return(size*chunksize);
	} else {
	  return(-1);
	}
	break;
      case 2:
	/* gzipped file */
	return(gz_fread_pipe(gz_fptr->pd, data_ptr, chunksize*numchunks));
	break;

      default:
          /* Somehow, whichtype isn't what we expect...error! */
          return( -1 );
          break;
      }
  }
}


/**************************************************************************
 * int open_gz_pipe(char * name, gz_FILE * gz_fptr)                       *
 **************************************************************************
 * Routine that sets up a pipe to a gzip file reading process.  The pipe  *
 * is then used to read the gzipped file.                                 *
 * Variables:                                                             *
 *   name:      Path and name to file you wish to open                    *   
 *   gz_fptr:   ptr to open gz_FILE structure                             *
 * Notes:                                                                 *
 *   > Returns 1 on success                                               *
 *   > 0 on failure                                                       *
 **************************************************************************/
int open_gz_pipe(char * name, gz_FILE * gz_fptr) {
  int dd, pid;

  if (!gz_fptr) return(0);

  /* Create a pipe:
   * pd[0] is for reading (parent does the reading),
   * pd[1] is for writing (child does the writing)
   */
  if (pipe (gz_fptr->pd) < 0) {
    fprintf (stderr, "Couldn't open pipe.\n");
    return(0);
  }
    
  /* We fork to have both a parent and a child process */
  /* Upon return, the child sees pid==0 whereas the parent
   * sees the child's pid OR -1 if there was an error
   * Note:  Both child and parent have a copy of pd[]
   */
  
    switch(pid=fork())
      {
      case -1:
	fprintf(stderr, "Couldn't fork gzip process.\n");
	return(0);
	break;
      case 0: /* The child sees this */
	close(gz_fptr->pd[0]);         /* only parent uses this      */
	close(1 /*stdout->_fileno*/);  /* child closes stdout        */
	dd=dup(gz_fptr->pd[1]);        /* child stdout goes to pd[1] */
	
	/* Execute the gzip program to uncompress the file */
	/* command:  gzip -dc regionfile                   */
	execlp("gzip", "gzip", "-dc", name, NULL);
	
	fprintf(stderr, "Execlp failed.  Must exit.  It's likely that gzip is not in your path.\n");
	fprintf(stderr, "Suggestion:  Make sure gzip is in your path _or_ use uncompressed files only.\n");
	/* Since this is the child, it's OK for it to exit */
	exit(EXIT_FAILURE);
	
	/* close(dd); --> no need cause we never get here */
	/* close(gz_fptr->pd[1]); */
	break;
      default: /* The parent sees this */
	gz_fptr->pid = pid;
	close(gz_fptr->pd[1]);            /* only child uses this */
	return(1);
      }
}


/**************************************************************************
 * int gz_fread_pipe(int *pd, void *buf, int numbytes)                    *
 **************************************************************************
 * Routine similar to fread but is used for reading a pipe.  In this      *
 * case, the pipe is used to read data from a gzipped file                *
 * Variables:                                                             *
 *   pd:        Pipe descriptor (int[2])                                  *
 *   buf:       ptr to buffer to place data that is read                  *
 *   numbytes:  Number of bytes desired to be read                        *
 **************************************************************************/
int gz_fread_pipe(int *pd, void *buf, int numbytes) {
  unsigned char * bptr, * ptr;
  int rdsize, blocksize=64, remaining, numread=0;

  if (numbytes<=0) return(0);
  if (!buf) return(0);

  bptr = (unsigned char *)buf;

  remaining = numbytes;
  ptr = bptr;
  /* Here, reading pd[0] is same as reading child's stdout */
  while (remaining>0) {
    rdsize=read(pd[0], ptr, remaining);
    if (rdsize<0) {
      fprintf(stderr, "Error reading gzipped file.\n");
      fprintf(stderr, "Please check file integrity, make sure gzip is in your path,\nor uncompress file and try again.\n");
      return(-1);
    } else if (rdsize==0) {
      remaining = 0;
    } else {
      remaining-=rdsize;
      ptr+=rdsize;
      numread+=rdsize;
    }
  }
  return(numread);
}
