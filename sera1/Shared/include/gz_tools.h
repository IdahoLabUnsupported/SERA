/**************************************************************************/
/* Some routines made for READING gzipped or standard (non-gzipped) files */
/* NOTE:  Very important to CLOSE files after you open them               */
/**************************************************************************/

#ifndef GZ_TOOLS_H
#define GZ_TOOLS_H

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include "memory_tools.h"

typedef struct _gz_FILE {
  FILE * fptr;   /* the actual file ptr, for non-gzipped files */
  int pd[2];     /* a pipe -- necessary if it's a gzipped file */
  int whichtype; /* 1 for non-gzipped, 2 for gzipped           */
  pid_t pid;     /* if gzipped, pid of forked gzip process     */
} gz_FILE;

gz_FILE * gz_fopen(char *, char *);
void gz_fclose(gz_FILE *);
int gz_fread(void *, size_t, size_t, gz_FILE *);
#endif
