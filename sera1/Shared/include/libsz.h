#ifndef ZIP_TOOLS_H
#define ZIP_TOOLS_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "Zlib/zlib.h"

typedef struct _switches_t
{
    int keepFile;
    int nameFile;
    int verbose;
    int help;
} switches_t;

int SZ_ZipFile   ( char *sourceFile, char *destFile, int verbose, int keepFile );
int SZ_UnzipFile ( char *sourceFile, char *destFile, int verbose, int keepFile );

int SZ_UnzipFileIntoArray ( char *filename, char **array, int *arraySize );
int SZ_UnzipFileIntoBinaryArray ( char *filename, Bytef **array,
                                  uLong *arraySize, int dataSize );

int SZ_ZipArrayIntoFile ( char *filename, char *array, int arraySize );
int SZ_ZipBinaryArrayIntoFile ( char *filename, Bytef *array, uLong arraySize, int dataSize );

int  SZ_IsASeraZippedFile( char * filename );
void SZ_MakeIntoSeraZippedFilename ( char *filename, char **sz_filename );

void SZ_DeleteFile       ( char * filename );

int SZ_ReadSwitches ( char *, switches_t * );
void SZ_PrintHelp ( void );

void SZ_FreeArray ( char **array );


#endif /* ZIP_TOOLS_H */
