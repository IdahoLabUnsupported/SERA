#ifndef _filenames_h
#define _filenames_h

void get_filenames (char *directory, char *files);

void get_filenames2( char * directory, char * suffix, char *** files, int * numfiles );
void free_filenames( char ** files, int numfiles );
int breakIntoDirAndFilename( char * input, char * directory, char * filename );
int getDirAndFilename( char * title, char * directory, char * filename, char * suffix );

#endif
