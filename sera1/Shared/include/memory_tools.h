/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% File Name:     memory_tools.h:
%% 
%% Purpose:       Provide prototypes for the functions defined in 
%%                memory_tools.c. These tools provide a common interface
%%                for memory allocation and deallocation, and should 
%%                provide more reliable memory handling.
%%
%%                When -debug_memory is used it keeps track of memory that
%%                is allocated and deallocated.  On exit any memory that 
%%                has not been freed is printed along with the file:line that
%%                originally allocated the memory.  
%%                 
%% NOTES:         Memory_tools uses debug_tools, so debug_tools.h and
%%                debug_tools.c (or links to them) must be present in the
%%                same directory. In order for the tracing of memory
%%                with debug_tools to work correctly, MT_free should only be
%%                used with memory that was allocated with the other memory_
%%                tools functions. For example, doing a XmStringGetLtoR causes
%%                memory to be allocated. For the tracing of memory to work this
%%                memory should be freed with XtFree( char *). There is now problem
%%                using MT_free() for this memory, but the memory trace will say 
%%                that you have freed memory which you didn't allocate.
%% 
%%                In order for post program memory leaks to be printed 
%%                the function MT_onexit should be registered with atexit()
%%
%%                The MT_* macros are the functions that should be used.
%%                The _MT_* functions are here to be called by MT_* macros
%%
%%                Also, memory_tools.c and memory_tools.o
%%                must be added to the Imakefile.
%%  
%% 
%% First Written: 1-26-99
%% Last Modified: 1-27-99
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifndef MEMORY_TOOLS_H
#define MEMORY_TOOLS_H

#include <Xm/Xm.h>
#include <stdlib.h>

#include "debug_tools.h"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%   PROTOTYPES
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
#define MT_malloc(num)  _MT_malloc(num,__FILE__,__LINE__)
char * _MT_malloc  ( unsigned int num ,char * file, int line);
#define MT_calloc(num,size) _MT_calloc(num,size,__FILE__,__LINE__)
char * _MT_calloc  ( unsigned int num, unsigned int size, char * file , int line );
#define MT_realloc(ptr,size) _MT_realloc(ptr,size,__FILE__,__LINE__)
char * _MT_realloc ( char * ptr, unsigned int size , char * file, int line);
#define MT_free(ptr) _MT_free(ptr,__FILE__,__LINE__)
void   _MT_free    ( void * ptr, char * file, int line );
#define MT_fake_free(ptr) _MT_fake_free(ptr,__FILE__,__LINE__)
void   _MT_fake_free(void * ptr,char * file, int line );
void MT_onexit(void);

#endif

