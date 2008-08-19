#include "memory_tools.h"
#include <stdio.h>
#include <stdlib.h>

/* USE_MEMORY_TRACING can be a expensive operation.  
   remove it for release versions */
#define USE_MEMORY_TRACING

/* Memory Tracing uses a linked list to keep track of all the allocs 
   and deallocs that are called.
*/

enum memory_type { MALLOC, CALLOC, REALLOC, FREE };

struct memory_info {
  char * file;
  int filenumber;
  enum memory_type type;
  size_t size;
  void * pointer;
};

#ifdef USE_MEMORY_TRACING


struct node {
  struct memory_info info;
  struct node *next;
};

struct linked_list {
  struct node *first;
};

/* allocates space for a new node and returns the pointer to it */
struct node *get_new_node( void ){
  struct node *temp_ptr;
  
  temp_ptr = (struct node *) malloc(sizeof(struct node));
  if( temp_ptr == NULL){
    printf("out of memory for node\n");
    exit(1);
  }
  return (temp_ptr);
}

/* frees all elements */
void release(struct linked_list* list){
  struct node *temp1, *temp2;

  if(!list || !(list->first))
    return;
  temp1 = list->first;

  while(temp1-> next != NULL){
    temp2 = temp1->next;
    free((void *)temp1->info.file);
    free((void *)temp1);
    temp1 = temp2;
  }
  free((void *)temp1->info.file);
  free((void *)temp1);
  list->first = NULL;
}

/* prints all members of the list */
void print(struct linked_list * list){
  struct node * ptr;
  if(!list || !(list->first))
    return;
  
  ptr = list->first;
  while( ptr != NULL){
    printf("file: %s:%d \tpointer:%p \tsize:%d\n",ptr->info.file,
	   ptr->info.filenumber, ptr->info.pointer, ptr->info.size);
    ptr = ptr->next;
  }
}


/* adds new_node to begining of list */
void add_node_at_start(struct linked_list * list, struct node *new_node){
  if(!list)
    return;

  new_node->next = list->first;
  list->first = new_node;
}

void add_info_at_start(struct linked_list * list, struct memory_info info){
  struct node * temp;
  if(!list){
    printf("add_info_at_end called with a null list. Not added\n");
    return;
  }
  temp = get_new_node();
  temp->info = info;
  add_node_at_start(list, temp);
}

/* removes the node with pointer pointing to look_for returns 0 
 if not found or the amount of memory if found. */
int remove_node(struct linked_list * list, void * look_for){
  struct node *current, *prev=NULL;
  int found = 0;
  
  if(!list || !(list->first))
    return found;

  current = list->first;
  while( current != NULL){
    if(current->info.pointer == look_for){
      found = current->info.size;
      if(prev == NULL){
	list->first = current->next;
	free(current->info.file);
	free(current);
      } else {
	prev->next = current->next;
	free(current->info.file);
	free(current);
      }
      break;
    }
    prev = current;
    current = current->next;
  }
  return found;
}

#endif

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  The Real Thing
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/



/* MEMORY_STATS should be a function that prints information about
   malloc's left unfreed if such a function is available.
*/
#define MEMORY_STATS /*malloc_stats()*/

long MT_bytes_allocated = 0;

#ifdef USE_MEMORY_TRACING
struct linked_list * malloc_list = NULL;

void init_MT(){
  if(malloc_list == NULL){
    malloc_list = (struct linked_list *)malloc(sizeof(struct linked_list));
    malloc_list->first = NULL;
  }
}
#endif


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     MT_malloc()
%% 
%% Purpose:      Allocate a block of memory.
%% 
%% Parameters:   num -> An unsigned int, the number of bytes to allocate.
%% 
%% Return Value: A char *. If the requested memory is successfully 
%%               allocated, MT_malloc() will return a char ptr which 
%%               should be cast to the appropriate type. If there is an
%%               error allocating the memory, an error message will be 
%%               printed, and exit() will be called.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
char * _MT_malloc( unsigned int num, char * file,int line  )
{

  char * return_value;
  struct memory_info info;

  return_value = (char *) XtMalloc( (Cardinal) num );

  DEBUG_MEMORY {
    MT_bytes_allocated += num;
    DEBUG_MEMORY_ALLOC printf("%p Allocated %d bytes at %s:%d\n",return_value, num,file,line);
  }

#ifdef USE_MEMORY_TRACING
  DEBUG_MEMORY {
    if(malloc_list == NULL)
      init_MT();
    info.filenumber = line;
    info.pointer = return_value;
    info.file = (char *) malloc((strlen(file)+1)*sizeof(char *));
    strcpy(info.file,file);
    info.size = num;
    add_info_at_start(malloc_list,info);
  }
#endif 

  return( return_value );

}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:      MT_calloc()
%% 
%% Purpose:       Allocate memory for an array and initialize its bytes
%%                to zero.
%% 
%% Parameters:    num -> An unsigned int, the number of array elements to allocate
%%                size-> An unsigned int, the size of an array element in bytes.
%%
%% Return Value: A char *. If the requested memory is successfully 
%%               allocated, MT_calloc() will return a char ptr which 
%%               should be cast to the appropriate type. If there is an
%%               error allocating the memory, an error message will be 
%%               printed, and exit() will be called. 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
char * _MT_calloc( unsigned int num, unsigned int size, char * file,int line  )
{

  char * return_value;
  struct memory_info info;

  return_value = (char *) XtCalloc( (Cardinal) num, (Cardinal) size );

  DEBUG_MEMORY {
    MT_bytes_allocated += num;
    DEBUG_MEMORY_ALLOC printf("%p Allocated %d bytes at %s:%d\n",return_value, num*size,file,line);
  }
    
#ifdef USE_MEMORY_TRACING
  DEBUG_MEMORY {
    if(malloc_list == NULL)
      init_MT();
    info.filenumber = line;
    info.pointer = return_value;
    info.file = (char *) malloc((strlen(file)+1)*sizeof(char *));
    strcpy(info.file,file);
    info.size = num*size;
    add_info_at_start(malloc_list,info);
  }
#endif 

  return( return_value );
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     MT_realloc()
%% 
%% Purpose:      Change the size of an allocated block of storage.
%% 
%% Parameters:   ptr -> A char *, a pointer to previously allocated memory
%%                      or NULL. This ptr should be cast to type char *.
%%               size-> An unsigned int, the new number of bytes desired in the block.   
%% 
%% Return Value: A char *. If the requested memory is successfully 
%%               allocated, MT_realloc() will return a char ptr which 
%%               should be cast to the appropriate type. If there is an
%%               error allocating the memory, an error message will be 
%%               printed, and exit() will be called. 
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
char * _MT_realloc( char * ptr, unsigned int num, char * file,int line  )
{

  char * return_value, * orig=ptr;
  struct memory_info info;
  int memory = 0;
  
#ifdef USE_MEMORY_TRACING
  DEBUG_MEMORY {
    if(malloc_list == NULL)
      init_MT();
    memory = remove_node(malloc_list,ptr);
    if(memory == 0){    
      DEBUG_MEMORY_REALLOC printf("########### Reallocated was not allocated with MT*\n");     
    } else {
      MT_bytes_allocated -= memory;
    }
  }
#endif

  return_value = (char *) XtRealloc( (char *) ptr, (Cardinal) num );

  DEBUG_MEMORY {
    MT_bytes_allocated += num;
    if(memory == 0){
      DEBUG_MEMORY_REALLOC printf("%p to %p Reallocated %d bytes at %s:%d\n",
				  orig,return_value, num,file,line);    
    }else {
      DEBUG_MEMORY_REALLOC printf("%p to %p Reallocated from %d bytes to %d bytes at %s:%d\n",
				  orig,return_value,memory, num,file,line);    
    }
  }

#ifdef USE_MEMORY_TRACING
  DEBUG_MEMORY {
    if(malloc_list == NULL)
      init_MT();
    info.filenumber = line;
    info.pointer = return_value;
    info.file = (char *) malloc((strlen(file)+1)*sizeof(char *));
    strcpy(info.file,file);
    info.size = num;
    add_info_at_start(malloc_list,info);
  }
#endif 
  

  return( return_value );

}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     MT_free()
%% 
%% Purpose:      Free allocated memory.
%% 
%% Parameters:   ptr -> A void *, the address of the allocated memory
%%                      to be freed. This ptr should be cast to type void *.
%% 
%% Return Value: none
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void _MT_free( void * ptr, char * file,int line )
{
  int memory = 0;

#ifdef USE_MEMORY_TRACING
  DEBUG_MEMORY {
    if(malloc_list == NULL)
      init_MT();
    memory = remove_node(malloc_list,ptr);
    if(memory == 0){
      DEBUG_MEMORY_REALLOC printf("########### Freed was not allocated with MT*\n"); 
    }
  }
#endif

  DEBUG_MEMORY {
    MT_bytes_allocated -= memory;
    if(memory == 0){
      DEBUG_MEMORY_FREE printf("%p Freed memory at %s:%d\n",ptr,file,line);
    }else{
      DEBUG_MEMORY_FREE printf("%p Freed %d bytes at %s:%d\n",ptr,memory,
			       file,line);
    }
  }

  XtFree( (char *) ptr );


}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Function:     MT_free()
%% 
%% Purpose:      Fakes memory being freed. 
%% 
%% Parameters:   ptr -> A void *, the address of the allocated memory
%%                      to be fake freed. 
%% 
%% Return Value: none
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
void _MT_fake_free( void * ptr, char * file,int line )
{
  int memory = 0;

#ifdef USE_MEMORY_TRACING
  DEBUG_MEMORY {
    if(malloc_list == NULL)
      init_MT();
    memory = remove_node(malloc_list,ptr);
    if(memory == 0){
      DEBUG_MEMORY_REALLOC printf("########### Freed was not allocated with MT*\n"); 
    }
  }
#endif

  DEBUG_MEMORY {
    MT_bytes_allocated -= memory;
    if(memory == 0){
      DEBUG_MEMORY_FREE printf("%p Fake Freed memory at %s:%d\n",
			       ptr,file,line);
    }else{
      DEBUG_MEMORY_FREE printf("%p Fake Freed %d bytes at %s:%d\n",ptr,memory,
			       file,line);
    }
  }


}


/*
  MT_onexit()

  Purpose: print a message if memory left allocated

*/
void MT_onexit(void){
  DEBUG_MEMORY {
    if(MT_bytes_allocated != 0){
      printf("%ld bytes left allocated\n",MT_bytes_allocated);
      MEMORY_STATS;
#ifdef USE_MEMORY_TRACING
      printf("The Following Pointers were not 'MT_free'ed\n");
      print(malloc_list);
#endif
    }
  }
}


