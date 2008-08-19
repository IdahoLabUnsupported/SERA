/*#include "bnct.h" --DAVID */
#include "histo.h"


/* Get_Memory - Bnct allocate memory function
 *       A function to call XtMalloc and check it's return.  If null,
 *       an appropriate error message is generated.
 */

char * GetMemory( Cardinal size, char * calling_function )
{
  char * temp_address;

  temp_address = XtMalloc(size);
  if(temp_address == NULL)
   {
    fprintf(stderr,"Fatal Error Allocating Memory, function: %s\n", calling_function);
    exit(1);
   };

  return temp_address;
}
