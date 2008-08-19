#include <string.h>
#include "rtt_include.h"

char *blank_trim(char *input_string)

/*
 * trim trailing blanks or cr from a string 
 *  "abc    /n" will become abc\0 null terminator at end
 *  "abc      " will become abc\0 null terminator at end
 */ 

{

    char  *sptr, *sptr_old;
    int    i, length;

    DEBUG_TRACE_IN printf("Entering blank_trim\n");
   
    length = strlen ( input_string );

    if ( !length )
    {
        DEBUG_TRACE_OUT printf("Leaving blank_trim\n");
        return ( "" );
    }

    sptr_old = input_string + length;

    for ( i = length; i >= 0; i-- ) {
        if ( sptr = (char *) strrchr(input_string, '\n') )
            *sptr = '\0';
        if ( sptr_old - ( sptr = (char *) strrchr(input_string, ' ') ) == 1 )
            *sptr = '\0';
        sptr_old = sptr--;
    }

    DEBUG_TRACE_OUT printf("Leaving blank_trim\n");
    return (input_string);

}
