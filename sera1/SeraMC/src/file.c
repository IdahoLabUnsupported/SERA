#include <stdio.h>
#include <string.h>
#include "../../Shared/include/libsz.h"

/*****************************************************************
 *			N U L L _ F77_ S T R I N G
 *
 *  Take a FORTRAN string with a length, and return a pointer to
 *  null terminated copy of that string in a STATIC buffer.
 *****************************************************************/
static char *
null_f77_string( string, string_len )
char	*string;
int      string_len;
{
	static char	*buf;
	int     	len;
	int     	i;

        buf = (char *)malloc( (string_len * sizeof(char)) + 1 );
	buf[string_len] = '\0';
	strncpy( buf, string, string_len );

	/* Remove any trailing blanks */
	for( i=string_len-1; i >= 0; i-- )  {
		if( buf[i] != ' ' && buf[i] != '\n' )  break;
		buf[i] = '\0';
	}
	return(buf);
}


void zipfile_(f_name, f_length)
char  *f_name;   	/* the file name */
int   f_length;
{
	int   ierr, length;
        char *f_name_null, *f_out_null;

        f_name_null = null_f77_string( f_name, f_length);
        f_out_null = (char *)malloc ( (strlen(f_name_null) * sizeof(char)) + 4 );
        f_out_null = strcpy ( f_out_null, f_name_null );
        f_out_null = strcat ( f_out_null, ".sz" );

/*
 *  use seraZip to compress the named file, removing original file
 */

        ierr = SZ_ZipFile ( f_name_null, f_out_null, 0, 0 );
        if ( !ierr )
           printf ("Problem compressing file %s\n", f_name_null );

        free(f_name_null);
        free(f_out_null);
	return;
}

void zipfile(f_name, f_length)
char  *f_name;   	/* the file name */
int   f_length;
{
        zipfile_(f_name, f_length);
}

