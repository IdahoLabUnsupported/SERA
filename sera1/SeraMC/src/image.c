#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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


void image_(row, f_name, f_length)
int   *row;      	/* a row of integer values */
char  *f_name;   	/* the file name */
int   f_length;
{
	int i;
	int *rp;
	unsigned char buf[256];
	static FILE *fp = NULL;
        char        *f_name_null;

	/* open the file */
        f_name_null = null_f77_string( f_name, f_length);
	if ((fp = fopen(f_name_null, "a")) == NULL)
           return;
        free(f_name_null);

	/* pack the row and write it out to the file */
	for (i = 0, rp = row; i < 256; i++, rp++)
		buf[i] = (unsigned char) *rp;
	(void) fwrite((char *) buf, 256, 1, fp);

	/* close the file */
	(void) fclose(fp);
	return;
}

void image(row, f_name, f_length)
int   *row;      	/* a row of integer values */
char  *f_name;   	/* the file name */
int   f_length;
{
        image_(row, f_name, f_length);
}

