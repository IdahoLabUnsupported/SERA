#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define OP_CREATE	0	/* open the file by truncation or creation */
#define OP_APPEND	1	/* just append (don't open or close) */
#define OP_CLOSE	2	/* close the file */

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


void write_row__ (row, opflag, width, f_name, f_length)
int   *row;      	/* a row of integer values */
char  *f_name;   	/* the file name */
int   *opflag;  	/* op flag (see #defines above) */
int   f_length;
int   *width;	/* length of row in pixels */
{
	int i;
	int *rp;
	unsigned char buf[512];
	static FILE *fp = NULL;
        char        *f_name_null;

	/* create the file */
	if (*opflag == OP_CREATE) {
                f_name_null = null_f77_string( f_name, f_length);
                printf("name = %s length = %d\n", f_name_null, f_length);
		if ((fp = fopen(f_name_null, "w")) == NULL)
			return;
                free(f_name_null);
	}

	/* pack the row and write it out to the file */
	for (i = 0, rp = row; i < *width; i++, rp++)
		buf[i] = (unsigned char) *rp;
	(void) fwrite((char *) buf, *width, 1, fp);

	/* close the file */
	if (*opflag == OP_CLOSE)
		(void) fclose(fp);
	return;
}

void write_row(row, opflag, width, f_name, f_length)
int   *row;      	/* a row of integer values */
char  *f_name;   	/* the file name */
int   *opflag;  	/* op flag (see #defines above) */
int   f_length;
int   *width;	/* length of row in pixels */
{
    write_row__ (row, opflag, width, f_name, f_length);
}

void write_row_ (row, opflag, width, f_name, f_length)
int   *row;      	/* a row of integer values */
char  *f_name;   	/* the file name */
int   *opflag;  	/* op flag (see #defines above) */
int   f_length;
int   *width;	/* length of row in pixels */
{
    write_row__ (row, opflag, width, f_name, f_length);
}
