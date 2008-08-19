#include <stdio.h>
#include <ctype.h>

void read_row__ (array, opflag, nrow, fname, len)
int   array[512];        /* a row of integer values */
char  *fname;            /* the input file name*/
int   *opflag;          /* =0, open file else read file*/
int   *nrow;            /* number pixels in row*/
int   len;              /* # characters in fname*/
{
	int i, j;
	unsigned char buf[512];
	FILE *fp;

        if(*opflag==0) {
        /* printf("open file %d:  ", *opflag);  */
        /* printf("width of row %d:  ", *nrow); */
        /* fix up the file name */
	for (i = 0; i < len; i++) {
		if (isspace(fname[i])) {
			fname[i] = '\0';
			break;
		}
	}

	/* open the file */
	if ((fp = fopen(fname, "r")) == NULL)
			return;
        /* printf("file opened %d:  ", *opflag); */
                  }

	/* read in a row at a time -- unpack each row into an array column */
		(void) fread((char *) buf, *nrow, 1, fp);
		for (j = 0; j < *nrow; j++)
			array[j] = (int) buf[j];

	/* close the file */
        if(*opflag==2) {
        /*     printf("close file %d:  ", *opflag); */
              (void) fclose(fp);
                        }

	return;
}
