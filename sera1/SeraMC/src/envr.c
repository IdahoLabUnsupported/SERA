/*
 * Below is the set of C routines that I use in PDQ to query user
 * environment variables in a somewhat portable fashion.  The calling
 * Fortran routine should assign the "getvar" return value to a
 * Fortran pointer variable.  For example:
 * 
 *       ...
 *       pointer (PATHNAMp, PATHNAM)
 *       character*64 PATHNAM
 *       integer GETVAR, NULL
 *       external GETVAR
 *       parameter (NULL = 0)
 *       ...
 *       PATHNAMp = GETVAR ('BNCTVAR' // char(NULL))
 *       if (PATHNAMp .ne. NULL) then
 *           ... use the character PATHNAM variable somehow ...
 *    endif
 *       ...
 */

#include <stdio.h>
#include <stdlib.h>
#ifdef cray
#include <string.h>
#endif

/*
 * getvar - Searches the environment list for the specified environment
 *          variable.  This function serves as a bridge to the 'getenv'
 *          function of the C run-time library.
 */
char *getvar (name)
char *name;			/* environment variable name */
{
#ifdef cray
	int len;
	char *sp, *wp;

	if ((sp = getenv(name)) == NULL)
		return ((char *) NULL);

	if ((len = strlen(sp)) == 0)
		return ((char *) NULL);

	if ((wp = (char *) malloc ((size_t) (((len/8)+1)*8))) == NULL)
		return ((char *) NULL);

	(void) memcpy(wp, sp, len+1);
	return (wp);
#else
	return(getenv(name));
#endif
}


/*
 * Alternative entry point names for 'getvar'.
 */

char *GETVAR (name)
char *name;
{
	return(getvar(name));
}

char *getvar_ (name)
char *name;
{
	return(getvar(name));
}



/******************************************************************************/

int relpath ( char *filename, char *newfile, int *len )

{

   char *tmp, *tmp2, *tmp3, *tmp4;
   int  mlen, nlen, init;

/*
 *  Get the present directory for the relative path expansion
 */
   tmp = (char *) malloc ( 81*sizeof(char) );
   tmp = getenv ( "PWD" );

   tmp2 = filename;
   for ( init = 0, mlen = 0; !strncmp(tmp2,"..",2); tmp2+=3 ) {
      mlen += 3;
      tmp3 = (char *) strrchr ( tmp, '/' );
      if ( init )
         *tmp4 = '/';
      *tmp3 = '\0';
      tmp4 = tmp3;
      init = 1;
   }
   *len = mlen - 1;
   strcpy ( newfile, tmp );
   *tmp3 = '/';

   return ( (int) strlen(newfile) );
   
   free ( tmp );

}


/*
 * Alternative entry points for relpath
 */

int RELPATH ( char *filename, char *newfile, int *len )
{
   return ( relpath(filename, newfile, len) );
}

int relpath_ ( char *filename, char *newfile, int *len )
{
   return ( relpath(filename, newfile, len) );
}
