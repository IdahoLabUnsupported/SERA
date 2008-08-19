#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "memory_tools.h"
#include "keyval_tools.h"

#define KV_MAX_SPLIT_STRING_SIZE 10
#define KV_COMMENT_CHAR '#'
#define KV_MAX_KEYVAL_LINE_SIZE 1024

char KV_KEY_VALUE_SPLIT_STRING[KV_MAX_SPLIT_STRING_SIZE+1]=":";



/******************************************************************
 * ROUTINES
 ******************************************************************/


/******************************************************************
 * KV_set_split_characters
 ******************************************************************
 * By default, reading a line and splitting it into a key and
 * value looks for a ":" separator.  However, you can use this
 * function to set it to something else like a ":=", etc.
 ******************************************************************/
void KV_set_split_characters(char * new_split) {
  if (strlen(new_split)<=KV_MAX_SPLIT_STRING_SIZE) {
    strcpy(KV_KEY_VALUE_SPLIT_STRING, new_split);
  } else {
    fprintf(stderr, "String in KV_set_split_characters too long.\n");
    exit(EXIT_FAILURE);
  }
}


/******************************************************************
 * KV_readln
 ******************************************************************
 * Reads a line from a file up to (maxsize-1) characters.  Because
 * it is intended to read a line, will stop after reading a '\n'.
 * The line is then terminated with a '\0' which uses a byte in
 * the array and is why at most (maxsize-1) characters can be read.
 * The LENGTH returned should be the same as that that would be
 * reported by STRLEN.
 ******************************************************************/
int KV_readln(FILE *fptr, char *s, int maxsize) {
  int i=0, ch;

  while (i<(maxsize-1)) {
    ch=fgetc(fptr);
    if (ch<0) {
      /* don't increment i because we didn't really read a character,
       * we hit the end of the file or had an error -- so just stop
       */
      s[i]='\0';
      return(i);
    } else {
      s[i] = (char)ch;
      i++;
      if ((char)ch=='\n') {
	s[i] = '\0';
	return(i);
      }
    }
  }
  /* If here, filled all the characters before encountering a \n */
  /* --Better terminate the string */
  s[maxsize-1] = '\0';
  return(i);
}


/******************************************************************
 * KV_SZ_readln
 ******************************************************************
 * Reads a line from an array up to (maxsize-1) characters.  Because
 * it is intended to read a line, will stop after reading a '\n'.
 * The line is then terminated with a '\0' which uses a byte in
 * the array and is why at most (maxsize-1) characters can be read.
 * The LENGTH returned should be the same as that that would be
 * reported by STRLEN.
 ******************************************************************/
int KV_SZ_readln ( char **buffer, char *s, int maxsize )
{
    int  i = 0;
    char ch;
    
    while ( i < ( maxsize - 1 ) )
    {
        ch = (*buffer)[0];
        (*buffer)++;

        if ( ch == '\0' || ch < 0 )
        {
            /* don't increment i because we didn't really read a character,
             * we hit the end of the array or had an error -- so just stop
             */
            s[i] = '\0';
            return ( i );
        }
        else
        {
            s[i] = ch;
            i++;
            
            if ( ch == '\n' )
            {
                s[i] = '\0';
                return ( i );
            }
        }
    }
    
    /* If here, filled all the characters before encountering a \n */
    /* --Better terminate the string */
    s[maxsize - 1] = '\0';
    return ( i );
}


/******************************************************************
 * KV_readln_skip_comments_and_trim
 ******************************************************************
 * Acts a lot like KV_readln with these additions:
 *   * Skips comments
 *   * Trims the string when done (removes trailing and leading
 *   * white space)
 *   * If what remains is of length 0, will look for the next line
 *   * SO when 0 returns that means no more meaningful lines
 *   * in file
 * :: As of 6-1-1998, will take any per-slice information strings
 * and 'crunch' the []'s into the string as in str [ 5] --> str[5]
 ******************************************************************/
int KV_readln_skip_comments_and_trim(FILE *fptr, char *s, int maxsize) {
  int length;
  char * loc, * loc2;

  do {
    length = KV_readln(fptr, s, maxsize);
    if (length==0) return(0); /* hit end of file */
    
    /* In this case, remove comments and then trim */
    if (loc = strchr(s, KV_COMMENT_CHAR)) {
      loc[0] = '\0';
      length = strlen(s);
    }

    if (length>0) {
      length = KV_trim_string(s);
    }
  } while (length==0);

  /* Here, have a non-zero length string -- compress any []'s */
  /* Example:  a [ 5]bcdefg --> a[5]  bcdefg                  */
  if ((loc=strstr(s, "["))&&(loc2=strstr(loc, "]"))) {
    int index;
    char copy[KV_MAX_KEYVAL_LINE_SIZE];

    strcpy(copy, s);

    loc[0] = '\0';
    KV_trim_string(s);
    loc2[0] = '\0';
    index = atoi(loc+1);
    sprintf(loc, "[%d]", index);

    if ((loc=strstr(copy, "["))&&(loc2=strstr(loc, "]"))) {
      strcat(s, loc2+1);
    }
  }
  
  return(length);
}


/******************************************************************
 * KV_SZ_readln_skip_comments_and_trim
 ******************************************************************
 * Acts a lot like KV_SZ_readln with these additions:
 *   * Skips comments
 *   * Trims the string when done (removes trailing and leading
 *   * white space)
 *   * If what remains is of length 0, will look for the next line
 *   * SO when 0 returns that means no more meaningful lines
 *   * in buffer
 * :: As of 6-1-1998, will take any per-slice information strings
 * and 'crunch' the []'s into the string as in str [ 5] --> str[5]
 ******************************************************************/
int KV_SZ_readln_skip_comments_and_trim ( char **buffer, char *s, int maxsize )
{
    int length;
    char *loc, *loc2;

    do
    {
        length = KV_SZ_readln ( buffer, s, maxsize );
        
        if ( length == 0 )
            return ( 0 ); /* hit end of file */
    
        if ( loc = strchr ( s, KV_COMMENT_CHAR ) )
        {
            loc[0] = '\0';
            length = strlen ( s );
        }

        if ( length > 0 )
        {
            length = KV_trim_string ( s );
        }
    } while ( length == 0 );

    /* Here, have a non-zero length string -- compress any []'s */
    /* Example:  a [ 5]bcdefg --> a[5]  bcdefg                  */
    if ( ( loc = strstr ( s, "[" ) ) && ( loc2 = strstr ( loc, "]" ) ) )
    {
        int index;
        char copy[KV_MAX_KEYVAL_LINE_SIZE];

        strcpy ( copy, s );

        loc[0] = '\0';
        KV_trim_string ( s );
        loc2[0] = '\0';
        index = atoi ( loc + 1 );
        sprintf ( loc, "[%d]", index );

        if ( ( loc = strstr ( copy, "[" ) ) && ( loc2 = strstr ( loc, "]" ) ) )
        {
            strcat ( s, loc2 + 1 );
        }
    }
  
    return ( length );
}


/******************************************************************
 * KV_break_into_key_and_value
 ******************************************************************
 * Looks for first br to break the string in two (if none, all
 * of string is key)
 *   throws out the br portion
 *   'trims' each remaining string
 ******************************************************************/
void KV_break_into_key_and_value(char * br, char * s,
				 char ** key, char ** value) {
  char * wherebreak;
  
  if (key==value) {
    fprintf(stderr, "Illegal parameters to KV_break_into_key_and_value.\n");
    exit(EXIT_FAILURE);
  }

  if (wherebreak=strstr(s, br)) {
    *value = wherebreak + strlen(br);
    KV_trim_string(*value);
    wherebreak[0] = '\0';
  } else {
    *value = s+strlen(s); /* The '\0' at the end */
  }
  *key = s;
  KV_trim_string(*key);
}


/******************************************************************
 * KV_break_into_lower_key_and_value
 ******************************************************************
 * Looks for first br to break the string in two (if none, all
 * of string is key)
 *   throws out the br portion
 *   'trims' each remaining string
 *   converts the key to lowercase
 ******************************************************************/
void KV_break_into_lower_key_and_value(char * br, char * s,
				       char ** key, char ** value) {
  KV_break_into_key_and_value(br, s, key, value);
  KV_make_lower_string(*key);
}


/******************************************************************
 * KV_SZ_read_next_key_and_value
 ******************************************************************
 * returns true whenever it can read another non-null line from
 * the buffer.  It's not necessarily a key-value pair.  (For example,
 * 'end' will say 'end' is the key and the value is N/A.)  It then
 * fills in each passed sub-part.
 ******************************************************************/
int KV_SZ_read_next_key_and_value ( char **buffer, char *str, int maxl,
                                    char ** key, char ** value )
{
    int len;

    len = KV_SZ_readln_skip_comments_and_trim ( buffer, str, maxl );
    if ( len == 0 )
    {
        return ( 0 );    
    }
    
    KV_break_into_lower_key_and_value ( KV_KEY_VALUE_SPLIT_STRING, str,
                                        key, value );
    return ( 1 );
}


/******************************************************************
 * KV_read_next_key_and_value
 ******************************************************************
 * returns true whenever it can read another non-null line from
 * the file.  It's not necessarily a key-value pair.  (For example,
 * 'end' will say 'end' is the key and the value is N/A.)  It then
 * fills in each passed sub-part.
 ******************************************************************/
int KV_read_next_key_and_value(FILE * fptr, char * str, int maxl, char ** key, char ** value) {
  int len;

  len = KV_readln_skip_comments_and_trim(fptr, str, maxl);
  if (len==0) return(0);
  KV_break_into_lower_key_and_value(KV_KEY_VALUE_SPLIT_STRING, str, key, value);
  return(1);
}


/******************************************************************
 * KV_read_string_value_for_key
 ******************************************************************
 * rewinds the file, searches for first occurance of key,
 * returns value of the key
 * Returns 1 if it's found, 0 if it's not
 ******************************************************************/
int KV_read_string_value_for_key(FILE * fptr, char * key, char * value, int maxl) {
  char * local_str, *local_key, *local_value;

  local_str = (char *)MT_malloc(KV_MAX_KEYVAL_LINE_SIZE*sizeof(char));

  rewind(fptr);
  while (KV_read_next_key_and_value(fptr, local_str, KV_MAX_KEYVAL_LINE_SIZE,
				 &local_key, &local_value)) {
    if (!strcmp(local_key, key)) {
      if (strlen(local_value)<maxl) {
	strcpy(value, local_value);
      } else {
	strncpy(value, local_value, maxl-1);
	value[maxl-1] = '\0';
      }
      MT_free((void*)local_str);
      return(1); /* terminiate when first finds the key */
    }
  }

  MT_free((void*)local_str);
  return(0);
}


/******************************************************************
 * KV_SZ_read_string_value_for_key
 ******************************************************************
 * searches for first occurance of key in buffer,
 * returns value of the key
 * Returns 1 if it's found, 0 if it's not
 ******************************************************************/
int KV_SZ_read_string_value_for_key ( char *buffer, char *key,
                                      char *value, int maxl )
{
    char *local_str, *local_key, *local_value;
    char *tmpPtr;

    /* Use a temp pointer, so we don't lose the beginning of the array */
    tmpPtr = buffer;
    
    local_str
        = ( char * ) MT_malloc ( KV_MAX_KEYVAL_LINE_SIZE * sizeof ( char ) );

    while ( KV_SZ_read_next_key_and_value ( &tmpPtr, local_str,
                                            KV_MAX_KEYVAL_LINE_SIZE,
                                            &local_key, &local_value ) )
    {
        if ( !strcmp ( local_key, key ) )
        {
            if ( strlen ( local_value ) < maxl )
            {
                strcpy ( value, local_value );
            }
            else
            {
                strncpy ( value, local_value, maxl - 1 );
                value[maxl - 1] = '\0';
            }
            MT_free ( ( void * ) local_str );
            return ( 1 ); /* terminiate when first finds the key */
        }
    }

    MT_free ( ( void * ) local_str );
    return ( 0 );
}





/******************************************************************
 * KV_trim_string
 ******************************************************************
 * Based on length of passed string, removes all 'isspace'
 * characters at end of string and then those at the beginning.
 * If it removes characters at the beginning, it must translate
 * the string backwards.  Returns the length of the 'trimmed'
 * string.
 ******************************************************************/
int KV_trim_string(char * s) {
  int i, length, first_non_space;

  length = strlen(s);

  /* Trim the end of the string */
  while((length>0)&&(isspace(s[length-1]))) {
    length--;
    s[length] = '\0';
  }

  /* Now, trim the beginning of the string -- this will consist of
   * translating the string backwards a few bytes, as necessary
   */
  if (length>0) {
    first_non_space = 0;

    while((s[first_non_space]!='\0')&&(isspace(s[first_non_space])))
      first_non_space++;

    if (first_non_space>0) {
      for (i=first_non_space; i<=length; i++) {
	s[i-first_non_space] = s[i];
      }
      length -= first_non_space;
    }
  }

  return(length);
}


/******************************************************************
 * KV_make_lower_string
 ******************************************************************
 * Given a string, converts all uppercase characters found to
 * corresponding lowercase characters
 ******************************************************************/
void KV_make_lower_string(char *s) {
  int i;
  char ch;
  
  i=0;
  while (ch=s[i]) {
    s[i]=(char)tolower((int)ch);
    i++;
  }
}










