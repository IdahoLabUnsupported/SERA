#ifndef KEYVAL_TOOLS_H
#define KEYVAL_TOOLS_H

void KV_set_split_characters(char *);
int KV_readln(FILE *, char *, int);
int KV_readln_skip_comments_and_trim(FILE *, char *, int);
void KV_break_into_key_and_value(char *, char *, char **, char **);
void KV_break_into_lower_key_and_value(char *, char *, char **, char **);
int KV_read_next_key_and_value(FILE *, char *, int, char **, char **);
int KV_read_string_value_for_key(FILE *, char *, char *, int);
int KV_trim_string(char *);
void KV_make_lower_string(char *); 

/* New functions for use with seraZip */
int KV_SZ_readln ( char **, char *, int );
int KV_SZ_readln_skip_comments_and_trim ( char **, char *, int );
int KV_SZ_read_next_key_and_value ( char **, char *, int, char **, char ** );
int KV_SZ_read_string_value_for_key ( char *, char *, char *, int );



#endif
