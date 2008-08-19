/*******************************************************************************
  gen_neighbor_cases.c

  This file contains the functions which are used to generate the GIANT case
  statement found in mcubes_extended.c under the function 
  check_cube_for_extra_triangles ( ).  It needs to be compiled and run and then
  it will create a file called extended_cases.txt with the function written 
  in the new file.

  The neighbors are numbered in the following order:

  Right Neighbor  = 1
  Left Neighbor   = 2
  Front Neighbor  = 3
  Back Neighbor   = 4
  Top Neighbor    = 5
  Bottom Neighbor = 6

  MTC 3/3/99
*******************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define MAX_COMPLIMENTS 6

#define BIT_1 1
#define BIT_2 2
#define BIT_3 4
#define BIT_4 8
#define BIT_5 16
#define BIT_6 32
#define BIT_7 64
#define BIT_8 128

/* Globals */
static unsigned char special_cases[256];
static int compliments[256][6][MAX_COMPLIMENTS];

/* Function Prototypes */
void write_special_cases_to_file ( void );
void initialize_special_cases ( void );
void initialize_compliments ( void );
void check_for_special_case ( int );
void print_results ( void );
void get_bit_values ( int, unsigned char * );

void main ( void )
{
    int i;

    initialize_special_cases ( );
    initialize_compliments ( );

    for ( i = 0; i < 256; i++ )
      {
	check_for_special_case ( i );
      }

    /* print_results ( ); */
    write_special_cases_to_file ( );
}

void print_results ( void )
{
    int i, j, k, m, n = 0;
    unsigned char bit[8];

    for ( i = 0; i < 256; i++ )
      {
	if ( special_cases[i] )
	  {
	    n++;
	    get_bit_values ( i, bit );
	    printf ( "%d. Cube(%d) ", n, i );
	    for ( m = 0; m < 8; m++ )
	        printf ( "%d", bit[m] );
	    printf ( " compliments:\n" );
	    for ( j = 0; j < 6; j++ )
	      {
		printf ( "{ " );
		for ( k = 0; k < MAX_COMPLIMENTS; k++ )
		  {
		    if ( compliments[i][j][k] != -1 )
		      {
			get_bit_values ( compliments[i][j][k], bit );
			for ( m = 0; m < 8; m++ )
			  printf ( "%d", bit[m] );
			printf ( ", " );
		      }
		  }
		printf ( " }\n" );
	      }
	  }
      }
}

void write_special_cases_to_file ( void )
{
    int i, j, k, m, n;
    FILE *outfile;
    char buffer[256];
    char temp[256];

    outfile = fopen ( "extended_cases.txt", "w" );

    fputs ( "int check_cube_for_extra_triangles ( int cube_val, int neighbor_val, int side )\n", outfile );
    fputs ( "{\n  int retval = 0;\n\n  switch ( cube_val )\n    {\n", outfile );

    for ( i = 0; i < 256; i++ )
      {
	if ( special_cases[i] )
	  {
	    sprintf ( buffer, "    case %d:\n      {\n", i );
	    fputs ( buffer, outfile );
	    fputs ( "        switch ( side )\n          {\n", outfile );

	    for ( j = 0; j < 6; j ++ )
	      {
		if ( special_cases[i] & ((int)pow(2, j)) )
		  {
		    sprintf ( buffer, "          case %d:\n            {\n", j+1 );
		    fputs ( buffer, outfile );
		    fputs ( "              switch ( neighbor_val )\n                {\n", outfile );
		    for ( k = 0; k < MAX_COMPLIMENTS; k++ )
		      {
			if ( compliments[i][j][k] != -1 )
			  {
			    sprintf ( buffer, "                case %d:\n", compliments[i][j][k] );
			    fputs ( buffer, outfile );
			  }
		      }
		    sprintf ( buffer, "                  add_triangles ( %d );\n", j+1 );
		    fputs ( buffer, outfile );
		    fputs ( "                  retval = 1;\n", outfile );
		    fputs ( "                  break;\n", outfile );
		    fputs ( "                default:\n", outfile );
		    fputs ( "                  break;\n", outfile );
		    fputs ( "                }\n", outfile );
		    fputs ( "            }\n", outfile );		
		  }
	      }

	    fputs ( "          default:\n", outfile );
	    fputs ( "            {\n", outfile );
	    fputs ( "              break;\n", outfile );
	    fputs ( "            }\n", outfile );

	    fputs ( "          }\n", outfile );
	    fputs ( "      }\n", outfile );
	    fputs ( "      break;\n", outfile );
	  }
      }

    fputs ( "    default:\n", outfile );
    fputs ( "      {\n", outfile );
    fputs ( "        printf ( \"Invalid cube number! Returning.\\n\" );\n", outfile );
    fputs ( "        break;\n", outfile );
    fputs ( "      }\n", outfile );
    fputs ( "    }\n\n", outfile );

    fputs ( "  return ( retval );\n}\n", outfile );

    fclose ( outfile );
}



void initialize_special_cases ( void )
{
    memset ( special_cases, 0, 256 );
}

void initialize_compliments ( void )
{
    int i, j, k;
    
    for ( i = 0; i < 256; i++ )
      for ( j = 0; j < 6; j++ )
	for ( k = 0; k < MAX_COMPLIMENTS; k++ )
	  {
	    compliments[i][j][k] = -1;
	  }
}


void get_bit_values ( int cube_val, unsigned char *bit )
{
    bit[0] = (cube_val&1)   ? 1 : 0;
    bit[1] = (cube_val&2)   ? 1 : 0;
    bit[2] = (cube_val&4)   ? 1 : 0;
    bit[3] = (cube_val&8)   ? 1 : 0;
    bit[4] = (cube_val&16)  ? 1 : 0;
    bit[5] = (cube_val&32)  ? 1 : 0;
    bit[6] = (cube_val&64)  ? 1 : 0;
    bit[7] = (cube_val&128) ? 1 : 0;
}


void check_for_special_case ( int cube_val )
{
    unsigned char bit[8];
    int i, j, k;

    /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      RIGHT SIDE
      >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

    get_bit_values ( cube_val, bit );

    if ( bit[1] && bit[6] && !bit[2] && !bit[5] )
      {
	if ( bit[0] && bit[3] && bit[4] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( !bit[0] && bit[3] && bit[4] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && !bit[3] && bit[4] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && bit[3] && !bit[4] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && bit[3] && bit[4] && !bit[7] )
	  special_cases[cube_val] = 0;
	else
	  {
	    special_cases[cube_val] |= BIT_1;

	    /* Find all neighboring cubes that cause the hole */
	    k = 0;
	    for ( j = 0; j < 256; j++ )
	      {
		get_bit_values ( j, bit );
	    
		if ( bit[0] && bit[7] && !bit[3] && !bit[4] )
		  {
		    if ( bit[1] && bit[2] && bit[5] && bit[6] )
		      {
		        compliments[cube_val][0][k] = j;
			k++;
		      }
		    else if ( !bit[1] && bit[2] && bit[5] && bit[6] )
		      {		     
			compliments[cube_val][0][k] = j;
			k++;
		      }
		    else if ( bit[1] && !bit[2] && bit[5] && bit[6] )
		      {		       
			compliments[cube_val][0][k] = j;
			k++;
		      }
		    else if ( bit[1] && bit[2] && !bit[5] && bit[6] )
		      { 
			compliments[cube_val][0][k] = j;
			k++;
		      }
		    else if ( bit[1] && bit[2] && bit[5] && !bit[6] )
		      {		      
			compliments[cube_val][0][k] = j;
			k++;
		      }
		  }
	      }
	  }
      }
    else if ( !bit[1] && !bit[6] && bit[2] && bit[5] )
      {
	if ( bit[0] && bit[3] && bit[4] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( !bit[0] && bit[3] && bit[4] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && !bit[3] && bit[4] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && bit[3] && !bit[4] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && bit[3] && bit[4] && !bit[7] )
	  special_cases[cube_val] = 0;
	else
	  {
	    special_cases[cube_val] |= BIT_1;

	    /* Find all neighboring cubes that cause the hole */
	    k = 0;
	    for ( j = 0; j < 256; j++ )
	      {
		get_bit_values ( j, bit );
	    
		if ( !bit[0] && !bit[7] && bit[3] && bit[4] )
		  {
		    if ( bit[1] && bit[2] && bit[5] && bit[6] )
		      {
		        compliments[cube_val][0][k] = j;
			k++;
		      }
		    else if ( !bit[1] && bit[2] && bit[5] && bit[6] )
		      {		     
			compliments[cube_val][0][k] = j;
			k++;
		      }
		    else if ( bit[1] && !bit[2] && bit[5] && bit[6] )
		      {		       
			compliments[cube_val][0][k] = j;
			k++;
		      }
		    else if ( bit[1] && bit[2] && !bit[5] && bit[6] )
		      { 
			compliments[cube_val][0][k] = j;
			k++;
		      }
		    else if ( bit[1] && bit[2] && bit[5] && !bit[6] )
		      {		      
			compliments[cube_val][0][k] = j;
			k++;
		      }
		  }
	      }
	  }
      }

    /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      LEFT SIDE
      >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

    get_bit_values ( cube_val, bit );

    if ( bit[0] && bit[7] && !bit[3] && !bit[4] )
      {
	if ( bit[1] && bit[2] && bit[5] && bit[6] )
	  special_cases[cube_val] = 0;
	else if ( !bit[1] && bit[2] && bit[5] && bit[6] )
	  special_cases[cube_val] = 0;
	else if ( bit[1] && !bit[2] && bit[5] && bit[6] )
	  special_cases[cube_val] = 0;
	else if ( bit[1] && bit[2] && !bit[5] && bit[6] )
	  special_cases[cube_val] = 0;
	else if ( bit[1] && bit[2] && bit[5] && !bit[6] )
	  special_cases[cube_val] = 0;
	else
	  {
	    special_cases[cube_val] |= BIT_2;

	    /* Find all neighboring cubes that cause the hole */
	    k = 0;
	    for ( j = 0; j < 256; j++ )
	      {
		get_bit_values ( j, bit );
	    
		if ( bit[1] && bit[6] && !bit[2] && !bit[5] )
		  {
		    if ( bit[0] && bit[3] && bit[4] && bit[7] )
		      {
		        compliments[cube_val][1][k] = j;
			k++;
		      }
		    else if ( !bit[0] && bit[3] && bit[4] && bit[7] )
		      {		     
			compliments[cube_val][1][k] = j;
			k++;
		      }
		    else if ( bit[0] && !bit[3] && bit[4] && bit[7] )
		      {		       
			compliments[cube_val][1][k] = j;
			k++;
		      }
		    else if ( bit[0] && bit[3] && !bit[4] && bit[7] )
		      { 
			compliments[cube_val][1][k] = j;
			k++;
		      }
		    else if ( bit[0] && bit[3] && bit[4] && !bit[7] )
		      {		      
			compliments[cube_val][1][k] = j;
			k++;
		      }
		  }
	      }
	  }
      }
    else if ( !bit[0] && !bit[7] && bit[3] && bit[4] )
      {
	if ( bit[1] && bit[2] && bit[5] && bit[6] )
	  special_cases[cube_val] = 0;
	else if ( !bit[1] && bit[2] && bit[5] && bit[6] )
	  special_cases[cube_val] = 0;
	else if ( bit[1] && !bit[2] && bit[5] && bit[6] )
	  special_cases[cube_val] = 0;
	else if ( bit[1] && bit[2] && !bit[5] && bit[6] )
	  special_cases[cube_val] = 0;
	else if ( bit[1] && bit[2] && bit[5] && !bit[6] )
	  special_cases[cube_val] = 0;
	else
	  {
	    special_cases[cube_val] |= BIT_2;

	    /* Find all neighboring cubes that cause the hole */
	    k = 0;
	    for ( j = 0; j < 256; j++ )
	      {
		get_bit_values ( j, bit );
	    
		if ( !bit[1] && !bit[6] && bit[2] && bit[5] )
		  {
		    if ( bit[0] && bit[3] && bit[4] && bit[7] )
		      {
		        compliments[cube_val][1][k] = j;
			k++;
		      }
		    else if ( !bit[0] && bit[3] && bit[4] && bit[7] )
		      {		     
			compliments[cube_val][1][k] = j;
			k++;
		      }
		    else if ( bit[0] && !bit[3] && bit[4] && bit[7] )
		      {		       
			compliments[cube_val][1][k] = j;
			k++;
		      }
		    else if ( bit[0] && bit[3] && !bit[4] && bit[7] )
		      { 
			compliments[cube_val][1][k] = j;
			k++;
		      }
		    else if ( bit[0] && bit[3] && bit[4] && !bit[7] )
		      {		      
			compliments[cube_val][1][k] = j;
			k++;
		      }
		  }
	      }
	  }
      }

    /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      FRONT SIDE
      >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

    get_bit_values ( cube_val, bit );

    if ( bit[2] && bit[7] && !bit[3] && !bit[6] )
      {
	if ( bit[0] && bit[1] && bit[4] && bit[5] )
	  special_cases[cube_val] = 0;
	else if ( !bit[0] && bit[1] && bit[4] && bit[5] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && !bit[1] && bit[4] && bit[5] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && bit[1] && !bit[4] && bit[5] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && bit[1] && bit[4] && !bit[5] )
	  special_cases[cube_val] = 0;
	else
	  {
	    special_cases[cube_val] |= BIT_3;

	    /* Find all neighboring cubes that cause the hole */
	    k = 0;
	    for ( j = 0; j < 256; j++ )
	      {
		get_bit_values ( j, bit );
	    
		if ( bit[1] && bit[4] && !bit[0] && !bit[5] )
		  {
		    if ( bit[2] && bit[3] && bit[6] && bit[7] )
		      {
		        compliments[cube_val][2][k] = j;
			k++;
		      }
		    else if ( !bit[2] && bit[3] && bit[6] && bit[7] )
		      {		     
			compliments[cube_val][2][k] = j;
			k++;
		      }
		    else if ( bit[2] && !bit[3] && bit[6] && bit[7] )
		      {		       
			compliments[cube_val][2][k] = j;
			k++;
		      }
		    else if ( bit[2] && bit[3] && !bit[6] && bit[7] )
		      { 
			compliments[cube_val][2][k] = j;
			k++;
		      }
		    else if ( bit[2] && bit[3] && bit[6] && !bit[7] )
		      {		      
			compliments[cube_val][2][k] = j;
			k++;
		      }
		  }
	      }
	  }
      }
    else if ( !bit[2] && !bit[7] && bit[3] && bit[6] )
      {
	if ( bit[0] && bit[1] && bit[4] && bit[5] )
	  special_cases[cube_val] = 0;
	else if ( !bit[0] && bit[1] && bit[4] && bit[5] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && !bit[1] && bit[4] && bit[5] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && bit[1] && !bit[4] && bit[5] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && bit[1] && bit[4] && !bit[5] )
	  special_cases[cube_val] = 0;
	else
	  {
	    special_cases[cube_val] |= BIT_3;

	    /* Find all neighboring cubes that cause the hole */
	    k = 0;
	    for ( j = 0; j < 256; j++ )
	      {
		get_bit_values ( j, bit );
	    
		if ( !bit[1] && !bit[4] && bit[0] && bit[5] )
		  {
		    if ( bit[2] && bit[3] && bit[6] && bit[7] )
		      {
		        compliments[cube_val][2][k] = j;
			k++;
		      }
		    else if ( !bit[2] && bit[3] && bit[6] && bit[7] )
		      {		     
			compliments[cube_val][2][k] = j;
			k++;
		      }
		    else if ( bit[2] && !bit[3] && bit[6] && bit[7] )
		      {		       
			compliments[cube_val][2][k] = j;
			k++;
		      }
		    else if ( bit[2] && bit[3] && !bit[6] && bit[7] )
		      { 
			compliments[cube_val][2][k] = j;
			k++;
		      }
		    else if ( bit[2] && bit[3] && bit[6] && !bit[7] )
		      {		      
			compliments[cube_val][2][k] = j;
			k++;
		      }
		  }
	      }
	  }
      }


    /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      BACK SIDE
      >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

    get_bit_values ( cube_val, bit );

    if ( bit[0] && bit[5] && !bit[1] && !bit[4] )
      {
	if ( bit[2] && bit[3] && bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( !bit[2] && bit[3] && bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[2] && !bit[3] && bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[2] && bit[3] && !bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[2] && bit[3] && bit[6] && !bit[7] )
	  special_cases[cube_val] = 0;
	else
	  {
	    special_cases[cube_val] |= BIT_4;

	    /* Find all neighboring cubes that cause the hole */
	    k = 0;
	    for ( j = 0; j < 256; j++ )
	      {
		get_bit_values ( j, bit );
	    
		if ( bit[3] && bit[6] && !bit[2] && !bit[7] )
		  {
		    if ( bit[0] && bit[1] && bit[4] && bit[5] )
		      {
		        compliments[cube_val][3][k] = j;
			k++;
		      }
		    else if ( !bit[0] && bit[1] && bit[4] && bit[5] )
		      {		     
			compliments[cube_val][3][k] = j;
			k++;
		      }
		    else if ( bit[0] && !bit[1] && bit[4] && bit[5] )
		      {		       
			compliments[cube_val][3][k] = j;
			k++;
		      }
		    else if ( bit[0] && bit[1] && !bit[4] && bit[5] )
		      { 
			compliments[cube_val][3][k] = j;
			k++;
		      }
		    else if ( bit[0] && bit[1] && bit[4] && !bit[5] )
		      {		      
			compliments[cube_val][3][k] = j;
			k++;
		      }
		  }
	      }
	  }
      }
    else if ( !bit[0] && !bit[5] && bit[1] && bit[4] )
      {
	if ( bit[2] && bit[3] && bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( !bit[2] && bit[3] && bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[2] && !bit[3] && bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[2] && bit[3] && !bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[2] && bit[3] && bit[6] && !bit[7] )
	  special_cases[cube_val] = 0;
	else
	  {
	    special_cases[cube_val] |= BIT_4;

	    /* Find all neighboring cubes that cause the hole */
	    k = 0;
	    for ( j = 0; j < 256; j++ )
	      {
		get_bit_values ( j, bit );
	    
		if ( !bit[3] && !bit[6] && bit[2] && bit[7] )
		  {
		    if ( bit[0] && bit[1] && bit[4] && bit[5] )
		      {
		        compliments[cube_val][3][k] = j;
			k++;
		      }
		    else if ( !bit[0] && bit[1] && bit[4] && bit[5] )
		      {		     
			compliments[cube_val][3][k] = j;
			k++;
		      }
		    else if ( bit[0] && !bit[1] && bit[4] && bit[5] )
		      {		       
			compliments[cube_val][3][k] = j;
			k++;
		      }
		    else if ( bit[0] && bit[1] && !bit[4] && bit[5] )
		      { 
			compliments[cube_val][3][k] = j;
			k++;
		      }
		    else if ( bit[0] && bit[1] && bit[4] && !bit[5] )
		      {		      
			compliments[cube_val][3][k] = j;
			k++;
		      }
		  }
	      }
	  }
      }

    /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      TOP SIDE
      >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

    get_bit_values ( cube_val, bit );

    if ( bit[4] && bit[6] && !bit[5] && !bit[7] )
      {
	if ( bit[0] && bit[1] && bit[2] && bit[3] )
	  special_cases[cube_val] = 0;
	else if ( !bit[0] && bit[1] && bit[2] && bit[3] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && !bit[1] && bit[2] && bit[3] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && bit[1] && !bit[2] && bit[3] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && bit[1] && bit[2] && !bit[3] )
	  special_cases[cube_val] = 0;
	else
	  {
	    special_cases[cube_val] |= BIT_5;

	    /* Find all neighboring cubes that cause the hole */
	    k = 0;
	    for ( j = 0; j < 256; j++ )
	      {
		get_bit_values ( j, bit );
	    
		if ( bit[0] && bit[2] && !bit[1] && !bit[3] )
		  {
		    if ( bit[4] && bit[5] && bit[6] && bit[7] )
		      {
		        compliments[cube_val][4][k] = j;
			k++;
		      }
		    else if ( !bit[4] && bit[5] && bit[6] && bit[7] )
		      {		     
			compliments[cube_val][4][k] = j;
			k++;
		      }
		    else if ( bit[4] && !bit[5] && bit[6] && bit[7] )
		      {		       
			compliments[cube_val][4][k] = j;
			k++;
		      }
		    else if ( bit[4] && bit[5] && !bit[6] && bit[7] )
		      { 
			compliments[cube_val][4][k] = j;
			k++;
		      }
		    else if ( bit[4] && bit[5] && bit[6] && !bit[7] )
		      {		      
			compliments[cube_val][4][k] = j;
			k++;
		      }
		  }
	      }
	  }
      }
    else if ( !bit[4] && !bit[6] && bit[5] && bit[7] )
      {
	if ( bit[0] && bit[1] && bit[2] && bit[3] )
	  special_cases[cube_val] = 0;
	else if ( !bit[0] && bit[1] && bit[2] && bit[3] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && !bit[1] && bit[2] && bit[3] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && bit[1] && !bit[2] && bit[3] )
	  special_cases[cube_val] = 0;
	else if ( bit[0] && bit[1] && bit[2] && !bit[3] )
	  special_cases[cube_val] = 0;
	else
	  {
	    special_cases[cube_val] |= BIT_5;

	    /* Find all neighboring cubes that cause the hole */
	    k = 0;
	    for ( j = 0; j < 256; j++ )
	      {
		get_bit_values ( j, bit );
	    
		if ( !bit[0] && !bit[2] && bit[1] && bit[3] )
		  {
		    if ( bit[4] && bit[5] && bit[6] && bit[7] )
		      {
		        compliments[cube_val][4][k] = j;
			k++;
		      }
		    else if ( !bit[4] && bit[5] && bit[6] && bit[7] )
		      {		     
			compliments[cube_val][4][k] = j;
			k++;
		      }
		    else if ( bit[4] && !bit[5] && bit[6] && bit[7] )
		      {		       
			compliments[cube_val][4][k] = j;
			k++;
		      }
		    else if ( bit[4] && bit[5] && !bit[6] && bit[7] )
		      { 
			compliments[cube_val][4][k] = j;
			k++;
		      }
		    else if ( bit[4] && bit[5] && bit[6] && !bit[7] )
		      {		      
			compliments[cube_val][4][k] = j;
			k++;
		      }
		  }
	      }
	  }
      }

    /*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      BOTTOM SIDE
      >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*/

    get_bit_values ( cube_val, bit );

    if ( bit[0] && bit[2] && !bit[1] && !bit[3] )
      {
	if ( bit[4] && bit[5] && bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( !bit[4] && bit[5] && bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[4] && !bit[5] && bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[4] && bit[5] && !bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[4] && bit[5] && bit[6] && !bit[7] )
	  special_cases[cube_val] = 0;
	else
	  {
	    special_cases[cube_val] |= BIT_6;

	    /* Find all neighboring cubes that cause the hole */
	    k = 0;
	    for ( j = 0; j < 256; j++ )
	      {
		get_bit_values ( j, bit );
	    
		if ( bit[4] && bit[6] && !bit[5] && !bit[7] )
		  {
		    if ( bit[0] && bit[1] && bit[2] && bit[3] )
		      {
		        compliments[cube_val][5][k] = j;
			k++;
		      }
		    else if ( !bit[0] && bit[1] && bit[2] && bit[3] )
		      {		     
			compliments[cube_val][5][k] = j;
			k++;
		      }
		    else if ( bit[0] && !bit[1] && bit[2] && bit[3] )
		      {		       
			compliments[cube_val][5][k] = j;
			k++;
		      }
		    else if ( bit[0] && bit[1] && !bit[2] && bit[3] )
		      { 
			compliments[cube_val][5][k] = j;
			k++;
		      }
		    else if ( bit[0] && bit[1] && bit[2] && !bit[3] )
		      {		      
			compliments[cube_val][5][k] = j;
			k++;
		      }
		  }
	      }
	  }
      }
    else if ( !bit[0] && !bit[2] && bit[1] && bit[3] )
      {
	if ( bit[4] && bit[5] && bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( !bit[4] && bit[5] && bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[4] && !bit[5] && bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[4] && bit[5] && !bit[6] && bit[7] )
	  special_cases[cube_val] = 0;
	else if ( bit[4] && bit[5] && bit[6] && !bit[7] )
	  special_cases[cube_val] = 0;
	else
	  {
	    special_cases[cube_val] |= BIT_6;

	    /* Find all neighboring cubes that cause the hole */
	    k = 0;
	    for ( j = 0; j < 256; j++ )
	      {
		get_bit_values ( j, bit );
	    
		if ( !bit[4] && !bit[6] && bit[5] && bit[7] )
		  {
		    if ( bit[0] && bit[1] && bit[2] && bit[3] )
		      {
		        compliments[cube_val][5][k] = j;
			k++;
		      }
		    else if ( !bit[0] && bit[1] && bit[2] && bit[3] )
		      {		     
			compliments[cube_val][5][k] = j;
			k++;
		      }
		    else if ( bit[0] && !bit[1] && bit[2] && bit[3] )
		      {		       
			compliments[cube_val][5][k] = j;
			k++;
		      }
		    else if ( bit[0] && bit[1] && !bit[2] && bit[3] )
		      { 
			compliments[cube_val][5][k] = j;
			k++;
		      }
		    else if ( bit[0] && bit[1] && bit[2] && !bit[3] )
		      {		      
			compliments[cube_val][5][k] = j;
			k++;
		      }
		  }
	      }
	  }
      }

}
