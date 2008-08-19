/*******************************************************************************
  mcubes_extended.c

  This file contains routines to extend the marching cubes algorithm.  It is
  an attempt to address some of the holes that can be created when certain
  cubes are neighboring.  Below is a list of how I numbered neighboring cubes:

  Right Neighbor  = 1
  Left Neighbor   = 2
  Front Neighbor  = 3
  Back Neighbor   = 4
  Top Neighbor    = 5
  Bottom Neighbor = 6


  MTC 3/1/99
*******************************************************************************/
#include "sera3d.h"

#define MAX_EXTRA_TRIS 12

/* Structure to hold added triangles */
typedef struct _extra_tris_t
{
    int triangles[MAX_EXTRA_TRIS*3];  
    int num_added;

}extra_tris_t;

/* Global */
static extra_tris_t extra;

/* Local Function Prototypes */
void init_extra_triangles           ( void );
void add_triangles                  ( int );
int  check_cube_for_extra_triangles ( int, int, int );


/*================================================================================
  Function:    build_triangles_for_special_cube

  Purpose:     Determines if a 'special case' cube has any neighbors that cause
               the cube to require extra triangles to close holes.

  Parameters:  cube_val - value of the 'special case' cube.
               neighbor[1-6] - values of the neighboring cubes.
	       tris - Array of triangles.
	       num_triangles - Current number of triangles in the cube.

  Returned:    Extra triangles are returned through tris parameter.
               In the return variable, a value is sent depending on which 
	       neighbors were causing holes.
               
  MTC 3/3/99
  ==============================================================================*/
int build_triangles_for_special_cube ( int cube_val, int neighbor1, int neighbor2, 
				       int neighbor3, int neighbor4, 
				       int neighbor5, int neighbor6, 
				       Cell_Triangle_t *tris, int *num_triangles )
{
    int retval = 0;
    int index = 0;
    int i, num_tris;

    num_tris = *num_triangles;

    init_extra_triangles ( );

    if ( check_cube_for_extra_triangles ( cube_val, neighbor1, 1 ) ) 
        retval |= BIT_1;
    if ( check_cube_for_extra_triangles ( cube_val, neighbor2, 2 ) ) 
        retval |= BIT_2;
    if ( check_cube_for_extra_triangles ( cube_val, neighbor3, 3 ) ) 
        retval |= BIT_3;
    if ( check_cube_for_extra_triangles ( cube_val, neighbor4, 4 ) ) 
        retval |= BIT_4;
    if ( check_cube_for_extra_triangles ( cube_val, neighbor5, 5 ) ) 
        retval |= BIT_5;
    if ( check_cube_for_extra_triangles ( cube_val, neighbor6, 6 ) ) 
        retval |= BIT_6;

    if ( retval )
    {
	for ( i = 0; i < extra.num_added; i++ )
        {
	    tris[num_tris].a = extra.triangles[i  ];
	    tris[num_tris].b = extra.triangles[i+1];
	    tris[num_tris].c = extra.triangles[i+2];

	    num_tris++;
	}
    }

    *num_triangles = num_tris;

    return ( retval );
}


/*================================================================================
  Function:    init_extra_triangles

  Purpose:     Initializes the global structure variable extra.

  Parameters:  None.

  Returned:    None.
               
  MTC 3/3/99
  ==============================================================================*/
void init_extra_triangles ( void )
{
    int i;

    for ( i = 0; i < MAX_EXTRA_TRIS*3; i++ )
    {
	extra.triangles[i] = -1;
    }

    extra.num_added = 0;
}


/*================================================================================
  Function:    add_triangles

  Purpose:     Adds triangles to the global triangle structure bases on which 
               side it receives.

  Parameters:  side - Which side to add the extra triangles to.

  Returned:    None.
               
  MTC 3/3/99
  ==============================================================================*/
void add_triangles ( int side )
{
    switch ( side )
      {
      case 1:  /* Right Side */
	{
	  extra.triangles[extra.num_added  ] = 10; 
	  extra.triangles[extra.num_added+1] = 5;  
	  extra.triangles[extra.num_added+2] = 9;  extra.num_added++;
	  extra.triangles[extra.num_added  ] = 10; 
	  extra.triangles[extra.num_added+1] = 9;  
	  extra.triangles[extra.num_added+2] = 1;  extra.num_added++;
	  break;
	}
      case 2:  /* Left Side */
	{
	  extra.triangles[extra.num_added  ] = 8;  
	  extra.triangles[extra.num_added+1] = 7;  
	  extra.triangles[extra.num_added+2] = 11; extra.num_added++;
	  extra.triangles[extra.num_added  ] = 8;  
	  extra.triangles[extra.num_added+1] = 11; 
	  extra.triangles[extra.num_added+2] = 2;  extra.num_added++;
	  break;
	}
      case 3:  /* Front Side */
	{
	  extra.triangles[extra.num_added  ] = 11; 
	  extra.triangles[extra.num_added+1] = 6;  
	  extra.triangles[extra.num_added+2] = 10; extra.num_added++;
	  extra.triangles[extra.num_added  ] = 11; 
	  extra.triangles[extra.num_added+1] = 10; 
	  extra.triangles[extra.num_added+2] = 2;  extra.num_added++;
	  break;
	}
      case 4:  /* Back Side */
	{
	  extra.triangles[extra.num_added  ] = 9;  
	  extra.triangles[extra.num_added+1] = 4;  
	  extra.triangles[extra.num_added+2] = 8;  extra.num_added++;
	  extra.triangles[extra.num_added  ] = 9;  
	  extra.triangles[extra.num_added+1] = 8;  
	  extra.triangles[extra.num_added+2] = 0;  extra.num_added++;
	  break;
	}
      case 5:  /* Top Side */
	{
	  extra.triangles[extra.num_added  ] = 7;  
	  extra.triangles[extra.num_added+1] = 4;  
	  extra.triangles[extra.num_added+2] = 5;  extra.num_added++;
	  extra.triangles[extra.num_added  ] = 7;  
	  extra.triangles[extra.num_added+1] = 5;  
	  extra.triangles[extra.num_added+2] = 6;  extra.num_added++;
	  break;
	}
      case 6:  /* Bottom Side */
	{
	  extra.triangles[extra.num_added  ] = 1;  
	  extra.triangles[extra.num_added+1] = 0;  
	  extra.triangles[extra.num_added+2] = 3;  extra.num_added++;
	  extra.triangles[extra.num_added  ] = 1;  
	  extra.triangles[extra.num_added+1] = 3;  
	  extra.triangles[extra.num_added+2] = 2;  extra.num_added++;
	  break;
	}
      default:
	{
	  printf ( "Invalide cube face: %d (must be 1-6 only)!\n", side );
	  break;
	}
      }
}


/*================================================================================
  Function:    check_cube_for_extra_triangles

  Purpose:     Given the cube_val, neighbor_val, and which side to look at, this
               function determines whether a hole will be formed.  If a hole
	       will be formed, triangles are added to the cube to fill it in.

  Parameters:  cube_val - value of cube in question.
               neighbor_val - value of the neighbor being considered.
	       side - represents which side the neighbor is on.

  Returned:    1 if triangles were added, 0 if not.
               
  MTC 3/3/99

  Note:  This function was generated using the code found in gen_neighbor_cases.c
  ==============================================================================*/
int check_cube_for_extra_triangles ( int cube_val, int neighbor_val, int side )
{
  int retval = 0;

  switch ( cube_val )
    {
    case 5:
      {
        switch ( side )
          {
          case 6:
            {
              switch ( neighbor_val )
                {
                case 87:
                case 91:
                case 93:
                case 94:
                case 95:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 10:
      {
        switch ( side )
          {
          case 6:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 171:
                case 173:
                case 174:
                case 175:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 18:
      {
        switch ( side )
          {
          case 4:
            {
              switch ( neighbor_val )
                {
                case 151:
                case 167:
                case 181:
                case 182:
                case 183:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 21:
      {
        switch ( side )
          {
          case 6:
            {
              switch ( neighbor_val )
                {
                case 87:
                case 91:
                case 93:
                case 94:
                case 95:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 22:
      {
        switch ( side )
          {
          case 4:
            {
              switch ( neighbor_val )
                {
                case 151:
                case 167:
                case 181:
                case 182:
                case 183:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 24:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 61:
                case 173:
                case 181:
                case 188:
                case 189:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 26:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 61:
                case 173:
                case 181:
                case 188:
                case 189:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 4:
            {
              switch ( neighbor_val )
                {
                case 151:
                case 167:
                case 181:
                case 182:
                case 183:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 171:
                case 173:
                case 174:
                case 175:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 28:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 61:
                case 173:
                case 181:
                case 188:
                case 189:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 30:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 61:
                case 173:
                case 181:
                case 188:
                case 189:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 4:
            {
              switch ( neighbor_val )
                {
                case 151:
                case 167:
                case 181:
                case 182:
                case 183:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 33:
      {
        switch ( side )
          {
          case 4:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 107:
                case 121:
                case 122:
                case 123:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 36:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 62:
                case 94:
                case 122:
                case 124:
                case 126:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 37:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 62:
                case 94:
                case 122:
                case 124:
                case 126:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 4:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 107:
                case 121:
                case 122:
                case 123:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 87:
                case 91:
                case 93:
                case 94:
                case 95:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 41:
      {
        switch ( side )
          {
          case 4:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 107:
                case 121:
                case 122:
                case 123:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 42:
      {
        switch ( side )
          {
          case 6:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 171:
                case 173:
                case 174:
                case 175:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 44:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 62:
                case 94:
                case 122:
                case 124:
                case 126:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 45:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 62:
                case 94:
                case 122:
                case 124:
                case 126:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 4:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 107:
                case 121:
                case 122:
                case 123:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 52:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 62:
                case 94:
                case 122:
                case 124:
                case 126:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 53:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 62:
                case 94:
                case 122:
                case 124:
                case 126:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 87:
                case 91:
                case 93:
                case 94:
                case 95:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 56:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 61:
                case 173:
                case 181:
                case 188:
                case 189:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 58:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 61:
                case 173:
                case 181:
                case 188:
                case 189:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 171:
                case 173:
                case 174:
                case 175:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 60:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 62:
                case 94:
                case 122:
                case 124:
                case 126:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 2:
            {
              switch ( neighbor_val )
                {
                case 61:
                case 173:
                case 181:
                case 188:
                case 189:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 66:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 199:
                case 227:
                case 229:
                case 231:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 67:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 199:
                case 227:
                case 229:
                case 231:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 69:
      {
        switch ( side )
          {
          case 6:
            {
              switch ( neighbor_val )
                {
                case 87:
                case 91:
                case 93:
                case 94:
                case 95:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 72:
      {
        switch ( side )
          {
          case 3:
            {
              switch ( neighbor_val )
                {
                case 109:
                case 173:
                case 229:
                case 233:
                case 237:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 73:
      {
        switch ( side )
          {
          case 3:
            {
              switch ( neighbor_val )
                {
                case 109:
                case 173:
                case 229:
                case 233:
                case 237:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 74:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 199:
                case 227:
                case 229:
                case 231:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 3:
            {
              switch ( neighbor_val )
                {
                case 109:
                case 173:
                case 229:
                case 233:
                case 237:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 171:
                case 173:
                case 174:
                case 175:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 75:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 199:
                case 227:
                case 229:
                case 231:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 3:
            {
              switch ( neighbor_val )
                {
                case 109:
                case 173:
                case 229:
                case 233:
                case 237:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 80:
      {
        switch ( side )
          {
          case 5:
            {
              switch ( neighbor_val )
                {
                case 117:
                case 181:
                case 213:
                case 229:
                case 245:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 81:
      {
        switch ( side )
          {
          case 5:
            {
              switch ( neighbor_val )
                {
                case 117:
                case 181:
                case 213:
                case 229:
                case 245:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 82:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 199:
                case 227:
                case 229:
                case 231:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 4:
            {
              switch ( neighbor_val )
                {
                case 151:
                case 167:
                case 181:
                case 182:
                case 183:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 117:
                case 181:
                case 213:
                case 229:
                case 245:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 83:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 199:
                case 227:
                case 229:
                case 231:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 117:
                case 181:
                case 213:
                case 229:
                case 245:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 84:
      {
        switch ( side )
          {
          case 5:
            {
              switch ( neighbor_val )
                {
                case 117:
                case 181:
                case 213:
                case 229:
                case 245:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 85:
      {
        switch ( side )
          {
          case 5:
            {
              switch ( neighbor_val )
                {
                case 117:
                case 181:
                case 213:
                case 229:
                case 245:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 87:
                case 91:
                case 93:
                case 94:
                case 95:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 86:
      {
        switch ( side )
          {
          case 4:
            {
              switch ( neighbor_val )
                {
                case 151:
                case 167:
                case 181:
                case 182:
                case 183:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 117:
                case 181:
                case 213:
                case 229:
                case 245:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 88:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 61:
                case 173:
                case 181:
                case 188:
                case 189:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 3:
            {
              switch ( neighbor_val )
                {
                case 109:
                case 173:
                case 229:
                case 233:
                case 237:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 117:
                case 181:
                case 213:
                case 229:
                case 245:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 89:
      {
        switch ( side )
          {
          case 3:
            {
              switch ( neighbor_val )
                {
                case 109:
                case 173:
                case 229:
                case 233:
                case 237:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 117:
                case 181:
                case 213:
                case 229:
                case 245:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 90:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 199:
                case 227:
                case 229:
                case 231:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 2:
            {
              switch ( neighbor_val )
                {
                case 61:
                case 173:
                case 181:
                case 188:
                case 189:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 3:
            {
              switch ( neighbor_val )
                {
                case 109:
                case 173:
                case 229:
                case 233:
                case 237:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 4:
            {
              switch ( neighbor_val )
                {
                case 151:
                case 167:
                case 181:
                case 182:
                case 183:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 117:
                case 181:
                case 213:
                case 229:
                case 245:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 171:
                case 173:
                case 174:
                case 175:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 92:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 61:
                case 173:
                case 181:
                case 188:
                case 189:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 117:
                case 181:
                case 213:
                case 229:
                case 245:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 97:
      {
        switch ( side )
          {
          case 4:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 107:
                case 121:
                case 122:
                case 123:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 101:
      {
        switch ( side )
          {
          case 4:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 107:
                case 121:
                case 122:
                case 123:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 87:
                case 91:
                case 93:
                case 94:
                case 95:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 104:
      {
        switch ( side )
          {
          case 3:
            {
              switch ( neighbor_val )
                {
                case 109:
                case 173:
                case 229:
                case 233:
                case 237:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 105:
      {
        switch ( side )
          {
          case 3:
            {
              switch ( neighbor_val )
                {
                case 109:
                case 173:
                case 229:
                case 233:
                case 237:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 4:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 107:
                case 121:
                case 122:
                case 123:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 106:
      {
        switch ( side )
          {
          case 3:
            {
              switch ( neighbor_val )
                {
                case 109:
                case 173:
                case 229:
                case 233:
                case 237:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 171:
                case 173:
                case 174:
                case 175:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 120:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 61:
                case 173:
                case 181:
                case 188:
                case 189:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 3:
            {
              switch ( neighbor_val )
                {
                case 109:
                case 173:
                case 229:
                case 233:
                case 237:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 129:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 203:
                case 211:
                case 218:
                case 219:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 131:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 203:
                case 211:
                case 218:
                case 219:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 132:
      {
        switch ( side )
          {
          case 3:
            {
              switch ( neighbor_val )
                {
                case 94:
                case 158:
                case 214:
                case 218:
                case 222:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 133:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 203:
                case 211:
                case 218:
                case 219:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 3:
            {
              switch ( neighbor_val )
                {
                case 94:
                case 158:
                case 214:
                case 218:
                case 222:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 87:
                case 91:
                case 93:
                case 94:
                case 95:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 134:
      {
        switch ( side )
          {
          case 3:
            {
              switch ( neighbor_val )
                {
                case 94:
                case 158:
                case 214:
                case 218:
                case 222:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 135:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 203:
                case 211:
                case 218:
                case 219:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 3:
            {
              switch ( neighbor_val )
                {
                case 94:
                case 158:
                case 214:
                case 218:
                case 222:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 138:
      {
        switch ( side )
          {
          case 6:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 171:
                case 173:
                case 174:
                case 175:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 146:
      {
        switch ( side )
          {
          case 4:
            {
              switch ( neighbor_val )
                {
                case 151:
                case 167:
                case 181:
                case 182:
                case 183:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 148:
      {
        switch ( side )
          {
          case 3:
            {
              switch ( neighbor_val )
                {
                case 94:
                case 158:
                case 214:
                case 218:
                case 222:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 149:
      {
        switch ( side )
          {
          case 3:
            {
              switch ( neighbor_val )
                {
                case 94:
                case 158:
                case 214:
                case 218:
                case 222:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 87:
                case 91:
                case 93:
                case 94:
                case 95:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 150:
      {
        switch ( side )
          {
          case 3:
            {
              switch ( neighbor_val )
                {
                case 94:
                case 158:
                case 214:
                case 218:
                case 222:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 4:
            {
              switch ( neighbor_val )
                {
                case 151:
                case 167:
                case 181:
                case 182:
                case 183:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 154:
      {
        switch ( side )
          {
          case 4:
            {
              switch ( neighbor_val )
                {
                case 151:
                case 167:
                case 181:
                case 182:
                case 183:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 171:
                case 173:
                case 174:
                case 175:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 160:
      {
        switch ( side )
          {
          case 5:
            {
              switch ( neighbor_val )
                {
                case 122:
                case 186:
                case 218:
                case 234:
                case 250:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 161:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 203:
                case 211:
                case 218:
                case 219:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 4:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 107:
                case 121:
                case 122:
                case 123:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 122:
                case 186:
                case 218:
                case 234:
                case 250:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 162:
      {
        switch ( side )
          {
          case 5:
            {
              switch ( neighbor_val )
                {
                case 122:
                case 186:
                case 218:
                case 234:
                case 250:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 163:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 203:
                case 211:
                case 218:
                case 219:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 122:
                case 186:
                case 218:
                case 234:
                case 250:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 164:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 62:
                case 94:
                case 122:
                case 124:
                case 126:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 3:
            {
              switch ( neighbor_val )
                {
                case 94:
                case 158:
                case 214:
                case 218:
                case 222:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 122:
                case 186:
                case 218:
                case 234:
                case 250:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 165:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 62:
                case 94:
                case 122:
                case 124:
                case 126:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 2:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 203:
                case 211:
                case 218:
                case 219:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 3:
            {
              switch ( neighbor_val )
                {
                case 94:
                case 158:
                case 214:
                case 218:
                case 222:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 4:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 107:
                case 121:
                case 122:
                case 123:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 122:
                case 186:
                case 218:
                case 234:
                case 250:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 87:
                case 91:
                case 93:
                case 94:
                case 95:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 166:
      {
        switch ( side )
          {
          case 3:
            {
              switch ( neighbor_val )
                {
                case 94:
                case 158:
                case 214:
                case 218:
                case 222:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 122:
                case 186:
                case 218:
                case 234:
                case 250:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 168:
      {
        switch ( side )
          {
          case 5:
            {
              switch ( neighbor_val )
                {
                case 122:
                case 186:
                case 218:
                case 234:
                case 250:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 169:
      {
        switch ( side )
          {
          case 4:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 107:
                case 121:
                case 122:
                case 123:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 122:
                case 186:
                case 218:
                case 234:
                case 250:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 170:
      {
        switch ( side )
          {
          case 5:
            {
              switch ( neighbor_val )
                {
                case 122:
                case 186:
                case 218:
                case 234:
                case 250:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 171:
                case 173:
                case 174:
                case 175:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 172:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 62:
                case 94:
                case 122:
                case 124:
                case 126:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 5:
            {
              switch ( neighbor_val )
                {
                case 122:
                case 186:
                case 218:
                case 234:
                case 250:
                  add_triangles ( 5 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 180:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 62:
                case 94:
                case 122:
                case 124:
                case 126:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 3:
            {
              switch ( neighbor_val )
                {
                case 94:
                case 158:
                case 214:
                case 218:
                case 222:
                  add_triangles ( 3 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 193:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 203:
                case 211:
                case 218:
                case 219:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 194:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 199:
                case 227:
                case 229:
                case 231:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 195:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 199:
                case 227:
                case 229:
                case 231:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 2:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 203:
                case 211:
                case 218:
                case 219:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 197:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 203:
                case 211:
                case 218:
                case 219:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 87:
                case 91:
                case 93:
                case 94:
                case 95:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 202:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 199:
                case 227:
                case 229:
                case 231:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 6:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 171:
                case 173:
                case 174:
                case 175:
                  add_triangles ( 6 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 210:
      {
        switch ( side )
          {
          case 1:
            {
              switch ( neighbor_val )
                {
                case 167:
                case 199:
                case 227:
                case 229:
                case 231:
                  add_triangles ( 1 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 4:
            {
              switch ( neighbor_val )
                {
                case 151:
                case 167:
                case 181:
                case 182:
                case 183:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    case 225:
      {
        switch ( side )
          {
          case 2:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 203:
                case 211:
                case 218:
                case 219:
                  add_triangles ( 2 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          case 4:
            {
              switch ( neighbor_val )
                {
                case 91:
                case 107:
                case 121:
                case 122:
                case 123:
                  add_triangles ( 4 );
                  retval = 1;
                  break;
                default:
                  break;
                }
            }
          default:
            {
              break;
            }
          }
      }
      break;
    default:
      {
        printf ( "Invalid cube number (%d)! Returning.\n", cube_val );
        break;
      }
    }

  return ( retval );
}
