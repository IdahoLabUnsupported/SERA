/******************************************************************************
 *  normals.c
 *
 *  This file is used to find the normals for the verticies of triangles in an
 *  isosurface.  Here is a representation of the standard cube being used in this
 *  implementation of the marching cubes algorithm:
 *
 *              4 ---------- 4 ---------- 5 
 *             /|                        /|
 * 	      7 |                       5 |
 *           /  |                      /  | 
 *          7 ---------- 6 ---------- 6   | 
 *          |   |                     |   | 
 *          |   8                     |   9       z
 *          |   |                     |   |       |   y
 *          |   |                     |   |       |  /
 *          |   |                     |   |       | /
 *         11   |                    10   |       |/
 *	    |   |                     |   |        -----x
 *	    |   0 ---------- 0 -------|-- 1 
 *	    |  /                      |  /
 *	    | 3                       | 1
 *	    |/                        |/
 *	    3 ---------- 2 ---------- 2 
 *       (x,y,z)
 *	reference 
 *        point                    
 *
 *******************************************************************************/
#include "sera3d.h"

typedef struct _XYZ
{
    float x, y, z;
} XYZ;

/* Globals */
static float ***mid_normals;
static XYZ *midpoint_loc;

static int triTable[256][16] = {
    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 1, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {1, 8, 3, 9, 8, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {1, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 8, 3, 1, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {9, 2, 10, 0, 2, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {2, 8, 3, 2, 10, 8, 10, 9, 8, -1, -1, -1, -1, -1, -1, -1},
    {3, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 11, 2, 8, 11, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {1, 9, 0, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {1, 11, 2, 1, 9, 11, 9, 8, 11, -1, -1, -1, -1, -1, -1, -1},
    {3, 10, 1, 11, 10, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 10, 1, 0, 8, 10, 8, 11, 10, -1, -1, -1, -1, -1, -1, -1},
    {3, 9, 0, 3, 11, 9, 11, 10, 9, -1, -1, -1, -1, -1, -1, -1},
    {9, 8, 10, 10, 8, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {4, 3, 0, 7, 3, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 1, 9, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {4, 1, 9, 4, 7, 1, 7, 3, 1, -1, -1, -1, -1, -1, -1, -1},
    {1, 2, 10, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {3, 4, 7, 3, 0, 4, 1, 2, 10, -1, -1, -1, -1, -1, -1, -1},
    {9, 2, 10, 9, 0, 2, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1},
    {2, 10, 9, 2, 9, 7, 2, 7, 3, 7, 9, 4, -1, -1, -1, -1},
    {8, 4, 7, 3, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {11, 4, 7, 11, 2, 4, 2, 0, 4, -1, -1, -1, -1, -1, -1, -1},
    {9, 0, 1, 8, 4, 7, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1},
    {4, 7, 11, 9, 4, 11, 9, 11, 2, 9, 2, 1, -1, -1, -1, -1},
    {3, 10, 1, 3, 11, 10, 7, 8, 4, -1, -1, -1, -1, -1, -1, -1},
    {1, 11, 10, 1, 4, 11, 1, 0, 4, 7, 11, 4, -1, -1, -1, -1},
    {4, 7, 8, 9, 0, 11, 9, 11, 10, 11, 0, 3, -1, -1, -1, -1},
    {4, 7, 11, 4, 11, 9, 9, 11, 10, -1, -1, -1, -1, -1, -1, -1},
    {9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {9, 5, 4, 0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 5, 4, 1, 5, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {8, 5, 4, 8, 3, 5, 3, 1, 5, -1, -1, -1, -1, -1, -1, -1},
    {1, 2, 10, 9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {3, 0, 8, 1, 2, 10, 4, 9, 5, -1, -1, -1, -1, -1, -1, -1},
    {5, 2, 10, 5, 4, 2, 4, 0, 2, -1, -1, -1, -1, -1, -1, -1},
    {2, 10, 5, 3, 2, 5, 3, 5, 4, 3, 4, 8, -1, -1, -1, -1},
    {9, 5, 4, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 11, 2, 0, 8, 11, 4, 9, 5, -1, -1, -1, -1, -1, -1, -1},
    {0, 5, 4, 0, 1, 5, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1},
    {2, 1, 5, 2, 5, 8, 2, 8, 11, 4, 8, 5, -1, -1, -1, -1},
    {10, 3, 11, 10, 1, 3, 9, 5, 4, -1, -1, -1, -1, -1, -1, -1},
    {4, 9, 5, 0, 8, 1, 8, 10, 1, 8, 11, 10, -1, -1, -1, -1},
    {5, 4, 0, 5, 0, 11, 5, 11, 10, 11, 0, 3, -1, -1, -1, -1},
    {5, 4, 8, 5, 8, 10, 10, 8, 11, -1, -1, -1, -1, -1, -1, -1},
    {9, 7, 8, 5, 7, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {9, 3, 0, 9, 5, 3, 5, 7, 3, -1, -1, -1, -1, -1, -1, -1},
    {0, 7, 8, 0, 1, 7, 1, 5, 7, -1, -1, -1, -1, -1, -1, -1},
    {1, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {9, 7, 8, 9, 5, 7, 10, 1, 2, -1, -1, -1, -1, -1, -1, -1},
    {10, 1, 2, 9, 5, 0, 5, 3, 0, 5, 7, 3, -1, -1, -1, -1},
    {8, 0, 2, 8, 2, 5, 8, 5, 7, 10, 5, 2, -1, -1, -1, -1},
    {2, 10, 5, 2, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1, -1},
    {7, 9, 5, 7, 8, 9, 3, 11, 2, -1, -1, -1, -1, -1, -1, -1},
    {9, 5, 7, 9, 7, 2, 9, 2, 0, 2, 7, 11, -1, -1, -1, -1},
    {2, 3, 11, 0, 1, 8, 1, 7, 8, 1, 5, 7, -1, -1, -1, -1},
    {11, 2, 1, 11, 1, 7, 7, 1, 5, -1, -1, -1, -1, -1, -1, -1},
    {9, 5, 8, 8, 5, 7, 10, 1, 3, 10, 3, 11, -1, -1, -1, -1},
    {5, 7, 0, 5, 0, 9, 7, 11, 0, 1, 0, 10, 11, 10, 0, -1},
    {11, 10, 0, 11, 0, 3, 10, 5, 0, 8, 0, 7, 5, 7, 0, -1},
    {11, 10, 5, 7, 11, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {10, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 8, 3, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {9, 0, 1, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {1, 8, 3, 1, 9, 8, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1},
    {1, 6, 5, 2, 6, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {1, 6, 5, 1, 2, 6, 3, 0, 8, -1, -1, -1, -1, -1, -1, -1},
    {9, 6, 5, 9, 0, 6, 0, 2, 6, -1, -1, -1, -1, -1, -1, -1},
    {5, 9, 8, 5, 8, 2, 5, 2, 6, 3, 2, 8, -1, -1, -1, -1},
    {2, 3, 11, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {11, 0, 8, 11, 2, 0, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1},
    {0, 1, 9, 2, 3, 11, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1},
    {5, 10, 6, 1, 9, 2, 9, 11, 2, 9, 8, 11, -1, -1, -1, -1},
    {6, 3, 11, 6, 5, 3, 5, 1, 3, -1, -1, -1, -1, -1, -1, -1},
    {0, 8, 11, 0, 11, 5, 0, 5, 1, 5, 11, 6, -1, -1, -1, -1},
    {3, 11, 6, 0, 3, 6, 0, 6, 5, 0, 5, 9, -1, -1, -1, -1},
    {6, 5, 9, 6, 9, 11, 11, 9, 8, -1, -1, -1, -1, -1, -1, -1},
    {5, 10, 6, 4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {4, 3, 0, 4, 7, 3, 6, 5, 10, -1, -1, -1, -1, -1, -1, -1},
    {1, 9, 0, 5, 10, 6, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1},
    {10, 6, 5, 1, 9, 7, 1, 7, 3, 7, 9, 4, -1, -1, -1, -1},
    {6, 1, 2, 6, 5, 1, 4, 7, 8, -1, -1, -1, -1, -1, -1, -1},
    {1, 2, 5, 5, 2, 6, 3, 0, 4, 3, 4, 7, -1, -1, -1, -1},
    {8, 4, 7, 9, 0, 5, 0, 6, 5, 0, 2, 6, -1, -1, -1, -1},
    {7, 3, 9, 7, 9, 4, 3, 2, 9, 5, 9, 6, 2, 6, 9, -1},
    {3, 11, 2, 7, 8, 4, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1},
    {5, 10, 6, 4, 7, 2, 4, 2, 0, 2, 7, 11, -1, -1, -1, -1},
    {0, 1, 9, 4, 7, 8, 2, 3, 11, 5, 10, 6, -1, -1, -1, -1},
    {9, 2, 1, 9, 11, 2, 9, 4, 11, 7, 11, 4, 5, 10, 6, -1},
    {8, 4, 7, 3, 11, 5, 3, 5, 1, 5, 11, 6, -1, -1, -1, -1},
    {5, 1, 11, 5, 11, 6, 1, 0, 11, 7, 11, 4, 0, 4, 11, -1},
    {0, 5, 9, 0, 6, 5, 0, 3, 6, 11, 6, 3, 8, 4, 7, -1},
    {6, 5, 9, 6, 9, 11, 4, 7, 9, 7, 11, 9, -1, -1, -1, -1},
    {10, 4, 9, 6, 4, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {4, 10, 6, 4, 9, 10, 0, 8, 3, -1, -1, -1, -1, -1, -1, -1},
    {10, 0, 1, 10, 6, 0, 6, 4, 0, -1, -1, -1, -1, -1, -1, -1},
    {8, 3, 1, 8, 1, 6, 8, 6, 4, 6, 1, 10, -1, -1, -1, -1},
    {1, 4, 9, 1, 2, 4, 2, 6, 4, -1, -1, -1, -1, -1, -1, -1},
    {3, 0, 8, 1, 2, 9, 2, 4, 9, 2, 6, 4, -1, -1, -1, -1},
    {0, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {8, 3, 2, 8, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1, -1},
    {10, 4, 9, 10, 6, 4, 11, 2, 3, -1, -1, -1, -1, -1, -1, -1},
    {0, 8, 2, 2, 8, 11, 4, 9, 10, 4, 10, 6, -1, -1, -1, -1},
    {3, 11, 2, 0, 1, 6, 0, 6, 4, 6, 1, 10, -1, -1, -1, -1},
    {6, 4, 1, 6, 1, 10, 4, 8, 1, 2, 1, 11, 8, 11, 1, -1},
    {9, 6, 4, 9, 3, 6, 9, 1, 3, 11, 6, 3, -1, -1, -1, -1},
    {8, 11, 1, 8, 1, 0, 11, 6, 1, 9, 1, 4, 6, 4, 1, -1},
    {3, 11, 6, 3, 6, 0, 0, 6, 4, -1, -1, -1, -1, -1, -1, -1},
    {6, 4, 8, 11, 6, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {7, 10, 6, 7, 8, 10, 8, 9, 10, -1, -1, -1, -1, -1, -1, -1},
    {0, 7, 3, 0, 10, 7, 0, 9, 10, 6, 7, 10, -1, -1, -1, -1},
    {10, 6, 7, 1, 10, 7, 1, 7, 8, 1, 8, 0, -1, -1, -1, -1},
    {10, 6, 7, 10, 7, 1, 1, 7, 3, -1, -1, -1, -1, -1, -1, -1},
    {1, 2, 6, 1, 6, 8, 1, 8, 9, 8, 6, 7, -1, -1, -1, -1},
    {2, 6, 9, 2, 9, 1, 6, 7, 9, 0, 9, 3, 7, 3, 9, -1},
    {7, 8, 0, 7, 0, 6, 6, 0, 2, -1, -1, -1, -1, -1, -1, -1},
    {7, 3, 2, 6, 7, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {2, 3, 11, 10, 6, 8, 10, 8, 9, 8, 6, 7, -1, -1, -1, -1},
    {2, 0, 7, 2, 7, 11, 0, 9, 7, 6, 7, 10, 9, 10, 7, -1},
    {1, 8, 0, 1, 7, 8, 1, 10, 7, 6, 7, 10, 2, 3, 11, -1},
    {11, 2, 1, 11, 1, 7, 10, 6, 1, 6, 7, 1, -1, -1, -1, -1},
    {8, 9, 6, 8, 6, 7, 9, 1, 6, 11, 6, 3, 1, 3, 6, -1},
    {0, 9, 1, 11, 6, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {7, 8, 0, 7, 0, 6, 3, 11, 0, 11, 6, 0, -1, -1, -1, -1},
    {7, 11, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {7, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {3, 0, 8, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 1, 9, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {8, 1, 9, 8, 3, 1, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1},
    {10, 1, 2, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {1, 2, 10, 3, 0, 8, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1},
    {2, 9, 0, 2, 10, 9, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1},
    {6, 11, 7, 2, 10, 3, 10, 8, 3, 10, 9, 8, -1, -1, -1, -1},
    {7, 2, 3, 6, 2, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {7, 0, 8, 7, 6, 0, 6, 2, 0, -1, -1, -1, -1, -1, -1, -1},
    {2, 7, 6, 2, 3, 7, 0, 1, 9, -1, -1, -1, -1, -1, -1, -1},
    {1, 6, 2, 1, 8, 6, 1, 9, 8, 8, 7, 6, -1, -1, -1, -1},
    {10, 7, 6, 10, 1, 7, 1, 3, 7, -1, -1, -1, -1, -1, -1, -1},
    {10, 7, 6, 1, 7, 10, 1, 8, 7, 1, 0, 8, -1, -1, -1, -1},
    {0, 3, 7, 0, 7, 10, 0, 10, 9, 6, 10, 7, -1, -1, -1, -1},
    {7, 6, 10, 7, 10, 8, 8, 10, 9, -1, -1, -1, -1, -1, -1, -1},
    {6, 8, 4, 11, 8, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {3, 6, 11, 3, 0, 6, 0, 4, 6, -1, -1, -1, -1, -1, -1, -1},
    {8, 6, 11, 8, 4, 6, 9, 0, 1, -1, -1, -1, -1, -1, -1, -1},
    {9, 4, 6, 9, 6, 3, 9, 3, 1, 11, 3, 6, -1, -1, -1, -1},
    {6, 8, 4, 6, 11, 8, 2, 10, 1, -1, -1, -1, -1, -1, -1, -1},
    {1, 2, 10, 3, 0, 11, 0, 6, 11, 0, 4, 6, -1, -1, -1, -1},
    {4, 11, 8, 4, 6, 11, 0, 2, 9, 2, 10, 9, -1, -1, -1, -1},
    {10, 9, 3, 10, 3, 2, 9, 4, 3, 11, 3, 6, 4, 6, 3, -1},
    {8, 2, 3, 8, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1, -1},
    {0, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {1, 9, 0, 2, 3, 4, 2, 4, 6, 4, 3, 8, -1, -1, -1, -1},
    {1, 9, 4, 1, 4, 2, 2, 4, 6, -1, -1, -1, -1, -1, -1, -1},
    {8, 1, 3, 8, 6, 1, 8, 4, 6, 6, 10, 1, -1, -1, -1, -1},
    {10, 1, 0, 10, 0, 6, 6, 0, 4, -1, -1, -1, -1, -1, -1, -1},
    {4, 6, 3, 4, 3, 8, 6, 10, 3, 0, 3, 9, 10, 9, 3, -1},
    {10, 9, 4, 6, 10, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {4, 9, 5, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 8, 3, 4, 9, 5, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1},
    {5, 0, 1, 5, 4, 0, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1},
    {11, 7, 6, 8, 3, 4, 3, 5, 4, 3, 1, 5, -1, -1, -1, -1},
    {9, 5, 4, 10, 1, 2, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1},
    {6, 11, 7, 1, 2, 10, 0, 8, 3, 4, 9, 5, -1, -1, -1, -1},
    {7, 6, 11, 5, 4, 10, 4, 2, 10, 4, 0, 2, -1, -1, -1, -1},
    {3, 4, 8, 3, 5, 4, 3, 2, 5, 10, 5, 2, 11, 7, 6, -1},
    {7, 2, 3, 7, 6, 2, 5, 4, 9, -1, -1, -1, -1, -1, -1, -1},
    {9, 5, 4, 0, 8, 6, 0, 6, 2, 6, 8, 7, -1, -1, -1, -1},
    {3, 6, 2, 3, 7, 6, 1, 5, 0, 5, 4, 0, -1, -1, -1, -1},
    {6, 2, 8, 6, 8, 7, 2, 1, 8, 4, 8, 5, 1, 5, 8, -1},
    {9, 5, 4, 10, 1, 6, 1, 7, 6, 1, 3, 7, -1, -1, -1, -1},
    {1, 6, 10, 1, 7, 6, 1, 0, 7, 8, 7, 0, 9, 5, 4, -1},
    {4, 0, 10, 4, 10, 5, 0, 3, 10, 6, 10, 7, 3, 7, 10, -1},
    {7, 6, 10, 7, 10, 8, 5, 4, 10, 4, 8, 10, -1, -1, -1, -1},
    {6, 9, 5, 6, 11, 9, 11, 8, 9, -1, -1, -1, -1, -1, -1, -1},
    {3, 6, 11, 0, 6, 3, 0, 5, 6, 0, 9, 5, -1, -1, -1, -1},
    {0, 11, 8, 0, 5, 11, 0, 1, 5, 5, 6, 11, -1, -1, -1, -1},
    {6, 11, 3, 6, 3, 5, 5, 3, 1, -1, -1, -1, -1, -1, -1, -1},
    {1, 2, 10, 9, 5, 11, 9, 11, 8, 11, 5, 6, -1, -1, -1, -1},
    {0, 11, 3, 0, 6, 11, 0, 9, 6, 5, 6, 9, 1, 2, 10, -1},
    {11, 8, 5, 11, 5, 6, 8, 0, 5, 10, 5, 2, 0, 2, 5, -1},
    {6, 11, 3, 6, 3, 5, 2, 10, 3, 10, 5, 3, -1, -1, -1, -1},
    {5, 8, 9, 5, 2, 8, 5, 6, 2, 3, 8, 2, -1, -1, -1, -1},
    {9, 5, 6, 9, 6, 0, 0, 6, 2, -1, -1, -1, -1, -1, -1, -1},
    {1, 5, 8, 1, 8, 0, 5, 6, 8, 3, 8, 2, 6, 2, 8, -1},
    {1, 5, 6, 2, 1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {1, 3, 6, 1, 6, 10, 3, 8, 6, 5, 6, 9, 8, 9, 6, -1},
    {10, 1, 0, 10, 0, 6, 9, 5, 0, 5, 6, 0, -1, -1, -1, -1},
    {0, 3, 8, 5, 6, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {10, 5, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {11, 5, 10, 7, 5, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {11, 5, 10, 11, 7, 5, 8, 3, 0, -1, -1, -1, -1, -1, -1, -1},
    {5, 11, 7, 5, 10, 11, 1, 9, 0, -1, -1, -1, -1, -1, -1, -1},
    {10, 7, 5, 10, 11, 7, 9, 8, 1, 8, 3, 1, -1, -1, -1, -1},
    {11, 1, 2, 11, 7, 1, 7, 5, 1, -1, -1, -1, -1, -1, -1, -1},
    {0, 8, 3, 1, 2, 7, 1, 7, 5, 7, 2, 11, -1, -1, -1, -1},
    {9, 7, 5, 9, 2, 7, 9, 0, 2, 2, 11, 7, -1, -1, -1, -1},
    {7, 5, 2, 7, 2, 11, 5, 9, 2, 3, 2, 8, 9, 8, 2, -1},
    {2, 5, 10, 2, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1, -1},
    {8, 2, 0, 8, 5, 2, 8, 7, 5, 10, 2, 5, -1, -1, -1, -1},
    {9, 0, 1, 5, 10, 3, 5, 3, 7, 3, 10, 2, -1, -1, -1, -1},
    {9, 8, 2, 9, 2, 1, 8, 7, 2, 10, 2, 5, 7, 5, 2, -1},
    {1, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 8, 7, 0, 7, 1, 1, 7, 5, -1, -1, -1, -1, -1, -1, -1},
    {9, 0, 3, 9, 3, 5, 5, 3, 7, -1, -1, -1, -1, -1, -1, -1},
    {9, 8, 7, 5, 9, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {5, 8, 4, 5, 10, 8, 10, 11, 8, -1, -1, -1, -1, -1, -1, -1},
    {5, 0, 4, 5, 11, 0, 5, 10, 11, 11, 3, 0, -1, -1, -1, -1},
    {0, 1, 9, 8, 4, 10, 8, 10, 11, 10, 4, 5, -1, -1, -1, -1},
    {10, 11, 4, 10, 4, 5, 11, 3, 4, 9, 4, 1, 3, 1, 4, -1},
    {2, 5, 1, 2, 8, 5, 2, 11, 8, 4, 5, 8, -1, -1, -1, -1},
    {0, 4, 11, 0, 11, 3, 4, 5, 11, 2, 11, 1, 5, 1, 11, -1},
    {0, 2, 5, 0, 5, 9, 2, 11, 5, 4, 5, 8, 11, 8, 5, -1},
    {9, 4, 5, 2, 11, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {2, 5, 10, 3, 5, 2, 3, 4, 5, 3, 8, 4, -1, -1, -1, -1},
    {5, 10, 2, 5, 2, 4, 4, 2, 0, -1, -1, -1, -1, -1, -1, -1},
    {3, 10, 2, 3, 5, 10, 3, 8, 5, 4, 5, 8, 0, 1, 9, -1},
    {5, 10, 2, 5, 2, 4, 1, 9, 2, 9, 4, 2, -1, -1, -1, -1},
    {8, 4, 5, 8, 5, 3, 3, 5, 1, -1, -1, -1, -1, -1, -1, -1},
    {0, 4, 5, 1, 0, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {8, 4, 5, 8, 5, 3, 9, 0, 5, 0, 3, 5, -1, -1, -1, -1},
    {9, 4, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {4, 11, 7, 4, 9, 11, 9, 10, 11, -1, -1, -1, -1, -1, -1, -1},
    {0, 8, 3, 4, 9, 7, 9, 11, 7, 9, 10, 11, -1, -1, -1, -1},
    {1, 10, 11, 1, 11, 4, 1, 4, 0, 7, 4, 11, -1, -1, -1, -1},
    {3, 1, 4, 3, 4, 8, 1, 10, 4, 7, 4, 11, 10, 11, 4, -1},
    {4, 11, 7, 9, 11, 4, 9, 2, 11, 9, 1, 2, -1, -1, -1, -1},
    {9, 7, 4, 9, 11, 7, 9, 1, 11, 2, 11, 1, 0, 8, 3, -1},
    {11, 7, 4, 11, 4, 2, 2, 4, 0, -1, -1, -1, -1, -1, -1, -1},
    {11, 7, 4, 11, 4, 2, 8, 3, 4, 3, 2, 4, -1, -1, -1, -1},
    {2, 9, 10, 2, 7, 9, 2, 3, 7, 7, 4, 9, -1, -1, -1, -1},
    {9, 10, 7, 9, 7, 4, 10, 2, 7, 8, 7, 0, 2, 0, 7, -1},
    {3, 7, 10, 3, 10, 2, 7, 4, 10, 1, 10, 0, 4, 0, 10, -1},
    {1, 10, 2, 8, 7, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {4, 9, 1, 4, 1, 7, 7, 1, 3, -1, -1, -1, -1, -1, -1, -1},
    {4, 9, 1, 4, 1, 7, 0, 8, 1, 8, 7, 1, -1, -1, -1, -1},
    {4, 0, 3, 7, 4, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {4, 8, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {9, 10, 8, 10, 11, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {3, 0, 9, 3, 9, 11, 11, 9, 10, -1, -1, -1, -1, -1, -1, -1},
    {0, 1, 10, 0, 10, 8, 8, 10, 11, -1, -1, -1, -1, -1, -1, -1},
    {3, 1, 10, 11, 3, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {1, 2, 11, 1, 11, 9, 9, 11, 8, -1, -1, -1, -1, -1, -1, -1},
    {3, 0, 9, 3, 9, 11, 1, 2, 9, 2, 11, 9, -1, -1, -1, -1},
    {0, 2, 11, 8, 0, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {3, 2, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {2, 3, 8, 2, 8, 10, 10, 8, 9, -1, -1, -1, -1, -1, -1, -1},
    {9, 10, 2, 0, 9, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {2, 3, 8, 2, 8, 10, 0, 1, 8, 1, 10, 8, -1, -1, -1, -1},
    {1, 10, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {1, 3, 8, 9, 1, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 9, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {0, 3, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
    {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}};

static unsigned char special_cases[256] = {
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0,
  0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0,
  0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0,
  0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
  1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0,
  0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };


/* Local Prototypes */
void fill_midpoint_location_array ( void );
void fill_midpoint_normal_array ( void );
void find_midpoint_normal_sum ( int, int, float *, float *, float * );
void make_unit_normal ( float *, float *, float * );
void find_normal_vector ( float, float, float, float, float, float,
			  float, float, float, float *, float *, float * );


int get_marching_cube_polygons_for_8_cell ( main_gui_t *gui, Cell_Triangle_t *tris, int *num_triangles,
					    int x, int y, int z )
{
  int num_tris, i, retval = 0;
  unsigned char cube0, cube1, cube2, cube3, cube4, cube5, cube6;

  cube0 = gui->block.data[(z  )*gui->block.y_size*gui->block.x_size + (y  )*gui->block.x_size + (x  )];

  /* find the number of triangles in the cube */
  num_tris = 0;
  for (i = 0; triTable[cube0][i] != -1; i +=3 )
  {
      tris[num_tris].a = triTable[cube0][i];
      tris[num_tris].b = triTable[cube0][i+1];
      tris[num_tris].c = triTable[cube0][i+2];
      
      num_tris++;
  }

  if ( special_cases[cube0] )
  {
    if ( x+1 < gui->block.x_size )
        cube1 = gui->block.data[(z  )*gui->block.y_size*gui->block.x_size + (y  )*gui->block.x_size + (x+1)];
    else 
        cube1 = 0;
    
    if ( x-1 > 0 )
        cube2 = gui->block.data[(z  )*gui->block.y_size*gui->block.x_size + (y  )*gui->block.x_size + (x-1)];
    else
        cube2 = 0;
    
    if ( y-1 > 0 )
        cube3 = gui->block.data[(z  )*gui->block.y_size*gui->block.x_size + (y-1)*gui->block.x_size + (x  )];
    else
        cube3 = 0;  
    
    if ( y+1 < gui->block.y_size )
        cube4 = gui->block.data[(z  )*gui->block.y_size*gui->block.x_size + (y+1)*gui->block.x_size + (x  )];
    else
        cube4 = 0;
    
    if ( z+1 < gui->block.z_size )
        cube5 = gui->block.data[(z+1)*gui->block.y_size*gui->block.x_size + (y  )*gui->block.x_size + (x  )];
    else
        cube5 = 0;

    if ( z-1 > 0 )
        cube6 = gui->block.data[(z-1)*gui->block.y_size*gui->block.x_size + (y  )*gui->block.x_size + (x  )];
    else
        cube6 = 0;
    
      retval = build_triangles_for_special_cube ( cube0, cube1, cube2, cube3, cube4, cube5, cube6, tris, &num_tris );
  }

  *num_triangles = num_tris;

  return ( retval );
}


/*void get_marching_cube_polygons_for_8_cell(Cell_Triangle_t *tris, int *num_triangles, unsigned char cell)
{
  int num_tris;
  int i;
  
  num_tris = 0;
  for (i = 0; triTable[cell][i] != -1; i +=3 )
  {
      tris[num_tris].a = triTable[cell][i];
      tris[num_tris].b = triTable[cell][i+1];
      tris[num_tris].c = triTable[cell][i+2];
      
      num_tris++;
  }

  *num_triangles = num_tris;
}*/




/* ============================================================================
   Function:     construct_midpoint_normal_array

   Purpose:      Fills in midpoint normal array and midpoint locations array.

   Parmeters:    None.

   Returned:     1 if arrays are allocated correctly, 0 otherwise.
		 
   MTC 2/22/99
==============================================================================*/
int construct_midpoint_normal_array ( void )
{
    int i, j;
    
    midpoint_loc = ( XYZ * ) MT_malloc ( sizeof ( XYZ ) * 12 );

    fill_midpoint_location_array ( );

    mid_normals = ( float *** ) MT_malloc ( sizeof ( float ** ) * 12 );

    for ( i = 0; i < 12; i++ )
    {
	mid_normals[i] = ( float ** ) MT_malloc ( sizeof ( float * ) * 256 );
    }

    for ( i = 0; i < 12; i++ )
    {
	for ( j = 0; j < 256; j++ )
	{
	    mid_normals[i][j] = ( float * ) MT_malloc ( sizeof ( float ) * 3 );
	}
    }

    fill_midpoint_normal_array ( );

    return ( 1 );
}


/* ============================================================================
   Function:     destroy_midpoint_normal_array

   Purpose:      Frees midpoint normal array and midpoint locations array.

   Parmeters:    None.

   Returned:     None.
		 
   MTC 2/22/99
==============================================================================*/
void destroy_midpoint_normal_array ( void )
{
    int i, j;

    for ( i = 0; i < 12; i++ )
    {
	for ( j = 0; j < 256; j++ )
	{
	    MT_free ( (void *) mid_normals[i][j] );
	}
    }

    for ( i = 0; i < 12; i++ )
    {
	MT_free ( (void *) mid_normals[i] );
    }
    
    MT_free ( (void *) mid_normals );
    MT_free ( (void *) midpoint_loc );
}


/* ============================================================================
   Function:     fill_midpoint_location_array

   Purpose:      Fills the midpoint location array.

   Parmeters:    None.

   Returned:     None.
		 
   MTC 2/22/99
==============================================================================*/
void fill_midpoint_location_array ( void )
{
    midpoint_loc[0].x = 0.5;
    midpoint_loc[0].y = 0.0;
    midpoint_loc[0].z = 0.0;

    midpoint_loc[1].x = 1.0;
    midpoint_loc[1].y = 0.0;
    midpoint_loc[1].z = 0.5;

    midpoint_loc[2].x = 0.5;
    midpoint_loc[2].y = 0.0;
    midpoint_loc[2].z = 1.0;

    midpoint_loc[3].x = 0.0;
    midpoint_loc[3].y = 0.0;
    midpoint_loc[3].z = 0.5;

    midpoint_loc[4].x = 0.5;
    midpoint_loc[4].y = 1.0;
    midpoint_loc[4].z = 0.0;

    midpoint_loc[5].x = 1.0;
    midpoint_loc[5].y = 1.0;
    midpoint_loc[5].z = 0.5;

    midpoint_loc[6].x = 0.5;
    midpoint_loc[6].y = 1.0;
    midpoint_loc[6].z = 1.0;

    midpoint_loc[7].x = 0.0;
    midpoint_loc[7].y = 1.0;
    midpoint_loc[7].z = 0.5;

    midpoint_loc[8].x = 0.0;
    midpoint_loc[8].y = 0.5;
    midpoint_loc[8].z = 0.0;

    midpoint_loc[9].x = 1.0;
    midpoint_loc[9].y = 0.5;
    midpoint_loc[9].z = 0.0;

    midpoint_loc[10].x = 1.0;
    midpoint_loc[10].y = 0.5;
    midpoint_loc[10].z = 1.0;

    midpoint_loc[11].x = 0.0;
    midpoint_loc[11].y = 0.5;
    midpoint_loc[11].z = 1.0;
}  


/* ============================================================================
   Function:     fill_midpoint_normal_array

   Purpose:      Fills the midpoint normal array.

   Parmeters:    None.

   Returned:     None.
		 
   MTC 2/22/99
==============================================================================*/
void fill_midpoint_normal_array ( void )
{
    int i, j;
    float x, y, z;

    for ( i = 0; i < 12; i++ )
    {
	for ( j = 0; j < 256; j++ )
        {
	    find_midpoint_normal_sum ( i, j, &x, &y, &z );
	    mid_normals[i][j][0] = x;
	    mid_normals[i][j][1] = y;
	    mid_normals[i][j][2] = z;

	}
    }
}


/* ============================================================================
   Function:     find_midpoint_normal_sum

   Purpose:      Finds the sum of the normals to a midpoint in a cube.

   Parmeters:    midnum - midpoint number
                 cube_val - Index value of the cube.

   Returned:     norm_x, norm_y, norm_z - Normal sum
		 
   MTC 2/22/99
==============================================================================*/
void find_midpoint_normal_sum ( int midnum, int cube_val, float *norm_x, float *norm_y, float *norm_z )
{
    int i, j;
    float x, y, z;
    float previous[10][3];
    int count = 0;
    int add_to_normal;

    *norm_x = 0.0;
    *norm_y = 0.0;
    *norm_z = 0.0;

    for ( i = 0; triTable[cube_val][i] != -1; i += 3 ) 
    {
        /* Only count triangles using the specified midpoint */
        if ( ( triTable[cube_val][i  ] == midnum ) ||
	     ( triTable[cube_val][i+1] == midnum ) ||
	     ( triTable[cube_val][i+2] == midnum ) )
	{
	     find_normal_vector ( midpoint_loc[triTable[cube_val][i  ]].x, 
				  midpoint_loc[triTable[cube_val][i  ]].y,
				  midpoint_loc[triTable[cube_val][i  ]].z,
				  
				  midpoint_loc[triTable[cube_val][i+1]].x, 
				  midpoint_loc[triTable[cube_val][i+1]].y,
				  midpoint_loc[triTable[cube_val][i+1]].z,
				  
				  midpoint_loc[triTable[cube_val][i+2]].x, 
				  midpoint_loc[triTable[cube_val][i+2]].y,
				  midpoint_loc[triTable[cube_val][i+2]].z,
				  
				  &x, &y, &z);
	     
	     /* Check if this normal has already been added to the sum */
	     add_to_normal= 1;
	     for ( j = 0; j < count; j++ )
	     {
	         if ( x == previous[j][0] && y == previous[j][1] && z == previous[j][2] )
		 {
		     add_to_normal = 0;
		 }
	     }

	     if ( add_to_normal )
	     {
	         previous[count][0] = x;
	         previous[count][1] = y;
	         previous[count][2] = z;
		 count++;
		 
		 *norm_x += x;
		 *norm_y += y;
		 *norm_z += z;
	     }
	}
    }
}    


/* ============================================================================
   Function:     find_normal_for_midpoint

   Purpose:      Given a cube and a midpoint on the cube, this function finds
                 the vertex normal for the midpoint.  

		 How it works:
		 1. Determine the index value of the four surrounding cubes.
		    The cube1 value is passed in and the other three are 
		    calculated.
		 2. Using the index of the four cubes, get the sum of the normals
		    for the cube by using the array found in normals.h
		 3. Sum up the sums for the cubes and find the unit normal.

   Parmeters:    *data - The array of data representing the cubes in the isosurface.
                 midpoint - The midpoint of the cube we are looking at.
		 cube1_val - Index value of cube1.
		 x, y, z - Reference point for cube1.
		 xsize, ysize, zsize - max value in data array.
		 *normx, *normy, *normz - return values for he normal.

   Returned:     1 for success, 0 otherwise.
		 
   MTC 2/22/99
==============================================================================*/
int find_normal_for_midpoint ( unsigned char *data, int midpoint, int cube1_val,
			       int special_case, int x, int y, int z,
			       int xsize, int ysize, int zsize,
			       float *normx, float *normy, float *normz )
{
    static int point[18];
    int cube2_val = 0;
    int cube3_val = 0;
    int cube4_val = 0;

    int neighbor1, neighbor2, neighbor3, neighbor4, neighbor5, neighbor6;
    int retval;

    *normx = 0.0;
    *normy = 0.0;
    *normz = 0.0;

    if ( special_case )
    {
	neighbor1 = special_case & 1;
	neighbor2 = special_case & 2;
	neighbor3 = special_case & 4;	
	neighbor4 = special_case & 8;	
	neighbor5 = special_case & 16;	
	neighbor6 = special_case & 32;	
    }

    switch ( midpoint )
    {
    case 0:
      {	
	  if ( y + 1 >= ysize || z - 1 < 0 )
	  {
	      retval = 0;
              break;
          }

	  cube2_val = data[(z  )*ysize*xsize + (y+1)*xsize + (x  )];
	  cube3_val = data[(z-1)*ysize*xsize + (y  )*xsize + (x  )];
	  cube4_val = data[(z-1)*ysize*xsize + (y+1)*xsize + (x  )];
	  
	  *normx += mid_normals[0][cube1_val][0];
	  *normy += mid_normals[0][cube1_val][1];
	  *normz += mid_normals[0][cube1_val][2];
	  
	  *normx += mid_normals[2][cube2_val][0];
	  *normy += mid_normals[2][cube2_val][1];
	  *normz += mid_normals[2][cube2_val][2];

	  *normx += mid_normals[4][cube3_val][0];
	  *normy += mid_normals[4][cube3_val][1];
	  *normz += mid_normals[4][cube3_val][2];
	  
	  *normx += mid_normals[6][cube4_val][0];
	  *normy += mid_normals[6][cube4_val][1];
	  *normz += mid_normals[6][cube4_val][2];

	  if ( special_case )
	  {
	      if ( neighbor4 )
		  *normz += 1.0;
	      if ( neighbor6 )
		  *normy += 1.0;
	  }
	  
	  make_unit_normal ( normx, normy, normz );
	  retval = 1;
	  break;
      }
    case 1:
      {	
	  if ( x + 1 >= xsize || y + 1 >= ysize || z - 1 < 0 )
	  {
	      retval = 0;
              break;
          }

	  cube2_val = data[(z  )*ysize*xsize + (y  )*xsize + (x+1)];
	  cube3_val = data[(z-1)*ysize*xsize + (y  )*xsize + (x  )];
	  cube4_val = data[(z-1)*ysize*xsize + (y  )*xsize + (x+1)];
	  
	  *normx += mid_normals[1][cube1_val][0];
	  *normy += mid_normals[1][cube1_val][1];
	  *normz += mid_normals[1][cube1_val][2];

	  *normx += mid_normals[3][cube2_val][0];
	  *normy += mid_normals[3][cube2_val][1];
	  *normz += mid_normals[3][cube2_val][2];

	  *normx += mid_normals[5][cube3_val][0];
	  *normy += mid_normals[5][cube3_val][1];
	  *normz += mid_normals[5][cube3_val][2];
	  
	  *normx += mid_normals[7][cube4_val][0];
	  *normy += mid_normals[7][cube4_val][1];
	  *normz += mid_normals[7][cube4_val][2];
	  
	  if ( special_case )
	  {
	      if ( neighbor1 )
		  *normx -= 1.0;
	      if ( neighbor6 )
		  *normy += 1.0;
	  }

	  make_unit_normal ( normx, normy, normz );
	  retval = 1;
	  break;
      }
    case 2:
      {	
	  if ( y - 1 < 0 || z - 1 < 0 )
	  {
	      retval = 0;
              break;
          }

	  cube2_val = data[(z  )*ysize*xsize + (y-1)*xsize + (x  )];
	  cube3_val = data[(z-1)*ysize*xsize + (y-1)*xsize + (x  )];
	  cube4_val = data[(z-1)*ysize*xsize + (y  )*xsize + (x  )];
        
	  *normx += mid_normals[2][cube1_val][0];
	  *normy += mid_normals[2][cube1_val][1];
	  *normz += mid_normals[2][cube1_val][2];

	  *normx += mid_normals[0][cube2_val][0];
	  *normy += mid_normals[0][cube2_val][1];
	  *normz += mid_normals[0][cube2_val][2];

	  *normx += mid_normals[4][cube3_val][0];
	  *normy += mid_normals[4][cube3_val][1];
	  *normz += mid_normals[4][cube3_val][2];
	  
	  *normx += mid_normals[6][cube4_val][0];
	  *normy += mid_normals[6][cube4_val][1];
	  *normz += mid_normals[6][cube4_val][2];

	  if ( special_case )
	  {
	      if ( neighbor3 )
		  *normz -= 1.0;
	      if ( neighbor6 )
		  *normy += 1.0;
	  }
	  
	  make_unit_normal ( normx, normy, normz );
	  retval = 1;
	  break;
      }
    case 3:
      {	
	  if ( z - 1 < 0 || x - 1 < 0 )
	  {
	      retval = 0;
              break;
          }

	  cube2_val = data[(z  )*ysize*xsize + (y  )*xsize + (x-1)];
	  cube3_val = data[(z-1)*ysize*xsize + (y  )*xsize + (x-1)];
	  cube4_val = data[(z-1)*ysize*xsize + (y  )*xsize + (x  )];
	  
	  *normx += mid_normals[3][cube1_val][0];
	  *normy += mid_normals[3][cube1_val][1];
	  *normz += mid_normals[3][cube1_val][2];

	  *normx += mid_normals[1][cube2_val][0];
	  *normy += mid_normals[1][cube2_val][1];
	  *normz += mid_normals[1][cube2_val][2];

	  *normx += mid_normals[5][cube3_val][0];
	  *normy += mid_normals[5][cube3_val][1];
	  *normz += mid_normals[5][cube3_val][2];
	  
	  *normx += mid_normals[7][cube4_val][0];
	  *normy += mid_normals[7][cube4_val][1];
	  *normz += mid_normals[7][cube4_val][2];
	  
	  if ( special_case )
	  {
	      if ( neighbor2 )
		  *normx += 1.0;
	      if ( neighbor6 )
		  *normy += 1.0;
	  }

	  make_unit_normal ( normx, normy, normz );
	  retval = 1;
	  break;
      }
    case 4:
      {	
	  if ( y + 1 >= ysize || z + 1 >= zsize )
	  {
	      retval = 0;
              break;
          }

	  cube2_val = data[(z+1)*ysize*xsize + (y  )*xsize + (x  )];
	  cube3_val = data[(z+1)*ysize*xsize + (y+1)*xsize + (x  )];
	  cube4_val = data[(z  )*ysize*xsize + (y+1)*xsize + (x  )];
	  
	  *normx += mid_normals[4][cube1_val][0];
	  *normy += mid_normals[4][cube1_val][1];
	  *normz += mid_normals[4][cube1_val][2];

	  *normx += mid_normals[0][cube2_val][0];
	  *normy += mid_normals[0][cube2_val][1];
	  *normz += mid_normals[0][cube2_val][2];

	  *normx += mid_normals[2][cube3_val][0];
	  *normy += mid_normals[2][cube3_val][1];
	  *normz += mid_normals[2][cube3_val][2];
	  
	  *normx += mid_normals[6][cube4_val][0];
	  *normy += mid_normals[6][cube4_val][1];
	  *normz += mid_normals[6][cube4_val][2];

	  if ( special_case )
	  {
	      if ( neighbor4 )
		  *normz += 1.0;
	      if ( neighbor5 )
		  *normy -= 1.0;
	  }
	  
	  make_unit_normal ( normx, normy, normz );
	  retval = 1;
	  break;
      }
    case 5:
      {	
	  if ( x + 1 >= xsize || z + 1 >= zsize )
	  {
	      retval = 0;
              break;
          }

	  cube2_val = data[(z+1)*ysize*xsize + (y  )*xsize + (x  )];
	  cube3_val = data[(z+1)*ysize*xsize + (y  )*xsize + (x+1)];
	  cube4_val = data[(z  )*ysize*xsize + (y  )*xsize + (x+1)];
	  
	  *normx += mid_normals[5][cube1_val][0];
	  *normy += mid_normals[5][cube1_val][1];
	  *normz += mid_normals[5][cube1_val][2];
	  
	  *normx += mid_normals[1][cube2_val][0];
	  *normy += mid_normals[1][cube2_val][1];
	  *normz += mid_normals[1][cube2_val][2];

	  *normx += mid_normals[3][cube3_val][0];
	  *normy += mid_normals[3][cube3_val][1];
	  *normz += mid_normals[3][cube3_val][2];
	  
	  *normx += mid_normals[7][cube4_val][0];
	  *normy += mid_normals[7][cube4_val][1];
	  *normz += mid_normals[7][cube4_val][2];
	  
	  if ( special_case )
	  {
	      if ( neighbor1 )
		  *normx -= 1.0;
	      if ( neighbor5 )
		  *normy -= 1.0;
	  }

	  make_unit_normal ( normx, normy, normz );
	  retval = 1;
	  break;
      }
    case 6:
      {	
	  if ( z + 1 >= zsize || y - 1 < 0 )
	  {
	      retval = 0;
              break;
          }

	  cube2_val = data[(z+1)*ysize*xsize + (y  )*xsize + (x  )];
	  cube3_val = data[(z  )*ysize*xsize + (y-1)*xsize + (x  )];
	  cube4_val = data[(z+1)*ysize*xsize + (y-1)*xsize + (x  )];
	  
	  *normx += mid_normals[6][cube1_val][0];
	  *normy += mid_normals[6][cube1_val][1];
	  *normz += mid_normals[6][cube1_val][2];
	  
	  *normx += mid_normals[2][cube2_val][0];
	  *normy += mid_normals[2][cube2_val][1];
	  *normz += mid_normals[2][cube2_val][2];

	  *normx += mid_normals[4][cube3_val][0];
	  *normy += mid_normals[4][cube3_val][1];
	  *normz += mid_normals[4][cube3_val][2];
	  
	  *normx += mid_normals[0][cube4_val][0];
	  *normy += mid_normals[0][cube4_val][1];
	  *normz += mid_normals[0][cube4_val][2];
	  
	  if ( special_case )
	  {
	      if ( neighbor3 )
		  *normz -= 1.0;
	      if ( neighbor5 )
		  *normy -= 1.0;
	  }

	  make_unit_normal ( normx, normy, normz );
	  retval = 1;
	  break;
      }
    case 7:
      {	
	  if ( z + 1 >= zsize || x - 1 < 0 )
	  {
	      retval = 0;
              break;
          }

	  cube2_val = data[(z+1)*ysize*xsize + (y  )*xsize + (x  )];
	  cube3_val = data[(z  )*ysize*xsize + (y  )*xsize + (x-1)];
	  cube4_val = data[(z+1)*ysize*xsize + (y  )*xsize + (x-1)];
	  
	  *normx += mid_normals[7][cube1_val][0];
	  *normy += mid_normals[7][cube1_val][1];
	  *normz += mid_normals[7][cube1_val][2];
	  
	  *normx += mid_normals[3][cube2_val][0];
	  *normy += mid_normals[3][cube2_val][1];
	  *normz += mid_normals[3][cube2_val][2];

	  *normx += mid_normals[5][cube3_val][0];
	  *normy += mid_normals[5][cube3_val][1];
	  *normz += mid_normals[5][cube3_val][2];
	  
	  *normx += mid_normals[1][cube4_val][0];
	  *normy += mid_normals[1][cube4_val][1];
	  *normz += mid_normals[1][cube4_val][2];

	  if ( special_case )
	  {
	      if ( neighbor2 )
		  *normx += 1.0;
	      if ( neighbor5 )
		  *normy -= 1.0;
	  }	  

	  make_unit_normal ( normx, normy, normz );
	  retval = 1;
	  break;
      }
    case 8:
      {	
	  if ( y + 1 >= ysize || x - 1 < 0 )
	  {
	      retval = 0;
              break;
          }

	  cube2_val = data[(z  )*ysize*xsize + (y  )*xsize + (x-1)];
	  cube3_val = data[(z  )*ysize*xsize + (y+1)*xsize + (x  )];
	  cube4_val = data[(z  )*ysize*xsize + (y+1)*xsize + (x-1)];
	  
	  *normx += mid_normals[8][cube1_val][0];
	  *normy += mid_normals[8][cube1_val][1];
	  *normz += mid_normals[8][cube1_val][2];

	  *normx += mid_normals[9][cube2_val][0];
	  *normy += mid_normals[9][cube2_val][1];
	  *normz += mid_normals[9][cube2_val][2];

	  *normx += mid_normals[11][cube3_val][0];
	  *normy += mid_normals[11][cube3_val][1];
	  *normz += mid_normals[11][cube3_val][2];
	  
	  *normx += mid_normals[10][cube4_val][0];
	  *normy += mid_normals[10][cube4_val][1];
	  *normz += mid_normals[10][cube4_val][2];
	  
	  if ( special_case )
	  {
	      if ( neighbor4 )
		  *normz += 1.0;
	      if ( neighbor2 )
		  *normx += 1.0;
	  }

	  make_unit_normal ( normx, normy, normz );
	  retval = 1;
	  break;
      }
    case 9:
      {	
	  if ( x + 1 >= xsize || y + 1 >= ysize )
	  {
	      retval = 0;
              break;
	  }

	  cube2_val = data[(z  )*ysize*xsize + (y  )*xsize + (x+1)];
	  cube3_val = data[(z  )*ysize*xsize + (y+1)*xsize + (x+1)];
	  cube4_val = data[(z  )*ysize*xsize + (y+1)*xsize + (x  )];
	  
	  *normx += mid_normals[9][cube1_val][0];
	  *normy += mid_normals[9][cube1_val][1];
	  *normz += mid_normals[9][cube1_val][2];

	  *normx += mid_normals[8][cube2_val][0];
	  *normy += mid_normals[8][cube2_val][1];
	  *normz += mid_normals[8][cube2_val][2];

	  *normx += mid_normals[11][cube3_val][0];
	  *normy += mid_normals[11][cube3_val][1];
	  *normz += mid_normals[11][cube3_val][2];
	  
	  *normx += mid_normals[10][cube4_val][0];
	  *normy += mid_normals[10][cube4_val][1];
	  *normz += mid_normals[10][cube4_val][2];
	  
	  if ( special_case )
	  {
	      if ( neighbor4 )
		  *normz += 1.0;
	      if ( neighbor1 )
		  *normx -= 1.0;
	  }

	  make_unit_normal ( normx, normy, normz );
	  retval = 1;
	  break;
      }
    case 10:
      {	
	  if ( x + 1 >= xsize || y - 1 < 0 )
	  {
	      retval = 0;
              break;
          }

	  cube2_val = data[(z  )*ysize*xsize + (y-1)*xsize + (x  )];
	  cube3_val = data[(z  )*ysize*xsize + (y  )*xsize + (x+1)];
	  cube4_val = data[(z  )*ysize*xsize + (y-1)*xsize + (x+1)];
	  
	  *normx += mid_normals[10][cube1_val][0];
	  *normy += mid_normals[10][cube1_val][1];
	  *normz += mid_normals[10][cube1_val][2];

	  *normx += mid_normals[9][cube2_val][0];
	  *normy += mid_normals[9][cube2_val][1];
	  *normz += mid_normals[9][cube2_val][2];

	  *normx += mid_normals[11][cube3_val][0];
	  *normy += mid_normals[11][cube3_val][1];
	  *normz += mid_normals[11][cube3_val][2];
	  
	  *normx += mid_normals[8][cube4_val][0];
	  *normy += mid_normals[8][cube4_val][1];
	  *normz += mid_normals[8][cube4_val][2];
	  
	  if ( special_case )
	  {
	      if ( neighbor3 )
		  *normz -= 1.0;
	      if ( neighbor1 )
		  *normx -= 1.0;
	  }

	  make_unit_normal ( normx, normy, normz );
	  retval = 1;
	  break;
      }
    case 11:
      {	
	  if ( x - 1 < 0 || y - 1 < 0 )
	  {
	      retval = 0;
              break;
          }

	  cube2_val = data[(z  )*ysize*xsize + (y-1)*xsize + (x  )];
	  cube3_val = data[(z  )*ysize*xsize + (y-1)*xsize + (x-1)];
	  cube4_val = data[(z  )*ysize*xsize + (y  )*xsize + (x-1)];
	  
	  *normx += mid_normals[11][cube1_val][0];
	  *normy += mid_normals[11][cube1_val][1];
	  *normz += mid_normals[11][cube1_val][2];

	  *normx += mid_normals[8][cube2_val][0];
	  *normy += mid_normals[8][cube2_val][1];
	  *normz += mid_normals[8][cube2_val][2];

	  *normx += mid_normals[9][cube3_val][0];
	  *normy += mid_normals[9][cube3_val][1];
	  *normz += mid_normals[9][cube3_val][2];
	  
	  *normx += mid_normals[10][cube4_val][0];
	  *normy += mid_normals[10][cube4_val][1];
	  *normz += mid_normals[10][cube4_val][2];
	  
	  if ( special_case )
	  {
	      if ( neighbor2 )
		  *normx += 1.0;
	      if ( neighbor3 )
		  *normz -= 1.0;
	  }

	  make_unit_normal ( normx, normy, normz );
	  retval = 1;
	  break;
      }
    default:
      {
	  printf ( "Invalid midpoint (%d)! Returning.\n", midpoint );
	  retval = 0;
	  break;
      }
    }

    return ( retval );
}


/* Takes a normal of any length and makes it have length 1 */
void make_unit_normal ( float *x, float *y, float *z )
{
    float mag;

    mag = sqrt ( (*x)*(*x) + (*y)*(*y) + (*z)*(*z) );

    if ( (int)(mag+0.5) == 0 )
    {
	*x = 0.0;
	*y = 0.0;
	*z = 0.0;
    }
    else
    {
	*x = (*x)/mag;
	*y = (*y)/mag;
	*z = (*z)/mag;
    }
}


/* Finds normal to a triangle... not unit normal */
void find_normal_vector ( float x1, float y1, float z1,
			  float x2, float y2, float z2,
			  float x3, float y3, float z3,
			  float *normx, float *normy, float *normz )
{
   float ux, uy, uz, vx, vy, vz, wx, wy, wz, wnorm;
   /*u=p1-p2   v=p3-p2    w=u x v     wnorm=||w||   */

   ux = x1-x2;
   uy = y1-y2;           /*p1-p2*/
   uz = z1-z2;

   vx = x3-x2;
   vy = y3-y2;           /*p3-p2*/
   vz = z3-z2;

   wx = uy*vz-uz*vy;
   wy = uz*vx-ux*vz;     /*u cross v*/
   wz = ux*vy-uy*vx;

   *normx = -wx;
   *normy = -wy;
   *normz = -wz;
}
