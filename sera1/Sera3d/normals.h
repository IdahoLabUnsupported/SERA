#ifndef NORMALS_H
#define NORMALS_H

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define BIT1 1
#define BIT2 2
#define BIT3 4
#define BIT4 8
#define BIT5 16
#define BIT6 32
#define BIT7 64
#define BIT8 128

typedef struct _XYZ
{
    float x, y, z;
} XYZ;

/* Globals */
float ***mid_normals;
XYZ *midpoint_loc;

/* Function Prototypes */
int construct_midpoint_normal_array ( void );
void destroy_midpoint_normal_array ( void );
int find_normal_for_midpoint ( unsigned char *, int, int, int, int, int, 
			       int, int, int, float *, float *, float * );

#endif /* NORMALS_H */




