#include <math.h>

#include "transform.h"

void fswap (float *x, float *y)
{
  float temp;

  temp = *x;
  *x = *y;
  *y = temp;

}




void scale (float c[], float sx, float sy, float sz)
{
  c[0] *= sx;
  c[1] *= sy;
  c[2] *= sz;
}




void translate (float c[], float dx, float dy, float dz)
{
  c[0] += dx;
  c[1] += dy;
  c[2] += dz;
}




void rotate_x (float c[], float theta)
{
  float t1, t2;

  t1 = c[1] * cos((double)theta) - c[2]*sin((double)theta);
  t2 = c[1] * sin((double)theta) + c[2] * cos((double)theta);

  c[1] = t1;
  c[2] = t2;

}





void rotate_z (float c[], float theta)
{
  float t1, t2;

  t1 = c[0] * cos((double)theta) - c[1]*sin((double)theta);
  t2 = c[0] * sin((double)theta) + c[1] * cos((double)theta);

  c[0] = t1;
  c[1] = t2;

}





void display (float c[], char *message)
{
  /* printf ("%s\n\t%f\t%f\t%f\n\n", c[0], c[1], c[2]); */
  printf ("%s\n\t%f\t%f\t%f\n\n", message, c[0], c[1], c[2]);
}



