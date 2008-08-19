#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "libuv.h"

void main(int argc, char **argv) {
  geom_info_t geom;
  float zval=0.0; /* --> symmetric about center */
  int i, j;
  int cpn=4;
  float cp[]={-10.0, -10.0,
  -10.0, 10.0,
  10.0, 10.0,
  10.0, -10.0};
  int cpn2=4;
  float cp2[]={-8.0, -8.0,
  -8.0, 8.0,
  8.0, 8.0,
  8.0, -8.0};
  int cpn3=4;
  float cp3[]={-8.0, -8.0,
  -8.0, 6.0,
  6.0, 6.0,
  6.0, -6.0};
  int cpn4=4;
  float cp4[]={0.0, 2.0,
  4.0, 0.0,
  0.0, -4.0,
  -4.0, 0.0};
  int cpn5=18;
  float cp5[36];

  for (i=0; i<cpn5; i++) {
    cp5[2*i]=(float)10.0*cos((double)(6.28*(float)i/(float)cpn5));
    cp5[2*i+1]=(float)10.0*sin((double)(6.28*(float)i/(float)cpn5));
  }
  
  printf("Trying read_uvh\n");
  read_uvh(&geom, "test.uvh");
  printf("Trying initialize_empty_slices\n");
  initialize_empty_slices(&geom);
  printf("Trying cp_to_vox\n");
  for (i=0; i<3; i++) {
    cp_to_vox(&geom, cpn, cp, 1, "'scalp'", zval);
    cp_to_vox(&geom, cpn2, cp2, 2, "'skull'", zval);
    cp_to_vox(&geom, cpn3, cp3, 3, "'brain'", zval);
    cp_to_vox(&geom, cpn4, cp4, 4, "'tumor'", zval);
    if (i!=0) {
      cp_to_vox(&geom, cpn, cp, 1, "'scalp'", -zval);
      cp_to_vox(&geom, cpn2, cp2, 2, "'skull'", -zval);
      cp_to_vox(&geom, cpn3, cp3, 3, "'brain'", -zval);
      cp_to_vox(&geom, cpn4, cp4, 4, "'tumor'", -zval);
    }
    for (j=0; j<2*cpn; j++) {
      cp[j]/=1.5;
      cp2[j]/=1.5;
      cp3[j]/=1.5;
      cp4[j]/=1.5;
    }
    zval+=1.0;
  }
  cp_to_vox(&geom, cpn5, cp5, 5, "'circle'", zval);
  printf("Trying make_regions\n");
  make_regions(&geom);
  printf("Trying write_uvh\n");
  write_uvh(&geom, "output.uvh");
  printf("Trying write_uv\n");
  write_uv(&geom, "output.uv");
  printf("Trying free_geom\n");
  free_geom(&geom);
  printf("Completed successfully!\n");
  exit(EXIT_SUCCESS);
}
