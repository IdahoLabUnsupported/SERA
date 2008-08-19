#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#define INTERSECT_CODE
#include "libuv.h"

void get_info(char*,int);
void intersection_tests();
clock_t random_intersections(int, int, float, int, int, char *);

/* Don't use the extern total_intersections.  Ignore it from now on. */
int num_intersections=0;
extern int univels_hit;
float distance_traveled=0.0;

/**************************************************************************
 * void get_info
 **************************************************************************
 * Gets information from user in order to load a 3D dataset
 *
 * Parameters:
 *  name      --> returns name of file to load
 */
void get_info(char *name,int size) {
    printf("Enter input filename.\n");
    name[0] = '\0';
    do {
	fgets(name,size,stdin);
    } while (name[0]=='\0');
}




void intersection_tests() {
    int cont = 0;
    float vx, vy, vz, x0, y0, z0, size;
    double dist, pos[3], dir[3];
    int this, next, miss;
    char tmp;

    do {
	if (cont == 0) {
	    printf("Give me an x, y, z, and a direction vector (u, v, w)\n\
and I'll give you intersections.\n");
	    printf("Example:  0.0 0.0 0.0 1.0 1.0 1.0\n");
	    scanf("%f", &x0);
	    scanf("%f", &y0);
	    scanf("%f", &z0);
	    pos[0] = x0;
	    pos[1] = y0;
	    pos[2] = z0;
	    do {
	      scanf("%f", &vx);
	      scanf("%f", &vy);
	      scanf("%f", &vz);
	      /* normalize direction vector */
	      size = sqrt((double)(vx*vx+vy*vy+vz*vz));
	    } while (size<0.000001);
	    dir[0] = (double)(vx/size);
	    dir[1] = (double)(vy/size);
	    dir[2] = (double)(vz/size);
	    
	    IG_NAME(pos, dir, &dist, &this, &next, &miss);
	}
	printf("Enter 0 to continue, -1 to stop.\n");
	cont = 2; /* just to be safe in case they enter something invalid
		   * and possibly cont doesn't get reset
		   */
	scanf("%d", &cont);
	scanf("%c", &tmp);
    } while(cont!=-1);
}





clock_t random_intersections(int iters, int varies_most, float expand,
			     int forward_only, int continue_ray,
			     char * output_name) {
    geom_info_t *geom_ptr;
    float x0, y0, z0, vx, vy, vz, vtemp, vv[3], size,
      x_offset, y_offset, z_offset;
    double dist, pos[3], dir[3], pos_reverse[3], dir_reverse[3];
    int this, next, miss, miss_reverse, highest;
    int saved_this, saved_next;
    clock_t t1, t2;
    int i, error=0, hits=0, misses=0, symmetry_error=0, num_shot_in_reverse=0;
    FILE * fptr=0;
    int new_ray;
    int before_univels, after_univels, num_univels;

    if (strlen(output_name)>0) {
      fptr = fopen(output_name, "w");
    }

    geom_ptr = get_geometry_ptr();
    t1 = clock();

    for (i=0; i<iters; i++) {
      do {
	vx = ((float)(rand()%10001))/10000.0;
	vy = ((float)(rand()%10001))/10000.0;
	vz = ((float)(rand()%10001))/10000.0;

	if (varies_most!=0) {
	  if (vx>=vy) {
	    if (vx>=vz) {
	      highest = 1;     /* vx is highest */
	    } else {
	      highest = 3;     /* vz is highest */
	    }
	  } else {
	    if (vy>=vz) {
	      highest = 2;     /* vy is highest */
	    } else {
	      highest = 3;     /* vz is highest */
	    }
	  }
	  switch(highest)
	    {
	    case 1:
	      vv[0] = vx;
	      vv[1] = vy;
	      vv[2] = vz;
	      break;
	    case 2:
	      vv[0] = vy;
	      vv[1] = vx;
	      vv[2] = vz;
	      break;
	    case 3:
	      vv[0] = vz;
	      vv[1] = vy;
	      vv[2] = vx;
	      break;
	    }
	  /* assert vv[0] is highest */

 	  switch(varies_most) {
	  case 1: /* x */
	    vx = vv[0];
	    vy = vv[1];
	    vz = vv[2];
	    break;
	  case 2: /* y */
	    vx = vv[1];
	    vy = vv[0];
	    vz = vv[2];
	    break;
	  case 3: /* z */
	    vx = vv[2];
	    vy = vv[1];
	    vz = vv[0];
	    break;
	  }
	}
	
	/* Since univels aren't necessarily square boxes, alter
	 * velocities slightly to reflect this
	 */

	/* Mike --> this has both pros and cons...
	 * Generally, this gives z an extra 'boost' since it will
	 * typically be divided by 5 in the univel geometry.  But,
	 * this does make the vectors point predominantly in z.
	 * Compromise:  if varies_most=0 then we won't mess
	 * with these directions.  But, if varies_most is not 0
	 * then we need to mess with these to make sure when
	 * we switch to univels that the x, y, or z direction
	 * will vary the most -- to test rays tracking
	 * predominantly in that direction.
	 */
	if (varies_most!=0) {
	  vx *= geom_ptr->pixelsizecolumns;
	  vy *= geom_ptr->pixelsizerows;
	  vz *= geom_ptr->pixelsizeslices;
	}

	/* Now, let each velocity be equally likely pos. or neg. */
	if (rand()%2) vx=-vx;
	if (rand()%2) vy=-vy;
	if (rand()%2) vz=-vz;

	/* normalize */
	size = sqrt((double)(vx*vx+vy*vy+vz*vz));
      } while (size<0.000001); /* don't want 0 length vector */
      
      dir[0] = (double)(vx/size);
      dir[1] = (double)(vy/size);
      dir[2] = (double)(vz/size);
      
      /* Get numbers between -expand/2 and expand/2 */
      x_offset = expand*(float)((rand()%20001)-10000)/20000.0;
      y_offset = expand*(float)((rand()%20001)-10000)/20000.0;
      z_offset = expand*(float)((rand()%20001)-10000)/20000.0;

      pos[0] = (double)(geom_ptr->rlaxismin + (x_offset+0.5)*(geom_ptr->rlaxismax-geom_ptr->rlaxismin));
      pos[1] = (double)(geom_ptr->paaxismin + (y_offset+0.5)*(geom_ptr->paaxismax-geom_ptr->paaxismin));
      pos[2] = (double)(geom_ptr->isaxismin + (z_offset+0.5)*(geom_ptr->isaxismax-geom_ptr->isaxismin));
      
      new_ray = 1;
      while ((continue_ray&&(!miss))||(new_ray)) {

	/* If the ray is continuing (not new) then:
	 *   Update the position based on last intersection distance
	 *   and let the direction remain the same.
	 */
	if (!new_ray) {
	  pos[0] += (double)(dir[0]*dist);
	  pos[1] += (double)(dir[1]*dist);
	  pos[2] += (double)(dir[2]*dist);
	}

#ifdef PRINTS
	printf("\nFor:  (%0.2f %0.2f %0.2f) dir (%0.2f %0.2f %0.2f)\n",
	       (float)pos[0], (float)pos[1], (float)pos[2],
	       (float)dir[0], (float)dir[1], (float)dir[2]);
#endif
	
#ifndef TESTGEN
	if (fptr) {
	  if (new_ray) {
	    fprintf(fptr, "%d  ", 1);
	  } else {
	    fprintf(fptr, "%d  ", 0);
	  }
	  fprintf(fptr, "%10.3f %10.3f %10.3f  %10.3f %10.3f %10.3f  ",
		  (float)pos[0], (float)pos[1], (float)pos[2],
		  (float)dir[0], (float)dir[1], (float)dir[2]);
	}
	before_univels = univels_hit;
	IG_NAME(pos, dir, &dist, &saved_this, &saved_next, &miss); 
	after_univels = univels_hit;
	if (!miss) {
	  distance_traveled += (float)dist;
	}
	num_univels = after_univels - before_univels;
	if (fptr) {
	  if (miss) {
	    dist=0.0;                 /* 0 dist when a miss (or inf?) */
	    saved_next = saved_this;  /* next material same as start */
	  }
	  fprintf(fptr, "%10.3f  %-20s  %-20s  %5d\n",
		  (float)dist,
		  geom_ptr->bodyname[geom_ptr->uvval[saved_this]],
		  geom_ptr->bodyname[geom_ptr->uvval[saved_next]],
		  num_univels);
	}
	if (!miss) {
	  hits++;
	  if (!forward_only) {
	    pos_reverse[0]=dir[0]*(1.0+dist);
	    pos_reverse[1]=dir[1]*(1.0+dist);
	    pos_reverse[2]=dir[2]*(1.0+dist);
	    dir_reverse[0]=-dir[0];
	    dir_reverse[1]=-dir[1];
	    dir_reverse[2]=-dir[2];
	    IG_NAME(pos_reverse, dir_reverse, &dist, &this, &next, &miss_reverse);
	    num_shot_in_reverse++;
	    if (!miss_reverse) {
	      hits++;
	      if (dist>0.05) {
		printf("ERROR:  dist=%f in REVERSE.  Should be near 0!\n",dist);
		symmetry_error++;
	      }
	    } else {
	      printf("ERROR:  MISSED WHEN SHOOTING IN REVERSE DIRECTION.\n");
	      misses++;
	    }
	    if (saved_next!=this) {
	      error++;
	      /*printf("this_1=%d  next_1=%d  this_2=%d  next_2=%d\n",
		saved_this, saved_next, this, next);*/
	    }
	  }
	} else {
	  misses++;
	}
#else
	printf("%f %f %f %f %f %f\n",
	       (float)pos[0], (float)pos[1], (float)pos[2],
	       (float)dir[0], (float)dir[1], (float)dir[2]);
#endif
	new_ray = 0; /* On the next loop, ray only continues */
      }
    }
    if (fptr) {
      fclose(fptr);
    }
    t2 = clock();

    printf("************************************************************\n");
    printf("Command line parameter summary:\n");
    printf("  Iterations:  %d (# forward random rays shot)\n", iters);
    if (continue_ray) {
      printf("  Each ray was shot fully through the geometry.\n");
    } else {
      printf("  Each 'random' ray was only shot to first point of intersection.\n");
    }
    printf("  Rays shot so ");
    switch(varies_most)
      {
      case 0:
	printf("direction of motion is arbitrary\n");
	break;
      case 1:
	printf("+/- x is primary ray direction in univel geometry.\n");
	break;
      case 2:
	printf("+/- y is primary ray direction in univel geometry.\n");
	break;
      case 3:
	printf("+/- z is primary ray direction in univel geometry.\n");
	break;
      }
    if (expand>1.0) {
      printf("  Some rays start outside of the bounding box/univel geometry.\n");
    } else {
      printf("  All rays start inside of the bounding box/univel geometry.\n");
    }
    printf("    Expansion ratio set to:  %f\n", expand);
    printf("    Ratio applies to each dimension of bounding box of univel\n");
    printf("    geometry to determine a new box within which all rays are\n");
    printf("    initially shot.\n");
    printf("      expand = 0:    all rays start at center of univel geometry\n");
    printf("      expand = 1:    rays start at random point in univel geometry\n");
    printf("      expand = 1.26: approximately half of rays start inside univel\n");
    printf("                      geometry and half outside.  1.26^3 ~= 2\n");
    printf("      expand = inf:  rays can start _anywhere_ in space\n");
    if (forward_only) {
      printf("  All rays shot only in forward direction; option exists to\n");
      printf("  shoot a forward ray and then shoot the ray in reverse\n");
      printf("  to partially verify the initial intersection.\n");
    }
    if (!forward_only) {
      printf("ALL RAYS NOT LEAVING BOX HAVE ALSO BEEN SHOT IN REVERSE\n");
      printf("FROM THEIR ENDING POINT TO ENSURE THE REGION WE LAND IN\n");
      printf("IS THE SAME AS THAT REPORTED.  ALSO, WE CAN CHECK TO SEE\n");
      printf("THAT THE 'NEXT' MATERIAL IS THE SAME AS THE INITIAL\n");
      printf("'THIS' MATERIAL.  (THIS IS EXPECTED TO BE WRONG SOMETIMES\n");
      printf("SINCE SHORT INTERSECTIONS MAY BE IGNORED.\n");
      printf("************************************************************\n");
      printf("%d times using reported distance puts us in incorrect material.\n", error);
      printf("%d times rays shot in reverse direction and distance near 0\n",
	     symmetry_error);
      printf("  was expected but not encountered (out of %d shot in reverse).\n",
	     num_shot_in_reverse);
    }
    if (strlen(output_name)>0) {
      printf("  Generated output file was named:  %s\n", output_name);
    } else {
      printf("  No output file was generated.\n");
    }
    printf("************************************************************\n");
    printf("%d intersections.\n", hits);
    printf("%d box exits.  (No need to compute intersection.)\n", misses);
    printf("totaling %d\n", hits+misses);
    printf("************************************************************\n");
    
    num_intersections = hits+misses;

    /* Note: we hope that t2 didn't wrap around... */
    return (t2-t1);
}


/**************************************************************************
 * main
 **************************************************************************
 * main driver that will compute intersections on a given geometry
 * and regions of interest
 */
int main(int argc, char **argv) {
    int continue_ray=0;
    char input_name[256];   /* just a file name for the input */
    char output_name[256]="\0";
    int do_random=0;
    int i, iterations=100, accuracy;
    int varies_most=0; /* 1=x, 2=y, 3=z */
    int forward_only=0;
    float expand=1.26;
    clock_t time=(clock_t)0;

    if (argc<=1) {
        printf("\n**************************************************************\n");
        printf("In the future, you may wish to supply command line parameters:\n");
	printf("  %s <filename> -r 100 -x -s 1.26 -f\n", argv[0]);
	printf("    -r 100    means to shoot 100 random rays\n");
	printf("    -c        means let ray continue beyond 1st intersection.\n");
	printf("    -x        means x should vary most rapidly\n");
        printf("              -y and -z are similar, default is random direction\n");
	printf("    -s 1.26   affects how many ray start inside the bounding\n");
        printf("              box; the box where the rays can start from\n");
        printf("              is bounding box * 1.26 in each of x, y, and z.\n");
	printf("              (Default is also 1.26 if not supplied\n");
	printf("               which means about 1/2 rays start in box.)\n");
	printf("    -f        Means shoot rays in forward direction only --\n");
	printf("              (Default is to shoot in reverse also to check.)\n");
	printf("    -o name   generates a nice output file (suitable for a\n");
	printf("              spreadsheet or somesuch).\n");
        printf("**************************************************************\n\n");
	/* Get input */
	printf("Need filename info:\n================\n");
	get_info(input_name,256);
    } else {
	strcpy(input_name, argv[1]);
    }

    i = 2;
    while (i<argc) {
      if ((argv[i][0]=='-')&&((argv[i][1]=='r')||(argv[i][1]=='R'))) {
	do_random = 1;
	i++;
	iterations = atoi(argv[i]);
      } else if ((argv[i][0]=='-')&&((argv[i][1]=='c')||(argv[i][1]=='C'))) {
        continue_ray = 1;
      } else if ((argv[i][0]=='-')&&((argv[i][1]=='x')||(argv[i][1]=='X')
				     ||(argv[i][1]=='y')||(argv[i][1]=='Y')
				     ||(argv[i][1]=='z')||(argv[i][1]=='Z'))) {
	switch(argv[i][1])
	  {
	  case 'x':
	  case 'X':
	    varies_most = 1;
	    break;
	  case 'y':
	  case 'Y':
	    varies_most = 2;
	    break;
	  case 'z':
	  case 'Z':
	    varies_most = 3;
	    break;
	  }
      } else if ((argv[i][0]=='-')&&((argv[i][1]=='s')||(argv[i][1]=='S'))) {
        i++;
	expand = (float)atof(argv[i]);
	if (expand<0.0) expand = 0.0;
      } else if ((argv[i][0]=='-')&&((argv[i][1]=='f')||(argv[i][1]=='F'))) {
        forward_only = 1;
      } else if ((argv[i][0]=='-')&&((argv[i][1]=='o')||(argv[i][1]=='O'))) {
	i++;
	strcpy(output_name, argv[i]);
      }
      i++;
    }

    /* Read input file */
    IRE_NAME((int *)(NULL), (double *)(NULL), input_name, (char *)(NULL),
	     strlen(input_name), (int) (NULL));

    if (!do_random) {
      /* Intersection tests */
      intersection_tests();
    } else {
      time+=random_intersections(iterations, varies_most, expand, forward_only, continue_ray, output_name);
    }

    /* Free the memory we malloc'ed earlier */
    /* Instead, just exit and it will be freed upon exit */

    printf("%d univels passed through.  --partially implemented\n", univels_hit);
    printf("Elapsed time:          %12.3f s\n", ((float)time)/((float)CLOCKS_PER_SEC));
    if (time>0) {
      printf("Intersections/s:       %12.3f\n", ((float)num_intersections*(float)CLOCKS_PER_SEC)/
	     (float)time);
      printf("Univels/s:             %12.3f\n", ((float)univels_hit*(float)CLOCKS_PER_SEC)/
	     (float)time);
    }
    printf("Univels/Intersection:  %12.3f\n", ((float)univels_hit)/(float)num_intersections);
    printf("Distance traveled:     %12.3f\n", distance_traveled);
    printf("Distance/sec:          %12.3f\n", (distance_traveled*CLOCKS_PER_SEC)/time);
    exit(EXIT_SUCCESS);
}
