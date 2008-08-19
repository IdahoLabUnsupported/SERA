#ifndef EXTRA_H
#define EXTRA_H

void reduce_grays_optimal_driver(int);
void reduce_grays_optimal(int *, int, int *, int, int *);
void pyramid_filter(int *);
void refine(unsigned char *, int *, int, int, int, int);
void iterative_update(unsigned char ***, unsigned char ***, int *,
		      int, int, int);
int get_cost(unsigned char ***, unsigned char ***, int, int, int, int, int,
	     int, unsigned char);
void rotate_images_FCN(image_matrix_type *, int);
void rotate_easy(unsigned char *, int, int, int);
void rotate_images_CB(Widget, XtPointer, XtPointer);

#endif
