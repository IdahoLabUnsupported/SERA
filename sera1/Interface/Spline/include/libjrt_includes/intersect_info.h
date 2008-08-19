#ifndef INTERSECT_INFO_H
#define INTERSECT_INFO_H

/*
 * This structure contains all pertinant information resulting from 
 * a ray intersecting with the NURB surface.
 */
typedef struct intersect_info {

    short    intersected;                   /* a boolean, whether or not */
					    /* the ray intersected the */
					    /* surface */

    point_t  intersection;                  /* coordinates of */
					    /* intersection */

    fastf_t  dist;                          /* distance from ray origin */
					    /* to intersection */

    fastf_t  cos_theta;                     /* cosine of the angle */
					    /* between the intersecting */
					    /* ray and the local surface */
					    /* normal */

    struct surface_type *srf_ptr;           /* pointer to the surface */
					    /* intersected */
 
} intersect_info;


#endif
