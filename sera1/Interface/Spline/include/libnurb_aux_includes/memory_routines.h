/*
 * These prototypes allocate data structures for ray tracing and
 * subdivision routines.
 */
struct surface_type *copy_surface_type ( surface_type *srf_ptr );
struct subsurface_type *r_copy_subsurface ( struct subsurface_type *old_subsrf );

struct triangle_type *malloc_triangle_type ();
struct subsurface_type *malloc_subsurface_type ();
struct surface_type *malloc_surface_type ();

arl_body_type *malloc_arl_body_type (); 
arl_slice_type *malloc_arl_slice_type (); 

/*
 * These are prototypes for memory deallocation.
 */
void free_subsrf_node ( struct subsurface_type **subsrf_node_ptr );
void free_surface_type ( struct surface_type **srf_ptr );
