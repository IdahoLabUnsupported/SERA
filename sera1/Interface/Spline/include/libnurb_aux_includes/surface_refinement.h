#ifndef BRL_OSLO_H
#define BRL_OSLO_H

void brl_oslo ( surface_type *surface_pointer, int level_of_refinement );
struct knot_vector *refine_closed_kv ( struct knot_vector *old_kv, 
				       int level_of_refinement,
				       int order);

#endif
