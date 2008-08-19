#ifndef MASTER_LIST_DEFS_H
#define MASTER_LIST_DEFS_H

#ifndef SURFACE_STRUCTURE_DEFS_H
#include "surface_structure_defs.h"
#endif

#ifndef DRAW_BRL_NODE_TYPE_DEFS
#include "draw_brl_node_type_defs.h"
#endif

/*
 * Definition of master linked list.
 */
typedef struct {
  arl_slice_type      *slice_head;
  surface_type        *srf_head;
  draw_brl_node_type  *draw_brl_list_head;
} ll_start_type;


#endif
