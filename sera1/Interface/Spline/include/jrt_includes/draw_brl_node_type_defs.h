#ifndef DRAW_BRL_NODE_TYPE_DEFS_H
#define DRAW_BRL_NODE_TYPE_DEFS_H


#ifndef SURFACE_STRUCTURE_DEFS_H
#include "surface_structure_defs.h"
#endif


/* 
 * Define a node for the drawing list. 
 * The drawing list is traversed, drawing each node.  "Duhhh".
 */
typedef struct draw_brl_node_type {
  surface_type                *srf_ptr;         /* body to be drawn         */

  struct draw_brl_node_type   *next;            /* next body in list        */

} draw_brl_node_type;


#endif


