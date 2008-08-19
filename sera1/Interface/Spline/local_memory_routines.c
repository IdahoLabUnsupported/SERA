#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nurb.h"


#ifndef MASTER_LIST_DEFS_H
#include "master_list_defs.h"
#endif

#include "memory_routines.h"
#include "local_memory_routines.h"




/***************************************************************************** 
 *
 * initialize_nurbs.c
 * BNCT project
 * Written 06/06/94 by John Evans 
 *
 *****************************************************************************
 *
 * This procedure initializes the nurb data structure.  All linked lists are
 * doubly linked, and have sentinals.
 * 
 *****************************************************************************/
void initialize_nurbs(ll_start_type *head)
{

  /* Initialize the slice list. */
  head->slice_head = malloc_arl_slice_type();

  /* Initialize the surface list. */
  head->srf_head = malloc_surface_type();

  /* Initialize the draw list for brl surfaces. */
  head->draw_brl_list_head = malloc_arl_draw_node();

}











/*
 * malloc_brl_draw_node
 *
 * Returns a pointer to a struct arl_body_type.
 */
draw_brl_node_type *malloc_arl_draw_node () 
{
  draw_brl_node_type *ptr = (draw_brl_node_type *) 
    malloc (sizeof(draw_brl_node_type));

  ptr->srf_ptr = NULL;
  ptr->next = NULL;

  return ( ptr );

}










