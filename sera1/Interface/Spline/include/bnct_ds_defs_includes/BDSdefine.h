/* BDSdefine.h - Generic List structure defines.
 *               BNCE_rtpe development project with INEL.
 *
 *		BDS (Bnct_rtpe Data Structure)
 *		Part of the BNCT radiation treatment planning environment  
 *		development project with INEL (Idaho National Engineering 
 *		Laboratory) and MSU (Montana State University).
 *
 *		Ray S. Babcock
 *		Computer Science Department
 *		Montana State University
 *		Bozeman, MT  59717 (406) 994-4780
 *		email: babcock@cs.montana.edu
 *		www: http://www.cs.montana.edu
 *
 * January 5, 1995 : original coding
 */


/* The following is the ISO void * declaration.  If this is not supported,
 * change the following to char * 
 */

#ifndef BDS_DEFINE_H
#define BDS_DEFINE_H

#define BDS_VOID_PTR void *

/* Macro Definitions */
#define BDS_APEQ(value,constant) (fabs(value-constant) < EPSILON)
#define BDS_SCAN(ptr,initial) for((ptr)=(initial)->start;(ptr)!=NULL;(ptr)=(ptr)->next)

#define BDS_BODY_LIST bds_env->list_env->body_list
#define BDS_BODY_START BDS_BODY_LIST->start
#define BDS_BODY_END BDS_BODY_LIST->end

#define BDS_RECON_LIST bds_env->list_env->recon_list
#define BDS_RECON_START BDS_RECON_LIST->start
#define BDS_RECON_END BDS_RECON_LIST->end

#define BDS_SLICE_LIST bds_env->list_env->slice_list
#define BDS_SLICE_START BDS_SLICE_LIST->start
#define BDS_SLICE_END BDS_SLICE_LIST->end


#define BDS_CURRENT_BODY bds_env->current_env->current_body

#define BDS_CURRENT_RECON bds_env->current_env->current_recon

#define BDS_CURRENT_SLICE bds_env->current_env->current_slice

/* Defines for body color index.  These are(should be) related to the
 * file "body_colors" that is in the resources directory for bnct_rtpe.
 * The order of these defines is(should be) the same as in that file.
 */

#define BDS_RED 0
#define BDS_GREEN 1
#define BDS_BLUE 2
#define BDS_CYAN 3
#define BDS_MAGENTA 4
#define BDS_YELLOW 5
#define BDS_BLACK 6
#define BDS_WHITE 7

#endif

