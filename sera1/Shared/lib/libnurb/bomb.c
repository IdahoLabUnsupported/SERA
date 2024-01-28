/*                          B O M B . C
 * BRL-CAD
 *
 * Copyright (c) 2004-2007 United States Government as represented by
 * the U.S. Army Research Laboratory.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this file; see the file named COPYING for more
 * information.
 */
/** @addtogroup bu_log */
/** @{ */
/** @file ./libbu/bomb.c
 *
 *  This routine is called on a fatal
 *  error, where no recovery is possible.
 *
 *  @par Functions -
 *	bu_bomb		Called upon fatal error.
 *
 *  @author	Michael John Muuss
 *
 *  @par Source -
 *	The U. S. Army Research Laboratory			@n
 *	Aberdeen Proving Ground, Maryland  21005-5068  USA
 *
 */

#ifndef lint
static const char RCSbomb[] = "@(#)$Header$ (ARL)";
#endif

#include "common.h"

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef HAVE_STRING_H
#  include <string.h>
#endif
#ifdef HAVE_UNIX_IO
#  include <fcntl.h>
#endif

#include "machine.h"
#include "bu.h"

#if 1
struct bu_hook_list bu_bomb_hook_list = {
	{	BU_LIST_HEAD_MAGIC,
		&bu_bomb_hook_list.l,
		&bu_bomb_hook_list.l
	},
	BUHOOK_NULL,
	GENPTR_NULL
};
#else
struct bu_hook_list bu_bomb_hook_list;
#endif

/*
 * These variables are global because BU_SETJUMP() *must* be a macro.
 * If you replace this version of bu_bomb() with one of your own,
 * you must also provide these variables, even if you don't use them.
 */
int		bu_setjmp_valid = 0;	/**< @brief !0 = bu_jmpbuf is valid */
jmp_buf		bu_jmpbuf;		/**< @brief for BU_SETJMP() */

/**
 *			B U _ B O M B
 *@brief
 *  Abort the program with a message.
 *
 *  Only produce a core-dump when that debugging bit is set.  Note
 *  that this function is meant to be a last resort graceful abort.
 *  It should not attempt to allocate anything on the stack or heap.
 */
void
bu_bomb(const char *str)
{

	/* First thing, always always always try to print the string.
	 * Avoid passing additional format arguments so as to avoid
	 * buffer allocations inside fprintf().
	 */
	fprintf(stderr, "\n");
	fprintf(stderr, str);
	fprintf(stderr, "\n");
	fflush(stderr);

	exit(12);
}
/** @} */
/*
 * Local Variables:
 * mode: C
 * tab-width: 8
 * c-basic-offset: 4
 * indent-tabs-mode: t
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
