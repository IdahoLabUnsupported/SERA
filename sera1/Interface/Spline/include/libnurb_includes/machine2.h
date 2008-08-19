/*
 * $Id: machine2.h,v 1.1 1998/01/18 14:23:34 babcock Exp $
 * Currently locked by $Locker:  $ (not locked if blank)
 * (Time in GMT, MST=GMT-7:00, MDT=GMT-6:00)
 * $Log: machine2.h,v $
 * Revision 1.1  1998/01/18 14:23:34  babcock
 * Original submittal of bnct3.0 project with files        as of 9:45 a.m. Friday, 1/19/98 by RSB
 *
 * Revision 1.1.1.1  1997/04/17  18:54:52  babcock
 * Recovery build of the bnct2root directory from Dan's copy
 * as of 4/17/97 in /bnct2.2.  Ray Babcock.
 *
 * Revision 1.2  1997/03/12  19:26:03  wessol
 * Removed hypotenuse definition.
 *
 * Revision 1.1.1.1  1996/07/17  20:31:35  voss
 * Creating rev 2.2 of bnct_rtpe.
 *
 * Revision 1.1.1.1  1996/02/12  23:29:12  babcock
 * Imported sources
 *
 * Revision 1.1  1995/04/06  19:37:52  astrakan
 * Initial revision
 *
 *
 */



/*
 *			M A C H I N E . H
 *
 *  This header file defines all the
 *
 *	fundamental data types (lower case names, created with "typedef")
 *
 *  and
 *
 *	fundamental manifest constants (upper case, created with "#define")
 *
 *  used throughout the BRL-CAD Package.  Virtually all other BRL-CAD
 *  header files depend on this header file being included first.
 *
 *  Many of these fundamental data types are machine (vendor) dependent.
 *  Some may assume different values on the same machine, depending on
 *  which version of the compiler is being used.
 *
 *  The above paragraph is not valid.  All machine dependent stuff is
 *  GONE.
 *  --John Evans
 *
 *  Additions will need to be made here when porting BRL-CAD to a new machine
 *  which is anything but a 32-bit big-endian uniprocessor.
 *
 *  General Symbols and Types Defined -
 *
 *	CONST -
 *		A portable way of indicating that the ANSI C "const"
 *		keyword is desired, when compiling on an ANSI compiler.
 *
 *	genptr_t -
 *		A portable way of declaring a "generic" pointer that is
 *		wide enough to point to anything, which can be used on
 *		both ANSI C and K&R C environments.
 *		On some machines, pointers to functions can be wider than
 *		pointers to data bytes, so a declaration of "char *"
 *		isn't generic enough.
 *
 *	SIGNED -
 *		A portable way of declaring a signed variable, since
 *		the "signed" keyword is not known in K&R compilers.  e.g.:
 *			register SIGNED int twoway;
 *
 *	fastf_t -
 *		Intended to be the fastest floating point data type on
 *		the current machine, with at least 64 bits of precision.
 *		On 16 and 32 bit machine, this is typically "double",
 *		but on 64 bit machines, it is often "float".
 *		Virtually all floating point variables (and more complicated
 *		data types, like vect_t and mat_t) are defined as fastf_t.
 *		The one exception is when a subroutine return is a floating
 *		point value;  that is always declared as "double".
 *
 *	LOCAL -
 *		The fastest storage class for local variables within a
 *		subroutine.  On parallel machines, this needs to be "auto",
 *		but on serial machines there can sometimes be a performance
 *		advantage to using "static".
 *
 *	FAST -
 *		The fastest storage class for fastf_t variables.
 *		On most machines with abundant registers, this is "register",
 *		but on machines like the VAX with only 3 "register double"s
 *		available to C programmers, it is set to LOCAL.
 *		Thus, declaring a fast temporary fastf_t variable is done like:
 *			FAST fastf_t var;
 *
 *	HIDDEN -
 *		Functions intended to be local to one module should be
 *		declared HIDDEN.  For production use, and lint, it will
 *		be defined as "static", but for debugging it can be defined
 *		as NIL, so that the routine names can be made available
 *		to the debugger.
 *
 *	MAX_FASTF -
 *		Very close to the largest value that can be held by a
 *		fastf_t without overflow.  Typically specified as an
 *		integer power of ten, to make the value easy to spot when
 *		printed.
 *
 *	SQRT_MAX_FASTF -
 *		sqrt(MAX_FASTF), or slightly smaller.  Any number larger than
 *		this, if squared, can be expected to produce an overflow.
 *
 *	SMALL_FASTF -
 *		Very close to the smallest value that can be represented
 *		while still being greater than zero.  Any number smaller
 *		than this (and non-negative) can be considered to be
 *		zero;  dividing by such a number can be expected to produce
 *		a divide-by-zero error.
 *		All divisors should be checked against this value before
 *		actual division is performed.
 *
 *	SQRT_SMALL_FASTF -
 *		sqrt(SMALL_FASTF), or slightly larger.  The value of this
 *		is quite a lot larger than that of SMALL_FASTF.
 *		Any number smaller than this, when squared, can be expected
 *		to produce a zero result.
 *
 *	bzero(ptr,n) -
 *		Defined to be the fasted system-specific method for
 *		zeroing a block of 'n' bytes, where the pointer has
 *		arbitrary byte alignment.  BSD semantics.
 *
 *	bcopy(from,to,n) -
 *		Defined to be the fastest system-specific method for
 *		copying a block of 'n' bytes, where both the "from" and
 *		"to" pointers have arbitrary byte alignment.  BSD semantics.
 *
 *	bitv_t -
 *		The widest fast integer type available, used to implement bit
 *		vectors.  On most machines, this is "long", but on some
 *		machines a vendor-specific type such as "long long" can
 *		give access to wider integers.
 *
 *	BITV_SHIFT -
 *		log2( bits_wide(bitv_t) ).  Used to determine how many
 *		bits of a bit-vector subscript are index-of-bit in bitv_t
 *		word, and how many bits of the subscript are for word index.
 *		On a 32-bit machine, BITV_SHIFT is 5.
 *
 *	XXX The BYTE_ORDER handling needs to change to match the POSIX
 *	XXX recommendations.
 *
 *  PARALLEL Symbols Defined -
 *    These are used only for applications linked with LIBRT,
 *    and interact heavily with the support routines in librt/machine.c
 *    XXX These are likely to get new, more descriptive names sometime.
 *
 *	PARALLEL -
 *		When defined, the code is being compiled for a parallel processor.
 *		This has implications for signal handling, math library
 *		exception handling, etc.
 *
 *	MAX_PSW -
 *		The maximum number of processors that can be expected on
 *		this hardware.  Used to allocate application-specific
 *		per-processor tables.
 *		The actual number of processors is found at runtime by calling
 *		rt_avail_cpus().
 *
 *	DEFAULT_PSW -
 *		The number of processors to use when the user has not
 *		specifically indicated the number of processors desired.
 *		On some machines like the Alliant, this should be MAX_PSW,
 *		because the parallel complex is allocated as a unit.
 *		On timesharing machines like the Cray, this should be 1,
 *		because running multi-tasking consumes special resources
 *		(and sometimes requires special queues/privs), so ordinary
 *		runs should just stay serial.
 *
 *	RES_INIT() -
 *		Macro to initialize a semaphore.
 *		For now, the semaphore *must* be given as the address of
 *		one of these five variables from raytrace.h:
 *			rt_g.res_syscall
 *			rt_g.res_worker
 *			rt_g.res_stats
 *			rt_g.res_results
 *			rt_g.res_model
 *		This is a historical limitation that will be removed when
 *		the macro names are changed.
 *
 *	RES_ACQUIRE() -
 *		Macro to acquire exclusive use of a semaphore, entering
 *		a (1 processor only) critical section.  If another processor
 *		already has exclusive use of this semaphore, it will be
 *		forced to wait, either in a spin-lock, or by relinquishing
 *		it's CPU.
 *
 *	RES_RELEASE() -
 *		Macro to release a semaphore, ending a critical section.
 *		Uses of RES_ACQUIRE() and RES_RELEASE() need to be carefully
 *		paired.  No more than a few lines of code should exist
 *		between them, to keep the critical (non-parallel) sections
 *		as brief as possible.  The frequency and duration of
 *		critical sections determines the asymptotic performance
 *		of a parallel application as the number of processors
 *		is made large.
 *
 *  Author -
 *	Michael John Muuss
 *
 *  Source -
 *	The U. S. Army Research Laboratory
 *	Aberdeen Proving Ground, Maryland  21005
 *  
 *  Distribution Status -
 *	This file is public domain, distribution unlimited.
 *
 *  Include Sequencing -
 *	#include <stdio.h>
 *	#include <math.h>
 *	#include "machine.h"
 *
 *  Libraries Used -
 *	-lm -lc				(serial-only applications)
 *	LIBRT LIBRT_LIBES -lm -lc	(parallel applications)
 *
 *  $Header: /home/jjc/repos_cvs/cvsroot/sera1/Interface/Spline/include/libnurb_includes/machine2.h,v 1.1 1998/01/18 14:23:34 babcock Exp $
 */

#ifndef MACHINE2_H
#define MACHINE2_H seen

/*
 * Figure out the maximum number of files that can simultaneously be open 
 * by a process.
 */

#if !defined(FOPEN_MAX) && defined(_NFILE)
#	define FOPEN_MAX	_NFILE
#endif
#if !defined(FOPEN_MAX) && defined(NOFILE)
#	define FOPEN_MAX	NOFILE
#endif
#if !defined(FOPEN_MAX) && defined(OPEN_MAX)
#	define FOPEN_MAX	OPEN_MAX
#endif
#if !defined(FOPEN_MAX)
#	define FOPEN_MAX	32
#endif

/**********************************
 *                                *
 *  Machine specific definitions  *
 *  Choose for maximum speed      *
 *				  *
 **********************************/


#ifndef SINGLE
  typedef double fastf_t;	/* double|float, "Fastest" float type */
#else
  typedef float  fastf_t;       
#endif


#define LOCAL	auto		/* static|auto, for serial|parallel cpu */
#define FAST	register	/* LOCAL|register, for fastest floats */
typedef long	bitv_t;		/* largest integer type */
#define BITV_SHIFT	6	/* log2( bits_wide(bitv_t) ) */

/* full means resource free, empty means resource busy */
#define RES_INIT(ptr)		RES_RELEASE(ptr)
#define	RES_ACQUIRE(ptr)	(void)Daread(ptr)	/* wait full set empty */
#define RES_RELEASE(ptr)	(void)Daset(ptr,3)	/* set full */
#define MAX_PSW		128	/* Max number of process streams */
#define DEFAULT_PSW	MAX_PSW
#define PARALLEL	1
#endif


/*
 * Definitions about limits of floating point representation
 * Eventually, should be tied to type of hardware (IEEE, IBM, Cray)
 * used to implement the fastf_t type.
 */
#define MAX_FASTF		1.0e37	/* Very close to the largest number */
#define SQRT_MAX_FASTF		1.0e18	/* This squared just avoids overflow */
#define SMALL_FASTF		1.0e-37	/* Anything smaller is zero */
#define SQRT_SMALL_FASTF	1.0e-18	/* This squared gives zero */
#define SMALL			SQRT_SMALL_FASTF

/*
 *  Definition of a "generic" pointer that can hold a pointer to anything.
 *  According to tradition, a (char *) was generic, but the ANSI folks
 *  worry about machines where (int *) might be wider than (char *),
 *  so here is the clean way of handling it.
 */
#if !defined(GENPTR_NULL)
#  if __STDC__
	typedef void	*genptr_t;
#  else
	typedef char	*genptr_t;
#  endif
#  define GENPTR_NULL	((genptr_t)0)
#endif

/* A portable way of handling the ANSI C const keyword: use CONST */
#if !defined(CONST)
# if __STDC__
#	define	CONST	const
# else
#	define	CONST	/**/
# endif
#endif

/* A portable way of dealing with pre-ANSI C.  Assume signed variables */
#if !defined(SIGNED)
# if __STDC__
#	define SIGNED	signed
# else
#	define SIGNED	/**/
# endif
#endif

/*
 *  Some very common BSD --> SYSV conversion aids
 */
#if defined(SYSV) && !defined(bzero)
#	define bzero(str,n)		memset( str, '\0', n )
#	define bcopy(from,to,count)	memcpy( to, from, count )
#endif

/* XXX Soon BRL-CAD will convert to sources written using the SYSV names,
 * XXX with defines back to the old UNIX V6 names for antique systems.
 */
/* REMOVED 2/9/96  RSB
#if !defined(HAVE_STRCHR)
#	define strchr(sp,c)	index(sp,c)
#	define strrchr(sp,c)	rindex(sp,c)
	extern char *index();
	extern char *rindex();
#endif
*/



/* Functions local to one file should be declared HIDDEN:  (nil)|static */
/* To aid in using ADB, generally leave this as nil. */
#if !defined(HIDDEN)
# if defined(lint)
#	define HIDDEN	static
# else
#	define HIDDEN	/***/
# endif
#endif

