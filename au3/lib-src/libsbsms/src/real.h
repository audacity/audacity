// -*- mode: c++ -*-
/*
** Copyright (C) 2001-2003 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef REAL_H
#define REAL_H

/* Version 1.3 */


/*============================================================================
**	On Intel Pentium processors (especially PIII and probably P4), converting
**	from float to int is very slow. To meet the C specs, the code produced by
**	most C compilers targeting Pentium needs to change the FPU rounding mode
**	before the float to int conversion is performed.
**
**	Changing the FPU rounding mode causes the FPU pipeline to be flushed. It
**	is this flushing of the pipeline which is so slow.
**
**	Fortunately the ISO C99 specifications define the functions lrint, lrintf,
**	llrint and llrintf which fix this problem as a side effect.
**
**	On Unix-like systems, the configure process should have detected the
**	presence of these functions. If they weren't found we have to replace them
**	here with a standard C cast.
*/

/*
**	The C99 prototypes for lrint and lrintf are as follows:
**
**		long int lrintf (float x) ;
**		long int lrint  (double x) ;
*/

#include "config.h"
#include "sbsms.h"

/*
**	The presence of the required functions are detected during the configure
**	process and the values HAVE_LRINT and HAVE_LRINTF are set accordingly in
**	the config.h file.
*/

#define		HAVE_LRINT_REPLACEMENT	0

#if (HAVE_LRINT && HAVE_LRINTF)

	/*
	**	These defines enable functionality introduced with the 1999 ISO C
	**	standard. They must be defined before the inclusion of math.h to
	**	engage them. If optimisation is enabled, these functions will be
	**	inlined. With optimisation switched off, you have to link in the
	**	maths library using -lm.
	*/

	#define	_ISOC9X_SOURCE	1
	#define _ISOC99_SOURCE	1

	#define	__USE_ISOC9X	1
	#define	__USE_ISOC99	1

	#include	<math.h>

#elif (defined (WIN32) || defined (_WIN32))

	#undef		HAVE_LRINT_REPLACEMENT
	#define		HAVE_LRINT_REPLACEMENT	1
	#include	<math.h>

	/*
	**	Win32 doesn't seem to have these functions.
	**	Therefore implement inline versions of these functions here.
	*/

	__inline long int
	lrint (double flt)
	{	int intgr ;

		_asm
		{	fld flt
			fistp intgr
			} ;

		return intgr ;
	}

	__inline long int
	lrintf (float flt)
	{	int intgr ;

		_asm
		{	fld flt
			fistp intgr
			} ;

		return intgr ;
	}

#elif (defined (__MWERKS__) && defined (macintosh))

	/* This MacOS 9 solution was provided by Stephane Letz */

	#undef		HAVE_LRINT_REPLACEMENT
	#define		HAVE_LRINT_REPLACEMENT	1
	#include	<math.h>

	#undef	lrint
	#undef	lrintf

	#define	lrint	double2int
	#define	lrintf	float2int

	inline int
	float2int (register float in)
	{	long res [2] ;

		asm
		{	fctiw	in, in
			stfd	 in, res
		}
		return res [1] ;
	} /* float2int */

	inline int
	double2int (register double in)
	{	long res [2] ;

		asm
		{	fctiw	in, in
			stfd	 in, res
		}
		return res [1] ;
	} /* double2int */

#elif (defined (__MACH__) && defined (__APPLE__))

	/* For Apple MacOSX. */

	#undef		HAVE_LRINT_REPLACEMENT
	#define		HAVE_LRINT_REPLACEMENT	1
	#include	<math.h>

	#undef lrint
	#undef lrintf

	#define lrint	double2int
	#define lrintf	float2int

	inline static long int
	float2int (register float in)
	{	int res [2] ;

		__asm__ __volatile__
		(	"fctiw	%1, %1\n\t"
			"stfd	%1, %0"
			: "=m" (res)	/* Output */
			: "f" (in)		/* Input */
			: "memory"
			) ;

		return res [1] ;
	} /* lrintf */

	inline static long int
	double2int (register double in)
	{	int res [2] ;

		__asm__ __volatile__
		(	"fctiw	%1, %1\n\t"
			"stfd	%1, %0"
			: "=m" (res)	/* Output */
			: "f" (in)		/* Input */
			: "memory"
			) ;

		return res [1] ;
	} /* lrint */

#else
	#ifndef __sgi
	#warning "Don't have the functions lrint() and lrintf()."
	#warning "Replacing these functions with a standard C cast."
	#endif

	#include	<math.h>

	#define	lrint(dbl)		((long) (dbl))
	#define	lrintf(flt)		((long) (flt))

#endif

#endif
