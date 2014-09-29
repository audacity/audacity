/*
** Copyright (C) 2006-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <math.h>

#include "common.h"
#include "test_main.h"

void
test_float_convert (void)
{	static float data [] =
	{	0.0, 1.0, -1.0, 1.0 * M_PI, -1.0 * M_PI,
		1e9, -1e9, 1e-9, -1e-9, 1e-10, -1e-10,
		1e-19, -1e-19, 1e19, -1e19, 1e-20, -1e-20,
		} ;

	int k ;

	print_test_name (__func__) ;

	for (k = 0 ; k < ARRAY_LEN (data) ; k++)
	{	unsigned char bytes [4] ;
		float test ;

		float32_le_write (data [k], bytes) ;
		test = float32_le_read (bytes) ;

		if (fabs (data [k] - test) > 1e-20)
		{	printf ("\n\nLine %d : Test %d, little endian error %.15g -> %.15g.\n\n", __LINE__, k, data [k], test ) ;
			exit (1) ;
			} ;

		float32_be_write (data [k], bytes) ;
		test = float32_be_read (bytes) ;

		if (fabs (data [k] - test) > 1e-20)
		{	printf ("\n\nLine %d : Test %d, big endian error %.15g -> %.15g.\n\n", __LINE__, k, data [k], test) ;
			exit (1) ;
			} ;

		} ;

	puts ("ok") ;
} /* test_float_convert */

void
test_double_convert (void)
{	static double data [] =
	{	0.0, 1.0, -1.0, 1.0 * M_PI, -1.0 * M_PI,
		1e9, -1e9, 1e-9, -1e-9, 1e-10, -1e-10,
		1e-19, -1e-19, 1e19, -1e19, 1e-20, -1e-20,
		} ;

	int k ;

	print_test_name (__func__) ;

	for (k = 0 ; k < ARRAY_LEN (data) ; k++)
	{	unsigned char bytes [8] ;
		double test ;

		double64_le_write (data [k], bytes) ;
		test = double64_le_read (bytes) ;

		if (fabs (data [k] - test) > 1e-20)
		{	printf ("\n\nLine %d : Test %d, little endian error %.15g -> %.15g.\n\n", __LINE__, k, data [k], test ) ;
			exit (1) ;
			} ;

		double64_be_write (data [k], bytes) ;
		test = double64_be_read (bytes) ;

		if (fabs (data [k] - test) > 1e-20)
		{	printf ("\n\nLine %d : Test %d, big endian error %.15g -> %.15g.\n\n", __LINE__, k, data [k], test) ;
			exit (1) ;
			} ;

		} ;

	puts ("ok") ;
} /* test_double_convert */

