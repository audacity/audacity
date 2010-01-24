/*
** Copyright (C) 2002-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <errno.h>
#include <inttypes.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "common.h"
#include "sfendian.h"

#include "test_main.h"

#define	FMT_SHORT	"0x%04x\n"
#define	FMT_INT		"0x%08x\n"
#define	FMT_INT64	"0x%016" PRIx64 "\n"

/*==============================================================================
** Test functions.
*/


static void
dump_short_array (const char * name, short * data, int datalen)
{	int k ;

	printf ("%-6s : ", name) ;
	for (k = 0 ; k < datalen ; k++)
		printf (FMT_SHORT, data [k]) ;
	putchar ('\n') ;
} /* dump_short_array */

static void
test_endswap_short (void)
{	short orig [4], first [4], second [4] ;
	int k ;

	printf ("    %-40s : ", "test_endswap_short") ;
	fflush (stdout) ;

	for (k = 0 ; k < ARRAY_LEN (orig) ; k++)
		orig [k] = 0x3210 + k ;

	endswap_short_copy (first, orig, ARRAY_LEN (first)) ;
	endswap_short_copy (second, first, ARRAY_LEN (second)) ;

	if (memcmp (orig, first, sizeof (orig)) == 0)
	{	printf ("\n\nLine %d : test 1 : these two array should not be the same:\n\n", __LINE__) ;
		dump_short_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_short_array ("first", first, ARRAY_LEN (first)) ;
		exit (1) ;
		} ;

	if (memcmp (orig, second, sizeof (orig)) != 0)
	{	printf ("\n\nLine %d : test 2 : these two array should be the same:\n\n", __LINE__) ;
		dump_short_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_short_array ("second", second, ARRAY_LEN (second)) ;
		exit (1) ;
		} ;

	endswap_short_array (first, ARRAY_LEN (first)) ;

	if (memcmp (orig, first, sizeof (orig)) != 0)
	{	printf ("\n\nLine %d : test 3 : these two array should be the same:\n\n", __LINE__) ;
		dump_short_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_short_array ("first", first, ARRAY_LEN (first)) ;
		exit (1) ;
		} ;

	endswap_short_copy (first, orig, ARRAY_LEN (first)) ;
	endswap_short_copy (first, first, ARRAY_LEN (first)) ;

	if (memcmp (orig, first, sizeof (orig)) != 0)
	{	printf ("\n\nLine %d : test 4 : these two array should be the same:\n\n", __LINE__) ;
		dump_short_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_short_array ("first", first, ARRAY_LEN (first)) ;
		exit (1) ;
		} ;

	puts ("ok") ;
} /* test_endswap_short */

static void
dump_int_array (const char * name, int * data, int datalen)
{	int k ;

	printf ("%-6s : ", name) ;
	for (k = 0 ; k < datalen ; k++)
		printf (FMT_INT, data [k]) ;
	putchar ('\n') ;
} /* dump_int_array */

static void
test_endswap_int (void)
{	int orig [4], first [4], second [4] ;
	int k ;

	printf ("    %-40s : ", "test_endswap_int") ;
	fflush (stdout) ;

	for (k = 0 ; k < ARRAY_LEN (orig) ; k++)
		orig [k] = 0x76543210 + k ;

	endswap_int_copy (first, orig, ARRAY_LEN (first)) ;
	endswap_int_copy (second, first, ARRAY_LEN (second)) ;

	if (memcmp (orig, first, sizeof (orig)) == 0)
	{	printf ("\n\nLine %d : test 1 : these two array should not be the same:\n\n", __LINE__) ;
		dump_int_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_int_array ("first", first, ARRAY_LEN (first)) ;
		exit (1) ;
		} ;

	if (memcmp (orig, second, sizeof (orig)) != 0)
	{	printf ("\n\nLine %d : test 2 : these two array should be the same:\n\n", __LINE__) ;
		dump_int_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_int_array ("second", second, ARRAY_LEN (second)) ;
		exit (1) ;
		} ;

	endswap_int_array (first, ARRAY_LEN (first)) ;

	if (memcmp (orig, first, sizeof (orig)) != 0)
	{	printf ("\n\nLine %d : test 3 : these two array should be the same:\n\n", __LINE__) ;
		dump_int_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_int_array ("first", first, ARRAY_LEN (first)) ;
		exit (1) ;
		} ;

	endswap_int_copy (first, orig, ARRAY_LEN (first)) ;
	endswap_int_copy (first, first, ARRAY_LEN (first)) ;

	if (memcmp (orig, first, sizeof (orig)) != 0)
	{	printf ("\n\nLine %d : test 4 : these two array should be the same:\n\n", __LINE__) ;
		dump_int_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_int_array ("first", first, ARRAY_LEN (first)) ;
		exit (1) ;
		} ;

	puts ("ok") ;
} /* test_endswap_int */

static void
dump_int64_t_array (const char * name, int64_t * data, int datalen)
{	int k ;

	printf ("%-6s : ", name) ;
	for (k = 0 ; k < datalen ; k++)
		printf (FMT_INT64, data [k]) ;
	putchar ('\n') ;
} /* dump_int64_t_array */

static void
test_endswap_int64_t (void)
{	int64_t orig [4], first [4], second [4] ;
	int k ;

	printf ("    %-40s : ", "test_endswap_int64_t") ;
	fflush (stdout) ;

	for (k = 0 ; k < ARRAY_LEN (orig) ; k++)
		orig [k] = 0x0807050540302010LL + k ;

	endswap_int64_t_copy (first, orig, ARRAY_LEN (first)) ;
	endswap_int64_t_copy (second, first, ARRAY_LEN (second)) ;

	if (memcmp (orig, first, sizeof (orig)) == 0)
	{	printf ("\n\nLine %d : test 1 : these two array should not be the same:\n\n", __LINE__) ;
		dump_int64_t_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_int64_t_array ("first", first, ARRAY_LEN (first)) ;
		exit (1) ;
		} ;

	if (memcmp (orig, second, sizeof (orig)) != 0)
	{	printf ("\n\nLine %d : test 2 : these two array should be the same:\n\n", __LINE__) ;
		dump_int64_t_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_int64_t_array ("second", second, ARRAY_LEN (second)) ;
		exit (1) ;
		} ;

	endswap_int64_t_array (first, ARRAY_LEN (first)) ;

	if (memcmp (orig, first, sizeof (orig)) != 0)
	{	printf ("\n\nLine %d : test 3 : these two array should be the same:\n\n", __LINE__) ;
		dump_int64_t_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_int64_t_array ("first", first, ARRAY_LEN (first)) ;
		exit (1) ;
		} ;

	endswap_int64_t_copy (first, orig, ARRAY_LEN (first)) ;
	endswap_int64_t_copy (first, first, ARRAY_LEN (first)) ;

	if (memcmp (orig, first, sizeof (orig)) != 0)
	{	printf ("\n\nLine %d : test 4 : these two array should be the same:\n\n", __LINE__) ;
		dump_int64_t_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_int64_t_array ("first", first, ARRAY_LEN (first)) ;
		exit (1) ;
		} ;

	puts ("ok") ;
} /* test_endswap_int64_t */



void
test_endswap (void)
{
	test_endswap_short () ;
	test_endswap_int () ;
	test_endswap_int64_t () ;

} /* test_endswap */

