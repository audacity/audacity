[+ AutoGen5 template c +]
/*
** Copyright (C) 2002-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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

[+ FOR int_type +]
static void
dump_[+ (get "name") +]_array (const char * name, [+ (get "name") +] * data, int datalen)
{	int k ;

	printf ("%-6s : ", name) ;
	for (k = 0 ; k < datalen ; k++)
		printf ([+ (get "format") +], data [k]) ;
	putchar ('\n') ;
} /* dump_[+ (get "name") +]_array */

static void
test_endswap_[+ (get "name") +] (void)
{	[+ (get "name") +] orig [4], first [4], second [4] ;
	int k ;

	printf ("    %-40s : ", "test_endswap_[+ (get "name") +]") ;
	fflush (stdout) ;

	for (k = 0 ; k < ARRAY_LEN (orig) ; k++)
		orig [k] = [+ (get "value") +] + k ;

	endswap_[+ (get "name") +]_copy (first, orig, ARRAY_LEN (first)) ;
	endswap_[+ (get "name") +]_copy (second, first, ARRAY_LEN (second)) ;

	if (memcmp (orig, first, sizeof (orig)) == 0)
	{	printf ("\n\nLine %d : test 1 : these two array should not be the same:\n\n", __LINE__) ;
		dump_[+ (get "name") +]_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_[+ (get "name") +]_array ("first", first, ARRAY_LEN (first)) ;
		exit (1) ;
		} ;

	if (memcmp (orig, second, sizeof (orig)) != 0)
	{	printf ("\n\nLine %d : test 2 : these two array should be the same:\n\n", __LINE__) ;
		dump_[+ (get "name") +]_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_[+ (get "name") +]_array ("second", second, ARRAY_LEN (second)) ;
		exit (1) ;
		} ;

	endswap_[+ (get "name") +]_array (first, ARRAY_LEN (first)) ;

	if (memcmp (orig, first, sizeof (orig)) != 0)
	{	printf ("\n\nLine %d : test 3 : these two array should be the same:\n\n", __LINE__) ;
		dump_[+ (get "name") +]_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_[+ (get "name") +]_array ("first", first, ARRAY_LEN (first)) ;
		exit (1) ;
		} ;

	endswap_[+ (get "name") +]_copy (first, orig, ARRAY_LEN (first)) ;
	endswap_[+ (get "name") +]_copy (first, first, ARRAY_LEN (first)) ;

	if (memcmp (orig, first, sizeof (orig)) != 0)
	{	printf ("\n\nLine %d : test 4 : these two array should be the same:\n\n", __LINE__) ;
		dump_[+ (get "name") +]_array ("orig", orig, ARRAY_LEN (orig)) ;
		dump_[+ (get "name") +]_array ("first", first, ARRAY_LEN (first)) ;
		exit (1) ;
		} ;

	puts ("ok") ;
} /* test_endswap_[+ (get "name") +] */
[+ ENDFOR int_type
+]


void
test_endswap (void)
{
[+ FOR int_type
+]	test_endswap_[+ (get "name") +] () ;
[+ ENDFOR int_type
+]
} /* test_endswap */

