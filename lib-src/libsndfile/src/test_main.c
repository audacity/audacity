/*
** Copyright (C) 2008-2016 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#if defined (HAVE_SYS_TYPES_H) && (HAVE_SYS_TYPES_H == 1)
#include <sys/types.h>
#endif
#include <string.h>
#include <stdarg.h>
#include <errno.h>

#include "test_main.h"

static void
test_file_offsets_are_64_bit (void)
{
	print_test_name ("File offsets are 64 bit") ;

	// The Windows specific code path uses the 64 bit file I/O APIs.
	if (! USE_WINDOWS_API && sizeof (off_t) != 8)
	{	printf ("\n\nError : sizeof (off_t) is %zd (should be 8).\n\n", sizeof (off_t)) ;
		exit (1) ;
		} ;

	puts ("ok") ;
} /* test_file_offsets_are_64_bit */

int
main (void)
{
	test_file_offsets_are_64_bit () ;
	test_conversions () ;
	test_endswap () ;
	test_float_convert () ;
	test_double_convert () ;

	test_log_printf () ;
	test_binheader_writef () ;
	test_file_io () ;

	test_audio_detect () ;
	test_ima_oki_adpcm () ;

	test_psf_strlcpy_crlf () ;
	test_broadcast_var () ;
	test_cart_var () ;

	test_nms_adpcm () ;

	return 0 ;
} /* main */

