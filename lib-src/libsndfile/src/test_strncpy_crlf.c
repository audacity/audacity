/*
** Copyright (C) 2010-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include "common.h"

#include "test_main.h"

void
test_psf_strlcpy_crlf (void)
{	const char *src = "a\nb\nc\n" ;
	char *dest ;
	int dest_len ;

	print_test_name ("Testing psf_strlcpy_crlf") ;

	for (dest_len = 3 ; dest_len < 30 ; dest_len++)
	{	dest = calloc (1, dest_len + 1) ;
		if (dest == NULL)
		{	printf ("\n\nLine %d: calloc failed!\n\n", __LINE__) ;
			exit (1) ;
			} ;

		dest [dest_len] = '\xea' ;

		psf_strlcpy_crlf (dest, src, dest_len, sizeof (src)) ;

		if (dest [dest_len] != '\xea')
		{	printf ("\n\nLine %d: buffer overrun for dest_len == %d\n\n", __LINE__, dest_len) ;
			exit (1) ;
			} ;

		free (dest) ;
		} ;

	puts ("ok") ;
} /* test_psf_strlcpy_crlf */
