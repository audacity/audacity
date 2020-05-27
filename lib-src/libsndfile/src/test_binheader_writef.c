/*
** Copyright (C) 2017 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include "common.h"

#include "test_main.h"

void
test_binheader_writef (void)
{	char buffer [18] ;
	SF_PRIVATE	sf_private, *psf ;
	int			k, errors = 0 ;

	print_test_name ("Testing binheader_writef") ;

	memset (&sf_private, 0, sizeof (sf_private)) ;

	psf = &sf_private ;
	for (k = 0 ; errors == 0 && k < 10 ; k++)
	{	psf_strlcpy (buffer, sizeof (buffer), "abcdefghijklmnop") ;
		buffer [k] = 0 ;

		psf_binheader_writef (psf, "Ep", BHWp (buffer)) ;

		if ((psf->header.indx & 1) != 0)
			errors = 1 ;
		} ;

	free (psf->header.ptr) ;

	if (errors)
	{	puts ("\nExiting due to errors.\n") ;
		exit (1) ;
		} ;

	puts ("ok") ;
} /* test_log_printf */

