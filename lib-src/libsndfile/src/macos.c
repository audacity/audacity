/*
** Copyright (C) 2003-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	"sfconfig.h"

#include	<stdlib.h>
#include	<string.h>
#include	<sys/stat.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

#define	STR_MARKER	MAKE_MARKER ('S', 'T', 'R', ' ')

int
macos_guess_file_type (SF_PRIVATE * psf, const char *filename)
{	static char rsrc_name [1024] ;
	struct stat statbuf ;

	snprintf (rsrc_name, sizeof (rsrc_name), "%s/rsrc", filename) ;

	/* If there is no resource fork, just return. */
	if (stat (rsrc_name, &statbuf) != 0)
	{	psf_log_printf (psf, "No resource fork.\n") ;
		return 0 ;
		} ;

	if (statbuf.st_size == 0)
	{	psf_log_printf (psf, "Have zero size resource fork.\n") ;
		return 0 ;
		} ;

	return 0 ;
} /* macos_guess_file_type */

