/*
** Copyright (C) 1999-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sndfile.h>

#define	BUFFER_SIZE	(256)


int
main (void)
{	static char	strbuffer [BUFFER_SIZE] ;
	const char * ver ;

	sf_command (NULL, SFC_GET_LIB_VERSION, strbuffer, sizeof (strbuffer)) ;
	ver = sf_version_string () ;

	if (strcmp (ver, strbuffer) != 0)
	{	printf ("Version mismatch : '%s' != '%s'\n\n", ver, strbuffer) ;
		exit (1) ;
		} ;

	printf ("%s", strbuffer) ;

	return 0 ;
} /* main */

