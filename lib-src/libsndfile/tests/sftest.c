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

#include "sfconfig.h"

#include <stdio.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#else
#include "sf_unistd.h"
#endif

#include <sndfile.h>

#define	BUFFER_SIZE		(1024)


static short buffer [BUFFER_SIZE] ;

int
main (int argc, char *argv [])
{	SNDFILE	*file ;
	SF_INFO sfinfo ;
	int		k, count, max = 0, total = 0 ;

	if (argc < 2)
	{	printf ("Expecting input file name.\n") ;
		return 0 ;
		} ;

	if (! (file = sf_open (argv [1], SFM_READ, &sfinfo)))
	{	printf ("sf_open_read failed with error : ") ;
		puts (sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	while ((count = sf_read_short (file, buffer, BUFFER_SIZE)))
	{	for (k = 0 ; k < count ; k++)
			if (abs (buffer [k]) > max)
				max = abs (buffer [k]) ;
		total += count ;
		} ;

	printf ("Total         : %d\n", total) ;
	printf ("Maximun value : %d\n", max) ;

	sf_close (file) ;

	return 0 ;
} /* main */

