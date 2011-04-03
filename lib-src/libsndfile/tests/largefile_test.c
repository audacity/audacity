/*
** Copyright (C) 2006-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <stdlib.h>
#include <string.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <sndfile.h>

#include "utils.h"

#define	BUFFER_LEN		(1024 * 1024)
#define	BUFFER_COUNT	(768)

static void largefile_test (int filetype, const char * filename) ;

int
main (void)
{
	largefile_test (SF_FORMAT_WAV, "largefile.wav") ;
	largefile_test (SF_FORMAT_AIFF, "largefile.aiff") ;

	return 0 ;
} /* main */

static void
largefile_test (int filetype, const char * filename)
{	static float data [BUFFER_LEN] ;
	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int k ;

	print_test_name ("largefile_test", filename) ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.channels		= 2 ;
	sfinfo.frames		= 0 ;
	sfinfo.format = (filetype | SF_FORMAT_PCM_32) ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;

	for (k = 0 ; k < BUFFER_COUNT ; k++)
		test_write_float_or_die (file, k, data, BUFFER_LEN, __LINE__) ;

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	if ((sfinfo.frames * sfinfo.channels) / BUFFER_LEN != BUFFER_COUNT)
	{	printf ("\n\nLine %d : bad frame count.\n", __LINE__) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	unlink (filename) ;
	puts ("ok") ;


	return ;
} /* largefile_test */

