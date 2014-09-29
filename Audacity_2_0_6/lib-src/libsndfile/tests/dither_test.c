/*
** Copyright (C) 2003-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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



#include	<stdio.h>
#include	<stdlib.h>
#include	<unistd.h>
#include	<string.h>
#include	<math.h>

#include	<sndfile.h>

#include	"utils.h"

#define	BUFFER_LEN		(1<<16)
#define LOG_BUFFER_SIZE	1024

static void	dither_test (const char *filename, int filetype) ;

/* Force the start of this buffer to be double aligned. Sparc-solaris will
** choke if its not.
*/
static	short	data_out [BUFFER_LEN] ;

int
main (int argc, char *argv [])
{	int		do_all = 0 ;
	int		test_count = 0 ;

	if (argc != 2)
	{	printf ("Usage : %s <test>\n", argv [0]) ;
		printf ("    Where <test> is one of the following:\n") ;
		printf ("           wav  - test WAV file peak chunk\n") ;
		printf ("           aiff - test AIFF file PEAK chunk\n") ;
		printf ("           all  - perform all tests\n") ;
		exit (1) ;
		} ;

	do_all=!strcmp (argv [1], "all") ;

	if (do_all || ! strcmp (argv [1], "wav"))
	{	dither_test ("dither.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_U8) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "aiff"))
	{	dither_test ("dither.aiff", SF_FORMAT_AIFF | SF_FORMAT_PCM_S8) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "au"))
	{	dither_test ("dither.au", SF_FORMAT_AU | SF_FORMAT_PCM_S8) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "svx"))
	{	dither_test ("dither.svx", SF_FORMAT_SVX | SF_FORMAT_PCM_S8) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "nist"))
	{	dither_test ("dither.nist", SF_FORMAT_NIST | SF_FORMAT_PCM_S8) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "paf"))
	{	dither_test ("dither.paf", SF_FORMAT_PAF | SF_FORMAT_PCM_S8) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "ircam"))
	{	dither_test ("dither.ircam", SF_FORMAT_IRCAM | SF_FORMAT_PCM_S8) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "voc"))
	{	dither_test ("dither.voc", SF_FORMAT_VOC | SF_FORMAT_PCM_S8) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "w64"))
	{	dither_test ("dither.w64", SF_FORMAT_W64 | SF_FORMAT_PCM_S8) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "mat4"))
	{	dither_test ("dither.mat4", SF_FORMAT_MAT4 | SF_FORMAT_PCM_S8) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "mat5"))
	{	dither_test ("dither.mat5", SF_FORMAT_MAT5 | SF_FORMAT_PCM_S8) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "pvf"))
	{	dither_test ("dither.pvf", SF_FORMAT_PVF | SF_FORMAT_PCM_S8) ;
		test_count++ ;
		} ;

	if (test_count == 0)
	{	printf ("Mono : ************************************\n") ;
		printf ("Mono : *  No '%s' test defined.\n", argv [1]) ;
		printf ("Mono : ************************************\n") ;
		return 1 ;
		} ;

	return 0 ;
} /* main */


/*============================================================================================
**	Here are the test functions.
*/

static void
dither_test (const char *filename, int filetype)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	SF_DITHER_INFO	dither ;

	filetype = filetype ;

	print_test_name ("dither_test", filename) ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.format		= filetype ;
	sfinfo.channels		= 1 ;
	sfinfo.frames		= 0 ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;

	/* Check for old version of the dither API. */
	if (sf_command (file, SFC_SET_DITHER_ON_WRITE, NULL, SF_TRUE) == 0)
	{	printf ("\n\nLine %d: Should have an error here but don't.\n\n", __LINE__) ;
		exit (1) ;
		} ;

	memset (&dither, 0, sizeof (dither)) ;
	dither.type = SFD_WHITE ;
	dither.level = 0 ;

	if (sf_command (file, SFC_SET_DITHER_ON_WRITE, &dither, sizeof (dither)) != 0)
	{	printf ("\n\nLine %d: sf_command (SFC_SET_DITHER_ON_WRITE) returned error : %s\n\n",
			__LINE__, sf_strerror (file)) ;
		exit (1) ;
		} ;

	/* Write data to file. */
	test_write_short_or_die (file, 0, data_out, BUFFER_LEN, __LINE__) ;
	test_seek_or_die (file, 0, SEEK_SET, 0, sfinfo.channels, __LINE__) ;

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	if (sfinfo.frames != BUFFER_LEN)
	{	printf ("\n\nLine %d: Bad frame count %d (should be %d)\n\n", __LINE__, (int) sfinfo.frames, BUFFER_LEN) ;
		} ;

	sf_close (file) ;
	/*-unlink (filename) ;-*/

	puts ("ok") ;
} /* dither_test */

