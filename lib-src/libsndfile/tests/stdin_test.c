/*
** Copyright (C) 2001-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <math.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <sndfile.h>

#include "utils.h"

#define	BUFFER_LEN		(1<<16)

static	void	stdin_test	(int typemajor, int count) ;

int
main (int argc, char *argv [])
{	int		do_all = 0, test_count = 0 ;

	if (BUFFER_LEN < PIPE_TEST_LEN)
	{	fprintf (stderr, "Error : BUFFER_LEN < PIPE_TEST_LEN.\n\n") ;
		exit (1) ;
		} ;

	if (argc != 2)
	{	fprintf (stderr, "This program cannot be run by itself. It needs\n") ;
		fprintf (stderr, "to be run from the stdio_test program.\n") ;
		exit (1) ;
		} ;

	do_all = ! strcmp (argv [1], "all") ;

	if (do_all || ! strcmp (argv [1], "raw"))
	{	stdin_test	(SF_FORMAT_RAW, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "wav"))
	{	stdin_test	(SF_FORMAT_WAV, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "aiff"))
	{	stdin_test	(SF_FORMAT_AIFF, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "au"))
	{	stdin_test	(SF_FORMAT_AU, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "paf"))
	{	stdin_test	(SF_FORMAT_PAF, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "svx"))
	{	stdin_test	(SF_FORMAT_SVX, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "nist"))
	{	stdin_test	(SF_FORMAT_NIST, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "ircam"))
	{	stdin_test	(SF_FORMAT_IRCAM, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "voc"))
	{	stdin_test	(SF_FORMAT_VOC, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "w64"))
	{	stdin_test	(SF_FORMAT_W64, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "mat4"))
	{	stdin_test	(SF_FORMAT_MAT4, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "mat5"))
	{	stdin_test	(SF_FORMAT_MAT5, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "pvf"))
	{	stdin_test	(SF_FORMAT_PVF, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "htk"))
	{	stdin_test	(SF_FORMAT_HTK, PIPE_TEST_LEN) ;
		test_count++ ;
		} ;

	if (test_count == 0)
	{	fprintf (stderr, "************************************\n") ;
		fprintf (stderr, "*  No '%s' test defined.\n", argv [1]) ;
		fprintf (stderr, "************************************\n") ;
		return 1 ;
		} ;

	return 0 ;
} /* main */

static	void
stdin_test	(int typemajor, int count)
{	static	short	data [BUFFER_LEN] ;

	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int			k, total, err ;

	if (typemajor == SF_FORMAT_RAW)
	{	sfinfo.samplerate	= 44100 ;
		sfinfo.format		= SF_FORMAT_RAW | SF_FORMAT_PCM_16 ;
		sfinfo.channels		= 1 ;
		sfinfo.frames		= 0 ;
		}
	else
		memset (&sfinfo, 0, sizeof (sfinfo)) ;

	if ((file = sf_open_fd (STDIN_FILENO, SFM_READ, &sfinfo, SF_TRUE)) == NULL)
	{	fprintf (stderr, "sf_open_fd failed with error : ") ;
		puts (sf_strerror (NULL)) ;
		dump_log_buffer (NULL) ;
		exit (1) ;
		} ;

	err = sf_error (file) ;
	if (err != SF_ERR_NO_ERROR)
	{	printf ("Line %d : unexpected error : %s\n", __LINE__, sf_error_number (err)) ;
		exit (1) ;
		} ;

	if ((sfinfo.format & SF_FORMAT_TYPEMASK) != typemajor)
	{	fprintf (stderr, "\n\nError : File type doesn't match.\n") ;
		exit (1) ;
		} ;

	if (sfinfo.samplerate != 44100)
	{	fprintf (stderr, "\n\nError : Sample rate (%d) should be 44100\n", sfinfo.samplerate) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != 1)
	{	fprintf (stderr, "\n\nError : Channels (%d) should be 1\n", sfinfo.channels) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < count)
	{	fprintf (stderr, "\n\nError : Sample count (%ld) should be %d\n", (long) sfinfo.frames, count) ;
		exit (1) ;
		} ;

	total = 0 ;
	while ((k = sf_read_short (file, data + total, BUFFER_LEN - total)) > 0)
		total += k ;

	if (total != count)
	{	fprintf (stderr, "\n\nError : Expected %d frames, read %d.\n", count, total) ;
		exit (1) ;
		} ;

	for (k = 0 ; k < total ; k++)
		if (data [k] != PIPE_INDEX (k))
		{	printf ("\n\nError : data [%d] == %d, should have been %d.\n\n", k, data [k], k) ;
			exit (1) ;
			} ;

	sf_close (file) ;

	return ;
} /* stdin_test */

