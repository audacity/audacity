/*
** Copyright (C) 2001-2014 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#else
#include "sf_unistd.h"
#endif

#include <sndfile.h>

#include "utils.h"

static	void	stdout_test	(int typemajor, int count) ;

int
main (int argc, char *argv [])
{	int		do_all, test_count = 0 ;

	if (argc != 2)
	{	fprintf (stderr, "This program cannot be run by itself. It needs\n") ;
		fprintf (stderr, "to be run from the stdio_test program.\n") ;
		exit (1) ;
		} ;

	do_all = ! strcmp (argv [1], "all") ;

	if (do_all || ! strcmp (argv [1], "raw"))
	{	stdout_test	(SF_FORMAT_RAW, PIPE_TEST_LEN) ;
		test_count ++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "wav"))
	{	stdout_test	(SF_FORMAT_WAV, PIPE_TEST_LEN) ;
		test_count ++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "aiff"))
	{	stdout_test	(SF_FORMAT_AIFF, PIPE_TEST_LEN) ;
		test_count ++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "au"))
	{	stdout_test	(SF_FORMAT_AU, PIPE_TEST_LEN) ;
		test_count ++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "paf"))
	{	stdout_test	(SF_FORMAT_PAF, PIPE_TEST_LEN) ;
		test_count ++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "svx"))
	{	stdout_test	(SF_FORMAT_SVX, PIPE_TEST_LEN) ;
		test_count ++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "nist"))
	{	stdout_test	(SF_FORMAT_NIST, PIPE_TEST_LEN) ;
		test_count ++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "ircam"))
	{	stdout_test	(SF_FORMAT_IRCAM, PIPE_TEST_LEN) ;
		test_count ++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "voc"))
	{	stdout_test	(SF_FORMAT_VOC, PIPE_TEST_LEN) ;
		test_count ++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "w64"))
	{	stdout_test	(SF_FORMAT_W64, PIPE_TEST_LEN) ;
		test_count ++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "mat4"))
	{	stdout_test	(SF_FORMAT_MAT4, PIPE_TEST_LEN) ;
		test_count ++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "mat5"))
	{	stdout_test	(SF_FORMAT_MAT5, PIPE_TEST_LEN) ;
		test_count ++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "pvf"))
	{	stdout_test	(SF_FORMAT_PVF, PIPE_TEST_LEN) ;
		test_count ++ ;
		} ;

	if (test_count == 0)
	{	fprintf (stderr, "\n******************************************\n") ;
		fprintf (stderr, "*  stdout_test : No '%s' test defined.\n", argv [1]) ;
		fprintf (stderr, "******************************************\n") ;
		return 1 ;
		} ;

	return 0 ;
} /* main */

static	void
stdout_test	(int typemajor, int count)
{	static	short	data [PIPE_TEST_LEN] ;

	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int			k, total, this_write ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.format		= (typemajor | SF_FORMAT_PCM_16) ;
	sfinfo.channels		= 1 ;
	sfinfo.frames		= 0 ;

	/* Create some random data. */
	for (k = 0 ; k < PIPE_TEST_LEN ; k++)
		data [k] = PIPE_INDEX (k) ;

	if ((file = sf_open ("-", SFM_WRITE, &sfinfo)) == NULL)
	{	fprintf (stderr, "%s % d: sf_open_write failed with error : %s\n",
									__func__, __LINE__, sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	if (sfinfo.frames != 0)
	{	fprintf (stderr, "%s % d: Frames is %d (should be 0).\n",
									__func__, __LINE__, (int) sfinfo.frames) ;
		exit (1) ;
		} ;

	total = 0 ;

	while (total < count)
	{	this_write = (count - total > 1024) ? 1024 : count - total ;
		if ((k = sf_write_short (file, data + total, this_write)) != this_write)
		{	fprintf (stderr, "sf_write_short # %d failed with short write (%d -> %d)\n", count, this_write, k) ;
			exit (1) ;
			} ;
		total += k ;
		} ;

	sf_close (file) ;

	return ;
} /* stdout_test */

