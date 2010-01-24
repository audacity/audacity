/*
** Copyright (C) 2002-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#define	BUFFER_LEN		(1<<10)
#define LOG_BUFFER_SIZE	1024

static	void	raw_offset_test (const char *filename, int typeminor) ;
static	void	bad_raw_test (void) ;

/* Force the start of this buffer to be double aligned. Sparc-solaris will
** choke if its not.
*/
static	short	data [BUFFER_LEN] ;

int
main (void)
{
	raw_offset_test ("offset.raw", SF_FORMAT_PCM_16) ;
	bad_raw_test () ;

	return 0 ;
} /* main */

/*============================================================================================
**	Here are the test functions.
*/

static void
raw_offset_test (const char *filename, int typeminor)
{	SNDFILE		*sndfile ;
	SF_INFO		sfinfo ;
	sf_count_t	start ;
	int			k, frames ;

	print_test_name ("raw_offset_test", filename) ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.format		= SF_FORMAT_RAW | typeminor ;
	sfinfo.channels		= 1 ;
	sfinfo.frames		= 0 ;

	frames = BUFFER_LEN / sfinfo.channels ;

	sndfile = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_TRUE, __LINE__) ;

	start = 0 ;
	sf_command (sndfile, SFC_FILE_TRUNCATE, &start, sizeof (start)) ;

	for (k = 0 ; k < BUFFER_LEN ; k++)
		data [k] = k ;
	test_write_short_or_die (sndfile, 0, data, BUFFER_LEN, __LINE__) ;

	sf_close (sndfile) ;

	sndfile = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	check_log_buffer_or_die (sndfile, __LINE__) ;

	if (abs (BUFFER_LEN - sfinfo.frames) > 1)
	{	printf ("\n\nLine %d : Incorrect sample count (%ld should be %d)\n", __LINE__, SF_COUNT_TO_LONG (sfinfo.frames), BUFFER_LEN) ;
		dump_log_buffer (sndfile) ;
		exit (1) ;
		} ;

	memset (data, 0 , sizeof (data)) ;
	test_read_short_or_die (sndfile, 0, data, BUFFER_LEN, __LINE__) ;
	for (k = 0 ; k < BUFFER_LEN ; k++)
		if (data [k] != k)
			printf ("Error : line %d\n", __LINE__) ;

	/* Set dataoffset to 2 bytes from beginning of file. */
	start = 2 ;
	sf_command (sndfile, SFC_SET_RAW_START_OFFSET, &start, sizeof (start)) ;

	/* Seek to new start */
	test_seek_or_die (sndfile, 0, SEEK_SET, 0, sfinfo.channels, __LINE__) ;

	memset (data, 0 , sizeof (data)) ;
	test_read_short_or_die (sndfile, 0, data, BUFFER_LEN - 1, __LINE__) ;
	for (k = 0 ; k < BUFFER_LEN - 1 ; k++)
		if (data [k] != k + 1)
		{	printf ("Error : line %d\n", __LINE__) ;
			exit (1) ;
			} ;

	/* Set dataoffset to 4 bytes from beginning of file. */
	start = 4 ;
	sf_command (sndfile, SFC_SET_RAW_START_OFFSET, &start, sizeof (start)) ;

	/* Seek to new start */
	test_seek_or_die (sndfile, 0, SEEK_SET, 0, sfinfo.channels, __LINE__) ;

	memset (data, 0 , sizeof (data)) ;
	test_read_short_or_die (sndfile, 0, data, BUFFER_LEN - 2, __LINE__) ;
	for (k = 0 ; k < BUFFER_LEN - 2 ; k++)
		if (data [k] != k + 2)
		{	printf ("Error : line %d\n", __LINE__) ;
			exit (1) ;
			} ;

	/* Set dataoffset back to 0 bytes from beginning of file. */
	start = 0 ;
	sf_command (sndfile, SFC_SET_RAW_START_OFFSET, &start, sizeof (start)) ;

	/* Seek to new start */
	test_seek_or_die (sndfile, 0, SEEK_SET, 0, sfinfo.channels, __LINE__) ;

	memset (data, 0 , sizeof (data)) ;
	test_read_short_or_die (sndfile, 0, data, BUFFER_LEN, __LINE__) ;
	for (k = 0 ; k < BUFFER_LEN ; k++)
		if (data [k] != k)
		{	printf ("Error : line %d\n", __LINE__) ;
			exit (1) ;
			} ;

	sf_close (sndfile) ;
	unlink (filename) ;

	puts ("ok") ;
} /* raw_offset_test */

static void
bad_raw_test (void)
{	FILE		*textfile ;
	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	const char	*errorstr, *filename = "bad.raw" ;

	print_test_name ("bad_raw_test", filename) ;

	if ((textfile = fopen (filename, "w")) == NULL)
	{	printf ("\n\nLine %d : not able to open text file for write.\n", __LINE__) ;
		exit (1) ;
		} ;

	fprintf (textfile, "This is not a valid file.\n") ;
	fclose (textfile) ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.format		= SF_FORMAT_RAW | 0xABCD ;
	sfinfo.channels		= 1 ;

	if ((file = sf_open (filename, SFM_READ, &sfinfo)) != NULL)
	{	printf ("\n\nLine %d : Error, file should not have opened.\n", __LINE__ - 1) ;
		exit (1) ;
		} ;

	errorstr = sf_strerror (file) ;

	if (strstr (errorstr, "Bad format field in SF_INFO struct") == NULL)
	{	printf ("\n\nLine %d : Error bad error string : %s.\n", __LINE__ - 1, errorstr) ;
		exit (1) ;
		} ;

	unlink (filename) ;

	puts ("ok") ;
} /* bad_raw_test */

