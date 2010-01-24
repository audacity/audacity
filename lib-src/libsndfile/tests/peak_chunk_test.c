/*
** Copyright (C) 2001-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#define	BUFFER_LEN		(1<<15)
#define LOG_BUFFER_SIZE	1024


static	void	test_float_peak	(const char *filename, int filetype) ;
static	void	read_write_peak_test	(const char *filename, int filetype) ;

static void		check_logged_peaks (char *buffer) ;

/* Force the start of this buffer to be double aligned. Sparc-solaris will
** choke if its not.
*/
static	double	data [BUFFER_LEN] ;
static	char	log_buffer [LOG_BUFFER_SIZE] ;

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

	do_all = ! strcmp (argv [1], "all") ;

	if (do_all || ! strcmp (argv [1], "wav"))
	{	test_float_peak ("peak_float.wav", SF_FORMAT_WAV | SF_FORMAT_FLOAT) ;
		test_float_peak ("peak_float.wavex", SF_FORMAT_WAVEX | SF_FORMAT_FLOAT) ;
		test_float_peak ("peak_float.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_FLOAT) ;

		read_write_peak_test ("rw_peak.wav", SF_FORMAT_WAV | SF_FORMAT_FLOAT) ;
		read_write_peak_test ("rw_peak.wavex", SF_FORMAT_WAVEX | SF_FORMAT_FLOAT) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "aiff"))
	{	test_float_peak	("peak_float.aiff", SF_FORMAT_AIFF | SF_FORMAT_FLOAT) ;

		read_write_peak_test ("rw_peak.aiff", SF_FORMAT_AIFF | SF_FORMAT_FLOAT) ;
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
test_float_peak (const char *filename, int filetype)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int			k, frames, count ;

	print_test_name ("test_float_peak", filename) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	sfinfo.samplerate	= 44100 ;
	sfinfo.format		= filetype ;
	sfinfo.channels		= 4 ;
	sfinfo.frames		= 0 ;

	frames = BUFFER_LEN / sfinfo.channels ;

	/* Create some random data with a peak value of 0.66. */
	for (k = 0 ; k < BUFFER_LEN ; k++)
		data [k] = (rand () % 2000) / 3000.0 ;

	/* Insert some larger peaks a know locations. */
	data [4 * (frames / 8) + 0] = (frames / 8) * 0.01 ;	/* First channel */
	data [4 * (frames / 6) + 1] = (frames / 6) * 0.01 ;	/* Second channel */
	data [4 * (frames / 4) + 2] = (frames / 4) * 0.01 ;	/* Third channel */
	data [4 * (frames / 2) + 3] = (frames / 2) * 0.01 ;	/* Fourth channel */

	/* Write a file with PEAK chunks. */
	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, 0, __LINE__) ;

	/* Try to confuse the header writer by adding a removing the PEAK chunk. */
	sf_command (file, SFC_SET_ADD_PEAK_CHUNK, NULL, SF_TRUE) ;
	sf_command (file, SFC_SET_ADD_PEAK_CHUNK, NULL, SF_FALSE) ;
	sf_command (file, SFC_SET_ADD_PEAK_CHUNK, NULL, SF_TRUE) ;

	/*	Write the data in four passed. The data is designed so that peaks will
	**	be written in the different calls to sf_write_double ().
	*/
	for (count = 0 ; count < 4 ; count ++)
		test_write_double_or_die (file, 0, data + count * BUFFER_LEN / 4, BUFFER_LEN / 4, BUFFER_LEN / 4) ;

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, 0, __LINE__) ;

	if (sfinfo.format != filetype)
	{	printf ("\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames != frames)
	{	printf ("\n\nLine %d: Incorrect number of frames in file. (%d => %ld)\n", __LINE__, frames, (long) sfinfo.frames) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != 4)
	{	printf ("\n\nLine %d: Incorrect number of channels in file.\n", __LINE__) ;
		exit (1) ;
		} ;

	/* Check these two commands. */
	if (sf_command (file, SFC_GET_SIGNAL_MAX, data, sizeof (double)) == SF_FALSE)
	{	printf ("\n\nLine %d: Command should have returned SF_TRUE.\n", __LINE__) ;
		exit (1) ;
		} ;

	if (fabs (data [0] - (frames / 2) * 0.01) > 0.01)
	{	printf ("\n\nLine %d: Bad peak value (%f should be %f) for command SFC_GET_SIGNAL_MAX.\n", __LINE__, data [0], (frames / 2) * 0.01) ;
		exit (1) ;
		} ;

	if (sf_command (file, SFC_GET_MAX_ALL_CHANNELS, data, sizeof (double) * sfinfo.channels) == SF_FALSE)
	{	printf ("\n\nLine %d: Command should have returned SF_TRUE.\n", __LINE__) ;
		exit (1) ;
		} ;

	if (fabs (data [3] - (frames / 2) * 0.01) > 0.01)
	{	printf ("\n\nLine %d: Bad peak value (%f should be %f) for command SFC_GET_MAX_ALL_CHANNELS.\n", __LINE__, data [0], (frames / 2) * 0.01) ;
		exit (1) ;
		} ;

	/* Get the log buffer data. */
	log_buffer [0] = 0 ;
	sf_command	(file, SFC_GET_LOG_INFO, log_buffer, LOG_BUFFER_SIZE) ;

	if (strlen (log_buffer) == 0)
	{	printf ("\n\nLine %d: Empty log buffer,\n", __LINE__) ;
		exit (1) ;
		} ;

	check_logged_peaks (log_buffer) ;

	sf_close (file) ;

	/* Write a file ***without*** PEAK chunks. */
	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, 0, __LINE__) ;

	/* Try to confuse the header writer by adding a removing the PEAK chunk. */
	sf_command (file, SFC_SET_ADD_PEAK_CHUNK, NULL, SF_FALSE) ;
	sf_command (file, SFC_SET_ADD_PEAK_CHUNK, NULL, SF_TRUE) ;
	sf_command (file, SFC_SET_ADD_PEAK_CHUNK, NULL, SF_FALSE) ;

	/*	Write the data in four passed. The data is designed so that peaks will
	**	be written in the different calls to sf_write_double ().
	*/
	for (count = 0 ; count < 4 ; count ++)
		test_write_double_or_die (file, 0, data + count * BUFFER_LEN / 4, BUFFER_LEN / 4, BUFFER_LEN / 4) ;

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, 0, __LINE__) ;

	/* Check these two commands. */
	if (sf_command (file, SFC_GET_SIGNAL_MAX, data, sizeof (double)))
	{	printf ("\n\nLine %d: Command should have returned SF_FALSE.\n", __LINE__) ;
		exit (1) ;
		} ;

	if (sf_command (file, SFC_GET_MAX_ALL_CHANNELS, data, sizeof (double) * sfinfo.channels))
	{	printf ("\n\nLine %d: Command should have returned SF_FALSE.\n", __LINE__) ;
		exit (1) ;
		} ;

	/* Get the log buffer data. */
	log_buffer [0] = 0 ;
	sf_command	(file, SFC_GET_LOG_INFO, log_buffer, LOG_BUFFER_SIZE) ;

	if (strlen (log_buffer) == 0)
	{	printf ("\n\nLine %d: Empty log buffer,\n", __LINE__) ;
		exit (1) ;
		} ;

	if (strstr (log_buffer, "PEAK :") != NULL)
	{	printf ("\n\nLine %d: Should not have a PEAK chunk in this file.\n\n", __LINE__) ;
		puts (log_buffer) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	unlink (filename) ;
	printf ("ok\n") ;
} /* test_float_peak */

static void
check_logged_peaks (char *buffer)
{	char 	*cptr ;
	int		k, chan, channel_count, position ;
	float	value ;

	if (strstr (buffer, "should") || strstr (buffer, "*"))
	{	printf ("\n\nLine %d: Something wrong in buffer. Dumping.\n", __LINE__) ;
		puts (buffer) ;
		exit (1) ;
		} ;

	if (! (cptr = strstr (buffer, "Channels")) || sscanf (cptr, "Channels      : %d", &channel_count) != 1)
	{	printf ("\n\nLine %d: Couldn't find channel count.\n", __LINE__) ;
		exit (1) ;
		} ;

	if (channel_count != 4)
	{	printf ("\n\nLine %d: Wrong channel count (4 ->%d).\n", __LINE__, channel_count) ;
		exit (1) ;
		} ;

	if (! (cptr = strstr (buffer, "Ch   Position       Value")))
	{	printf ("\n\nLine %d: Can't find PEAK data.\n", __LINE__) ;
		exit (1) ;
		} ;

	for (k = 0 ; k < channel_count ; k++)
	{	if (! (cptr = strchr (cptr, '\n')))
		{	printf ("\n\nLine %d: Got lost.\n", __LINE__) ;
			exit (1) ;
			} ;
		if (sscanf (cptr, "%d %d %f", &chan, &position, &value) != 3)
		{	printf ("\n\nLine %d: sscanf failed.\n", __LINE__) ;
			exit (1) ;
			} ;
		if (position == 0)
		{	printf ("\n\nLine %d: peak position for channel %d should not be at offset 0.\n", __LINE__, chan) ;
			printf ("%s", buffer) ;
			exit (1) ;
			} ;
		if (chan != k || fabs ((position) * 0.01 - value) > 1e-6)
		{	printf ("\n\nLine %d: Error : peak value incorrect!\n", __LINE__) ;
			printf ("%s", buffer) ;
			printf ("\n\nLine %d: %d %f %f\n", __LINE__, chan, position * 0.01, value) ;
			exit (1) ;
			} ;
		cptr ++ ; /* Move past current newline. */
		} ;

} /* check_logged_peaks */

static	void
read_write_peak_test (const char *filename, int filetype)
{	SNDFILE	*file ;
    SF_INFO	sfinfo ;

    double   small_data [10] ;
    double   max_peak = 0.0 ;
    unsigned k ;

	print_test_name (__func__, filename) ;

    for (k = 0 ; k < ARRAY_LEN (small_data) ; k ++)
        small_data [k] = 0.1 ;

    sfinfo.samplerate	= 44100 ;
    sfinfo.channels		= 2 ;
    sfinfo.format		= filetype ;
    sfinfo.frames		= 0 ;

	/* Open the file, add peak chunk and write samples with value 0.1. */
    file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;

    sf_command (file, SFC_SET_ADD_PEAK_CHUNK, NULL, SF_TRUE) ;

	test_write_double_or_die (file, 0, small_data, ARRAY_LEN (small_data), __LINE__) ;

    sf_close (file) ;

    /* Open the fiel RDWR, write sample valied 1.25. */
    file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_FALSE, __LINE__) ;

    for (k = 0 ; k < ARRAY_LEN (small_data) ; k ++)
        small_data [k] = 1.0 ;

	test_write_double_or_die (file, 0, small_data, ARRAY_LEN (small_data), __LINE__) ;

    sf_command (file, SFC_GET_SIGNAL_MAX, &max_peak, sizeof (max_peak)) ;

    sf_close (file) ;

    exit_if_true (max_peak < 0.1, "\n\nLine %d : max peak (%5.3f) should not be 0.1.\n\n", __LINE__, max_peak) ;

    /* Open the file and test the values written to the PEAK chunk. */
    file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	exit_if_true (sfinfo.channels * sfinfo.frames != 2 * ARRAY_LEN (small_data),
			"Line %d : frame count is %ld, should be %d\n", __LINE__, SF_COUNT_TO_LONG (sfinfo.frames), 2 * ARRAY_LEN (small_data)) ;

    sf_command (file, SFC_GET_SIGNAL_MAX, &max_peak, sizeof (double)) ;

    sf_close (file) ;

    exit_if_true (max_peak < 1.0, "\n\nLine %d : max peak (%5.3f) should be 1.0.\n\n", __LINE__, max_peak) ;

	unlink (filename) ;
	puts ("ok") ;
} /* read_write_peak_test */

