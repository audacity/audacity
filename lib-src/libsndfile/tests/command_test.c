/*
** Copyright (C) 2001-2019 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <time.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#else
#include "sf_unistd.h"
#endif

#include <math.h>

#include <sndfile.h>

#include "sfendian.h"
#include "utils.h"

#define	BUFFER_LEN		(1 << 10)
#define LOG_BUFFER_SIZE	1024
#define data_MARKER		MAKE_MARKER ('d', 'a', 't', 'a')

static	void	float_norm_test			(const char *filename) ;
static	void	double_norm_test		(const char *filename) ;
static	void	format_tests			(void) ;
static	void	calc_peak_test			(int filetype, const char *filename, int channels) ;
static	void	truncate_test			(const char *filename, int filetype) ;
static	void	instrument_test			(const char *filename, int filetype) ;
static	void	cue_test				(const char *filename, int filetype) ;
static	void	cue_test_var			(const char *filename, int filetype, int count) ;
static	void	channel_map_test		(const char *filename, int filetype) ;
static	void	current_sf_info_test	(const char *filename) ;
static	void	raw_needs_endswap_test	(const char *filename, int filetype) ;

static	void	broadcast_test			(const char *filename, int filetype) ;
static	void	broadcast_rdwr_test		(const char *filename, int filetype) ;
static	void	broadcast_coding_history_test	(const char *filename) ;
static	void	broadcast_coding_history_size	(const char *filename) ;

/* Cart Chunk tests */
static void	cart_test				(const char *filename, int filetype) ;
static void	cart_rdwr_test			(const char *filename, int filetype) ;

/* Force the start of this buffer to be double aligned. Sparc-solaris will
** choke if its not.
*/

static	int		int_data	[BUFFER_LEN] ;
static	float	float_data	[BUFFER_LEN] ;
static	double	double_data	[BUFFER_LEN] ;

int
main (int argc, char *argv [])
{	int		do_all = 0 ;
	int		test_count = 0 ;

	if (argc != 2)
	{	printf ("Usage : %s <test>\n", argv [0]) ;
		printf ("    Where <test> is one of the following:\n") ;
		printf ("           ver     - test sf_command (SFC_GETLIB_VERSION)\n") ;
		printf ("           norm    - test floating point normalisation\n") ;
		printf ("           format  - test format string commands\n") ;
		printf ("           peak    - test peak calculation\n") ;
		printf ("           trunc   - test file truncation\n") ;
		printf ("           inst    - test set/get of SF_INSTRUMENT.\n") ;
		printf ("           cue     - test set/get of SF_CUES.\n") ;
		printf ("           chanmap - test set/get of channel map data..\n") ;
		printf ("           bext    - test set/get of SF_BROADCAST_INFO.\n") ;
		printf ("           bextch  - test set/get of SF_BROADCAST_INFO coding_history.\n") ;
		printf ("           cart    - test set/get of SF_CART_INFO.\n") ;
		printf ("           rawend  - test SFC_RAW_NEEDS_ENDSWAP.\n") ;
		printf ("           all     - perform all tests\n") ;
		exit (1) ;
		} ;

	do_all = ! strcmp (argv [1], "all") ;

	if (do_all || strcmp (argv [1], "ver") == 0)
	{	char buffer [128] ;

		print_test_name ("version_test", "(none)") ;
		buffer [0] = 0 ;
		sf_command (NULL, SFC_GET_LIB_VERSION, buffer, sizeof (buffer)) ;
		if (strlen (buffer) < 1)
		{	printf ("Line %d: could not retrieve lib version.\n", __LINE__) ;
			exit (1) ;
			} ;
		puts ("ok") ;
		test_count ++ ;
		} ;

	if (do_all || strcmp (argv [1], "norm") == 0)
	{	/*	Preliminary float/double normalisation tests. More testing
		**	is done in the program 'floating_point_test'.
		*/
		float_norm_test		("float.wav") ;
		double_norm_test	("double.wav") ;
		test_count ++ ;
		} ;

	if (do_all || strcmp (argv [1], "peak") == 0)
	{	calc_peak_test (SF_ENDIAN_BIG		| SF_FORMAT_RAW, "be-peak.raw", 1) ;
		calc_peak_test (SF_ENDIAN_LITTLE	| SF_FORMAT_RAW, "le-peak.raw", 1) ;
		calc_peak_test (SF_ENDIAN_BIG		| SF_FORMAT_RAW, "be-peak.raw", 7) ;
		calc_peak_test (SF_ENDIAN_LITTLE	| SF_FORMAT_RAW, "le-peak.raw", 7) ;
		test_count ++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "format"))
	{	format_tests () ;
		test_count ++ ;
		} ;

	if (do_all || strcmp (argv [1], "trunc") == 0)
	{	truncate_test ("truncate.raw", SF_FORMAT_RAW | SF_FORMAT_PCM_32) ;
		truncate_test ("truncate.au" , SF_FORMAT_AU | SF_FORMAT_PCM_16) ;
		test_count ++ ;
		} ;

	if (do_all || strcmp (argv [1], "inst") == 0)
	{	instrument_test ("instrument.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_16) ;
		/*-instrument_test ("instrument.aiff" , SF_FORMAT_AIFF | SF_FORMAT_PCM_24) ;-*/
		/*-instrument_test ("instrument.xi", SF_FORMAT_XI | SF_FORMAT_DPCM_16) ;-*/
		test_count ++ ;
		} ;

	if (do_all || strcmp (argv [1], "cue") == 0)
	{	/* 2500 is close to the largest number of cues possible because of block sizes (enforced in aiff.c, wav.c) */
		int cuecounts [] = { 0, 1, 10, 100, 101, 1000, 1001, 2500 } ;
		unsigned int i ;

		cue_test ("cue.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_16) ;
		cue_test ("cue.aiff" , SF_FORMAT_AIFF | SF_FORMAT_PCM_24) ;

		for (i = 0 ; i < ARRAY_LEN (cuecounts) ; i++)
		{	cue_test_var ("cue.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_16, cuecounts [i]) ;
			cue_test_var ("cue.aiff", SF_FORMAT_AIFF | SF_FORMAT_PCM_24, cuecounts [i]) ;
			} ;

		test_count ++ ;
		} ;

	if (do_all || strcmp (argv [1], "current_sf_info") == 0)
	{	current_sf_info_test ("current.wav") ;
		test_count ++ ;
		} ;

	if (do_all || strcmp (argv [1], "bext") == 0)
	{	broadcast_test ("broadcast.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_16) ;
		broadcast_rdwr_test	("broadcast.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_16) ;

		broadcast_test ("broadcast.wavex", SF_FORMAT_WAVEX | SF_FORMAT_PCM_16) ;
		broadcast_rdwr_test	("broadcast.wavex", SF_FORMAT_WAVEX | SF_FORMAT_PCM_16) ;

		broadcast_test ("broadcast.rf64", SF_FORMAT_RF64 | SF_FORMAT_PCM_16) ;
		broadcast_rdwr_test	("broadcast.rf64", SF_FORMAT_RF64 | SF_FORMAT_PCM_16) ;
		test_count ++ ;
		} ;

	if (do_all || strcmp (argv [1], "cart") == 0)
	{	cart_test ("cart.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_16) ;
		cart_rdwr_test ("cart.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_16) ;
		cart_test ("cart.rf64", SF_FORMAT_RF64 | SF_FORMAT_PCM_16) ;
		cart_rdwr_test ("cart.rf64", SF_FORMAT_RF64 | SF_FORMAT_PCM_16) ;
		test_count ++ ;
		} ;

	if (do_all || strcmp (argv [1], "bextch") == 0)
	{	broadcast_coding_history_test ("coding_history.wav") ;
		broadcast_coding_history_size ("coding_hist_size.wav") ;
		test_count ++ ;
		} ;

	if (do_all || strcmp (argv [1], "chanmap") == 0)
	{	channel_map_test ("chanmap.wavex", SF_FORMAT_WAVEX | SF_FORMAT_PCM_16) ;
		channel_map_test ("chanmap.rf64", SF_FORMAT_RF64 | SF_FORMAT_PCM_16) ;
		channel_map_test ("chanmap.aifc" , SF_FORMAT_AIFF | SF_FORMAT_PCM_16) ;
		channel_map_test ("chanmap.caf" , SF_FORMAT_CAF | SF_FORMAT_PCM_16) ;
		test_count ++ ;
		} ;

	if (do_all || strcmp (argv [1], "rawend") == 0)
	{	raw_needs_endswap_test ("raw_end.wav", SF_FORMAT_WAV) ;
		raw_needs_endswap_test ("raw_end.wavex", SF_FORMAT_WAVEX) ;
		raw_needs_endswap_test ("raw_end.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV) ;
		raw_needs_endswap_test ("raw_end.aiff", SF_FORMAT_AIFF) ;
		raw_needs_endswap_test ("raw_end.aiff_le", SF_ENDIAN_LITTLE | SF_FORMAT_AIFF) ;
		test_count ++ ;
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
float_norm_test (const char *filename)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	unsigned int	k ;

	print_test_name ("float_norm_test", filename) ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.format		= (SF_FORMAT_RAW | SF_FORMAT_PCM_16) ;
	sfinfo.channels		= 1 ;
	sfinfo.frames		= BUFFER_LEN ;

	/* Create float_data with all values being less than 1.0. */
	for (k = 0 ; k < BUFFER_LEN / 2 ; k++)
		float_data [k] = (k + 5) / (2.0 * BUFFER_LEN) ;
	for (k = BUFFER_LEN / 2 ; k < BUFFER_LEN ; k++)
		float_data [k] = (k + 5) ;

	if (! (file = sf_open (filename, SFM_WRITE, &sfinfo)))
	{	printf ("Line %d: sf_open_write failed with error : ", __LINE__) ;
		fflush (stdout) ;
		puts (sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	/* Normalisation is on by default so no need to do anything here. */

	if ((k = sf_write_float (file, float_data, BUFFER_LEN / 2)) != BUFFER_LEN / 2)
	{	printf ("Line %d: sf_write_float failed with short write (%d ->%d)\n", __LINE__, BUFFER_LEN, k) ;
		exit (1) ;
		} ;

	/* Turn normalisation off. */
	sf_command (file, SFC_SET_NORM_FLOAT, NULL, SF_FALSE) ;

	if ((k = sf_write_float (file, float_data + BUFFER_LEN / 2, BUFFER_LEN / 2)) != BUFFER_LEN / 2)
	{	printf ("Line %d: sf_write_float failed with short write (%d ->%d)\n", __LINE__, BUFFER_LEN, k) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	/* sfinfo struct should still contain correct data. */
	if (! (file = sf_open (filename, SFM_READ, &sfinfo)))
	{	printf ("Line %d: sf_open_read failed with error : ", __LINE__) ;
		fflush (stdout) ;
		puts (sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	if (sfinfo.format != (SF_FORMAT_RAW | SF_FORMAT_PCM_16))
	{	printf ("Line %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, (SF_FORMAT_RAW | SF_FORMAT_PCM_16), sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames != BUFFER_LEN)
	{	printf ("\n\nLine %d: Incorrect number of.frames in file. (%d => %" PRId64 ")\n", __LINE__, BUFFER_LEN, sfinfo.frames) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != 1)
	{	printf ("Line %d: Incorrect number of channels in file.\n", __LINE__) ;
		exit (1) ;
		} ;

	/* Read float_data and check that it is normalised (ie default). */
	if ((k = sf_read_float (file, float_data, BUFFER_LEN)) != BUFFER_LEN)
	{	printf ("\n\nLine %d: sf_read_float failed with short read (%d ->%d)\n", __LINE__, BUFFER_LEN, k) ;
		exit (1) ;
		} ;

	for (k = 0 ; k < BUFFER_LEN ; k++)
		if (float_data [k] >= 1.0)
		{	printf ("\n\nLine %d: float_data [%d] == %f which is greater than 1.0\n", __LINE__, k, float_data [k]) ;
			exit (1) ;
			} ;

	/* Seek to start of file, turn normalisation off, read float_data and check again. */
	sf_seek (file, 0, SEEK_SET) ;
	sf_command (file, SFC_SET_NORM_FLOAT, NULL, SF_FALSE) ;

	if ((k = sf_read_float (file, float_data, BUFFER_LEN)) != BUFFER_LEN)
	{	printf ("\n\nLine %d: sf_read_float failed with short read (%d ->%d)\n", __LINE__, BUFFER_LEN, k) ;
		exit (1) ;
		} ;

	for (k = 0 ; k < BUFFER_LEN ; k++)
		if (float_data [k] < 1.0)
		{	printf ("\n\nLine %d: float_data [%d] == %f which is less than 1.0\n", __LINE__, k, float_data [k]) ;
			exit (1) ;
			} ;

	/* Seek to start of file, turn normalisation on, read float_data and do final check. */
	sf_seek (file, 0, SEEK_SET) ;
	sf_command (file, SFC_SET_NORM_FLOAT, NULL, SF_TRUE) ;

	if ((k = sf_read_float (file, float_data, BUFFER_LEN)) != BUFFER_LEN)
	{	printf ("\n\nLine %d: sf_read_float failed with short read (%d ->%d)\n", __LINE__, BUFFER_LEN, k) ;
		exit (1) ;
		} ;

	for (k = 0 ; k < BUFFER_LEN ; k++)
		if (float_data [k] > 1.0)
		{	printf ("\n\nLine %d: float_data [%d] == %f which is greater than 1.0\n", __LINE__, k, float_data [k]) ;
			exit (1) ;
			} ;


	sf_close (file) ;

	unlink (filename) ;

	printf ("ok\n") ;
} /* float_norm_test */

static void
double_norm_test (const char *filename)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	unsigned int	k ;

	print_test_name ("double_norm_test", filename) ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.format		= (SF_FORMAT_RAW | SF_FORMAT_PCM_16) ;
	sfinfo.channels		= 1 ;
	sfinfo.frames		= BUFFER_LEN ;

	/* Create double_data with all values being less than 1.0. */
	for (k = 0 ; k < BUFFER_LEN / 2 ; k++)
		double_data [k] = (k + 5) / (2.0 * BUFFER_LEN) ;
	for (k = BUFFER_LEN / 2 ; k < BUFFER_LEN ; k++)
		double_data [k] = (k + 5) ;

	if (! (file = sf_open (filename, SFM_WRITE, &sfinfo)))
	{	printf ("Line %d: sf_open_write failed with error : ", __LINE__) ;
		fflush (stdout) ;
		puts (sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	/* Normailsation is on by default so no need to do anything here. */
	/*-sf_command (file, "set-norm-double", "true", 0) ;-*/

	if ((k = sf_write_double (file, double_data, BUFFER_LEN / 2)) != BUFFER_LEN / 2)
	{	printf ("Line %d: sf_write_double failed with short write (%d ->%d)\n", __LINE__, BUFFER_LEN, k) ;
		exit (1) ;
		} ;

	/* Turn normalisation off. */
	sf_command (file, SFC_SET_NORM_DOUBLE, NULL, SF_FALSE) ;

	if ((k = sf_write_double (file, double_data + BUFFER_LEN / 2, BUFFER_LEN / 2)) != BUFFER_LEN / 2)
	{	printf ("Line %d: sf_write_double failed with short write (%d ->%d)\n", __LINE__, BUFFER_LEN, k) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	if (! (file = sf_open (filename, SFM_READ, &sfinfo)))
	{	printf ("Line %d: sf_open_read failed with error : ", __LINE__) ;
		fflush (stdout) ;
		puts (sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	if (sfinfo.format != (SF_FORMAT_RAW | SF_FORMAT_PCM_16))
	{	printf ("Line %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, (SF_FORMAT_RAW | SF_FORMAT_PCM_16), sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames != BUFFER_LEN)
	{	printf ("\n\nLine %d: Incorrect number of.frames in file. (%d => %" PRId64 ")\n", __LINE__, BUFFER_LEN, sfinfo.frames) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != 1)
	{	printf ("Line %d: Incorrect number of channels in file.\n", __LINE__) ;
		exit (1) ;
		} ;

	/* Read double_data and check that it is normalised (ie default). */
	if ((k = sf_read_double (file, double_data, BUFFER_LEN)) != BUFFER_LEN)
	{	printf ("\n\nLine %d: sf_read_double failed with short read (%d ->%d)\n", __LINE__, BUFFER_LEN, k) ;
		exit (1) ;
		} ;

	for (k = 0 ; k < BUFFER_LEN ; k++)
		if (double_data [k] >= 1.0)
		{	printf ("\n\nLine %d: double_data [%d] == %f which is greater than 1.0\n", __LINE__, k, double_data [k]) ;
			exit (1) ;
			} ;

	/* Seek to start of file, turn normalisation off, read double_data and check again. */
	sf_seek (file, 0, SEEK_SET) ;
	sf_command (file, SFC_SET_NORM_DOUBLE, NULL, SF_FALSE) ;

	if ((k = sf_read_double (file, double_data, BUFFER_LEN)) != BUFFER_LEN)
	{	printf ("\n\nLine %d: sf_read_double failed with short read (%d ->%d)\n", __LINE__, BUFFER_LEN, k) ;
		exit (1) ;
		} ;

	for (k = 0 ; k < BUFFER_LEN ; k++)
		if (double_data [k] < 1.0)
		{	printf ("\n\nLine %d: double_data [%d] == %f which is less than 1.0\n", __LINE__, k, double_data [k]) ;
			exit (1) ;
			} ;

	/* Seek to start of file, turn normalisation on, read double_data and do final check. */
	sf_seek (file, 0, SEEK_SET) ;
	sf_command (file, SFC_SET_NORM_DOUBLE, NULL, SF_TRUE) ;

	if ((k = sf_read_double (file, double_data, BUFFER_LEN)) != BUFFER_LEN)
	{	printf ("\n\nLine %d: sf_read_double failed with short read (%d ->%d)\n", __LINE__, BUFFER_LEN, k) ;
		exit (1) ;
		} ;

	for (k = 0 ; k < BUFFER_LEN ; k++)
		if (double_data [k] > 1.0)
		{	printf ("\n\nLine %d: double_data [%d] == %f which is greater than 1.0\n", __LINE__, k, double_data [k]) ;
			exit (1) ;
			} ;


	sf_close (file) ;

	unlink (filename) ;

	printf ("ok\n") ;
} /* double_norm_test */

static	void
format_tests	(void)
{	SF_FORMAT_INFO format_info ;
	SF_INFO		sfinfo ;
	const char	*last_name ;
	int 		k, count ;

	print_test_name ("format_tests", "(null)") ;

	/* Clear out SF_INFO struct and set channels > 0. */
	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	sfinfo.channels = 1 ;

	/* First test simple formats. */

	sf_command (NULL, SFC_GET_SIMPLE_FORMAT_COUNT, &count, sizeof (int)) ;

	if (count < 0 || count > 30)
	{	printf ("Line %d: Weird count.\n", __LINE__) ;
		exit (1) ;
		} ;

	format_info.format = 0 ;
	sf_command (NULL, SFC_GET_SIMPLE_FORMAT, &format_info, sizeof (format_info)) ;

	last_name = format_info.name ;
	for (k = 1 ; k < count ; k ++)
	{	format_info.format = k ;
		sf_command (NULL, SFC_GET_SIMPLE_FORMAT, &format_info, sizeof (format_info)) ;
		if (strcmp (last_name, format_info.name) >= 0)
		{	printf ("\n\nLine %d: format names out of sequence `%s' < `%s'.\n", __LINE__, last_name, format_info.name) ;
			exit (1) ;
			} ;
		sfinfo.format = format_info.format ;

		if (! sf_format_check (&sfinfo))
		{	printf ("\n\nLine %d: sf_format_check failed.\n", __LINE__) ;
			printf ("        Name : %s\n", format_info.name) ;
			printf ("        Format      : 0x%X\n", sfinfo.format) ;
			printf ("        Channels    : 0x%X\n", sfinfo.channels) ;
			printf ("        Sample Rate : 0x%X\n", sfinfo.samplerate) ;
			exit (1) ;
			} ;
		last_name = format_info.name ;
		} ;
	format_info.format = 666 ;
	sf_command (NULL, SFC_GET_SIMPLE_FORMAT, &format_info, sizeof (format_info)) ;

	/* Now test major formats. */
	sf_command (NULL, SFC_GET_FORMAT_MAJOR_COUNT, &count, sizeof (int)) ;

	if (count < 0 || count > 30)
	{	printf ("Line %d: Weird count.\n", __LINE__) ;
		exit (1) ;
		} ;

	format_info.format = 0 ;
	sf_command (NULL, SFC_GET_FORMAT_MAJOR, &format_info, sizeof (format_info)) ;

	last_name = format_info.name ;
	for (k = 1 ; k < count ; k ++)
	{	format_info.format = k ;
		sf_command (NULL, SFC_GET_FORMAT_MAJOR, &format_info, sizeof (format_info)) ;
		if (strcmp (last_name, format_info.name) >= 0)
		{	printf ("\n\nLine %d: format names out of sequence (%d) `%s' < `%s'.\n", __LINE__, k, last_name, format_info.name) ;
			exit (1) ;
			} ;

		last_name = format_info.name ;
		} ;
	format_info.format = 666 ;
	sf_command (NULL, SFC_GET_FORMAT_MAJOR, &format_info, sizeof (format_info)) ;

	/* Now test subtype formats. */
	sf_command (NULL, SFC_GET_FORMAT_SUBTYPE_COUNT, &count, sizeof (int)) ;

	if (count < 0 || count > 30)
	{	printf ("Line %d: Weird count.\n", __LINE__) ;
		exit (1) ;
		} ;

	format_info.format = 0 ;
	sf_command (NULL, SFC_GET_FORMAT_SUBTYPE, &format_info, sizeof (format_info)) ;

	last_name = format_info.name ;
	for (k = 1 ; k < count ; k ++)
	{	format_info.format = k ;
		sf_command (NULL, SFC_GET_FORMAT_SUBTYPE, &format_info, sizeof (format_info)) ;
		} ;
	format_info.format = 666 ;
	sf_command (NULL, SFC_GET_FORMAT_SUBTYPE, &format_info, sizeof (format_info)) ;


	printf ("ok\n") ;
} /* format_tests */

static	void
calc_peak_test (int filetype, const char *filename, int channels)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	char		label [128] ;
	int			k, format ;
	sf_count_t	buffer_len, frame_count ;
	double		peak ;

	snprintf (label, sizeof (label), "calc_peak_test (%d channels)", channels) ;
	print_test_name (label, filename) ;

	format = filetype | SF_FORMAT_PCM_16 ;

	buffer_len = BUFFER_LEN - (BUFFER_LEN % channels) ;
	frame_count = buffer_len / channels ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.format		= format ;
	sfinfo.channels		= channels ;
	sfinfo.frames		= frame_count ;

	/* Create double_data with max value of 0.5. */
	for (k = 0 ; k < buffer_len ; k++)
		double_data [k] = (k + 1) / (2.0 * buffer_len) ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;

	test_writef_double_or_die (file, 0, double_data, frame_count, __LINE__) ;

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	if (sfinfo.format != format)
	{	printf ("Line %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, format, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames != frame_count)
	{	printf ("\n\nLine %d: Incorrect number of frames in file. (%" PRId64 " => %" PRId64 ")\n", __LINE__, frame_count, sfinfo.frames) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != channels)
	{	printf ("Line %d: Incorrect number of channels in file.\n", __LINE__) ;
		exit (1) ;
		} ;

	sf_command (file, SFC_CALC_SIGNAL_MAX, &peak, sizeof (peak)) ;
	if (fabs (peak - (1 << 14)) > 1.0)
	{	printf ("Line %d : Peak value should be %d (is %f).\n", __LINE__, (1 << 14), peak) ;
		exit (1) ;
		} ;

	sf_command (file, SFC_CALC_NORM_SIGNAL_MAX, &peak, sizeof (peak)) ;
	if (fabs (peak - 0.5) > 4e-5)
	{	printf ("Line %d : Peak value should be %f (is %f).\n", __LINE__, 0.5, peak) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	format = (filetype | SF_FORMAT_FLOAT) ;
	sfinfo.samplerate	= 44100 ;
	sfinfo.format		= format ;
	sfinfo.channels		= channels ;
	sfinfo.frames		= frame_count ;

	/* Create double_data with max value of 0.5. */
	for (k = 0 ; k < buffer_len ; k++)
		double_data [k] = (k + 1) / (2.0 * buffer_len) ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;

	test_writef_double_or_die (file, 0, double_data, frame_count, __LINE__) ;

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	if (sfinfo.format != format)
	{	printf ("Line %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, format, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames != frame_count)
	{	printf ("\n\nLine %d: Incorrect number of.frames in file. (%" PRId64 " => %" PRId64 ")\n", __LINE__, frame_count, sfinfo.frames) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != channels)
	{	printf ("Line %d: Incorrect number of channels in file.\n", __LINE__) ;
		exit (1) ;
		} ;

	sf_command (file, SFC_CALC_SIGNAL_MAX, &peak, sizeof (peak)) ;
	if (fabs (peak - 0.5) > 1e-5)
	{	printf ("Line %d : Peak value should be %f (is %f).\n", __LINE__, 0.5, peak) ;
		exit (1) ;
		} ;

	sf_command (file, SFC_CALC_NORM_SIGNAL_MAX, &peak, sizeof (peak)) ;
	if (fabs (peak - 0.5) > 1e-5)
	{	printf ("Line %d : Peak value should be %f (is %f).\n", __LINE__, 0.5, peak) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	unlink (filename) ;

	printf ("ok\n") ;
} /* calc_peak_test */

static void
truncate_test (const char *filename, int filetype)
{	SNDFILE 	*file ;
	SF_INFO		sfinfo ;
	sf_count_t	len ;

	print_test_name ("truncate_test", filename) ;

	sfinfo.samplerate	= 11025 ;
	sfinfo.format		= filetype ;
	sfinfo.channels		= 2 ;

	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_TRUE, __LINE__) ;

	test_write_int_or_die (file, 0, int_data, BUFFER_LEN, __LINE__) ;

	len = 100 ;
	if (sf_command (file, SFC_FILE_TRUNCATE, &len, sizeof (len)))
	{	printf ("Line %d: sf_command (SFC_FILE_TRUNCATE) returned error.\n", __LINE__) ;
		exit (1) ;
		} ;

	test_seek_or_die (file, 0, SEEK_CUR, len, 2, __LINE__) ;
	test_seek_or_die (file, 0, SEEK_END, len, 2, __LINE__) ;

	sf_close (file) ;

	unlink (filename) ;
	puts ("ok") ;
} /* truncate_test */

/*------------------------------------------------------------------------------
*/

static void
instrumet_rw_test (const char *filename)
{	SNDFILE *sndfile ;
	SF_INFO sfinfo ;
	SF_INSTRUMENT inst ;
	memset (&sfinfo, 0, sizeof (SF_INFO)) ;

	sndfile = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_FALSE, __LINE__) ;

	if (sf_command (sndfile, SFC_GET_INSTRUMENT, &inst, sizeof (inst)) == SF_TRUE)
	{	inst.basenote = 22 ;

		if (sf_command (sndfile, SFC_SET_INSTRUMENT, &inst, sizeof (inst)) == SF_TRUE)
			printf ("Sucess: [%s] updated\n", filename) ;
		else
			printf ("Error: SFC_SET_INSTRUMENT on [%s] [%s]\n", filename, sf_strerror (sndfile)) ;
		}
	else
		printf ("Error: SFC_GET_INSTRUMENT on [%s] [%s]\n", filename, sf_strerror (sndfile)) ;


	if (sf_command (sndfile, SFC_UPDATE_HEADER_NOW, NULL, 0) != 0)
		printf ("Error: SFC_UPDATE_HEADER_NOW on [%s] [%s]\n", filename, sf_strerror (sndfile)) ;

	sf_write_sync (sndfile) ;
	sf_close (sndfile) ;

	return ;
} /* instrumet_rw_test */

static void
instrument_test (const char *filename, int filetype)
{	static SF_INSTRUMENT write_inst =
	{	2,		/* gain */
		3, 		/* detune */
		4, 		/* basenote */
		5, 6,	/* key low and high */
		7, 8,	/* velocity low and high */
		2,		/* loop_count */
		{	{	801, 2, 3, 0 },
			{	801, 3, 4, 0 },
		}
	} ;
	SF_INSTRUMENT read_inst ;
	SNDFILE	*file ;
	SF_INFO	sfinfo ;

	print_test_name ("instrument_test", filename) ;

	sfinfo.samplerate	= 11025 ;
	sfinfo.format		= filetype ;
	sfinfo.channels		= 1 ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_SET_INSTRUMENT, &write_inst, sizeof (write_inst)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_INSTRUMENT) failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;
	test_write_double_or_die (file, 0, double_data, BUFFER_LEN, __LINE__) ;
	sf_close (file) ;

	memset (&read_inst, 0, sizeof (read_inst)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_GET_INSTRUMENT, &read_inst, sizeof (read_inst)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_GET_INSTRUMENT) failed.\n\n", __LINE__) ;
		exit (1) ;
		return ;
		} ;
	check_log_buffer_or_die (file, __LINE__) ;
	sf_close (file) ;

	if ((filetype & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAV)
	{	/*
		**	For all the fields that WAV doesn't support, modify the
		**	write_inst struct to hold the default value that the WAV
		**	module should hold.
		*/
		write_inst.key_lo = write_inst.velocity_lo = 0 ;
		write_inst.key_hi = write_inst.velocity_hi = 127 ;
		write_inst.gain = 1 ;
		} ;

	if ((filetype & SF_FORMAT_TYPEMASK) == SF_FORMAT_XI)
	{	/*
		**	For all the fields that XI doesn't support, modify the
		**	write_inst struct to hold the default value that the XI
		**	module should hold.
		*/
		write_inst.basenote = 0 ;
		write_inst.detune = 0 ;
		write_inst.key_lo = write_inst.velocity_lo = 0 ;
		write_inst.key_hi = write_inst.velocity_hi = 127 ;
		write_inst.gain = 1 ;
		} ;

	if (memcmp (&write_inst, &read_inst, sizeof (write_inst)) != 0)
	{	printf ("\n\nLine %d : instrument comparison failed.\n\n", __LINE__) ;
		printf ("W  Base Note : %u\n"
			"   Detune    : %u\n"
			"   Low  Note : %u\tHigh Note : %u\n"
			"   Low  Vel. : %u\tHigh Vel. : %u\n"
			"   Gain      : %d\tCount     : %d\n"
			"   mode      : %d\n"
			"   start     : %d\tend       : %d\tcount  :%d\n"
			"   mode      : %d\n"
			"   start     : %d\tend       : %d\tcount  :%d\n\n",
			write_inst.basenote,
			write_inst.detune,
			write_inst.key_lo, write_inst.key_hi,
			write_inst.velocity_lo, write_inst.velocity_hi,
			write_inst.gain, write_inst.loop_count,
			write_inst.loops [0].mode, write_inst.loops [0].start,
			write_inst.loops [0].end, write_inst.loops [0].count,
			write_inst.loops [1].mode, write_inst.loops [1].start,
			write_inst.loops [1].end, write_inst.loops [1].count) ;
		printf ("R  Base Note : %u\n"
			"   Detune    : %u\n"
			"   Low  Note : %u\tHigh Note : %u\n"
			"   Low  Vel. : %u\tHigh Vel. : %u\n"
			"   Gain      : %d\tCount     : %d\n"
			"   mode      : %d\n"
			"   start     : %d\tend       : %d\tcount  :%d\n"
			"   mode      : %d\n"
			"   start     : %d\tend       : %d\tcount  :%d\n\n",
			read_inst.basenote,
			read_inst.detune,
			read_inst.key_lo, read_inst.key_hi,
			read_inst.velocity_lo, read_inst.velocity_hi,
			read_inst.gain,	read_inst.loop_count,
			read_inst.loops [0].mode, read_inst.loops [0].start,
			read_inst.loops [0].end, read_inst.loops [0].count,
			read_inst.loops [1].mode, read_inst.loops [1].start,
			read_inst.loops [1].end, read_inst.loops [1].count) ;

		if ((filetype & SF_FORMAT_TYPEMASK) != SF_FORMAT_XI)
			exit (1) ;
		} ;

	if (0) instrumet_rw_test (filename) ;

	unlink (filename) ;
	puts ("ok") ;
} /* instrument_test */

static void
print_cue (SF_CUES *cue, int i)
{
	printf ("   indx[%d]       : %d\n"
		"   position      : %u\n"
		"   fcc_chunk     : %x\n"
		"   chunk_start   : %d\n"
		"   block_start   : %d\n"
		"   sample_offset : %u\n"
		"   name          : %s\n",
		i,
		cue->cue_points [i].indx,
		cue->cue_points [i].position,
		cue->cue_points [i].fcc_chunk,
		cue->cue_points [i].chunk_start,
		cue->cue_points [i].block_start,
		cue->cue_points [i].sample_offset,
		cue->cue_points [i].name) ;
}

static int
cue_compare (SF_CUES *write_cue, SF_CUES *read_cue, size_t cue_size, int line)
{
	if (memcmp (write_cue, read_cue, cue_size) != 0)
	{
		printf ("\n\nLine %d : cue comparison failed.\n\n", line) ;
		printf ("W  Cue count     : %d\n", write_cue->cue_count) ;
		if (write_cue->cue_count > 0)
			print_cue (write_cue, 0) ;
		if (write_cue->cue_count > 2)	/* print last if at least 2 */
			print_cue (write_cue, write_cue->cue_count - 1) ;

		printf ("R  Cue count     : %d\n", read_cue->cue_count) ;
		if (read_cue->cue_count > 0)
			print_cue (read_cue, 0) ;
		if (read_cue->cue_count > 2)	/* print last if at least 2 */
			print_cue (read_cue, read_cue->cue_count - 1) ;

		return SF_FALSE ;
		} ;

	return SF_TRUE ;
} /* cue_compare */

static void
cue_rw_test (const char *filename)
{	SNDFILE *sndfile ;
	SF_INFO sfinfo ;
	SF_CUES cues ;
	memset (&sfinfo, 0, sizeof (SF_INFO)) ;

	sndfile = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_FALSE, __LINE__) ;

	exit_if_true (
		sf_command (sndfile, SFC_GET_CUE_COUNT, &cues.cue_count, sizeof (cues.cue_count)) != SF_TRUE,
		"\nLine %d: SFC_GET_CUE_COUNT command failed.\n\n", __LINE__
		) ;

	exit_if_true (
		cues.cue_count != 3,
		"\nLine %d: Expected cue_count (%u) to be 3.\n\n", __LINE__, cues.cue_count
		) ;

	if (sf_command (sndfile, SFC_GET_CUE, &cues, sizeof (cues)) == SF_TRUE)
	{	cues.cue_points [1].sample_offset = 3 ;

		if (sf_command (sndfile, SFC_SET_CUE, &cues, sizeof (cues)) == SF_TRUE)
			printf ("Sucess: [%s] updated\n", filename) ;
		else
			printf ("Error: SFC_SET_CUE on [%s] [%s]\n", filename, sf_strerror (sndfile)) ;
		}
	else
		printf ("Error: SFC_GET_CUE on [%s] [%s]\n", filename, sf_strerror (sndfile)) ;


	if (sf_command (sndfile, SFC_UPDATE_HEADER_NOW, NULL, 0) != 0)
		printf ("Error: SFC_UPDATE_HEADER_NOW on [%s] [%s]\n", filename, sf_strerror (sndfile)) ;

	sf_write_sync (sndfile) ;
	sf_close (sndfile) ;

	return ;
} /* cue_rw_test */

static void
cue_test (const char *filename, int filetype)
{	SF_CUES write_cue ;
	SF_CUES read_cue ;
	SNDFILE	*file ;
	SF_INFO	sfinfo ;

	if (filetype == (SF_FORMAT_WAV | SF_FORMAT_PCM_16))
	{	write_cue = (SF_CUES)
		{	2,		/* cue_count */
			{	{	1, 0, data_MARKER, 0, 0, 1, "" },
				{	2, 0, data_MARKER, 0, 0, 2, "" },
			}
		} ;
	}
	else
	{	write_cue = (SF_CUES)
		{	2,		/* cue_count */
			{	{	1, 0, data_MARKER, 0, 0, 1, "Cue1" },
				{	2, 0, data_MARKER, 0, 0, 2, "Cue2" },
			}
		} ;
	}

	print_test_name ("cue_test", filename) ;

	sfinfo.samplerate	= 11025 ;
	sfinfo.format		= filetype ;
	sfinfo.channels		= 1 ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_SET_CUE, &write_cue, sizeof (write_cue)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_CUE) failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;
	test_write_double_or_die (file, 0, double_data, BUFFER_LEN, __LINE__) ;
	sf_close (file) ;

	memset (&read_cue, 0, sizeof (read_cue)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_GET_CUE, &read_cue, sizeof (read_cue)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_GET_CUE) failed.\n\n", __LINE__) ;
		exit (1) ;
		return ;
		} ;
	check_log_buffer_or_die (file, __LINE__) ;
	sf_close (file) ;

	if (cue_compare (&write_cue, &read_cue, sizeof (write_cue), __LINE__) == SF_FALSE)
			exit (1) ;

	if (0) cue_rw_test (filename) ;

	unlink (filename) ;
	puts ("ok") ;
} /* cue_test */

/* calculate size of SF_CUES struct given number of cues */
#define SF_CUES_SIZE(count)	(sizeof (uint32_t) + sizeof (SF_CUE_POINT) * (count))

static void
cue_test_var (const char *filename, int filetype, int count)
{	size_t cues_size = SF_CUES_SIZE (count) ;
	SF_CUES *write_cue = calloc (1, cues_size) ;
	SF_CUES *read_cue = calloc (1, cues_size) ;
	SNDFILE	*file ;
	SF_INFO	sfinfo ;
	char name [40] ;
	int i ;

	snprintf (name, sizeof (name), "cue_test_var %d", count) ;
	print_test_name (name, filename) ;

	if (write_cue == NULL || read_cue == NULL)
	{	printf ("ok (can't alloc)\n") ;
		return ;
		} ;

	write_cue->cue_count = count ;
	for (i = 0 ; i < count ; i++)
	{	write_cue->cue_points [i] = (SF_CUE_POINT) { i, 0, data_MARKER, 0, 0, i, "" } ;
		if (filetype == (SF_FORMAT_AIFF | SF_FORMAT_PCM_24))
			snprintf (write_cue->cue_points [i].name, sizeof (write_cue->cue_points [i].name), "Cue%03d", i) ;
		} ;

	sfinfo.samplerate	= 11025 ;
	sfinfo.format		= filetype ;
	sfinfo.channels		= 1 ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_SET_CUE, write_cue, cues_size) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_CUE) failed with %d cues, datasize %zu --> error: %s\n\n", __LINE__, count, cues_size, sf_strerror (file)) ;
		exit (1) ;
		} ;
	test_write_double_or_die (file, 0, double_data, BUFFER_LEN, __LINE__) ;
	sf_close (file) ;

	memset (read_cue, 0, cues_size) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	if (sf_command (file, SFC_GET_CUE, read_cue, cues_size) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_GET_CUE) failed with %d cues, datasize %zu --> error: %s\n\n", __LINE__, count, cues_size, sf_strerror (file)) ;
		exit (1) ;
		} ;
	check_log_buffer_or_die (file, __LINE__) ;
	sf_close (file) ;

	if (cue_compare (write_cue, read_cue, cues_size, __LINE__) == SF_FALSE)
	{	printf ("\n\nLine %d : cue_compare failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;

	free (write_cue) ;
	free (read_cue) ;
	unlink (filename) ;
	puts ("ok") ;
} /* cue_test_var */

static	void
current_sf_info_test	(const char *filename)
{	SNDFILE *outfile, *infile ;
	SF_INFO outinfo, ininfo ;

	print_test_name ("current_sf_info_test", filename) ;

	outinfo.samplerate	= 44100 ;
	outinfo.format		= (SF_FORMAT_WAV | SF_FORMAT_PCM_16) ;
	outinfo.channels	= 1 ;
	outinfo.frames		= 0 ;

	outfile = test_open_file_or_die (filename, SFM_WRITE, &outinfo, SF_TRUE, __LINE__) ;
	sf_command (outfile, SFC_SET_UPDATE_HEADER_AUTO, NULL, 0) ;

	exit_if_true (outinfo.frames != 0,
		"\n\nLine %d : Initial sfinfo.frames is not zero.\n\n", __LINE__
		) ;

	test_write_double_or_die (outfile, 0, double_data, BUFFER_LEN, __LINE__) ;
	sf_command (outfile, SFC_GET_CURRENT_SF_INFO, &outinfo, sizeof (outinfo)) ;

	exit_if_true (outinfo.frames != BUFFER_LEN,
		"\n\nLine %d : Initial sfinfo.frames (%" PRId64 ") should be %d.\n\n", __LINE__,
		outinfo.frames, BUFFER_LEN
		) ;

	/* Read file making sure no channel map exists. */
	memset (&ininfo, 0, sizeof (ininfo)) ;
	infile = test_open_file_or_die (filename, SFM_READ, &ininfo, SF_TRUE, __LINE__) ;

	test_write_double_or_die (outfile, 0, double_data, BUFFER_LEN, __LINE__) ;

	sf_command (infile, SFC_GET_CURRENT_SF_INFO, &ininfo, sizeof (ininfo)) ;

	exit_if_true (ininfo.frames != BUFFER_LEN,
		"\n\nLine %d : Initial sfinfo.frames (%" PRId64 ") should be %d.\n\n", __LINE__,
		ininfo.frames, BUFFER_LEN
		) ;

	sf_close (outfile) ;
	sf_close (infile) ;

	unlink (filename) ;
	puts ("ok") ;
} /* current_sf_info_test */

static void
broadcast_test (const char *filename, int filetype)
{	static SF_BROADCAST_INFO bc_write, bc_read ;
	SNDFILE	*file ;
	SF_INFO	sfinfo ;
	int errors = 0 ;

	print_test_name ("broadcast_test", filename) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	sfinfo.samplerate	= 11025 ;
	sfinfo.format		= filetype ;
	sfinfo.channels		= 1 ;

	memset (&bc_write, 0, sizeof (bc_write)) ;

	snprintf (bc_write.description, sizeof (bc_write.description), "Test description") ;
	snprintf (bc_write.originator, sizeof (bc_write.originator), "Test originator") ;
	snprintf (bc_write.originator_reference, sizeof (bc_write.originator_reference), "%08x-%08x", (unsigned int) time (NULL), (unsigned int) (~ time (NULL))) ;
	snprintf (bc_write.origination_date, sizeof (bc_write.origination_date), "%d/%02d/%02d", 2006, 3, 30) ;
	snprintf (bc_write.origination_time, sizeof (bc_write.origination_time), "%02d:%02d:%02d", 20, 27, 0) ;
	snprintf (bc_write.umid, sizeof (bc_write.umid), "Some umid") ;
	bc_write.coding_history_size = 0 ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_SET_BROADCAST_INFO, &bc_write, sizeof (bc_write)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_BROADCAST_INFO) failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;
	test_write_double_or_die (file, 0, double_data, BUFFER_LEN, __LINE__) ;
	sf_close (file) ;

	memset (&bc_read, 0, sizeof (bc_read)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_GET_BROADCAST_INFO, &bc_read, sizeof (bc_read)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_GET_BROADCAST_INFO) failed.\n\n", __LINE__) ;
		exit (1) ;
		return ;
		} ;
	check_log_buffer_or_die (file, __LINE__) ;
	sf_close (file) ;

	if (bc_read.version != 2)
	{	printf ("\n\nLine %d : Read bad version number %d.\n\n", __LINE__, bc_read.version) ;
		exit (1) ;
		return ;
		} ;

	bc_read.version = bc_write.version = 0 ;

	if (memcmp (bc_write.description, bc_read.description, sizeof (bc_write.description)) != 0)
	{	printf ("\n\nLine %d : description mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, bc_write.description, bc_read.description) ;
		errors ++ ;
		} ;

	if (memcmp (bc_write.originator, bc_read.originator, sizeof (bc_write.originator)) != 0)
	{	printf ("\n\nLine %d : originator mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, bc_write.originator, bc_read.originator) ;
		errors ++ ;
		} ;

	if (memcmp (bc_write.originator_reference, bc_read.originator_reference, sizeof (bc_write.originator_reference)) != 0)
	{	printf ("\n\nLine %d : originator_reference mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, bc_write.originator_reference, bc_read.originator_reference) ;
		errors ++ ;
		} ;

	if (memcmp (bc_write.origination_date, bc_read.origination_date, sizeof (bc_write.origination_date)) != 0)
	{	printf ("\n\nLine %d : origination_date mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, bc_write.origination_date, bc_read.origination_date) ;
		errors ++ ;
		} ;

	if (memcmp (bc_write.origination_time, bc_read.origination_time, sizeof (bc_write.origination_time)) != 0)
	{	printf ("\n\nLine %d : origination_time mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, bc_write.origination_time, bc_read.origination_time) ;
		errors ++ ;
		} ;

	if (memcmp (bc_write.umid, bc_read.umid, sizeof (bc_write.umid)) != 0)
	{	printf ("\n\nLine %d : umid mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, bc_write.umid, bc_read.umid) ;
		errors ++ ;
		} ;

	if (errors)
		exit (1) ;

	unlink (filename) ;
	puts ("ok") ;
} /* broadcast_test */

static	void
broadcast_rdwr_test (const char *filename, int filetype)
{	SF_BROADCAST_INFO binfo ;
	SNDFILE *file ;
	SF_INFO sfinfo ;
	sf_count_t frames ;

	print_test_name (__func__, filename) ;

	create_short_sndfile (filename, filetype, 2) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	memset (&binfo, 0, sizeof (binfo)) ;

	snprintf (binfo.description, sizeof (binfo.description), "Test description") ;
	snprintf (binfo.originator, sizeof (binfo.originator), "Test originator") ;
	snprintf (binfo.originator_reference, sizeof (binfo.originator_reference), "%08x-%08x", (unsigned int) time (NULL), (unsigned int) (~ time (NULL))) ;
	snprintf (binfo.origination_date, sizeof (binfo.origination_date), "%d/%02d/%02d", 2006, 3, 30) ;
	snprintf (binfo.origination_time, sizeof (binfo.origination_time), "%02d:%02d:%02d", 20, 27, 0) ;
	snprintf (binfo.umid, sizeof (binfo.umid), "Some umid") ;
	binfo.coding_history_size = 0 ;

	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_TRUE, __LINE__) ;
	frames = sfinfo.frames ;
	if (sf_command (file, SFC_SET_BROADCAST_INFO, &binfo, sizeof (binfo)) != SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_BROADCAST_INFO) should have failed but didn't.\n\n", __LINE__) ;
		exit (1) ;
		} ;
	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	sf_close (file) ;
	exit_if_true (frames != sfinfo.frames, "\n\nLine %d : Frame count %" PRId64 " should be %" PRId64 ".\n", __LINE__, sfinfo.frames, frames) ;

	unlink (filename) ;
	puts ("ok") ;
} /* broadcast_rdwr_test */

static void
check_coding_history_newlines (const char *filename)
{	static SF_BROADCAST_INFO bc_write, bc_read ;
	SNDFILE	*file ;
	SF_INFO	sfinfo ;
	unsigned k ;

	sfinfo.samplerate	= 22050 ;
	sfinfo.format		= SF_FORMAT_WAV | SF_FORMAT_PCM_16 ;
	sfinfo.channels		= 1 ;

	memset (&bc_write, 0, sizeof (bc_write)) ;

	snprintf (bc_write.description, sizeof (bc_write.description), "Test description") ;
	snprintf (bc_write.originator, sizeof (bc_write.originator), "Test originator") ;
	snprintf (bc_write.originator_reference, sizeof (bc_write.originator_reference), "%08x-%08x", (unsigned int) time (NULL), (unsigned int) (~ time (NULL))) ;
	snprintf (bc_write.origination_date, sizeof (bc_write.origination_date), "%d/%02d/%02d", 2006, 3, 30) ;
	snprintf (bc_write.origination_time, sizeof (bc_write.origination_time), "%02d:%02d:%02d", 20, 27, 0) ;
	snprintf (bc_write.umid, sizeof (bc_write.umid), "Some umid") ;
	bc_write.coding_history_size = snprintf (bc_write.coding_history, sizeof (bc_write.coding_history), "This has\nUnix\nand\rMac OS9\rline endings.\nLast line") ; ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_SET_BROADCAST_INFO, &bc_write, sizeof (bc_write)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_BROADCAST_INFO) failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;

	test_write_double_or_die (file, 0, double_data, BUFFER_LEN, __LINE__) ;
	sf_close (file) ;

	memset (&bc_read, 0, sizeof (bc_read)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_GET_BROADCAST_INFO, &bc_read, sizeof (bc_read)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_BROADCAST_INFO) failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;
	check_log_buffer_or_die (file, __LINE__) ;
	sf_close (file) ;

	if (bc_read.coding_history_size == 0)
	{	printf ("\n\nLine %d : missing coding history.\n\n", __LINE__) ;
		exit (1) ;
		} ;

	if (strstr (bc_read.coding_history, "Last line") == NULL)
	{	printf ("\n\nLine %d : coding history truncated.\n\n", __LINE__) ;
		exit (1) ;
		} ;

	for (k = 1 ; k < bc_read.coding_history_size ; k++)
	{	if (bc_read.coding_history [k] == '\n' && bc_read.coding_history [k - 1] != '\r')
		{	printf ("\n\nLine %d : '\\n' without '\\r' before.\n\n", __LINE__) ;
			exit (1) ;
			} ;

		if (bc_read.coding_history [k] == '\r' && bc_read.coding_history [k + 1] != '\n')
		{	printf ("\n\nLine %d : '\\r' without '\\n' after.\n\n", __LINE__) ;
			exit (1) ;
			} ;

		if (bc_read.coding_history [k] == 0 && k < bc_read.coding_history_size - 1)
		{	printf ("\n\nLine %d : '\\0' within coding history at index %d of %d.\n\n", __LINE__, k, bc_read.coding_history_size) ;
			exit (1) ;
			} ;
		} ;

	return ;
} /* check_coding_history_newlines */

static void
broadcast_coding_history_test (const char *filename)
{	static SF_BROADCAST_INFO bc_write, bc_read ;
	SNDFILE	*file ;
	SF_INFO	sfinfo ;
	const char *default_history = "A=PCM,F=22050,W=16,M=mono" ;
	const char *supplied_history =
					"A=PCM,F=44100,W=24,M=mono,T=other\r\n"
					"A=PCM,F=22050,W=16,M=mono,T=yet_another\r\n" ;

	print_test_name ("broadcast_coding_history_test", filename) ;

	sfinfo.samplerate	= 22050 ;
	sfinfo.format		= SF_FORMAT_WAV | SF_FORMAT_PCM_16 ;
	sfinfo.channels		= 1 ;

	memset (&bc_write, 0, sizeof (bc_write)) ;

	snprintf (bc_write.description, sizeof (bc_write.description), "Test description") ;
	snprintf (bc_write.originator, sizeof (bc_write.originator), "Test originator") ;
	snprintf (bc_write.originator_reference, sizeof (bc_write.originator_reference), "%08x-%08x", (unsigned int) time (NULL), (unsigned int) (~ time (NULL))) ;
	snprintf (bc_write.origination_date, sizeof (bc_write.origination_date), "%d/%02d/%02d", 2006, 3, 30) ;
	snprintf (bc_write.origination_time, sizeof (bc_write.origination_time), "%02d:%02d:%02d", 20, 27, 0) ;
	snprintf (bc_write.umid, sizeof (bc_write.umid), "Some umid") ;
	/* Coding history will be filled in by the library. */
	bc_write.coding_history_size = 0 ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_SET_BROADCAST_INFO, &bc_write, sizeof (bc_write)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_BROADCAST_INFO) failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;

	test_write_double_or_die (file, 0, double_data, BUFFER_LEN, __LINE__) ;
	sf_close (file) ;

	memset (&bc_read, 0, sizeof (bc_read)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_GET_BROADCAST_INFO, &bc_read, sizeof (bc_read)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_BROADCAST_INFO) failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;
	check_log_buffer_or_die (file, __LINE__) ;
	sf_close (file) ;

	if (bc_read.coding_history_size == 0)
	{	printf ("\n\nLine %d : missing coding history.\n\n", __LINE__) ;
		exit (1) ;
		} ;

	if (bc_read.coding_history_size < strlen (default_history) || memcmp (bc_read.coding_history, default_history, strlen (default_history)) != 0)
	{	printf ("\n\n"
				"Line %d : unexpected coding history '%.*s',\n"
				"            should be '%s'\n\n", __LINE__, bc_read.coding_history_size, bc_read.coding_history, default_history) ;
		exit (1) ;
		} ;

	bc_write.coding_history_size = strlen (supplied_history) ;
	bc_write.coding_history_size = snprintf (bc_write.coding_history, sizeof (bc_write.coding_history), "%s", supplied_history) ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_SET_BROADCAST_INFO, &bc_write, sizeof (bc_write)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_BROADCAST_INFO) failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;

	test_write_double_or_die (file, 0, double_data, BUFFER_LEN, __LINE__) ;
	sf_close (file) ;

	memset (&bc_read, 0, sizeof (bc_read)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_GET_BROADCAST_INFO, &bc_read, sizeof (bc_read)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_BROADCAST_INFO) failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;

	check_log_buffer_or_die (file, __LINE__) ;
	sf_close (file) ;

	if (strstr (bc_read.coding_history, supplied_history) != bc_read.coding_history)
	{	printf ("\n\nLine %d : unexpected coding history :\n"
				"----------------------------------------------------\n%s"
				"----------------------------------------------------\n"
				"should be this :\n"
				"----------------------------------------------------\n%s"
				"----------------------------------------------------\n"
				"with one more line at the end.\n\n",
				__LINE__, bc_read.coding_history, supplied_history) ;
		exit (1) ;
		} ;

	check_coding_history_newlines (filename) ;

	unlink (filename) ;
	puts ("ok") ;
} /* broadcast_coding_history_test */

/*==============================================================================
*/

static void
broadcast_coding_history_size (const char *filename)
{	/* SF_BROADCAST_INFO struct with coding_history field of 1024 bytes. */
	static SF_BROADCAST_INFO_VAR (1024) bc_write ;
	static SF_BROADCAST_INFO_VAR (1024) bc_read ;
	SNDFILE	*file ;
	SF_INFO	sfinfo ;
	int k ;

	print_test_name (__func__, filename) ;

	sfinfo.samplerate	= 22050 ;
	sfinfo.format		= SF_FORMAT_WAV | SF_FORMAT_PCM_16 ;
	sfinfo.channels		= 1 ;

	memset (&bc_write, 0, sizeof (bc_write)) ;

	snprintf (bc_write.description, sizeof (bc_write.description), "Test description") ;
	snprintf (bc_write.originator, sizeof (bc_write.originator), "Test originator") ;
	snprintf (bc_write.originator_reference, sizeof (bc_write.originator_reference), "%08x-%08x", (unsigned int) time (NULL), (unsigned int) (~ time (NULL))) ;
	snprintf (bc_write.origination_date, sizeof (bc_write.origination_date), "%d/%02d/%02d", 2006, 3, 30) ;
	snprintf (bc_write.origination_time, sizeof (bc_write.origination_time), "%02d:%02d:%02d", 20, 27, 0) ;
	snprintf (bc_write.umid, sizeof (bc_write.umid), "Some umid") ;
	bc_write.coding_history_size = 0 ;

	for (k = 0 ; bc_write.coding_history_size < 512 ; k++)
	{	snprintf (bc_write.coding_history + bc_write.coding_history_size,
			sizeof (bc_write.coding_history) - bc_write.coding_history_size, "line %4d\n", k) ;
		bc_write.coding_history_size = strlen (bc_write.coding_history) ;
		} ;

	exit_if_true (bc_write.coding_history_size < 512,
			"\n\nLine %d : bc_write.coding_history_size (%d) should be > 512.\n\n", __LINE__, bc_write.coding_history_size) ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_SET_BROADCAST_INFO, &bc_write, sizeof (bc_write)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_BROADCAST_INFO) failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;

	test_write_double_or_die (file, 0, double_data, BUFFER_LEN, __LINE__) ;
	sf_close (file) ;

	memset (&bc_read, 0, sizeof (bc_read)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_GET_BROADCAST_INFO, &bc_read, sizeof (bc_read)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_BROADCAST_INFO) failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;
	check_log_buffer_or_die (file, __LINE__) ;
	sf_close (file) ;

	exit_if_true (bc_read.coding_history_size < 512,
			"\n\nLine %d : unexpected coding history size %d (should be > 512).\n\n", __LINE__, bc_read.coding_history_size) ;

	exit_if_true (strstr (bc_read.coding_history, "libsndfile") == NULL,
			"\n\nLine %d : coding history incomplete (should contain 'libsndfile').\n\n", __LINE__) ;

	unlink (filename) ;
	puts ("ok") ;
} /* broadcast_coding_history_size */

/*==============================================================================
*/
static void
cart_test (const char *filename, int filetype)
{	static SF_CART_INFO ca_write, ca_read ;
	SNDFILE	*file ;
	SF_INFO	sfinfo ;
	int errors = 0 ;

	print_test_name ("cart_test", filename) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	sfinfo.samplerate	= 11025 ;
	sfinfo.format		= filetype ;
	sfinfo.channels		= 1 ;
	memset (&ca_write, 0, sizeof (ca_write)) ;

	// example test data
	snprintf (ca_write.artist, sizeof (ca_write.artist), "Test artist") ;
	snprintf (ca_write.version, sizeof (ca_write.version), "Test version") ;
	snprintf (ca_write.cut_id, sizeof (ca_write.cut_id), "Test cut ID") ;
	snprintf (ca_write.client_id, sizeof (ca_write.client_id), "Test client ID") ;
	snprintf (ca_write.category, sizeof (ca_write.category), "Test category") ;
	snprintf (ca_write.classification, sizeof (ca_write.classification), "Test classification") ;
	snprintf (ca_write.out_cue, sizeof (ca_write.out_cue), "Test out cue") ;
	snprintf (ca_write.start_date, sizeof (ca_write.start_date), "%d/%02d/%02d", 2006, 3, 30) ;
	snprintf (ca_write.start_time, sizeof (ca_write.start_time), "%02d:%02d:%02d", 20, 27, 0) ;
	snprintf (ca_write.end_date, sizeof (ca_write.end_date), "%d/%02d/%02d", 2006, 3, 30) ;
	snprintf (ca_write.end_time, sizeof (ca_write.end_time), "%02d:%02d:%02d", 20, 27, 0) ;
	snprintf (ca_write.producer_app_id, sizeof (ca_write.producer_app_id), "Test producer app id") ;
	snprintf (ca_write.producer_app_version, sizeof (ca_write.producer_app_version), "Test producer app version") ;
	snprintf (ca_write.user_def, sizeof (ca_write.user_def), "test user def test test") ;
	ca_write.level_reference = 42 ;
	snprintf (ca_write.url, sizeof (ca_write.url), "http://www.test.com/test_url") ;
	snprintf (ca_write.tag_text, sizeof (ca_write.tag_text), "tag text test! \r\n") ; // must be terminated \r\n to be valid
	ca_write.tag_text_size = strlen (ca_write.tag_text) ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_SET_CART_INFO, &ca_write, sizeof (ca_write)) == SF_FALSE)
		exit (1) ;

	test_write_double_or_die (file, 0, double_data, BUFFER_LEN, __LINE__) ;
	sf_close (file) ;

	memset (&ca_read, 0, sizeof (ca_read)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	if (sf_command (file, SFC_GET_CART_INFO, &ca_read, sizeof (ca_read)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_GET_CART_INFO) failed.\n\n", __LINE__) ;
		exit (1) ;
		return ;
		} ;
	check_log_buffer_or_die (file, __LINE__) ;
	sf_close (file) ;


	if (memcmp (ca_write.artist, ca_read.artist, sizeof (ca_write.artist)) != 0)
	{	printf ("\n\nLine %d : artist mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.artist, ca_read.artist) ;
		errors ++ ;
		} ;

	if (memcmp (ca_write.version, ca_read.version, sizeof (ca_write.version)) != 0)
	{	printf ("\n\nLine %d : version mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.version, ca_read.version) ;
		errors ++ ;
		} ;

	if (memcmp (ca_write.title, ca_read.title, sizeof (ca_write.title)) != 0)
	{	printf ("\n\nLine %d : title mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.title, ca_read.title) ;
		errors ++ ;
		} ;

	if (memcmp (ca_write.cut_id, ca_read.cut_id, sizeof (ca_write.cut_id)) != 0)
	{	printf ("\n\nLine %d : cut_id mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.cut_id, ca_read.cut_id) ;
		errors ++ ;
		} ;

	if (memcmp (ca_write.client_id, ca_read.client_id, sizeof (ca_write.client_id)) != 0)
	{	printf ("\n\nLine %d : client_id mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.client_id, ca_read.client_id) ;
		errors ++ ;
		} ;

	if (memcmp (ca_write.category, ca_read.category, sizeof (ca_write.category)) != 0)
	{	printf ("\n\nLine %d : category mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.category, ca_read.category) ;
		errors ++ ;
		} ;

	if (memcmp (ca_write.out_cue, ca_read.out_cue, sizeof (ca_write.out_cue)) != 0)
	{	printf ("\n\nLine %d : out_cue mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.out_cue, ca_read.out_cue) ;
		errors ++ ;
		} ;

	if (memcmp (ca_write.start_date, ca_read.start_date, sizeof (ca_write.start_date)) != 0)
	{	printf ("\n\nLine %d : start_date mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.start_date, ca_read.start_date) ;
		errors ++ ;
		} ;


	if (memcmp (ca_write.start_time, ca_read.start_time, sizeof (ca_write.start_time)) != 0)
	{	printf ("\n\nLine %d : start_time mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.start_time, ca_read.start_time) ;
		errors ++ ;
		} ;


	if (memcmp (ca_write.end_date, ca_read.end_date, sizeof (ca_write.end_date)) != 0)
	{	printf ("\n\nLine %d : end_date mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.end_date, ca_read.end_date) ;
		errors ++ ;
		} ;


	if (memcmp (ca_write.end_time, ca_read.end_time, sizeof (ca_write.end_time)) != 0)
	{	printf ("\n\nLine %d : end_time mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.end_time, ca_read.end_time) ;
		errors ++ ;
		} ;


	if (memcmp (ca_write.producer_app_id, ca_read.producer_app_id, sizeof (ca_write.producer_app_id)) != 0)
	{	printf ("\n\nLine %d : producer_app_id mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.producer_app_id, ca_read.producer_app_id) ;
		errors ++ ;
		} ;


	if (memcmp (ca_write.producer_app_version, ca_read.producer_app_version, sizeof (ca_write.producer_app_version)) != 0)
	{	printf ("\n\nLine %d : producer_app_version mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.producer_app_version, ca_read.producer_app_version) ;
		errors ++ ;
		} ;


	if (memcmp (ca_write.user_def, ca_read.user_def, sizeof (ca_write.user_def)) != 0)
	{	printf ("\n\nLine %d : user_def mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.user_def, ca_read.user_def) ;
		errors ++ ;
		} ;


	if (ca_write.level_reference != ca_read.level_reference)
	{	printf ("\n\nLine %d : level_reference mismatch :\n\twrite : '%d'\n\tread  : '%d'\n\n", __LINE__, ca_write.level_reference, ca_read.level_reference) ;
		errors ++ ;
		} ;

	// TODO: make this more helpful
	if (memcmp (ca_write.post_timers, ca_read.post_timers, sizeof (ca_write.post_timers)) != 0)
	{	printf ("\n\nLine %d : post_timers mismatch :\n'\n\n", __LINE__) ;
		errors ++ ;
		} ;

	if (memcmp (ca_write.url, ca_read.url, sizeof (ca_write.url)) != 0)
	{	printf ("\n\nLine %d : url mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.url, ca_read.url) ;
		errors ++ ;
		} ;


	if (memcmp (ca_write.tag_text, ca_read.tag_text, (size_t) (ca_read.tag_text_size)) != 0)
	{	printf ("\n\nLine %d : tag_text mismatch :\n\twrite : '%s'\n\tread  : '%s'\n\n", __LINE__, ca_write.tag_text, ca_read.tag_text) ;
		errors ++ ;
		} ;


	if (errors)
		exit (1) ;

	unlink (filename) ;
	puts ("ok") ;
} /* cart_test */

static	void
cart_rdwr_test (const char *filename, int filetype)
{	SF_CART_INFO cinfo ;
	SNDFILE *file ;
	SF_INFO sfinfo ;
	sf_count_t frames ;

	print_test_name (__func__, filename) ;

	create_short_sndfile (filename, filetype, 2) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	memset (&cinfo, 0, sizeof (cinfo)) ;

	snprintf (cinfo.artist, sizeof (cinfo.artist), "Test artist") ;
	snprintf (cinfo.version, sizeof (cinfo.version), "Test version") ;
	snprintf (cinfo.cut_id, sizeof (cinfo.cut_id), "Test cut ID") ;
	snprintf (cinfo.client_id, sizeof (cinfo.client_id), "Test client ID") ;
	snprintf (cinfo.category, sizeof (cinfo.category), "Test category") ;
	snprintf (cinfo.classification, sizeof (cinfo.classification), "Test classification") ;
	snprintf (cinfo.out_cue, sizeof (cinfo.out_cue), "Test out cue") ;
	snprintf (cinfo.start_date, sizeof (cinfo.start_date), "%d/%02d/%02d", 2006, 3, 30) ;
	snprintf (cinfo.start_time, sizeof (cinfo.start_time), "%02d:%02d:%02d", 20, 27, 0) ;
	snprintf (cinfo.end_date, sizeof (cinfo.end_date), "%d/%02d/%02d", 2006, 3, 30) ;
	snprintf (cinfo.end_time, sizeof (cinfo.end_time), "%02d:%02d:%02d", 20, 27, 0) ;
	snprintf (cinfo.producer_app_id, sizeof (cinfo.producer_app_id), "Test producer app id") ;
	snprintf (cinfo.producer_app_version, sizeof (cinfo.producer_app_version), "Test producer app version") ;
	snprintf (cinfo.user_def, sizeof (cinfo.user_def), "test user def test test") ;
	cinfo.level_reference = 42 ;
	snprintf (cinfo.url, sizeof (cinfo.url), "http://www.test.com/test_url") ;
	snprintf (cinfo.tag_text, sizeof (cinfo.tag_text), "tag text test!\r\n") ;
	cinfo.tag_text_size = strlen (cinfo.tag_text) ;

	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_TRUE, __LINE__) ;
	frames = sfinfo.frames ;
	if (sf_command (file, SFC_SET_CART_INFO, &cinfo, sizeof (cinfo)) != SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_CART_INFO) should have failed but didn't.\n\n", __LINE__) ;
		exit (1) ;
		} ;
	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	sf_close (file) ;
	exit_if_true (frames != sfinfo.frames, "\n\nLine %d : Frame count %" PRId64 " should be %" PRId64 ".\n", __LINE__, sfinfo.frames, frames) ;

	unlink (filename) ;
	puts ("ok") ;
} /* cart_rdwr_test */

/*==============================================================================
*/

static	void
channel_map_test (const char *filename, int filetype)
{	SNDFILE	*file ;
	SF_INFO	sfinfo ;
	int channel_map_read [4], channel_map_write [4] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_LFE,
		SF_CHANNEL_MAP_REAR_CENTER
		} ;

	print_test_name ("channel_map_test", filename) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	sfinfo.samplerate	= 11025 ;
	sfinfo.format		= filetype ;
	sfinfo.channels		= ARRAY_LEN (channel_map_read) ;

	switch (filetype & SF_FORMAT_TYPEMASK)
	{	/* WAVEX and RF64 have a default channel map, even if you don't specify one. */
		case SF_FORMAT_WAVEX :
		case SF_FORMAT_RF64 :
			/* Write file without channel map. */
			file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
			test_write_double_or_die (file, 0, double_data, BUFFER_LEN, __LINE__) ;
			sf_close (file) ;

			/* Read file making default channel map exists. */
			file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
			exit_if_true (
				sf_command (file, SFC_GET_CHANNEL_MAP_INFO, channel_map_read, sizeof (channel_map_read)) == SF_FALSE,
				"\n\nLine %d : sf_command (SFC_GET_CHANNEL_MAP_INFO) should not have failed.\n\n", __LINE__
				) ;
			check_log_buffer_or_die (file, __LINE__) ;
			sf_close (file) ;
			break ;

		default :
			break ;
		} ;

	/* Write file with a channel map. */
	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	exit_if_true (
		sf_command (file, SFC_SET_CHANNEL_MAP_INFO, channel_map_write, sizeof (channel_map_write)) == SF_FALSE,
		"\n\nLine %d : sf_command (SFC_SET_CHANNEL_MAP_INFO) failed.\n\n", __LINE__
		) ;
	test_write_double_or_die (file, 0, double_data, BUFFER_LEN, __LINE__) ;
	sf_close (file) ;

	/* Read file making sure no channel map exists. */
	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	exit_if_true (
		sf_command (file, SFC_GET_CHANNEL_MAP_INFO, channel_map_read, sizeof (channel_map_read)) != SF_TRUE,
		"\n\nLine %d : sf_command (SFC_GET_CHANNEL_MAP_INFO) failed.\n\n", __LINE__
		) ;
	check_log_buffer_or_die (file, __LINE__) ;
	sf_close (file) ;

	exit_if_true (
		memcmp (channel_map_read, channel_map_write, sizeof (channel_map_read)) != 0,
		"\n\nLine %d : Channel map read does not match channel map written.\n\n", __LINE__
		) ;

	unlink (filename) ;
	puts ("ok") ;
} /* channel_map_test */

static	void
raw_needs_endswap_test (const char *filename, int filetype)
{	static int subtypes [] =
	{	SF_FORMAT_FLOAT, SF_FORMAT_DOUBLE,
		SF_FORMAT_PCM_16, SF_FORMAT_PCM_24, SF_FORMAT_PCM_32
		} ;
	SNDFILE	*file ;
	SF_INFO	sfinfo ;
	unsigned k ;
	int needs_endswap ;

	print_test_name (__func__, filename) ;

	for (k = 0 ; k < ARRAY_LEN (subtypes) ; k++)
	{
		if (filetype == (SF_ENDIAN_LITTLE | SF_FORMAT_AIFF))
			switch (subtypes [k])
			{	/* Little endian AIFF does not AFAIK support fl32 and fl64. */
				case SF_FORMAT_FLOAT :
				case SF_FORMAT_DOUBLE :
					continue ;
				default :
					break ;
				} ;

		memset (&sfinfo, 0, sizeof (sfinfo)) ;
		sfinfo.samplerate	= 11025 ;
		sfinfo.format		= filetype | subtypes [k] ;
		sfinfo.channels		= 1 ;

		file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
		test_write_double_or_die (file, 0, double_data, BUFFER_LEN, __LINE__) ;
		sf_close (file) ;

		memset (&sfinfo, 0, sizeof (sfinfo)) ;
		file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

		needs_endswap = sf_command (file, SFC_RAW_DATA_NEEDS_ENDSWAP, NULL, 0) ;

		switch (filetype)
		{	case SF_FORMAT_WAV :
			case SF_FORMAT_WAVEX :
			case SF_FORMAT_AIFF | SF_ENDIAN_LITTLE :
				exit_if_true (needs_endswap != CPU_IS_BIG_ENDIAN,
					"\n\nLine %d : SFC_RAW_DATA_NEEDS_ENDSWAP failed for (%d | %d).\n\n", __LINE__, filetype, k) ;
				break ;

			case SF_FORMAT_AIFF :
			case SF_FORMAT_WAV | SF_ENDIAN_BIG :
				exit_if_true (needs_endswap != CPU_IS_LITTLE_ENDIAN,
					"\n\nLine %d : SFC_RAW_DATA_NEEDS_ENDSWAP failed for (%d | %d).\n\n", __LINE__, filetype, k) ;
				break ;

			default :
				printf ("\n\nLine %d : bad format value %d.\n\n", __LINE__, filetype) ;
				exit (1) ;
				break ;
			} ;

		sf_close (file) ;
		} ;

	unlink (filename) ;
	puts ("ok") ;
} /* raw_needs_endswap_test */
