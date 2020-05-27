/*
** Copyright (C) 2007-2016 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#else
#include "sf_unistd.h"
#endif

#include <math.h>

#include	<sndfile.h>

#include	"utils.h"
#include	"dft_cmp.h"

#define	SAMPLE_RATE		16000
#define	DATA_LENGTH		(SAMPLE_RATE)

static float data_out [DATA_LENGTH] ;

static inline float
max_float (float a, float b)
{	return a > b ? a : b ;
} /* max_float */

static void
vorbis_test (void)
{	static float float_data [DFT_DATA_LENGTH] ;
	const char * filename = "vorbis_test.oga" ;
	SNDFILE * file ;
	SF_INFO sfinfo ;
	float max_abs = 0.0 ;
	unsigned k ;

	print_test_name ("vorbis_test", filename) ;

	/* Generate float data. */
	gen_windowed_sine_float (float_data, ARRAY_LEN (float_data), 1.0) ;

	/* Set up output file type. */
	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	sfinfo.format = SF_FORMAT_OGG | SF_FORMAT_VORBIS ;
	sfinfo.channels = 1 ;
	sfinfo.samplerate = SAMPLE_RATE ;

	/* Write the output file. */

	/*	The Vorbis encoder has a bug on PowerPC and X86-64 with sample rates
	**	<= 22050. Increasing the sample rate to 32000 avoids triggering it.
	**	See https://trac.xiph.org/ticket/1229
	*/
	if ((file = sf_open (filename, SFM_WRITE, &sfinfo)) == NULL)
	{	const char * errstr ;

		errstr = sf_strerror (NULL) ;
		if (strstr (errstr, "Sample rate chosen is known to trigger a Vorbis") == NULL)
		{	printf ("Line %d: sf_open (SFM_WRITE) failed : %s\n", __LINE__, errstr) ;
			dump_log_buffer (NULL) ;
			exit (1) ;
			} ;

		printf ("\n                                  Sample rate -> 32kHz    ") ;
		sfinfo.samplerate = 32000 ;

		file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
		} ;

	test_write_float_or_die (file, 0, float_data, ARRAY_LEN (float_data), __LINE__) ;
	sf_close (file) ;

	memset (float_data, 0, sizeof (float_data)) ;

	/* Read the file back in again. */
	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;
	test_read_float_or_die (file, 0, float_data, ARRAY_LEN (float_data), __LINE__) ;
	sf_close (file) ;

	for (k = 0 ; k < ARRAY_LEN (float_data) ; k ++)
		max_abs = max_float (max_abs, fabs (float_data [k])) ;

	exit_if_true (max_abs > 1.023,
		"\n\nLine %d : max_abs %f should be < 1.023.\n\n", __LINE__, max_abs) ;

	puts ("ok") ;
	unlink (filename) ;
} /* vorbis_test */

static void
compression_size_test (int format, const char * filename)
{	/*
	**	Encode two files, one at quality 0.3 and one at quality 0.5 and then
	**	make sure that the quality 0.3 files is the smaller of the two.
	*/
	char q3_fname [64] ;
	char q6_fname [64] ;
	char test_name [64] ;

	SNDFILE *q3_file, *q6_file ;
	SF_INFO sfinfo ;
	int q3_size, q6_size ;
	double quality ;
	int k ;

	snprintf (q3_fname, sizeof (q3_fname), "q3_%s", filename) ;
	snprintf (q6_fname, sizeof (q6_fname), "q6_%s", filename) ;

	snprintf (test_name, sizeof (test_name), "q[36]_%s", filename) ;
	print_test_name (__func__, test_name) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	/* Set up output file type. */
	sfinfo.format = format ;
	sfinfo.channels = 1 ;
	sfinfo.samplerate = SAMPLE_RATE ;

	/* Write the output file. */
	q3_file = test_open_file_or_die (q3_fname, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	q6_file = test_open_file_or_die (q6_fname, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;

	quality = 0.3 ;
	sf_command (q3_file, SFC_SET_VBR_ENCODING_QUALITY, &quality, sizeof (quality)) ;
	quality = 0.6 ;
	sf_command (q6_file, SFC_SET_VBR_ENCODING_QUALITY, &quality, sizeof (quality)) ;

	for (k = 0 ; k < 5 ; k++)
	{	gen_lowpass_signal_float (data_out, ARRAY_LEN (data_out)) ;
		test_write_float_or_die (q3_file, 0, data_out, ARRAY_LEN (data_out), __LINE__) ;
		test_write_float_or_die (q6_file, 0, data_out, ARRAY_LEN (data_out), __LINE__) ;
		} ;

	sf_close (q3_file) ;
	sf_close (q6_file) ;

	q3_size = file_length (q3_fname) ;
	q6_size = file_length (q6_fname) ;

	exit_if_true (q3_size >= q6_size,
		"\n\nLine %d : q3 size (%d) >= q6 size (%d)\n\n", __LINE__, q3_size, q6_size) ;

	puts ("ok") ;
	unlink (q3_fname) ;
	unlink (q6_fname) ;
} /* compression_size_test */



int
main (int argc, char *argv [])
{	int all_tests = 0, tests = 0 ;

	if (argc != 2)
	{	printf (
			"Usage : %s <test>\n"
			"    Where <test> is one of:\n"
			"        vorbis - test Ogg/Vorbis\n"
			"        flac   - test FLAC\n"
			"        opus   - test Opus\n"
			"        all    - perform all tests\n",
			argv [0]) ;
		exit (0) ;
		} ;

	if (! HAVE_EXTERNAL_XIPH_LIBS)
	{	puts ("    No Ogg/Vorbis tests because Ogg/Vorbis support was not compiled in.") ;
		return 0 ;
	} ;

	if (strcmp (argv [1], "all") == 0)
		all_tests = 1 ;

	if (all_tests || strcmp (argv [1], "vorbis") == 0)
	{	vorbis_test () ;
		compression_size_test (SF_FORMAT_OGG | SF_FORMAT_VORBIS, "vorbis.oga") ;
		tests ++ ;
		} ;

	if (all_tests || strcmp (argv [1], "flac") == 0)
	{	compression_size_test (SF_FORMAT_FLAC | SF_FORMAT_PCM_16, "pcm16.flac") ;
		tests ++ ;
		} ;

	if (all_tests || strcmp (argv [1], "opus") == 0)
	{	compression_size_test (SF_FORMAT_OGG | SF_FORMAT_OPUS, "opus.opus") ;
		tests ++ ;
		} ;

	return 0 ;
} /* main */
