/*
** Copyright (C) 2007-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <unistd.h>

#include <math.h>

#include	<sndfile.h>

#include	"utils.h"
#include	"dft_cmp.h"

#define	SAMPLE_RATE		16000
#define	DATA_LENGTH		(SAMPLE_RATE / 8)

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

	if (max_abs > 1.021)
	{	printf ("\n\n    Error : max_abs %f should be < 1.021.\n\n", max_abs) ;
		exit (1) ;
		} ;

	puts ("ok") ;
	unlink (filename) ;
} /* vorbis_test */

static void
vorbis_quality_test (void)
{	/*
	**	Encode two files, one at quality 0.3 and one at quality 0.5 and then
	**	make sure that the quality 0.3 files is the smaller of the two.
	*/
	const char * q3_fname = "q3_vorbis.oga" ;
	const char * q5_fname = "q5_vorbis.oga" ;

	SNDFILE *q3_file, *q5_file ;
	SF_INFO sfinfo ;
	int q3_size, q5_size ;
	double quality ;
	int k ;

	print_test_name (__func__, "q[35]_vorbis.oga") ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	/* Set up output file type. */
	sfinfo.format = SF_FORMAT_OGG | SF_FORMAT_VORBIS ;
	sfinfo.channels = 1 ;
	sfinfo.samplerate = SAMPLE_RATE ;

	/* Write the output file. */
	q3_file = test_open_file_or_die (q3_fname, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	q5_file = test_open_file_or_die (q5_fname, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;

	quality = 0.3 ;
	sf_command (q3_file, SFC_SET_VBR_ENCODING_QUALITY, &quality, sizeof (quality)) ;
	quality = 0.5 ;
	sf_command (q5_file, SFC_SET_VBR_ENCODING_QUALITY, &quality, sizeof (quality)) ;

	for (k = 0 ; k < 5 ; k++)
	{	gen_lowpass_noise_float (data_out, ARRAY_LEN (data_out)) ;
		test_write_float_or_die (q3_file, 0, data_out, ARRAY_LEN (data_out), __LINE__) ;
		test_write_float_or_die (q5_file, 0, data_out, ARRAY_LEN (data_out), __LINE__) ;
		} ;

	sf_close (q3_file) ;
	sf_close (q5_file) ;

	q3_size = file_length (q3_fname) ;
	q5_size = file_length (q5_fname) ;

	if (q3_size >= q5_size)
	{	printf ("\n\nLine %d : q3 size (%d) >= q5 size (%d)\n\n", __LINE__, q3_size, q5_size) ;
		exit (1) ;
		} ;

	puts ("ok") ;
	unlink (q3_fname) ;
	unlink (q5_fname) ;
} /* vorbis_quality_test */



int
main (void)
{
	if (HAVE_EXTERNAL_LIBS)
	{	vorbis_test () ;
		vorbis_quality_test () ;
		}
	else
		puts ("    No Ogg/Vorbis tests because Ogg/Vorbis support was not compiled in.") ;

	return 0 ;
} /* main */
