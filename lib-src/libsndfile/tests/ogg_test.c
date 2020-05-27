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

#define	SAMPLE_RATE			44100
#define	DATA_LENGTH			(SAMPLE_RATE / 8)

typedef union
{	double d [DATA_LENGTH] ;
	float f [DATA_LENGTH] ;
	int i [DATA_LENGTH] ;
	short s [DATA_LENGTH] ;
} BUFFER ;

static BUFFER data_out ;
static BUFFER data_in ;

static void
ogg_short_test (void)
{	const char * filename = "vorbis_short.oga" ;

	SNDFILE * file ;
	SF_INFO sfinfo ;
	short seek_data [10] ;
	unsigned k ;

	print_test_name ("ogg_short_test", filename) ;

	/* Generate float data. */
	gen_windowed_sine_float (data_out.f, ARRAY_LEN (data_out.f), 1.0 * 0x7F00) ;

	/* Convert to shorteger. */
	for (k = 0 ; k < ARRAY_LEN (data_out.s) ; k++)
		data_out.s [k] = lrintf (data_out.f [k]) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	/* Set up output file type. */
	sfinfo.format = SF_FORMAT_OGG | SF_FORMAT_VORBIS ;
	sfinfo.channels = 1 ;
	sfinfo.samplerate = SAMPLE_RATE ;

	/* Write the output file. */
	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	test_write_short_or_die (file, 0, data_out.s, ARRAY_LEN (data_out.s), __LINE__) ;
	sf_close (file) ;

	/* Read the file in again. */
	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;
	test_read_short_or_die (file, 0, data_in.s, ARRAY_LEN (data_in.s), __LINE__) ;
	sf_close (file) ;

	puts ("ok") ;

	/* Test seeking. */
	print_test_name ("ogg_seek_test", filename) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	test_seek_or_die (file, 10, SEEK_SET, 10, sfinfo.channels, __LINE__) ;
	test_read_short_or_die (file, 0, seek_data, ARRAY_LEN (seek_data), __LINE__) ;
	compare_short_or_die (seek_data, data_in.s + 10, ARRAY_LEN (seek_data), __LINE__) ;

	/* Test seek to end of file. */
	test_seek_or_die (file, 0, SEEK_END, sfinfo.frames, sfinfo.channels, __LINE__) ;

	sf_close (file) ;

	puts ("ok") ;

	unlink (filename) ;
} /* ogg_short_test */

static void
ogg_int_test (void)
{	const char * filename = "vorbis_int.oga" ;

	SNDFILE * file ;
	SF_INFO sfinfo ;
	int seek_data [10] ;
	unsigned k ;

	print_test_name ("ogg_int_test", filename) ;

	/* Generate float data. */
	gen_windowed_sine_float (data_out.f, ARRAY_LEN (data_out.f), 1.0 * 0x7FFF0000) ;

	/* Convert to integer. */
	for (k = 0 ; k < ARRAY_LEN (data_out.i) ; k++)
		data_out.i [k] = lrintf (data_out.f [k]) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	/* Set up output file type. */
	sfinfo.format = SF_FORMAT_OGG | SF_FORMAT_VORBIS ;
	sfinfo.channels = 1 ;
	sfinfo.samplerate = SAMPLE_RATE ;

	/* Write the output file. */
	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	test_write_int_or_die (file, 0, data_out.i, ARRAY_LEN (data_out.i), __LINE__) ;
	sf_close (file) ;

	/* Read the file in again. */
	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;
	test_read_int_or_die (file, 0, data_in.i, ARRAY_LEN (data_in.i), __LINE__) ;
	sf_close (file) ;

	puts ("ok") ;

	/* Test seeking. */
	print_test_name ("ogg_seek_test", filename) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	test_seek_or_die (file, 10, SEEK_SET, 10, sfinfo.channels, __LINE__) ;
	test_read_int_or_die (file, 0, seek_data, ARRAY_LEN (seek_data), __LINE__) ;
	compare_int_or_die (seek_data, data_in.i + 10, ARRAY_LEN (seek_data), __LINE__) ;

	sf_close (file) ;

	puts ("ok") ;

	unlink (filename) ;
} /* ogg_int_test */

static void
ogg_float_test (void)
{	const char * filename = "vorbis_float.oga" ;

	SNDFILE * file ;
	SF_INFO sfinfo ;
	float seek_data [10] ;

	print_test_name ("ogg_float_test", filename) ;

	gen_windowed_sine_float (data_out.f, ARRAY_LEN (data_out.f), 0.95) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	/* Set up output file type. */
	sfinfo.format = SF_FORMAT_OGG | SF_FORMAT_VORBIS ;
	sfinfo.channels = 1 ;
	sfinfo.samplerate = SAMPLE_RATE ;

	/* Write the output file. */
	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	test_write_float_or_die (file, 0, data_out.f, ARRAY_LEN (data_out.f), __LINE__) ;
	sf_close (file) ;

	/* Read the file in again. */
	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;
	test_read_float_or_die (file, 0, data_in.f, ARRAY_LEN (data_in.f), __LINE__) ;
	sf_close (file) ;

	puts ("ok") ;

	/* Test seeking. */
	print_test_name ("ogg_seek_test", filename) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	test_seek_or_die (file, 10, SEEK_SET, 10, sfinfo.channels, __LINE__) ;
	test_read_float_or_die (file, 0, seek_data, ARRAY_LEN (seek_data), __LINE__) ;
	compare_float_or_die (seek_data, data_in.f + 10, ARRAY_LEN (seek_data), __LINE__) ;

	sf_close (file) ;

	puts ("ok") ;

	unlink (filename) ;
} /* ogg_float_test */

static void
ogg_double_test (void)
{	const char * filename = "vorbis_double.oga" ;

	SNDFILE * file ;
	SF_INFO sfinfo ;
	double seek_data [10] ;

	print_test_name ("ogg_double_test", filename) ;

	gen_windowed_sine_double (data_out.d, ARRAY_LEN (data_out.d), 0.95) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	/* Set up output file type. */
	sfinfo.format = SF_FORMAT_OGG | SF_FORMAT_VORBIS ;
	sfinfo.channels = 1 ;
	sfinfo.samplerate = SAMPLE_RATE ;

	/* Write the output file. */
	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	test_write_double_or_die (file, 0, data_out.d, ARRAY_LEN (data_out.d), __LINE__) ;
	sf_close (file) ;

	/* Read the file in again. */
	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;
	test_read_double_or_die (file, 0, data_in.d, ARRAY_LEN (data_in.d), __LINE__) ;
	sf_close (file) ;

	puts ("ok") ;

	/* Test seeking. */
	print_test_name ("ogg_seek_test", filename) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	test_seek_or_die (file, 10, SEEK_SET, 10, sfinfo.channels, __LINE__) ;
	test_read_double_or_die (file, 0, seek_data, ARRAY_LEN (seek_data), __LINE__) ;
	compare_double_or_die (seek_data, data_in.d + 10, ARRAY_LEN (seek_data), __LINE__) ;

	sf_close (file) ;

	puts ("ok") ;

	unlink (filename) ;
} /* ogg_double_test */


static void
ogg_stereo_seek_test (const char * filename, int format)
{	static float data [SAMPLE_RATE] ;
	static float stereo_out [SAMPLE_RATE * 2] ;

	SNDFILE * file ;
	SF_INFO sfinfo ;
	sf_count_t pos ;
	unsigned k ;

	print_test_name (__func__, filename) ;

	gen_windowed_sine_float (data, ARRAY_LEN (data), 0.95) ;
	for (k = 0 ; k < ARRAY_LEN (data) ; k++)
	{	stereo_out [2 * k] = data [k] ;
		stereo_out [2 * k + 1] = data [ARRAY_LEN (data) - k - 1] ;
		} ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	/* Set up output file type. */
	sfinfo.format = format ;
	sfinfo.channels = 2 ;
	sfinfo.samplerate = SAMPLE_RATE ;

	/* Write the output file. */
	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	test_write_float_or_die (file, 0, stereo_out, ARRAY_LEN (stereo_out), __LINE__) ;
	sf_close (file) ;

	/* Open file in again for reading. */
	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	/* Read in the whole file. */
	test_read_float_or_die (file, 0, stereo_out, ARRAY_LEN (stereo_out), __LINE__) ;

	/* Now hammer seeking code. */
	test_seek_or_die (file, 234, SEEK_SET, 234, sfinfo.channels, __LINE__) ;
	test_readf_float_or_die (file, 0, data, 10, __LINE__) ;
	compare_float_or_die (data, stereo_out + (234 * sfinfo.channels), 10, __LINE__) ;

	test_seek_or_die (file, 442, SEEK_SET, 442, sfinfo.channels, __LINE__) ;
	test_readf_float_or_die (file, 0, data, 10, __LINE__) ;
	compare_float_or_die (data, stereo_out + (442 * sfinfo.channels), 10, __LINE__) ;

	test_seek_or_die (file, 12, SEEK_CUR, 442 + 10 + 12, sfinfo.channels, __LINE__) ;
	test_readf_float_or_die (file, 0, data, 10, __LINE__) ;
	compare_float_or_die (data, stereo_out + ((442 + 10 + 12) * sfinfo.channels), 10, __LINE__) ;

	test_seek_or_die (file, 12, SEEK_CUR, 442 + 20 + 24, sfinfo.channels, __LINE__) ;
	test_readf_float_or_die (file, 0, data, 10, __LINE__) ;
	compare_float_or_die (data, stereo_out + ((442 + 20 + 24) * sfinfo.channels), 10, __LINE__) ;

	pos = 500 - sfinfo.frames ;
	test_seek_or_die (file, pos, SEEK_END, 500, sfinfo.channels, __LINE__) ;
	test_readf_float_or_die (file, 0, data, 10, __LINE__) ;
	compare_float_or_die (data, stereo_out + (500 * sfinfo.channels), 10, __LINE__) ;

	pos = 10 - sfinfo.frames ;
	test_seek_or_die (file, pos, SEEK_END, 10, sfinfo.channels, __LINE__) ;
	test_readf_float_or_die (file, 0, data, 10, __LINE__) ;
	compare_float_or_die (data, stereo_out + (10 * sfinfo.channels), 10, __LINE__) ;

	sf_close (file) ;

	puts ("ok") ;
	unlink (filename) ;
} /* ogg_stereo_seek_test */


int
main (void)
{
	if (HAVE_EXTERNAL_XIPH_LIBS)
	{	ogg_short_test () ;
		ogg_int_test () ;
		ogg_float_test () ;
		ogg_double_test () ;

		/*-ogg_stereo_seek_test ("pcm.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_16) ;-*/
		ogg_stereo_seek_test ("vorbis_seek.ogg", SF_FORMAT_OGG | SF_FORMAT_VORBIS) ;
		}
	else
		puts ("    No Ogg/Vorbis tests because Ogg/Vorbis support was not compiled in.") ;

	return 0 ;
} /* main */
