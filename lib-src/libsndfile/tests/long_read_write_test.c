/*
** Copyright (C) 2015-2016 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <inttypes.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#else
#include "sf_unistd.h"
#endif

#include <sndfile.h>

#include "dft_cmp.h"
#include "utils.h"

#define	BUFFER_LENGTH		10000
#define	SAMPLE_RATE			44010

static void	short_lrw_test	(const char *filename, int filetype, const short * output, int out_len) ;
static void	int_lrw_test	(const char *filename, int filetype, const int * output, int out_len) ;
static void	float_lrw_test	(const char *filename, int filetype, const float * output, int out_len) ;
static void	double_lrw_test	(const char *filename, int filetype, const double * output, int out_len) ;


static short	short_data [BUFFER_LENGTH] ;
static int		int_data [BUFFER_LENGTH] ;
static float	float_data [BUFFER_LENGTH] ;
static double	double_data [BUFFER_LENGTH] ;

int
main (int argc, char *argv [])
{	int do_all ;
	size_t k ;

	if (argc != 2)
	{	printf ("Usage : %s <test>\n", argv [0]) ;
		printf ("    Where <test> is one of the following:\n") ;
		printf ("           alac        - test CAF/ALAC file functions\n") ;
		printf ("           all         - perform all tests\n") ;
		exit (1) ;
		} ;

	for (k = 0 ; k < ARRAY_LEN (short_data) ; k++)
	{	int value = k / 32 ;
		int_data [k] = (value & 1 ? -1 : 1) * value ;
		short_data [k] = int_data [k] ;
		float_data [k] = int_data [k] / 32000.0 ;
		double_data [k] = int_data [k] / 32000.0 ;
		}

	do_all = ! strcmp (argv [1], "all") ;

	if (do_all || strcmp (argv [1], "alac") == 0)
	{	short_lrw_test	("alac.caf", SF_FORMAT_CAF | SF_FORMAT_ALAC_16, short_data, ARRAY_LEN (short_data)) ;
		int_lrw_test	("alac.caf", SF_FORMAT_CAF | SF_FORMAT_ALAC_32, int_data, ARRAY_LEN (int_data)) ;
		float_lrw_test	("alac.caf", SF_FORMAT_CAF | SF_FORMAT_ALAC_32, float_data, ARRAY_LEN (float_data)) ;
		double_lrw_test	("alac.caf", SF_FORMAT_CAF | SF_FORMAT_ALAC_32, double_data, ARRAY_LEN (double_data)) ;
		} ;

	return 0 ;
} /* main */

/*============================================================================================
 *	Here are the test functions.
 */

static void
short_lrw_test (const char *filename, int filetype, const short * output, int out_len)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int			k ;
	short		input [BUFFER_LENGTH] ;

	print_test_name ("short_lrw_test", filename) ;

	exit_if_true (BUFFER_LENGTH > out_len, "\n\nLine %d: Bad array length.\n", __LINE__) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= out_len ;
	sfinfo.channels		= 1 ;
	sfinfo.format		= filetype ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;

	test_write_short_or_die (file, 0, output, out_len, __LINE__) ;

	sf_close (file) ;

	memset (input, 0, sizeof (input)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	exit_if_true (sfinfo.format != filetype, "\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
	exit_if_true (sfinfo.frames < out_len, "\n\nLine %d: Incorrect number of frames in file (too short). (%" PRId64 " should be %d)\n", __LINE__, sfinfo.frames, DFT_DATA_LENGTH) ;
	exit_if_true (sfinfo.channels != 1, "\n\nLine %d: Incorrect number of channels in file.\n", __LINE__) ;

	check_log_buffer_or_die (file, __LINE__) ;

	test_read_short_or_die (file, 0, input, out_len, __LINE__) ;

	sf_close (file) ;

	for (k = 0 ; k < out_len ; k++)
		exit_if_true (input [k] != output [k],
			"\n\nLine: %d: Error on input %d, expected %d, got %d\n", __LINE__, k, output [k], input [k]) ;

	puts ("ok") ;
	unlink (filename) ;

	return ;
} /* short_lrw_test */

static void
int_lrw_test (const char *filename, int filetype, const int * output, int out_len)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int			k ;
	int			input [BUFFER_LENGTH] ;

	print_test_name ("int_lrw_test", filename) ;

	exit_if_true (BUFFER_LENGTH > out_len, "\n\nLine %d: Bad array length.\n", __LINE__) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= out_len ;
	sfinfo.channels		= 1 ;
	sfinfo.format		= filetype ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;

	test_write_int_or_die (file, 0, output, out_len, __LINE__) ;

	sf_close (file) ;

	memset (input, 0, sizeof (input)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	exit_if_true (sfinfo.format != filetype, "\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
	exit_if_true (sfinfo.frames < out_len, "\n\nLine %d: Incorrect number of frames in file (too int). (%" PRId64 " should be %d)\n", __LINE__, sfinfo.frames, DFT_DATA_LENGTH) ;
	exit_if_true (sfinfo.channels != 1, "\n\nLine %d: Incorrect number of channels in file.\n", __LINE__) ;

	check_log_buffer_or_die (file, __LINE__) ;

	test_read_int_or_die (file, 0, input, out_len, __LINE__) ;

	sf_close (file) ;

	for (k = 0 ; k < out_len ; k++)
		exit_if_true (input [k] != output [k],
			"\n\nLine: %d: Error on input %d, expected %d, got %d\n", __LINE__, k, output [k], input [k]) ;

	puts ("ok") ;
	unlink (filename) ;

	return ;
} /* int_lrw_test */

static void
float_lrw_test (const char *filename, int filetype, const float * output, int out_len)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int			k ;
	float		input [BUFFER_LENGTH] ;

	print_test_name ("float_lrw_test", filename) ;

	exit_if_true (BUFFER_LENGTH > out_len, "\n\nLine %d: Bad array length.\n", __LINE__) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= out_len ;
	sfinfo.channels		= 1 ;
	sfinfo.format		= filetype ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;

	test_write_float_or_die (file, 0, output, out_len, __LINE__) ;

	sf_close (file) ;

	memset (input, 0, sizeof (input)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	exit_if_true (sfinfo.format != filetype, "\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
	exit_if_true (sfinfo.frames < out_len, "\n\nLine %d: Incorrect number of frames in file (too float). (%" PRId64 " should be %d)\n", __LINE__, sfinfo.frames, DFT_DATA_LENGTH) ;
	exit_if_true (sfinfo.channels != 1, "\n\nLine %d: Incorrect number of channels in file.\n", __LINE__) ;

	check_log_buffer_or_die (file, __LINE__) ;

	test_read_float_or_die (file, 0, input, out_len, __LINE__) ;

	sf_close (file) ;

	for (k = 0 ; k < out_len ; k++)
		exit_if_true (fabs (input [k] - output [k]) > 0.00001,
			"\n\nLine: %d: Error on input %d, expected %f, got %f\n", __LINE__, k, output [k], input [k]) ;

	puts ("ok") ;
	unlink (filename) ;

	return ;
} /* float_lrw_test */

static void
double_lrw_test (const char *filename, int filetype, const double * output, int out_len)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int			k ;
	double		input [BUFFER_LENGTH] ;

	print_test_name ("double_lrw_test", filename) ;

	exit_if_true (BUFFER_LENGTH > out_len, "\n\nLine %d: Bad array length.\n", __LINE__) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= out_len ;
	sfinfo.channels		= 1 ;
	sfinfo.format		= filetype ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;

	test_write_double_or_die (file, 0, output, out_len, __LINE__) ;

	sf_close (file) ;

	memset (input, 0, sizeof (input)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	exit_if_true (sfinfo.format != filetype, "\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
	exit_if_true (sfinfo.frames < out_len, "\n\nLine %d: Incorrect number of frames in file (too double). (%" PRId64 " should be %d)\n", __LINE__, sfinfo.frames, DFT_DATA_LENGTH) ;
	exit_if_true (sfinfo.channels != 1, "\n\nLine %d: Incorrect number of channels in file.\n", __LINE__) ;

	check_log_buffer_or_die (file, __LINE__) ;

	test_read_double_or_die (file, 0, input, out_len, __LINE__) ;

	sf_close (file) ;

	for (k = 0 ; k < out_len ; k++)
		exit_if_true (fabs (input [k] - output [k]) > 0.00001,
			"\n\nLine: %d: Error on input %d, expected %f, got %f\n", __LINE__, k, output [k], input [k]) ;

	puts ("ok") ;
	unlink (filename) ;

	return ;
} /* double_lrw_test */

