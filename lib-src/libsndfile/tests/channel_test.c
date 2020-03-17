/*
** Copyright (C) 2001-2015 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include "utils.h"

#define	BUFFER_LEN		(1 << 10)
#define LOG_BUFFER_SIZE	1024

static	void	channel_test			(void) ;
static	double	max_diff		(const float *a, const float *b, unsigned int len, unsigned int * position) ;

int
main (void) // int argc, char *argv [])
{	channel_test () ;
	return 0 ;
} /* main */

/*============================================================================================
**	Here are the test functions.
*/

static void
channel_test (void)
{	static float	float_data [1024] ;
	static float	read_float [1024] ;
	static int		read_int [1024] ;
	static short	read_short [1024] ;
	unsigned int	ch, k, position = 0 ;

	gen_windowed_sine_float (float_data, ARRAY_LEN (float_data), 0.9) ;

	for (ch = 1 ; ch <= 8 ; ch++)
	{	SNDFILE	*file ;
		SF_INFO	wsfinfo, rsfinfo ;
		sf_count_t wframes = ARRAY_LEN (float_data) / ch ;
		double	maxdiff ;
		char	filename [256] ;

		snprintf (filename, sizeof (filename), "chan_%d.wav", ch) ;
		print_test_name (__func__, filename) ;

		sf_info_setup (&wsfinfo, SF_FORMAT_WAV | SF_FORMAT_FLOAT, 48000, ch) ;
		sf_info_clear (&rsfinfo) ;

		/* Write the test file. */
		file = test_open_file_or_die (filename, SFM_WRITE, &wsfinfo, SF_FALSE, __LINE__) ;
		test_writef_float_or_die (file, 0, float_data, wframes, __LINE__) ;
		sf_close (file) ;

		/* Read it as float and test. */
		file = test_open_file_or_die (filename, SFM_READ, &rsfinfo, SF_FALSE, __LINE__) ;
		exit_if_true (rsfinfo.frames == 0,
				"\n\nLine %d : Frames in file %" PRId64 ".\n\n", __LINE__, rsfinfo.frames) ;
		exit_if_true (wframes != rsfinfo.frames,
				"\n\nLine %d : Wrote %" PRId64 ", read %" PRId64 " frames.\n\n", __LINE__, wframes, rsfinfo.frames) ;

		sf_command (file, SFC_SET_SCALE_FLOAT_INT_READ, NULL, SF_TRUE) ;

		test_readf_float_or_die (file, 0, read_float, rsfinfo.frames, __LINE__) ;
		compare_float_or_die (float_data, read_float, ch * rsfinfo.frames, __LINE__) ;

		/* Read it as short and test. */
		test_seek_or_die (file, 0, SEEK_SET, 0, ch, __LINE__) ;
		test_readf_short_or_die (file, 0, read_short, rsfinfo.frames, __LINE__) ;

		for (k = 0 ; k < ARRAY_LEN (read_float) ; k++)
			read_float [k] = read_short [k] * (0.9 / 0x8000) ;

		maxdiff = max_diff (float_data, read_float, ch * rsfinfo.frames, &position) ;
		exit_if_true (maxdiff > 0.5, "\n\nLine %d : Max diff is %f at index %u\n\n", __LINE__, maxdiff, position) ;

		/* Read it as int and test. */
		test_seek_or_die (file, 0, SEEK_SET, 0, ch, __LINE__) ;
		test_readf_int_or_die (file, 0, read_int, rsfinfo.frames, __LINE__) ;

		for (k = 0 ; k < ARRAY_LEN (read_float) ; k++)
			read_float [k] = read_int [k] * (0.9 / 0x80000000) ;

		maxdiff = max_diff (float_data, read_float, ch * rsfinfo.frames, &position) ;
		exit_if_true (maxdiff > 0.5, "\n\nLine %d : Max diff is %f at index %u\n\n", __LINE__, maxdiff, position) ;

		sf_close (file) ;
		unlink (filename) ;
		printf ("ok\n") ;
		} ;

	return ;
} /* channel_test */

static double
max_diff (const float *a, const float *b, unsigned int len, unsigned int * position)
{	double mdiff = 0.0, diff ;
	unsigned int k ;

	for (k = 0 ; k < len ; k++)
	{	diff = fabs (a [k] - b [k]) ;
		if (diff > mdiff)
		{ 	mdiff = diff ;
			*position = k ;
			} ;
		} ;

	return mdiff ;
} /* max_diff */
