/*
** Copyright (C) 2010-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software ; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation ; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY ; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program ; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/stat.h>
#include <math.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#if (HAVE_DECL_S_IRGRP == 0)
#include <sf_unistd.h>
#endif

#if (defined (WIN32) || defined (_WIN32))
#include <io.h>
#include <direct.h>
#endif

#include	<sndfile.h>

#include	"utils.h"

static void	rdwr_short_test	(const char *filename) ;
static void	rdwr_int_test	(const char *filename) ;
static void	rdwr_float_test	(const char *filename) ;
static void	rdwr_double_test	(const char *filename) ;
static void	rdwr_raw_test	(const char *filename) ;


int
main (void)
{
	rdwr_short_test ("rdwr_short.wav") ;
	rdwr_int_test ("rdwr_int.wav") ;
	rdwr_float_test ("rdwr_float.wav") ;
	rdwr_double_test ("rdwr_double.wav") ;
	rdwr_raw_test ("rdwr_raw.wav") ;

	return 0 ;
} /* main */


/*============================================================================================
**	Here are the test functions.
*/

static void
rdwr_short_test	(const char *filename)
{	SNDFILE *file ;
	SF_INFO sfinfo ;
    sf_count_t frames ;
    short buffer [160] ;

	print_test_name ("rdwr_short_test", filename) ;

	memset (buffer, 0, sizeof (buffer)) ;

	/* Create sound file with no data. */
	sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16 ;
	sfinfo.samplerate = 16000 ;
	sfinfo.channels = 1 ;

	unlink (filename) ;

	frames = ARRAY_LEN (buffer) ;

	/* Open again for read/write. */
	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_TRUE, __LINE__) ;

	test_write_short_or_die (file, 0, buffer, frames, __LINE__) ;

	test_read_short_or_die (file, 0, buffer, frames, __LINE__) ;

	sf_close (file) ;
	unlink (filename) ;

	puts ("ok") ;
	return ;
} /* rdwr_short_test */

static void
rdwr_int_test	(const char *filename)
{	SNDFILE *file ;
	SF_INFO sfinfo ;
    sf_count_t frames ;
    int buffer [160] ;

	print_test_name ("rdwr_int_test", filename) ;

	memset (buffer, 0, sizeof (buffer)) ;

	/* Create sound file with no data. */
	sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_32 ;
	sfinfo.samplerate = 16000 ;
	sfinfo.channels = 1 ;

	unlink (filename) ;

	frames = ARRAY_LEN (buffer) ;

	/* Open again for read/write. */
	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_TRUE, __LINE__) ;

	test_write_int_or_die (file, 0, buffer, frames, __LINE__) ;

	test_read_int_or_die (file, 0, buffer, frames, __LINE__) ;

	sf_close (file) ;
	unlink (filename) ;

	puts ("ok") ;
	return ;
} /* rdwr_int_test */

static void
rdwr_float_test	(const char *filename)
{	SNDFILE *file ;
	SF_INFO sfinfo ;
    sf_count_t frames ;
    float buffer [160] ;

	print_test_name ("rdwr_float_test", filename) ;

	memset (buffer, 0, sizeof (buffer)) ;

	/* Create sound file with no data. */
	sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_FLOAT ;
	sfinfo.samplerate = 16000 ;
	sfinfo.channels = 1 ;

	unlink (filename) ;

	frames = ARRAY_LEN (buffer) ;

	/* Open again for read/write. */
	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_TRUE, __LINE__) ;

	test_write_float_or_die (file, 0, buffer, frames, __LINE__) ;

	test_read_float_or_die (file, 0, buffer, frames, __LINE__) ;

	sf_close (file) ;
	unlink (filename) ;

	puts ("ok") ;
	return ;
} /* rdwr_float_test */

static void
rdwr_double_test	(const char *filename)
{	SNDFILE *file ;
	SF_INFO sfinfo ;
    sf_count_t frames ;
    double buffer [160] ;

	print_test_name ("rdwr_double_test", filename) ;

	memset (buffer, 0, sizeof (buffer)) ;

	/* Create sound file with no data. */
	sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_DOUBLE ;
	sfinfo.samplerate = 16000 ;
	sfinfo.channels = 1 ;

	unlink (filename) ;

	frames = ARRAY_LEN (buffer) ;

	/* Open again for read/write. */
	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_TRUE, __LINE__) ;

	test_write_double_or_die (file, 0, buffer, frames, __LINE__) ;

	test_read_double_or_die (file, 0, buffer, frames, __LINE__) ;

	sf_close (file) ;
	unlink (filename) ;

	puts ("ok") ;
	return ;
} /* rdwr_double_test */

static void
rdwr_raw_test	(const char *filename)
{	SNDFILE *file ;
	SF_INFO sfinfo ;
    sf_count_t frames ;
    unsigned char buffer [160] ;

	print_test_name ("rdwr_raw_test", filename) ;

	memset (buffer, 0, sizeof (buffer)) ;

	/* Create sound file with no data. */
	sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_U8 ;
	sfinfo.samplerate = 16000 ;
	sfinfo.channels = 1 ;

	unlink (filename) ;

	frames = ARRAY_LEN (buffer) ;

	/* Open again for read/write. */
	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_TRUE, __LINE__) ;

	test_write_raw_or_die (file, 0, buffer, frames, __LINE__) ;

	test_read_raw_or_die (file, 0, buffer, frames, __LINE__) ;

	sf_close (file) ;
	unlink (filename) ;

	puts ("ok") ;
	return ;
} /* rdwr_raw_test */



