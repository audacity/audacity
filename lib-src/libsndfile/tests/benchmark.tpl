[+ AutoGen5 template c +]
/*
** Copyright (C) 2002-2012 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#if (HAVE_DECL_S_IRGRP == 0)
#include <sf_unistd.h>
#endif

#include <string.h>
#include <math.h>
#include <time.h>
#include <fcntl.h>
#include <sys/stat.h>

#include <sndfile.h>

#ifndef		M_PI
#define		M_PI		3.14159265358979323846264338
#endif

/*
**	Neat solution to the Win32/OS2 binary file flage requirement.
**	If O_BINARY isn't already defined by the inclusion of the system
**	headers, set it to zero.
*/
#ifndef O_BINARY
#define O_BINARY 0
#endif

#define	WRITE_FLAGS	(O_WRONLY | O_CREAT | O_TRUNC | O_BINARY)
#define	READ_FLAGS	(O_RDONLY | O_BINARY)

#if (defined (WIN32) || defined (_WIN32) || defined (__OS2__))
	#define WRITE_PERMS	0777
#else
	#define WRITE_PERMS	(S_IRUSR | S_IWUSR | S_IRGRP)
#endif

#define	BUFFER_SIZE		(1 << 18)
#define	BLOCK_COUNT		(30)
#define	TEST_DURATION	(5)		/* 5 Seconds. */

typedef struct
{	double	write_rate ;
	double	read_rate ;
} PERF_STATS ;

static void	*data = NULL ;

static void calc_raw_performance (PERF_STATS *stats) ;

[+ FOR data_type
+]static void	calc_[+ (get "type_name") +]_performance (int format, double read_rate, double write_rate) ;
[+ ENDFOR data_type
+]

static int cpu_is_big_endian (void) ;

static const char* get_subtype_str (int subtype) ;

int
main (int argc, char *argv [])
{	PERF_STATS	stats ;
	char		buffer [256] = "Benchmarking " ;
	int			format_major ;

	if (! (data = malloc (BUFFER_SIZE * sizeof (double))))
	{	perror ("Error : malloc failed") ;
		exit (1) ;
		} ;

	sf_command (NULL, SFC_GET_LIB_VERSION, buffer + strlen (buffer), sizeof (buffer) - strlen (buffer)) ;

	puts (buffer) ;
	memset (buffer, '-', strlen (buffer)) ;
	puts (buffer) ;
	printf ("Each test takes a little over %d seconds.\n\n", TEST_DURATION) ;

	calc_raw_performance (&stats) ;

	if (argc < 2 || strcmp ("--native-only", argv [1]) == 0)
	{	puts ("\nNative endian I/O :") ;
		format_major = cpu_is_big_endian () ? SF_FORMAT_AIFF : SF_FORMAT_WAV ;

		calc_short_performance	(format_major | SF_FORMAT_PCM_16, stats.read_rate, stats.write_rate) ;
		calc_int_performance	(format_major | SF_FORMAT_PCM_24, stats.read_rate, stats.write_rate) ;
		calc_int_performance	(format_major | SF_FORMAT_PCM_32, stats.read_rate, stats.write_rate) ;
		calc_float_performance	(format_major | SF_FORMAT_PCM_16, stats.read_rate, stats.write_rate) ;
		calc_float_performance	(format_major | SF_FORMAT_PCM_24, stats.read_rate, stats.write_rate) ;
		calc_float_performance	(format_major | SF_FORMAT_PCM_32, stats.read_rate, stats.write_rate) ;
		calc_float_performance	(format_major | SF_FORMAT_FLOAT , stats.read_rate, stats.write_rate) ;
		} ;

	if (argc < 2 || strcmp ("--swap-only", argv [1]) == 0)
	{	puts ("\nEndian swapped I/O :") ;
		format_major = cpu_is_big_endian () ? SF_FORMAT_WAV : SF_FORMAT_AIFF ;

		calc_short_performance	(format_major | SF_FORMAT_PCM_16, stats.read_rate, stats.write_rate) ;
		calc_int_performance	(format_major | SF_FORMAT_PCM_24, stats.read_rate, stats.write_rate) ;
		calc_int_performance	(format_major | SF_FORMAT_PCM_32, stats.read_rate, stats.write_rate) ;
		calc_float_performance	(format_major | SF_FORMAT_PCM_16, stats.read_rate, stats.write_rate) ;
		calc_float_performance	(format_major | SF_FORMAT_PCM_24, stats.read_rate, stats.write_rate) ;
		calc_float_performance	(format_major | SF_FORMAT_PCM_32, stats.read_rate, stats.write_rate) ;
		calc_float_performance	(format_major | SF_FORMAT_FLOAT , stats.read_rate, stats.write_rate) ;
		} ;

	puts ("") ;

	free (data) ;

	return 0 ;
} /* main */

/*==============================================================================
*/

static void
calc_raw_performance (PERF_STATS *stats)
{	clock_t start_clock, clock_time ;
	int fd, k, byte_count, retval, op_count ;
	const char *filename ;

	filename = "benchmark.dat" ;

	byte_count = BUFFER_SIZE * sizeof (short) ;

	/* Collect write stats */
	printf ("    Raw write PCM_16  : ") ;
	fflush (stdout) ;

	clock_time = 0 ;
	op_count = 0 ;
	start_clock = clock () ;

	while (clock_time < (CLOCKS_PER_SEC * TEST_DURATION))
	{	if ((fd = open (filename, WRITE_FLAGS, WRITE_PERMS)) < 0)
		{	printf ("Error : not able to open file : %s\n", filename) ;
			perror ("") ;
			exit (1) ;
			} ;

		for (k = 0 ; k < BLOCK_COUNT ; k++)
		{	if ((retval = write (fd, data, byte_count)) != byte_count)
			{	printf ("Error : write returned %d (should have been %d)\n", retval, byte_count) ;
				exit (1) ;
				} ;
			} ;

		close (fd) ;

		clock_time = clock () - start_clock ;
		op_count ++ ;
		} ;

	stats->write_rate = (1.0 * BUFFER_SIZE) * BLOCK_COUNT * op_count ;
	stats->write_rate *= (1.0 * CLOCKS_PER_SEC) / clock_time ;
	printf ("%10.0f samples per sec\n", stats->write_rate) ;

	/* Collect read stats */
	printf ("    Raw read  PCM_16  : ") ;
	fflush (stdout) ;

	clock_time = 0 ;
	op_count = 0 ;
	start_clock = clock () ;

	while (clock_time < (CLOCKS_PER_SEC * TEST_DURATION))
	{	if ((fd = open (filename, READ_FLAGS)) < 0)
		{	printf ("Error : not able to open file : %s\n", filename) ;
			perror ("") ;
			exit (1) ;
			} ;

		for (k = 0 ; k < BLOCK_COUNT ; k++)
		{	if ((retval = read (fd, data, byte_count)) != byte_count)
			{	printf ("Error : write returned %d (should have been %d)\n", retval, byte_count) ;
				exit (1) ;
				} ;
			} ;

		close (fd) ;

		clock_time = clock () - start_clock ;
		op_count ++ ;
		} ;

	stats->read_rate = (1.0 * BUFFER_SIZE) * BLOCK_COUNT * op_count ;
	stats->read_rate *= (1.0 * CLOCKS_PER_SEC) / clock_time ;
	printf ("%10.0f samples per sec\n", stats->read_rate) ;

	unlink (filename) ;
} /* calc_raw_performance */

/*------------------------------------------------------------------------------
*/

[+ FOR data_type
+]static void
calc_[+ (get "type_name") +]_performance (int format, double read_rate, double write_rate)
{	SNDFILE *file ;
	SF_INFO	sfinfo ;
	clock_t start_clock, clock_time ;
	double	performance ;
	int k, item_count, retval, op_count ;
	const char* subtype ;
	[+ (get "type_name") +] *[+ (get "type_name") +]_data ;
	const char *filename ;

	filename = "benchmark.dat" ;
	subtype = get_subtype_str (format & SF_FORMAT_SUBMASK) ;

	[+ (get "type_name") +]_data = data ;
	item_count = BUFFER_SIZE ;
	for (k = 0 ; k < item_count ; k++)
		[+ (get "type_name") +]_data [k] = [+ (get "multiplier") +] * sin (2 * M_PI * k / 32000.0) ;

	/* Collect write stats */
	printf ("    Write %-5s   to  %s : ", "[+ (get "type_name") +]", subtype) ;
	fflush (stdout) ;

	sfinfo.channels = 1 ;
	sfinfo.format = format ;
	sfinfo.frames = 1 ;
	sfinfo.samplerate = 32000 ;

	clock_time = 0 ;
	op_count = 0 ;
	start_clock = clock () ;

	while (clock_time < (CLOCKS_PER_SEC * TEST_DURATION))
	{	if (! (file = sf_open (filename, SFM_WRITE, &sfinfo)))
		{	printf ("Error : not able to open file : %s\n", filename) ;
			perror ("") ;
			exit (1) ;
			} ;

		/* Turn off the addition of a PEAK chunk. */
		sf_command (file, SFC_SET_ADD_PEAK_CHUNK, NULL, SF_FALSE) ;

		for (k = 0 ; k < BLOCK_COUNT ; k++)
		{	if ((retval = sf_write_[+ (get "type_name") +] (file, [+ (get "type_name") +]_data, item_count)) != item_count)
			{	printf ("Error : sf_write_short returned %d (should have been %d)\n", retval, item_count) ;
				exit (1) ;
				} ;
			} ;

		sf_close (file) ;

		clock_time = clock () - start_clock ;
		op_count ++ ;
		} ;

	performance = (1.0 * BUFFER_SIZE) * BLOCK_COUNT * op_count ;
	performance *= (1.0 * CLOCKS_PER_SEC) / clock_time ;
	printf ("%6.2f%% of raw write\n", 100.0 * performance / write_rate) ;

	/* Collect read stats */
	printf ("    Read  %-5s  from %s : ", "[+ (get "type_name") +]", subtype) ;
	fflush (stdout) ;

	clock_time = 0 ;
	op_count = 0 ;
	start_clock = clock () ;

	while (clock_time < (CLOCKS_PER_SEC * TEST_DURATION))
	{	if (! (file = sf_open (filename, SFM_READ, &sfinfo)))
		{	printf ("Error : not able to open file : %s\n", filename) ;
			perror ("") ;
			exit (1) ;
			} ;

		for (k = 0 ; k < BLOCK_COUNT ; k++)
		{	if ((retval = sf_read_[+ (get "type_name") +] (file, [+ (get "type_name") +]_data, item_count)) != item_count)
			{	printf ("Error : write returned %d (should have been %d)\n", retval, item_count) ;
				exit (1) ;
				} ;
			} ;

		sf_close (file) ;

		clock_time = clock () - start_clock ;
		op_count ++ ;
		} ;

	performance = (1.0 * item_count) * BLOCK_COUNT * op_count ;
	performance *= (1.0 * CLOCKS_PER_SEC) / clock_time ;
	printf ("%6.2f%% of raw read\n", 100.0 * performance / read_rate) ;

	unlink (filename) ;

} /* calc_[+ (get "type_name") +]_performance */
[+ ENDFOR data_type
+]

/*==============================================================================
*/

static int
cpu_is_big_endian (void)
{	unsigned char 	*cptr ;
	int 			endtest ;

	endtest = 0x12345678 ;

	cptr = (unsigned char*) (&endtest) ;

	if (cptr [0] == 0x12 && cptr [1] == 0x34 && cptr [3] == 0x78)
		return SF_TRUE ;

	return SF_FALSE ;
} /* cpu_is_big_endian */

static const char*
get_subtype_str (int subtype)
{	switch (subtype)
	{	case SF_FORMAT_PCM_16 :
				return "PCM_16" ;

		case SF_FORMAT_PCM_24 :
				return "PCM_24" ;

		case SF_FORMAT_PCM_32 :
				return "PCM_32" ;

		case SF_FORMAT_FLOAT :
				return "FLOAT " ;

		case SF_FORMAT_DOUBLE :
				return "DOUBLE" ;

		default : break ;
		} ;

	return "UNKNOWN" ;
} /* get_subtype_str */

