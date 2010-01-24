/*
** Copyright (C) 1999-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<unistd.h>
#include	<math.h>

#include	<sndfile.h>

#include	"utils.h"

#define		BUFFER_SIZE		(1<<14) /* Should be (1<<14) */
#define		SAMPLE_RATE		(11025)

#ifndef		M_PI
#define		M_PI		3.14159265358979323846264338
#endif

static	void	lcomp_test_int	(const char *str, const char *filename, int filetype, double margin) ;

static	int		error_function (double data, double orig, double margin) ;
static	int		decay_response (int k) ;

static	void	gen_signal_double (double *data, double scale, int datalen) ;

/* Force the start of these buffers to be double aligned. Sparc-solaris will
** choke if they are not.
*/

typedef union
{	double	d [BUFFER_SIZE + 1] ;
	int 	i [BUFFER_SIZE + 1] ;
} BUFFER ;

static	BUFFER	data_buffer ;
static	BUFFER	orig_buffer ;

int
main (void)
{	const char	*filename = "test.au" ;

	lcomp_test_int	("au_g721", filename, SF_ENDIAN_BIG | SF_FORMAT_AU | SF_FORMAT_G721_32, 0.06) ;

	return 0 ;
} /* main */

/*============================================================================================
**	Here are the test functions.
*/

static void
lcomp_test_int (const char *str, const char *filename, int filetype, double margin)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int			k, m, *orig, *data, sum_abs ;
	long		datalen, seekpos ;
	double		scale ;

	printf ("\nThis is program is not part of the libsndfile test suite.\n\n") ;

	printf ("    lcomp_test_int      : %s ... ", str) ;
	fflush (stdout) ;

	datalen = BUFFER_SIZE ;

	scale = 1.0 * 0x10000 ;

	data = data_buffer.i ;
	orig = orig_buffer.i ;

	gen_signal_double (orig_buffer.d, 32000.0 * scale, datalen) ;
	for (k = 0 ; k < datalen ; k++)
		orig [k] = orig_buffer.d [k] ;


	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= 123456789 ;	/* Ridiculous value. */
	sfinfo.channels		= 1 ;
	sfinfo.format		= filetype ;

	if (! (file = sf_open (filename, SFM_WRITE, &sfinfo)))
	{	printf ("sf_open_write failed with error : ") ;
		puts (sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	if ((k = sf_writef_int (file, orig, datalen)) != datalen)
	{	printf ("sf_writef_int failed with short write (%ld => %d).\n", datalen, k) ;
		exit (1) ;
		} ;
	sf_close (file) ;

	memset (data, 0, datalen * sizeof (int)) ;

	if ((filetype & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW)
		memset (&sfinfo, 0, sizeof (sfinfo)) ;

	if (! (file = sf_open (filename, SFM_READ, &sfinfo)))
	{	printf ("sf_open_read failed with error : ") ;
		puts (sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	if ((sfinfo.format & (SF_FORMAT_TYPEMASK | SF_FORMAT_SUBMASK)) != (filetype & (SF_FORMAT_TYPEMASK | SF_FORMAT_SUBMASK)))
	{	printf ("Line %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < datalen)
	{	printf ("Too few.frames in file. (%ld should be a little more than %ld)\n", datalen, SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	if (sfinfo.frames > (datalen + datalen / 2))
	{	printf ("Too many.frames in file. (%ld should be a little more than %ld)\n", datalen, SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != 1)
	{	printf ("Incorrect number of channels in file.\n") ;
		exit (1) ;
		} ;

	check_log_buffer_or_die (file, __LINE__) ;

	if ((k = sf_readf_int (file, data, datalen)) != datalen)
	{	printf ("Line %d: short read (%d should be %ld).\n", __LINE__, k, datalen) ;
		exit (1) ;
		} ;

	sum_abs = 0 ;
	for (k = 0 ; k < datalen ; k++)
	{	if (error_function (data [k] / scale, orig [k] / scale, margin))
		{	printf ("Line %d: Incorrect sample (#%d : %f should be %f).\n", __LINE__, k, data [k] / scale, orig [k] / scale) ;
			oct_save_int (orig, data, datalen) ;
			exit (1) ;
			} ;
		sum_abs = abs (sum_abs + abs (data [k])) ;
		} ;

	if (sum_abs < 1.0)
	{	printf ("Line %d: Signal is all zeros.\n", __LINE__) ;
		exit (1) ;
		} ;

	if ((k = sf_readf_int (file, data, datalen)) != sfinfo.frames - datalen)
	{	printf ("Line %d: Incorrect read length (%ld should be %d).\n", __LINE__, SF_COUNT_TO_LONG (sfinfo.frames - datalen), k) ;
		exit (1) ;
		} ;

	/*	This check is only for block based encoders which must append silence
	**	to the end of a file so as to fill out a block.
	*/
	if ((sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_MS_ADPCM)
		for (k = 0 ; k < sfinfo.frames - datalen ; k++)
			if (abs (data [k] / scale) > decay_response (k))
			{	printf ("Line %d : Incorrect sample B (#%d : abs (%d) should be < %d).\n", __LINE__, k, data [k], decay_response (k)) ;
				exit (1) ;
				} ;

	if (! sfinfo.seekable)
	{	printf ("ok\n") ;
		return ;
		} ;

	/* Now test sf_seek function. */

	if ((k = sf_seek (file, 0, SEEK_SET)) != 0)
	{	printf ("Line %d: Seek to start of file failed (%d).\n", __LINE__, k) ;
		exit (1) ;
		} ;

	for (m = 0 ; m < 3 ; m++)
	{	if ((k = sf_readf_int (file, data, 11)) != 11)
		{	printf ("Line %d: Incorrect read length (11 => %d).\n", __LINE__, k) ;
			exit (1) ;
			} ;

		for (k = 0 ; k < 11 ; k++)
			if (error_function (data [k] / scale, orig [k + m * 11] / scale, margin))
			{	printf ("Line %d: Incorrect sample (m = %d) (#%d : %d => %d).\n", __LINE__, m, k + m * 11, orig [k + m * 11], data [k]) ;
				for (m = 0 ; m < 1 ; m++)
					printf ("%d ", data [m]) ;
				printf ("\n") ;
				exit (1) ;
				} ;
		} ;

	seekpos = BUFFER_SIZE / 10 ;

	/* Check seek from start of file. */
	if ((k = sf_seek (file, seekpos, SEEK_SET)) != seekpos)
	{	printf ("Seek to start of file + %ld failed (%d).\n", seekpos, k) ;
		exit (1) ;
		} ;

	if ((k = sf_readf_int (file, data, 1)) != 1)
	{	printf ("Line %d: sf_readf_int (file, data, 1) returned %d.\n", __LINE__, k) ;
		exit (1) ;
		} ;

	if (error_function ((double) data [0], (double) orig [seekpos], margin))
	{	printf ("Line %d: sf_seek (SEEK_SET) followed by sf_readf_int failed (%d, %d).\n", __LINE__, orig [1], data [0]) ;
		exit (1) ;
		} ;

	if ((k = sf_seek (file, 0, SEEK_CUR)) != seekpos + 1)
	{	printf ("Line %d: sf_seek (SEEK_CUR) with 0 offset failed (%d should be %ld)\n", __LINE__, k, seekpos + 1) ;
		exit (1) ;
		} ;

	seekpos = sf_seek (file, 0, SEEK_CUR) + BUFFER_SIZE / 5 ;
	k = sf_seek (file, BUFFER_SIZE / 5, SEEK_CUR) ;
	sf_readf_int (file, data, 1) ;
	if (error_function ((double) data [0], (double) orig [seekpos], margin) || k != seekpos)
	{	printf ("Line %d: sf_seek (forwards, SEEK_CUR) followed by sf_readf_int failed (%d, %d) (%d, %ld).\n", __LINE__, data [0], orig [seekpos], k, seekpos + 1) ;
		exit (1) ;
		} ;

	seekpos = sf_seek (file, 0, SEEK_CUR) - 20 ;
	/* Check seek backward from current position. */
	k = sf_seek (file, -20, SEEK_CUR) ;
	sf_readf_int (file, data, 1) ;
	if (error_function ((double) data [0], (double) orig [seekpos], margin) || k != seekpos)
	{	printf ("sf_seek (backwards, SEEK_CUR) followed by sf_readf_int failed (%d, %d) (%d, %ld).\n", data [0], orig [seekpos], k, seekpos) ;
		exit (1) ;
		} ;

	/* Check that read past end of file returns number of items. */
	sf_seek (file, (int) sfinfo.frames, SEEK_SET) ;

 	if ((k = sf_readf_int (file, data, datalen)) != 0)
 	{	printf ("Line %d: Return value from sf_readf_int past end of file incorrect (%d).\n", __LINE__, k) ;
 		exit (1) ;
 		} ;

	/* Check seek backward from end. */
	if ((k = sf_seek (file, 5 - (int) sfinfo.frames, SEEK_END)) != 5)
	{	printf ("sf_seek (SEEK_END) returned %d instead of %d.\n", k, 5) ;
		exit (1) ;
		} ;

	sf_readf_int (file, data, 1) ;
	if (error_function (data [0] / scale, orig [5] / scale, margin))
	{	printf ("Line %d: sf_seek (SEEK_END) followed by sf_readf_short failed (%d should be %d).\n", __LINE__, data [0], orig [5]) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	printf ("ok\n") ;
} /* lcomp_test_int */

/*========================================================================================
**	Auxiliary functions
*/

#define		SIGNAL_MAXVAL	30000.0
#define		DECAY_COUNT		800

static int
decay_response (int k)
{	if (k < 1)
		return (int) (1.2 * SIGNAL_MAXVAL) ;
	if (k > DECAY_COUNT)
		return 0 ;
	return (int) (1.2 * SIGNAL_MAXVAL * (DECAY_COUNT - k) / (1.0 * DECAY_COUNT)) ;
} /* decay_response */

static void
gen_signal_double (double *data, double scale, int datalen)
{	int		k, ramplen ;
	double	amp = 0.0 ;

	ramplen = datalen / 18 ;

	for (k = 0 ; k < datalen ; k++)
	{	if (k <= ramplen)
			amp = scale * k / ((double) ramplen) ;
		else if (k > datalen - ramplen)
			amp = scale * (datalen - k) / ((double) ramplen) ;

		data [k] = amp * (0.4 * sin (33.3 * 2.0 * M_PI * ((double) (k + 1)) / ((double) SAMPLE_RATE))
							+ 0.3 * cos (201.1 * 2.0 * M_PI * ((double) (k + 1)) / ((double) SAMPLE_RATE))) ;
		} ;

	return ;
} /* gen_signal_double */

static int
error_function (double data, double orig, double margin)
{	double error ;

	if (fabs (orig) <= 500.0)
		error = fabs (fabs (data) - fabs (orig)) / 2000.0 ;
	else if (fabs (orig) <= 1000.0)
		error = fabs (data - orig) / 3000.0 ;
	else
		error = fabs (data - orig) / fabs (orig) ;

	if (error > margin)
	{	printf ("\n\n*******************\nError : %f\n", error) ;
		return 1 ;
		} ;
	return 0 ;
} /* error_function */

