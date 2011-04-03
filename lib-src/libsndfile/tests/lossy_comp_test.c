/*
** Copyright (C) 1999-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#define	BUFFER_SIZE		(1<<14) /* Should be (1<<14) */
#define	SAMPLE_RATE		11025

#ifndef		M_PI
#define		M_PI		3.14159265358979323846264338
#endif

#define		LCT_MAX(x,y)	((x) > (y) ? (x) : (y))

static	void	lcomp_test_short	(const char *filename, int filetype, int chan, double margin) ;
static	void	lcomp_test_int		(const char *filename, int filetype, int chan, double margin) ;
static	void	lcomp_test_float	(const char *filename, int filetype, int chan, double margin) ;
static	void	lcomp_test_double	(const char *filename, int filetype, int chan, double margin) ;

static	void	sdlcomp_test_short	(const char *filename, int filetype, int chan, double margin) ;
static	void	sdlcomp_test_int	(const char *filename, int filetype, int chan, double margin) ;
static	void	sdlcomp_test_float	(const char *filename, int filetype, int chan, double margin) ;
static	void	sdlcomp_test_double	(const char *filename, int filetype, int chan, double margin) ;

static void		read_raw_test (const char *filename, int filetype, int chan) ;

static	int		error_function (double data, double orig, double margin) ;
static	int		decay_response (int k) ;

static	void	gen_signal_double (double *data, double scale, int channels, int datalen) ;

static	void	smoothed_diff_short (short *data, unsigned int datalen) ;
static	void	smoothed_diff_int (int *data, unsigned int datalen) ;
static	void	smoothed_diff_float (float *data, unsigned int datalen) ;
static	void	smoothed_diff_double (double *data, unsigned int datalen) ;

static void		check_comment (SNDFILE * file, int format, int lineno) ;

static int		is_lossy (int filetype) ;

/*
** Force the start of these buffers to be double aligned. Sparc-solaris will
** choke if they are not.
*/
typedef union
{	double	d [BUFFER_SIZE + 1] ;
	float	f [BUFFER_SIZE + 1] ;
	int		i [BUFFER_SIZE + 1] ;
	short	s [BUFFER_SIZE + 1] ;
	char	c [BUFFER_SIZE + 1] ;
} BUFFER ;

static	BUFFER	data_buffer ;
static	BUFFER	orig_buffer ;
static	BUFFER	smooth_buffer ;

static const char *long_comment =
	"This is really quite a long comment. It is designed to be long enough "
	"to screw up the encoders and decoders if the file container format does "
	"not handle things correctly. If everything is working correctly, the "
	"decoder will only decode the actual audio data, and not this string at "
	"the end of the file." ;

int
main (int argc, char *argv [])
{	int		do_all = 0 ;
	int		test_count = 0 ;

	if (argc != 2)
	{	printf ("Usage : %s <test>\n", argv [0]) ;
		printf ("    Where <test> is one of the following:\n") ;
		printf ("           wav_ima     - test IMA ADPCM WAV file functions\n") ;
		printf ("           wav_msadpcm - test MS ADPCM WAV file functions\n") ;
		printf ("           wav_gsm610  - test GSM 6.10 WAV file functions\n") ;
		printf ("           wav_ulaw    - test u-law WAV file functions\n") ;
		printf ("           wav_alaw    - test A-law WAV file functions\n") ;
		printf ("           wve         - test Psion WVE file functions\n") ;
		printf ("           all         - perform all tests\n") ;
		exit (1) ;
		} ;

	do_all = ! strcmp (argv [1], "all") ;

	if (do_all || strcmp (argv [1], "wav_pcm") == 0)
	{	/* This is just a sanity test for PCM encoding. */
		lcomp_test_short	("pcm.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_16, 2, 1e-50) ;
		lcomp_test_int		("pcm.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_32, 2, 1e-50) ;
		lcomp_test_short	("pcm.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_PCM_16, 2, 1e-50) ;
		lcomp_test_int		("pcm.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_PCM_32, 2, 1e-50) ;
		/* Lite remove start */
		lcomp_test_float	("pcm.wav", SF_FORMAT_WAV | SF_FORMAT_FLOAT, 2, 1e-50) ;
		lcomp_test_double	("pcm.wav", SF_FORMAT_WAV | SF_FORMAT_DOUBLE, 2, 1e-50) ;
		/* Lite remove end */

		read_raw_test ("pcm.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_U8, 2) ;
		test_count++ ;
		} ;

	/* For all the rest, if the file format supports more than 1 channel, use stereo. */
	/* Lite remove start */
	if (do_all || strcmp (argv [1], "wav_ima") == 0)
	{	lcomp_test_short	("ima.wav", SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		lcomp_test_int		("ima.wav", SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 2, 0.65) ;
		lcomp_test_float	("ima.wav", SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		lcomp_test_double	("ima.wav", SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;

		lcomp_test_short	("ima.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		lcomp_test_int		("ima.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		lcomp_test_float	("ima.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		lcomp_test_double	("ima.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;

		sdlcomp_test_short	("ima.wav", SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		sdlcomp_test_int	("ima.wav", SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		sdlcomp_test_float	("ima.wav", SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		sdlcomp_test_double	("ima.wav", SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "wav_msadpcm") == 0)
	{	lcomp_test_short	("msadpcm.wav", SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		lcomp_test_int		("msadpcm.wav", SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		lcomp_test_float	("msadpcm.wav", SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		lcomp_test_double	("msadpcm.wav", SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 2, 0.36) ;

		lcomp_test_short	("msadpcm.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		lcomp_test_int		("msadpcm.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		lcomp_test_float	("msadpcm.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		lcomp_test_double	("msadpcm.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 2, 0.36) ;

		sdlcomp_test_short	("msadpcm.wav", SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		sdlcomp_test_int	("msadpcm.wav", SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		sdlcomp_test_float	("msadpcm.wav", SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		sdlcomp_test_double	("msadpcm.wav", SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, 2, 0.36) ;

		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "wav_g721") == 0)
	{	printf ("**** Fix this later : error bound should be 0.06 ****\n") ;
		lcomp_test_short	("g721.wav", SF_FORMAT_WAV | SF_FORMAT_G721_32, 1, 0.7) ;
		lcomp_test_int		("g721.wav", SF_FORMAT_WAV | SF_FORMAT_G721_32, 1, 0.7) ;

		lcomp_test_short	("g721.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_G721_32, 1, 0.7) ;
		lcomp_test_int		("g721.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_G721_32, 1, 0.7) ;

		test_count++ ;
		} ;
	/* Lite remove end */

	if (do_all || strcmp (argv [1], "wav_ulaw") == 0)
	{	lcomp_test_short	("ulaw.wav", SF_FORMAT_WAV | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_int		("ulaw.wav", SF_FORMAT_WAV | SF_FORMAT_ULAW, 2, 0.04) ;

		lcomp_test_short	("ulaw.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_int		("ulaw.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_ULAW, 2, 0.04) ;

		/* Lite remove start */
		lcomp_test_float	("ulaw.wav", SF_FORMAT_WAV | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_double	("ulaw.wav", SF_FORMAT_WAV | SF_FORMAT_ULAW, 2, 0.04) ;
		/* Lite remove end */

		read_raw_test ("ulaw.wav", SF_FORMAT_WAV | SF_FORMAT_ULAW, 2) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "wav_alaw") == 0)
	{	lcomp_test_short	("alaw.wav", SF_FORMAT_WAV | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_int		("alaw.wav", SF_FORMAT_WAV | SF_FORMAT_ALAW, 2, 0.04) ;
		/* Lite remove start */
		lcomp_test_float	("alaw.wav", SF_FORMAT_WAV | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_double	("alaw.wav", SF_FORMAT_WAV | SF_FORMAT_ALAW, 2, 0.04) ;
		/* Lite remove end */

		read_raw_test ("alaw.wav", SF_FORMAT_WAV | SF_FORMAT_ALAW, 2) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "wav_gsm610") == 0)
	{	/* Don't do lcomp_test_XXX as the errors are too big. */
		sdlcomp_test_short	("gsm610.wav", SF_FORMAT_WAV | SF_FORMAT_GSM610, 1, 0.24) ;
		sdlcomp_test_int	("gsm610.wav", SF_FORMAT_WAV | SF_FORMAT_GSM610, 1, 0.24) ;

		sdlcomp_test_short	("gsm610.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_GSM610, 1, 0.24) ;
		sdlcomp_test_int	("gsm610.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_GSM610, 1, 0.24) ;

		/* Lite remove start */
		sdlcomp_test_float	("gsm610.wav", SF_FORMAT_WAV | SF_FORMAT_GSM610, 1, 0.24) ;
		sdlcomp_test_double	("gsm610.wav", SF_FORMAT_WAV | SF_FORMAT_GSM610, 1, 0.24) ;
		/* Lite remove end */
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "aiff_ulaw") == 0)
	{	lcomp_test_short	("ulaw.aiff", SF_FORMAT_AIFF | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_int		("ulaw.aiff", SF_FORMAT_AIFF | SF_FORMAT_ULAW, 2, 0.04) ;
		/* Lite remove start */
		lcomp_test_float	("ulaw.aiff", SF_FORMAT_AIFF | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_double	("ulaw.aiff", SF_FORMAT_AIFF | SF_FORMAT_ULAW, 2, 0.04) ;
		/* Lite remove end */

		read_raw_test ("ulaw.aiff", SF_FORMAT_AIFF | SF_FORMAT_ULAW, 2) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "aiff_alaw") == 0)
	{	lcomp_test_short	("alaw.aiff", SF_FORMAT_AIFF | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_int		("alaw.aiff", SF_FORMAT_AIFF | SF_FORMAT_ALAW, 2, 0.04) ;
		/* Lite remove start */
		lcomp_test_float	("alaw.aiff", SF_FORMAT_AIFF | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_double	("alaw.aiff", SF_FORMAT_AIFF | SF_FORMAT_ALAW, 2, 0.04) ;
		/* Lite remove end */

		read_raw_test ("alaw.aiff", SF_FORMAT_AIFF | SF_FORMAT_ALAW, 2) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "aiff_gsm610") == 0)
	{	/* Don't do lcomp_test_XXX as the errors are too big. */
		sdlcomp_test_short	("gsm610.aiff", SF_FORMAT_AIFF | SF_FORMAT_GSM610, 1, 0.24) ;
		sdlcomp_test_int	("gsm610.aiff", SF_FORMAT_AIFF | SF_FORMAT_GSM610, 1, 0.24) ;
		/* Lite remove start */
		sdlcomp_test_float	("gsm610.aiff", SF_FORMAT_AIFF | SF_FORMAT_GSM610, 1, 0.24) ;
		sdlcomp_test_double	("gsm610.aiff", SF_FORMAT_AIFF | SF_FORMAT_GSM610, 1, 0.24) ;
		/* Lite remove end */
		test_count++ ;
		} ;

	if (strcmp (argv [1], "aiff_ima") == 0)
	{	lcomp_test_short	("ima.aiff", SF_FORMAT_AIFF | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		lcomp_test_int		("ima.aiff", SF_FORMAT_AIFF | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		/* Lite remove start */
		lcomp_test_float	("ima.aiff", SF_FORMAT_AIFF | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		lcomp_test_double	("ima.aiff", SF_FORMAT_AIFF | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		/* Lite remove end */
		} ;

	if (do_all || strcmp (argv [1], "au_ulaw") == 0)
	{	lcomp_test_short	("ulaw.au", SF_ENDIAN_BIG		| SF_FORMAT_AU | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_int		("ulaw.au", SF_ENDIAN_LITTLE	| SF_FORMAT_AU | SF_FORMAT_ULAW, 2, 0.04) ;
		/* Lite remove start */
		lcomp_test_float	("ulaw.au", SF_ENDIAN_LITTLE	| SF_FORMAT_AU | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_double	("ulaw.au", SF_ENDIAN_BIG		| SF_FORMAT_AU | SF_FORMAT_ULAW, 2, 0.04) ;
		/* Lite remove end */
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "au_alaw") == 0)
	{	lcomp_test_short	("alaw.au", SF_ENDIAN_LITTLE	| SF_FORMAT_AU | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_int		("alaw.au", SF_ENDIAN_BIG		| SF_FORMAT_AU | SF_FORMAT_ALAW, 2, 0.04) ;
		/* Lite remove start */
		lcomp_test_float	("alaw.au", SF_ENDIAN_BIG		| SF_FORMAT_AU | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_double	("alaw.au", SF_ENDIAN_LITTLE	| SF_FORMAT_AU | SF_FORMAT_ALAW, 2, 0.04) ;
		/* Lite remove end */
		test_count++ ;
		} ;

	/* Lite remove start */
	if (do_all || strcmp (argv [1], "au_g721") == 0)
	{	printf ("**** Fix this later : error bound should be 0.06 ****\n") ;
		lcomp_test_short	("g721.au", SF_ENDIAN_LITTLE	| SF_FORMAT_AU | SF_FORMAT_G721_32, 1, 0.7) ;
		lcomp_test_int		("g721.au", SF_ENDIAN_BIG		| SF_FORMAT_AU | SF_FORMAT_G721_32, 1, 0.7) ;
		lcomp_test_float	("g721.au", SF_ENDIAN_LITTLE 	| SF_FORMAT_AU | SF_FORMAT_G721_32, 1, 0.7) ;
		lcomp_test_double	("g721.au", SF_ENDIAN_BIG		| SF_FORMAT_AU | SF_FORMAT_G721_32, 1, 0.7) ;

/*-		sdlcomp_test_short	("g721.au", SF_ENDIAN_BIG    | SF_FORMAT_AU | SF_FORMAT_G721_32, 1, 0.07) ;
		sdlcomp_test_int	("g721.au", SF_ENDIAN_LITTLE | SF_FORMAT_AU | SF_FORMAT_G721_32, 1, 0.07) ;
		sdlcomp_test_float  ("g721.au", SF_ENDIAN_BIG    | SF_FORMAT_AU | SF_FORMAT_G721_32, 1, 0.07) ;
		sdlcomp_test_double	("g721.au", SF_ENDIAN_LITTLE | SF_FORMAT_AU | SF_FORMAT_G721_32, 1, 0.12) ;
-*/
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "au_g723") == 0)
	{	printf ("**** Fix this later : error bound should be 0.16 ****\n") ;
		lcomp_test_short	("g723_24.au", SF_ENDIAN_LITTLE	| SF_FORMAT_AU | SF_FORMAT_G723_24, 1, 0.7) ;
		lcomp_test_int		("g723_24.au", SF_ENDIAN_BIG	| SF_FORMAT_AU | SF_FORMAT_G723_24, 1, 0.7) ;
		lcomp_test_float	("g723_24.au", SF_ENDIAN_LITTLE | SF_FORMAT_AU | SF_FORMAT_G723_24, 1, 0.7) ;
		lcomp_test_double	("g723_24.au", SF_ENDIAN_BIG	| SF_FORMAT_AU | SF_FORMAT_G723_24, 1, 0.7) ;

		lcomp_test_short	("g723_40.au", SF_ENDIAN_LITTLE | SF_FORMAT_AU | SF_FORMAT_G723_40, 1, 0.85) ;
		lcomp_test_int		("g723_40.au", SF_ENDIAN_BIG	| SF_FORMAT_AU | SF_FORMAT_G723_40, 1, 0.84) ;
		lcomp_test_float	("g723_40.au", SF_ENDIAN_LITTLE | SF_FORMAT_AU | SF_FORMAT_G723_40, 1, 0.86) ;
		lcomp_test_double	("g723_40.au", SF_ENDIAN_BIG	| SF_FORMAT_AU | SF_FORMAT_G723_40, 1, 0.86) ;

/*-		sdlcomp_test_short	("g723.au", SF_ENDIAN_BIG    | SF_FORMAT_AU | SF_FORMAT_G723_24, 1, 0.15) ;
		sdlcomp_test_int	("g723.au", SF_ENDIAN_LITTLE | SF_FORMAT_AU | SF_FORMAT_G723_24, 1, 0.15) ;
		sdlcomp_test_float	("g723.au", SF_ENDIAN_BIG    | SF_FORMAT_AU | SF_FORMAT_G723_24, 1, 0.15) ;
		sdlcomp_test_double	("g723.au", SF_ENDIAN_LITTLE | SF_FORMAT_AU | SF_FORMAT_G723_24, 1, 0.15) ;
-*/
		test_count++ ;
		} ;
	/* Lite remove end */

	if (do_all || strcmp (argv [1], "caf_ulaw") == 0)
	{	lcomp_test_short	("ulaw.caf", SF_FORMAT_CAF | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_int		("ulaw.caf", SF_FORMAT_CAF | SF_FORMAT_ULAW, 2, 0.04) ;
		/* Lite remove start */
		lcomp_test_float	("ulaw.caf", SF_FORMAT_CAF | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_double	("ulaw.caf", SF_FORMAT_CAF | SF_FORMAT_ULAW, 2, 0.04) ;
		/* Lite remove end */

		read_raw_test ("ulaw.caf", SF_FORMAT_CAF | SF_FORMAT_ULAW, 2) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "caf_alaw") == 0)
	{	lcomp_test_short	("alaw.caf", SF_FORMAT_CAF | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_int		("alaw.caf", SF_FORMAT_CAF | SF_FORMAT_ALAW, 2, 0.04) ;
		/* Lite remove start */
		lcomp_test_float	("alaw.caf", SF_FORMAT_CAF | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_double	("alaw.caf", SF_FORMAT_CAF | SF_FORMAT_ALAW, 2, 0.04) ;
		/* Lite remove end */

		read_raw_test ("alaw.caf", SF_FORMAT_CAF | SF_FORMAT_ALAW, 2) ;
		test_count++ ;
		} ;


	if (do_all || strcmp (argv [1], "raw_ulaw") == 0)
	{	lcomp_test_short	("ulaw.raw", SF_ENDIAN_LITTLE	| SF_FORMAT_RAW | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_int		("ulaw.raw", SF_ENDIAN_BIG		| SF_FORMAT_RAW | SF_FORMAT_ULAW, 2, 0.04) ;
		/* Lite remove start */
		lcomp_test_float	("ulaw.raw", SF_ENDIAN_LITTLE	| SF_FORMAT_RAW | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_double	("ulaw.raw", SF_ENDIAN_BIG		| SF_FORMAT_RAW | SF_FORMAT_ULAW, 2, 0.04) ;
		/* Lite remove end */
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "raw_alaw") == 0)
	{	lcomp_test_short	("alaw.raw", SF_ENDIAN_LITTLE	| SF_FORMAT_RAW | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_int		("alaw.raw", SF_ENDIAN_BIG		| SF_FORMAT_RAW | SF_FORMAT_ALAW, 2, 0.04) ;
		/* Lite remove start */
		lcomp_test_float	("alaw.raw", SF_ENDIAN_LITTLE	| SF_FORMAT_RAW | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_double	("alaw.raw", SF_ENDIAN_BIG		| SF_FORMAT_RAW | SF_FORMAT_ALAW, 2, 0.04) ;
		/* Lite remove end */
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "raw_gsm610") == 0)
	{	/* Don't do lcomp_test_XXX as the errors are too big. */
		sdlcomp_test_short	("raw.gsm", SF_FORMAT_RAW | SF_FORMAT_GSM610, 1, 0.24) ;
		sdlcomp_test_int	("raw.gsm", SF_FORMAT_RAW | SF_FORMAT_GSM610, 1, 0.24) ;
		sdlcomp_test_float	("raw.gsm", SF_FORMAT_RAW | SF_FORMAT_GSM610, 1, 0.24) ;
		sdlcomp_test_double	("raw.gsm", SF_FORMAT_RAW | SF_FORMAT_GSM610, 1, 0.24) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "ogg_vorbis") == 0)
	{	if (HAVE_EXTERNAL_LIBS)
		{	/* Don't do lcomp_test_XXX as the errors are too big. */
			sdlcomp_test_short	("vorbis.oga", SF_FORMAT_OGG | SF_FORMAT_VORBIS, 1, 0.30) ;
			sdlcomp_test_int	("vorbis.oga", SF_FORMAT_OGG | SF_FORMAT_VORBIS, 1, 0.30) ;
			sdlcomp_test_float	("vorbis.oga", SF_FORMAT_OGG | SF_FORMAT_VORBIS, 1, 0.30) ;
			sdlcomp_test_double	("vorbis.oga", SF_FORMAT_OGG | SF_FORMAT_VORBIS, 1, 0.30) ;
			}
		else
			puts ("    No Ogg/Vorbis tests because Ogg/Vorbis support was not compiled in.") ;

		test_count++ ;
		} ;

	/* Lite remove start */
	if (do_all || strcmp (argv [1], "ircam_ulaw") == 0)
	{	lcomp_test_short	("ulaw.ircam", SF_ENDIAN_LITTLE | SF_FORMAT_IRCAM | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_int		("ulaw.ircam", SF_ENDIAN_BIG	| SF_FORMAT_IRCAM | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_float	("ulaw.ircam", SF_ENDIAN_LITTLE | SF_FORMAT_IRCAM | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_double	("ulaw.ircam", SF_ENDIAN_BIG	| SF_FORMAT_IRCAM | SF_FORMAT_ULAW, 2, 0.04) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "ircam_alaw") == 0)
	{	lcomp_test_short	("alaw.ircam", SF_ENDIAN_LITTLE | SF_FORMAT_IRCAM | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_int		("alaw.ircam", SF_ENDIAN_BIG	| SF_FORMAT_IRCAM | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_float	("alaw.ircam", SF_ENDIAN_LITTLE | SF_FORMAT_IRCAM | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_double	("alaw.ircam", SF_ENDIAN_BIG	| SF_FORMAT_IRCAM | SF_FORMAT_ALAW, 2, 0.04) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "nist_ulaw") == 0)
	{	lcomp_test_short	("ulaw.nist", SF_ENDIAN_LITTLE	| SF_FORMAT_NIST | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_int		("ulaw.nist", SF_ENDIAN_BIG		| SF_FORMAT_NIST | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_float	("ulaw.nist", SF_ENDIAN_LITTLE	| SF_FORMAT_NIST | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_double	("ulaw.nist", SF_ENDIAN_BIG		| SF_FORMAT_NIST | SF_FORMAT_ULAW, 2, 0.04) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "nist_alaw") == 0)
	{	lcomp_test_short	("alaw.nist", SF_ENDIAN_LITTLE	| SF_FORMAT_NIST | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_int		("alaw.nist", SF_ENDIAN_BIG		| SF_FORMAT_NIST | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_float	("alaw.nist", SF_ENDIAN_LITTLE	| SF_FORMAT_NIST | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_double	("alaw.nist", SF_ENDIAN_BIG		| SF_FORMAT_NIST | SF_FORMAT_ALAW, 2, 0.04) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "voc_ulaw") == 0)
	{	lcomp_test_short	("ulaw.voc", SF_FORMAT_VOC | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_int		("ulaw.voc", SF_FORMAT_VOC | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_float	("ulaw.voc", SF_FORMAT_VOC | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_double	("ulaw.voc", SF_FORMAT_VOC | SF_FORMAT_ULAW, 2, 0.04) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "voc_alaw") == 0)
	{	lcomp_test_short	("alaw.voc", SF_FORMAT_VOC | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_int		("alaw.voc", SF_FORMAT_VOC | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_float	("alaw.voc", SF_FORMAT_VOC | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_double	("alaw.voc", SF_FORMAT_VOC | SF_FORMAT_ALAW, 2, 0.04) ;
		test_count++ ;
		} ;
	/* Lite remove end */

	if (do_all || strcmp (argv [1], "w64_ulaw") == 0)
	{	lcomp_test_short	("ulaw.w64", SF_FORMAT_W64 | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_int		("ulaw.w64", SF_FORMAT_W64 | SF_FORMAT_ULAW, 2, 0.04) ;
		/* Lite remove start */
		lcomp_test_float	("ulaw.w64", SF_FORMAT_W64 | SF_FORMAT_ULAW, 2, 0.04) ;
		lcomp_test_double	("ulaw.w64", SF_FORMAT_W64 | SF_FORMAT_ULAW, 2, 0.04) ;
		/* Lite remove end */

		read_raw_test ("ulaw.w64", SF_FORMAT_W64 | SF_FORMAT_ULAW, 2) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "w64_alaw") == 0)
	{	lcomp_test_short	("alaw.w64", SF_FORMAT_W64 | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_int		("alaw.w64", SF_FORMAT_W64 | SF_FORMAT_ALAW, 2, 0.04) ;
		/* Lite remove start */
		lcomp_test_float	("alaw.w64", SF_FORMAT_W64 | SF_FORMAT_ALAW, 2, 0.04) ;
		lcomp_test_double	("alaw.w64", SF_FORMAT_W64 | SF_FORMAT_ALAW, 2, 0.04) ;
		/* Lite remove end */

		read_raw_test ("alaw.w64", SF_FORMAT_W64 | SF_FORMAT_ALAW, 2) ;
		test_count++ ;
		} ;

	/* Lite remove start */
	if (do_all || strcmp (argv [1], "w64_ima") == 0)
	{	lcomp_test_short	("ima.w64", SF_FORMAT_W64 | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		lcomp_test_int		("ima.w64", SF_FORMAT_W64 | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		lcomp_test_float	("ima.w64", SF_FORMAT_W64 | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		lcomp_test_double	("ima.w64", SF_FORMAT_W64 | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;

		sdlcomp_test_short	("ima.w64", SF_FORMAT_W64 | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		sdlcomp_test_int	("ima.w64", SF_FORMAT_W64 | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		sdlcomp_test_float	("ima.w64", SF_FORMAT_W64 | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		sdlcomp_test_double	("ima.w64", SF_FORMAT_W64 | SF_FORMAT_IMA_ADPCM, 2, 0.18) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "w64_msadpcm") == 0)
	{	lcomp_test_short	("msadpcm.w64", SF_FORMAT_W64 | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		lcomp_test_int		("msadpcm.w64", SF_FORMAT_W64 | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		lcomp_test_float	("msadpcm.w64", SF_FORMAT_W64 | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		lcomp_test_double	("msadpcm.w64", SF_FORMAT_W64 | SF_FORMAT_MS_ADPCM, 2, 0.36) ;

		sdlcomp_test_short	("msadpcm.w64", SF_FORMAT_W64 | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		sdlcomp_test_int	("msadpcm.w64", SF_FORMAT_W64 | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		sdlcomp_test_float	("msadpcm.w64", SF_FORMAT_W64 | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		sdlcomp_test_double	("msadpcm.w64", SF_FORMAT_W64 | SF_FORMAT_MS_ADPCM, 2, 0.36) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "wve") == 0)
	{	lcomp_test_short	("psion.wve", SF_FORMAT_WVE | SF_FORMAT_ALAW, 1, 0.04) ;
		lcomp_test_int		("psion.wve", SF_FORMAT_WVE | SF_FORMAT_ALAW, 1, 0.04) ;
		/* Lite remove start */
		lcomp_test_float	("psion.wve", SF_FORMAT_WVE | SF_FORMAT_ALAW, 1, 0.04) ;
		lcomp_test_double	("psion.wve", SF_FORMAT_WVE | SF_FORMAT_ALAW, 1, 0.04) ;
		/* Lite remove end */
		test_count++ ;
		} ;

	/* Lite remove end */

	if (do_all || strcmp (argv [1], "w64_gsm610") == 0)
	{	/* Don't do lcomp_test_XXX as the errors are too big. */
		sdlcomp_test_short	("gsm610.w64", SF_FORMAT_W64 | SF_FORMAT_GSM610, 1, 0.2) ;
		sdlcomp_test_int	("gsm610.w64", SF_FORMAT_W64 | SF_FORMAT_GSM610, 1, 0.2) ;
		/* Lite remove start */
		sdlcomp_test_float	("gsm610.w64", SF_FORMAT_W64 | SF_FORMAT_GSM610, 1, 0.2) ;
		sdlcomp_test_double	("gsm610.w64", SF_FORMAT_W64 | SF_FORMAT_GSM610, 1, 0.2) ;
		/* Lite remove end */
		test_count++ ;
		} ;

	/* Lite remove start */
	if (do_all || strcmp (argv [1], "vox_adpcm") == 0)
	{	lcomp_test_short	("adpcm.vox", SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM, 1, 0.17) ;
		lcomp_test_int		("adpcm.vox", SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM, 1, 0.17) ;
		lcomp_test_float	("adpcm.vox", SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM, 1, 0.17) ;
		lcomp_test_double	("adpcm.vox", SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM, 1, 0.17) ;

		sdlcomp_test_short	("adpcm.vox", SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM, 1, 0.072) ;
		sdlcomp_test_int	("adpcm.vox", SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM, 1, 0.072) ;
		sdlcomp_test_float	("adpcm.vox", SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM, 1, 0.072) ;
		sdlcomp_test_double	("adpcm.vox", SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM, 1, 0.072) ;
		test_count++ ;
		} ;

	if (do_all || strcmp (argv [1], "xi_dpcm") == 0)
	{	lcomp_test_short	("8bit.xi", SF_FORMAT_XI | SF_FORMAT_DPCM_8, 1, 0.25) ;
		lcomp_test_int		("8bit.xi", SF_FORMAT_XI | SF_FORMAT_DPCM_8, 1, 0.25) ;

		lcomp_test_short	("16bit.xi", SF_FORMAT_XI | SF_FORMAT_DPCM_16, 1, 0.002) ;
		lcomp_test_int		("16bit.xi", SF_FORMAT_XI | SF_FORMAT_DPCM_16, 1, 0.002) ;
		lcomp_test_float	("16bit.xi", SF_FORMAT_XI | SF_FORMAT_DPCM_16, 1, 0.002) ;
		lcomp_test_double	("16bit.xi", SF_FORMAT_XI | SF_FORMAT_DPCM_16, 1, 0.002) ;
		test_count++ ;
		} ;
	/* Lite remove end */

	if (test_count == 0)
	{	printf ("************************************\n") ;
		printf ("*  No '%s' test defined.\n", argv [1]) ;
		printf ("************************************\n") ;
		return 1 ;
		} ;

	return 0 ;
} /* main */

/*============================================================================================
**	Here are the test functions.
*/

static void
lcomp_test_short (const char *filename, int filetype, int channels, double margin)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	int				k, m, seekpos, half_max_abs ;
	long			datalen ;
	short			*orig, *data ;

	print_test_name ("lcomp_test_short", filename) ;

	datalen = BUFFER_SIZE / channels ;

	data = data_buffer.s ;
	orig = orig_buffer.s ;

	gen_signal_double (orig_buffer.d, 32000.0, channels, datalen) ;
	for (k = 0 ; k < channels * datalen ; k++)
		orig [k] = (short) (orig_buffer.d [k]) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= 123456789 ;	/* Ridiculous value. */
	sfinfo.channels		= channels ;
	sfinfo.format		= filetype ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	test_writef_short_or_die (file, 0, orig, datalen, __LINE__) ;
	sf_set_string (file, SF_STR_COMMENT, long_comment) ;
	sf_close (file) ;

	memset (data, 0, datalen * sizeof (short)) ;

	if ((filetype & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW)
		memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	if ((sfinfo.format & (SF_FORMAT_TYPEMASK | SF_FORMAT_SUBMASK)) != (filetype & (SF_FORMAT_TYPEMASK | SF_FORMAT_SUBMASK)))
	{	printf ("\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < datalen / channels)
	{	printf ("Too few frames in file. (%ld should be a little more than %ld)\n", SF_COUNT_TO_LONG (sfinfo.frames), datalen) ;
		exit (1) ;
		} ;

	if (sfinfo.frames > (datalen + datalen / 20))
	{	printf ("Too many frames in file. (%ld should be a little more than %ld)\n", SF_COUNT_TO_LONG (sfinfo.frames), datalen) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != channels)
	{	printf ("Incorrect number of channels in file.\n") ;
		exit (1) ;
		} ;

	check_log_buffer_or_die (file, __LINE__) ;

	check_comment (file, filetype, __LINE__) ;

	test_readf_short_or_die (file, 0, data, datalen, __LINE__) ;

	half_max_abs = 0 ;
	for (k = 0 ; k < datalen ; k++)
	{	if (error_function (data [k], orig [k], margin))
		{	printf ("\n\nLine %d: Incorrect sample A (#%d : %d should be %d).\n", __LINE__, k, data [k], orig [k]) ;
			oct_save_short (orig, data, datalen) ;
			exit (1) ;
			} ;
		half_max_abs = LCT_MAX (half_max_abs, abs (data [k] / 2)) ;
		} ;

	if (half_max_abs < 1.0)
	{	printf ("\n\nLine %d: Signal is all zeros.\n", __LINE__) ;
		exit (1) ;
		} ;

	if ((k = sf_readf_short (file, data, datalen)) != sfinfo.frames - datalen)
	{	printf ("\n\nLine %d: Incorrect read length (%ld should be %d).\n", __LINE__,
			SF_COUNT_TO_LONG (channels * sfinfo.frames - datalen), k) ;
		exit (1) ;
		} ;

	/*	This check is only for block based encoders which must append silence
	**	to the end of a file so as to fill out a block.
	*/
	for (k = 0 ; k < sfinfo.frames - datalen ; k++)
		if (abs (data [channels * k]) > decay_response (channels * k))
		{	printf ("\n\nLine %d : Incorrect sample B (#%d : abs (%d) should be < %d).\n", __LINE__, channels * k, data [channels * k], decay_response (channels * k)) ;
			exit (1) ;
			} ;

	if (! sfinfo.seekable)
	{	sf_close (file) ;
		unlink (filename) ;
		printf ("ok\n") ;
		return ;
		} ;

	/* Now test sf_seek function. */

	if ((k = sf_seek (file, 0, SEEK_SET)) != 0)
	{	printf ("\n\nLine %d: Seek to start of file failed (%d).\n", __LINE__, k) ;
		exit (1) ;
		} ;

	for (m = 0 ; m < 3 ; m++)
	{	test_readf_short_or_die (file, m, data, 11, __LINE__) ;

		for (k = 0 ; k < channels * 11 ; k++)
			if (error_function (1.0 * data [k], 1.0 * orig [k + channels * m * 11], margin))
			{	printf ("\n\nLine %d: Incorrect sample (m = %d) (#%d : %d => %d).\n", __LINE__, m, k + channels * m * 11, orig [k + channels * m * 11], data [k]) ;
				for (m = 0 ; m < channels ; m++)
					printf ("%d ", data [m]) ;
				printf ("\n") ;
				exit (1) ;
				} ;
		} ;

	seekpos = BUFFER_SIZE / 10 ;

	/* Check seek from start of file. */
	if ((k = sf_seek (file, seekpos, SEEK_SET)) != seekpos)
	{	printf ("Seek to start of file + %d failed (%d).\n", seekpos, k) ;
		exit (1) ;
		} ;

	test_readf_short_or_die (file, 0, data, 1, __LINE__) ;

	if (error_function (1.0 * data [0], 1.0 * orig [seekpos * channels], margin))
	{	printf ("\n\nLine %d: sf_seek (SEEK_SET) followed by sf_readf_short failed (%d, %d).\n", __LINE__, orig [1], data [0]) ;
		exit (1) ;
		} ;

	if ((k = sf_seek (file, 0, SEEK_CUR)) != seekpos + 1)
	{	printf ("\n\nLine %d: sf_seek (SEEK_CUR) with 0 offset failed (%d should be %d)\n", __LINE__, k, seekpos + 1) ;
		exit (1) ;
		} ;

	seekpos = sf_seek (file, 0, SEEK_CUR) + BUFFER_SIZE / 5 ;
	k = sf_seek (file, BUFFER_SIZE / 5, SEEK_CUR) ;
	test_readf_short_or_die (file, 0, data, 1, __LINE__) ;
	if (error_function (1.0 * data [0], 1.0 * orig [seekpos * channels], margin) || k != seekpos)
	{	printf ("\n\nLine %d: sf_seek (forwards, SEEK_CUR) followed by sf_readf_short failed (%d, %d) (%d, %d).\n", __LINE__, data [0], orig [seekpos * channels], k, seekpos + 1) ;
		oct_save_short (orig, data, datalen) ;
		exit (1) ;
		} ;

	seekpos = sf_seek (file, 0, SEEK_CUR) - 20 ;
	/* Check seek backward from current position. */
	k = sf_seek (file, -20, SEEK_CUR) ;
	test_readf_short_or_die (file, 0, data, 1, __LINE__) ;
	if (error_function (1.0 * data [0], 1.0 * orig [seekpos * channels], margin) || k != seekpos)
	{	printf ("\nLine %d: sf_seek (backwards, SEEK_CUR) followed by sf_readf_short failed (%d, %d) (%d, %d).\n", __LINE__, data [0], orig [seekpos * channels], k, seekpos) ;
		exit (1) ;
		} ;

	/* Check that read past end of file returns number of items. */
	sf_seek (file, sfinfo.frames, SEEK_SET) ;

 	if ((k = sf_readf_short (file, data, datalen)) != 0)
 	{	printf ("\n\nLine %d: Return value from sf_readf_short past end of file incorrect (%d).\n", __LINE__, k) ;
 		exit (1) ;
 		} ;

	/* Check seek backward from end. */
	if ((k = sf_seek (file, 5 - sfinfo.frames, SEEK_END)) != 5)
	{	printf ("\n\nLine %d: sf_seek (SEEK_END) returned %d instead of %d.\n", __LINE__, k, 5) ;
		exit (1) ;
		} ;

	test_readf_short_or_die (file, 0, data, 1, __LINE__) ;
	if (error_function (1.0 * data [0], 1.0 * orig [5 * channels], margin))
	{	printf ("\nLine %d: sf_seek (SEEK_END) followed by sf_readf_short failed (%d should be %d).\n", __LINE__, data [0], orig [5 * channels]) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	unlink (filename) ;
	printf ("ok\n") ;
} /* lcomp_test_short */

/*--------------------------------------------------------------------------------------------
*/

static void
lcomp_test_int (const char *filename, int filetype, int channels, double margin)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	int				k, m, half_max_abs ;
	long			datalen, seekpos ;
	double			scale, max_val ;
	int				*orig, *data ;

	print_test_name ("lcomp_test_int", filename) ;

	datalen = BUFFER_SIZE / channels ;

	if (is_lossy (filetype))
	{	scale = 1.0 * 0x10000 ;
		max_val = 32000.0 * scale ;
		}
	else
	{	scale = 1.0 ;
		max_val = 0x7fffffff * scale ;
		} ;

	data = data_buffer.i ;
	orig = orig_buffer.i ;

	gen_signal_double (orig_buffer.d, max_val, channels, datalen) ;

	for (k = 0 ; k < channels * datalen ; k++)
		orig [k] = lrint (orig_buffer.d [k]) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= 123456789 ;	/* Ridiculous value. */
	sfinfo.channels		= channels ;
	sfinfo.format		= filetype ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	test_writef_int_or_die (file, 0, orig, datalen, __LINE__) ;
	sf_set_string (file, SF_STR_COMMENT, long_comment) ;
	sf_close (file) ;

	memset (data, 0, datalen * sizeof (int)) ;

	if ((filetype & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW)
		memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	if ((sfinfo.format & (SF_FORMAT_TYPEMASK | SF_FORMAT_SUBMASK)) != (filetype & (SF_FORMAT_TYPEMASK | SF_FORMAT_SUBMASK)))
	{	printf ("\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < datalen / channels)
	{	printf ("Too few.frames in file. (%ld should be a little more than %ld)\n", datalen, SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	if (sfinfo.frames > (datalen + datalen / 20))
	{	printf ("Too many.frames in file. (%ld should be a little more than %ld)\n", datalen, SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != channels)
	{	printf ("Incorrect number of channels in file.\n") ;
		exit (1) ;
		} ;

	check_log_buffer_or_die (file, __LINE__) ;

	check_comment (file, filetype, __LINE__) ;

	test_readf_int_or_die (file, 0, data, datalen, __LINE__) ;

	half_max_abs = 0 ;
	for (k = 0 ; k < datalen ; k++)
	{	if (error_function (data [k] / scale, orig [k] / scale, margin))
		{	printf ("\nLine %d: Incorrect sample (#%d : %f should be %f).\n", __LINE__, k, data [k] / scale, orig [k] / scale) ;
			oct_save_int (orig, data, datalen) ;
			exit (1) ;
			} ;
		half_max_abs = LCT_MAX (half_max_abs, abs (data [k] / 2)) ;
		} ;

	if (half_max_abs < 1.0)
	{	printf ("\n\nLine %d: Signal is all zeros (%d, 0x%X).\n", __LINE__, half_max_abs, half_max_abs) ;
		exit (1) ;
		} ;

	if ((k = sf_readf_int (file, data, datalen)) != sfinfo.frames - datalen)
	{	printf ("\n\nLine %d: Incorrect read length (%ld should be %d).\n", __LINE__,
			SF_COUNT_TO_LONG (channels * sfinfo.frames - datalen), k) ;
		exit (1) ;
		} ;

	/*	This check is only for block based encoders which must append silence
	**	to the end of a file so as to fill out a block.
	*/
	if ((sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_MS_ADPCM)
		for (k = 0 ; k < sfinfo.frames - datalen ; k++)
			if (abs (data [channels * k] / scale) > decay_response (channels * k))
			{	printf ("\n\nLine %d : Incorrect sample B (#%d : abs (%d) should be < %d).\n", __LINE__, channels * k, data [channels * k], decay_response (channels * k)) ;
				exit (1) ;
				} ;

	if (! sfinfo.seekable)
	{	sf_close (file) ;
		unlink (filename) ;
		printf ("ok\n") ;
		return ;
		} ;

	/* Now test sf_seek function. */

	if ((k = sf_seek (file, 0, SEEK_SET)) != 0)
	{	printf ("\n\nLine %d: Seek to start of file failed (%d).\n", __LINE__, k) ;
		exit (1) ;
		} ;

	for (m = 0 ; m < 3 ; m++)
	{	test_readf_int_or_die (file, m, data, 11, __LINE__) ;

		for (k = 0 ; k < channels * 11 ; k++)
			if (error_function (data [k] / scale, orig [k + channels * m * 11] / scale, margin))
			{	printf ("\nLine %d: Incorrect sample (m = %d) (#%d : %d => %d).\n", __LINE__, m, k + channels * m * 11, orig [k + channels * m * 11], data [k]) ;
				for (m = 0 ; m < channels ; m++)
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

	test_readf_int_or_die (file, 0, data, 1, __LINE__) ;

	if (error_function (1.0 * data [0], 1.0 * orig [seekpos * channels], margin))
	{	printf ("\nLine %d: sf_seek (SEEK_SET) followed by sf_readf_int failed (%d, %d).\n", __LINE__, orig [1], data [0]) ;
		exit (1) ;
		} ;

	if ((k = sf_seek (file, 0, SEEK_CUR)) != seekpos + 1)
	{	printf ("\n\nLine %d: sf_seek (SEEK_CUR) with 0 offset failed (%d should be %ld)\n", __LINE__, k, seekpos + 1) ;
		exit (1) ;
		} ;

	seekpos = sf_seek (file, 0, SEEK_CUR) + BUFFER_SIZE / 5 ;
	k = sf_seek (file, BUFFER_SIZE / 5, SEEK_CUR) ;
	test_readf_int_or_die (file, 0, data, 1, __LINE__) ;
	if (error_function (1.0 * data [0], 1.0 * orig [seekpos * channels], margin) || k != seekpos)
	{	printf ("\nLine %d: sf_seek (forwards, SEEK_CUR) followed by sf_readf_int failed (%d, %d) (%d, %ld).\n", __LINE__, data [0], orig [seekpos * channels], k, seekpos + 1) ;
		exit (1) ;
		} ;

	seekpos = sf_seek (file, 0, SEEK_CUR) - 20 ;
	/* Check seek backward from current position. */
	k = sf_seek (file, -20, SEEK_CUR) ;
	test_readf_int_or_die (file, 0, data, 1, __LINE__) ;
	if (error_function (1.0 * data [0], 1.0 * orig [seekpos * channels], margin) || k != seekpos)
	{	printf ("\nLine %d: sf_seek (backwards, SEEK_CUR) followed by sf_readf_int failed (%d, %d) (%d, %ld).\n", __LINE__, data [0], orig [seekpos * channels], k, seekpos) ;
		exit (1) ;
		} ;

	/* Check that read past end of file returns number of items. */
	sf_seek (file, sfinfo.frames, SEEK_SET) ;

 	if ((k = sf_readf_int (file, data, datalen)) != 0)
 	{	printf ("\n\nLine %d: Return value from sf_readf_int past end of file incorrect (%d).\n", __LINE__, k) ;
 		exit (1) ;
 		} ;

	/* Check seek backward from end. */
	if ((k = sf_seek (file, 5 - sfinfo.frames, SEEK_END)) != 5)
	{	printf ("\n\nLine %d: sf_seek (SEEK_END) returned %d instead of %d.\n", __LINE__, k, 5) ;
		exit (1) ;
		} ;

	test_readf_int_or_die (file, 0, data, 1, __LINE__) ;
	if (error_function (data [0] / scale, orig [5 * channels] / scale, margin))
	{	printf ("\nLine %d: sf_seek (SEEK_END) followed by sf_readf_short failed (%d should be %d).\n", __LINE__, data [0], orig [5]) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	unlink (filename) ;
	printf ("ok\n") ;
} /* lcomp_test_int */

/*--------------------------------------------------------------------------------------------
*/

static void
lcomp_test_float (const char *filename, int filetype, int channels, double margin)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	int				k, m, seekpos ;
	long			datalen ;
	float			*orig, *data ;
	double			half_max_abs ;

	print_test_name ("lcomp_test_float", filename) ;

	datalen = BUFFER_SIZE / channels ;

	data = data_buffer.f ;
	orig = orig_buffer.f ;

	gen_signal_double (orig_buffer.d, 32000.0, channels, datalen) ;
	for (k = 0 ; k < channels * datalen ; k++)
		orig [k] = orig_buffer.d [k] ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= 123456789 ;	/* Ridiculous value. */
	sfinfo.channels		= channels ;
	sfinfo.format		= filetype ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	sf_command (file, SFC_SET_NORM_FLOAT, NULL, SF_FALSE) ;
	test_writef_float_or_die (file, 0, orig, datalen, __LINE__) ;
	sf_set_string (file, SF_STR_COMMENT, long_comment) ;
	sf_close (file) ;

	memset (data, 0, datalen * sizeof (float)) ;

	if ((filetype & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW)
		memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	if ((sfinfo.format & (SF_FORMAT_TYPEMASK | SF_FORMAT_SUBMASK)) != (filetype & (SF_FORMAT_TYPEMASK | SF_FORMAT_SUBMASK)))
	{	printf ("\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < datalen / channels)
	{	printf ("Too few.frames in file. (%ld should be a little more than %ld)\n", datalen, SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	if (sfinfo.frames > (datalen + datalen / 20))
	{	printf ("Too many.frames in file. (%ld should be a little more than %ld)\n", datalen, SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != channels)
	{	printf ("Incorrect number of channels in file.\n") ;
		exit (1) ;
		} ;

	check_comment (file, filetype, __LINE__) ;

	sf_command (file, SFC_SET_NORM_FLOAT, NULL, SF_FALSE) ;

	check_log_buffer_or_die (file, __LINE__) ;

	check_comment (file, filetype, __LINE__) ;

	sf_command (file, SFC_SET_NORM_FLOAT, NULL, SF_FALSE) ;

	test_readf_float_or_die (file, 0, data, datalen, __LINE__) ;

	half_max_abs = 0.0 ;
	for (k = 0 ; k < datalen ; k++)
	{	if (error_function (data [k], orig [k], margin))
		{	printf ("\nLine %d: Incorrect sample A (#%d : %f should be %f).\n", __LINE__, k, data [k], orig [k]) ;
			oct_save_float (orig, data, datalen) ;
			exit (1) ;
			} ;
		half_max_abs = LCT_MAX (half_max_abs, fabs (0.5 * data [k])) ;
		} ;

	if (half_max_abs < 1.0)
	{	printf ("\n\nLine %d: Signal is all zeros.\n", __LINE__) ;
		exit (1) ;
		} ;

	if ((k = sf_readf_float (file, data, datalen)) != sfinfo.frames - datalen)
	{	printf ("\n\nLine %d: Incorrect read length (%ld should be %d).\n", __LINE__,
			SF_COUNT_TO_LONG (channels * sfinfo.frames - datalen), k) ;
		exit (1) ;
		} ;

	/*	This check is only for block based encoders which must append silence
	**	to the end of a file so as to fill out a block.
	*/
	if ((sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_MS_ADPCM)
		for (k = 0 ; k < sfinfo.frames - datalen ; k++)
			if (abs (data [channels * k]) > decay_response (channels * k))
			{	printf ("\n\nLine %d : Incorrect sample B (#%d : abs (%f) should be < %d).\n", __LINE__, channels * k, data [channels * k], decay_response (channels * k)) ;
				exit (1) ;
				} ;

	if (! sfinfo.seekable)
	{	sf_close (file) ;
		unlink (filename) ;
		printf ("ok\n") ;
		return ;
		} ;

	/* Now test sf_seek function. */

	if ((k = sf_seek (file, 0, SEEK_SET)) != 0)
	{	printf ("\n\nLine %d: Seek to start of file failed (%d).\n", __LINE__, k) ;
		exit (1) ;
		} ;

	for (m = 0 ; m < 3 ; m++)
	{	test_readf_float_or_die (file, 0, data, 11, __LINE__) ;

		for (k = 0 ; k < channels * 11 ; k++)
			if (error_function (data [k], orig [k + channels * m * 11], margin))
			{	printf ("\nLine %d: Incorrect sample (m = %d) (#%d : %f => %f).\n", __LINE__, m, k + channels * m * 11, orig [k + channels * m * 11], data [k]) ;
				for (m = 0 ; m < channels ; m++)
					printf ("%f ", data [m]) ;
				printf ("\n") ;
				exit (1) ;
				} ;
		} ;

	seekpos = BUFFER_SIZE / 10 ;

	/* Check seek from start of file. */
	if ((k = sf_seek (file, seekpos, SEEK_SET)) != seekpos)
	{	printf ("Seek to start of file + %d failed (%d).\n", seekpos, k) ;
		exit (1) ;
		} ;

	test_readf_float_or_die (file, 0, data, 1, __LINE__) ;

	if (error_function (data [0], orig [seekpos * channels], margin))
	{	printf ("\nLine %d: sf_seek (SEEK_SET) followed by sf_readf_float failed (%f, %f).\n", __LINE__, orig [1], data [0]) ;
		exit (1) ;
		} ;

	if ((k = sf_seek (file, 0, SEEK_CUR)) != seekpos + 1)
	{	printf ("\n\nLine %d: sf_seek (SEEK_CUR) with 0 offset failed (%d should be %d)\n", __LINE__, k, seekpos + 1) ;
		exit (1) ;
		} ;

	seekpos = sf_seek (file, 0, SEEK_CUR) + BUFFER_SIZE / 5 ;
	k = sf_seek (file, BUFFER_SIZE / 5, SEEK_CUR) ;
	test_readf_float_or_die (file, 0, data, 1, __LINE__) ;
	if (error_function (data [0], orig [seekpos * channels], margin) || k != seekpos)
	{	printf ("\nLine %d: sf_seek (forwards, SEEK_CUR) followed by sf_readf_float failed (%f, %f) (%d, %d).\n", __LINE__, data [0], orig [seekpos * channels], k, seekpos + 1) ;
		exit (1) ;
		} ;

	seekpos = sf_seek (file, 0, SEEK_CUR) - 20 ;
	/* Check seek backward from current position. */
	k = sf_seek (file, -20, SEEK_CUR) ;
	test_readf_float_or_die (file, 0, data, 1, __LINE__) ;
	if (error_function (data [0], orig [seekpos * channels], margin) || k != seekpos)
	{	printf ("\nLine %d: sf_seek (backwards, SEEK_CUR) followed by sf_readf_float failed (%f, %f) (%d, %d).\n", __LINE__, data [0], orig [seekpos * channels], k, seekpos) ;
		exit (1) ;
		} ;

	/* Check that read past end of file returns number of items. */
	sf_seek (file, sfinfo.frames, SEEK_SET) ;

 	if ((k = sf_readf_float (file, data, datalen)) != 0)
 	{	printf ("\n\nLine %d: Return value from sf_readf_float past end of file incorrect (%d).\n", __LINE__, k) ;
 		exit (1) ;
 		} ;

	/* Check seek backward from end. */
	if ((k = sf_seek (file, 5 - sfinfo.frames, SEEK_END)) != 5)
	{	printf ("\n\nLine %d: sf_seek (SEEK_END) returned %d instead of %d.\n", __LINE__, k, 5) ;
		exit (1) ;
		} ;

	test_readf_float_or_die (file, 0, data, 1, __LINE__) ;
	if (error_function (data [0], orig [5 * channels], margin))
	{	printf ("\nLine %d: sf_seek (SEEK_END) followed by sf_readf_short failed (%f should be %f).\n", __LINE__, data [0], orig [5 * channels]) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	unlink (filename) ;
	printf ("ok\n") ;
} /* lcomp_test_float */

/*--------------------------------------------------------------------------------------------
*/

static void
lcomp_test_double (const char *filename, int filetype, int channels, double margin)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	int				k, m, seekpos ;
	long			datalen ;
	double			*orig, *data ;
	double			half_max_abs ;

	print_test_name ("lcomp_test_double", filename) ;

	datalen = BUFFER_SIZE / channels ;

	data = data_buffer.d ;
	orig = orig_buffer.d ;

	gen_signal_double (orig_buffer.d, 32000.0, channels, datalen) ;
	for (k = 0 ; k < channels * datalen ; k++)
		orig [k] = orig_buffer.d [k] ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= 123456789 ;	/* Ridiculous value. */
	sfinfo.channels		= channels ;
	sfinfo.format		= filetype ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	sf_command (file, SFC_SET_NORM_DOUBLE, NULL, SF_FALSE) ;
	test_writef_double_or_die (file, 0, orig, datalen, __LINE__) ;
	sf_set_string (file, SF_STR_COMMENT, long_comment) ;
	sf_close (file) ;

	memset (data, 0, datalen * sizeof (double)) ;

	if ((filetype & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW)
		memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	if ((sfinfo.format & (SF_FORMAT_TYPEMASK | SF_FORMAT_SUBMASK)) != (filetype & (SF_FORMAT_TYPEMASK | SF_FORMAT_SUBMASK)))
	{	printf ("\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < datalen / channels)
	{	printf ("Too few.frames in file. (%ld should be a little more than %ld)\n", datalen, SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	if (sfinfo.frames > (datalen + datalen / 20))
	{	printf ("Too many.frames in file. (%ld should be a little more than %ld)\n", datalen, SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != channels)
	{	printf ("Incorrect number of channels in file.\n") ;
		exit (1) ;
		} ;

	check_comment (file, filetype, __LINE__) ;

	sf_command (file, SFC_SET_NORM_DOUBLE, NULL, SF_FALSE) ;

	check_log_buffer_or_die (file, __LINE__) ;

	check_comment (file, filetype, __LINE__) ;

	sf_command (file, SFC_SET_NORM_DOUBLE, NULL, SF_FALSE) ;

	test_readf_double_or_die (file, 0, data, datalen, __LINE__) ;

	half_max_abs = 0.0 ;
	for (k = 0 ; k < datalen ; k++)
	{	if (error_function (data [k], orig [k], margin))
		{	printf ("\nLine %d: Incorrect sample A (#%d : %f should be %f).\n", __LINE__, k, data [k], orig [k]) ;
			oct_save_double (orig, data, datalen) ;
			exit (1) ;
			} ;
		half_max_abs = LCT_MAX (half_max_abs, abs (0.5 * data [k])) ;
		} ;

	if (half_max_abs < 1.0)
	{	printf ("\n\nLine %d: Signal is all zeros.\n", __LINE__) ;
		exit (1) ;
		} ;

	if ((k = sf_readf_double (file, data, datalen)) != sfinfo.frames - datalen)
	{	printf ("\n\nLine %d: Incorrect read length (%ld should be %d).\n", __LINE__,
			SF_COUNT_TO_LONG (channels * sfinfo.frames - datalen), k) ;
		exit (1) ;
		} ;

	/*	This check is only for block based encoders which must append silence
	**	to the end of a file so as to fill out a block.
	*/
	if ((sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_MS_ADPCM)
		for (k = 0 ; k < sfinfo.frames - datalen ; k++)
			if (abs (data [channels * k]) > decay_response (channels * k))
			{	printf ("\n\nLine %d : Incorrect sample B (#%d : abs (%f) should be < %d).\n", __LINE__, channels * k, data [channels * k], decay_response (channels * k)) ;
				exit (1) ;
				} ;

	if (! sfinfo.seekable)
	{	sf_close (file) ;
		unlink (filename) ;
		printf ("ok\n") ;
		return ;
		} ;

	/* Now test sf_seek function. */

	if ((k = sf_seek (file, 0, SEEK_SET)) != 0)
	{	printf ("\n\nLine %d: Seek to start of file failed (%d).\n", __LINE__, k) ;
		exit (1) ;
		} ;

	for (m = 0 ; m < 3 ; m++)
	{	test_readf_double_or_die (file, m, data, 11, __LINE__) ;

		for (k = 0 ; k < channels * 11 ; k++)
			if (error_function (data [k], orig [k + channels * m * 11], margin))
			{	printf ("\nLine %d: Incorrect sample (m = %d) (#%d : %f => %f).\n", __LINE__, m, k + channels * m * 11, orig [k + channels * m * 11], data [k]) ;
				for (m = 0 ; m < channels ; m++)
					printf ("%f ", data [m]) ;
				printf ("\n") ;
				exit (1) ;
				} ;
		} ;

	seekpos = BUFFER_SIZE / 10 ;

	/* Check seek from start of file. */
	if ((k = sf_seek (file, seekpos, SEEK_SET)) != seekpos)
	{	printf ("Seek to start of file + %d failed (%d).\n", seekpos, k) ;
		exit (1) ;
		} ;

	test_readf_double_or_die (file, 0, data, 1, __LINE__) ;

	if (error_function (data [0], orig [seekpos * channels], margin))
	{	printf ("\nLine %d: sf_seek (SEEK_SET) followed by sf_readf_double failed (%f, %f).\n", __LINE__, orig [1], data [0]) ;
		exit (1) ;
		} ;

	if ((k = sf_seek (file, 0, SEEK_CUR)) != seekpos + 1)
	{	printf ("\n\nLine %d: sf_seek (SEEK_CUR) with 0 offset failed (%d should be %d)\n", __LINE__, k, seekpos + 1) ;
		exit (1) ;
		} ;

	seekpos = sf_seek (file, 0, SEEK_CUR) + BUFFER_SIZE / 5 ;
	k = sf_seek (file, BUFFER_SIZE / 5, SEEK_CUR) ;
	test_readf_double_or_die (file, 0, data, 1, __LINE__) ;
	if (error_function (data [0], orig [seekpos * channels], margin) || k != seekpos)
	{	printf ("\nLine %d: sf_seek (forwards, SEEK_CUR) followed by sf_readf_double failed (%f, %f) (%d, %d).\n", __LINE__, data [0], orig [seekpos * channels], k, seekpos + 1) ;
		exit (1) ;
		} ;

	seekpos = sf_seek (file, 0, SEEK_CUR) - 20 ;
	/* Check seek backward from current position. */
	k = sf_seek (file, -20, SEEK_CUR) ;
	test_readf_double_or_die (file, 0, data, 1, __LINE__) ;
	if (error_function (data [0], orig [seekpos * channels], margin) || k != seekpos)
	{	printf ("\nLine %d: sf_seek (backwards, SEEK_CUR) followed by sf_readf_double failed (%f, %f) (%d, %d).\n", __LINE__, data [0], orig [seekpos * channels], k, seekpos) ;
		exit (1) ;
		} ;

	/* Check that read past end of file returns number of items. */
	sf_seek (file, sfinfo.frames, SEEK_SET) ;

 	if ((k = sf_readf_double (file, data, datalen)) != 0)
 	{	printf ("\n\nLine %d: Return value from sf_readf_double past end of file incorrect (%d).\n", __LINE__, k) ;
 		exit (1) ;
 		} ;

	/* Check seek backward from end. */
	if ((k = sf_seek (file, 5 - sfinfo.frames, SEEK_END)) != 5)
	{	printf ("\n\nLine %d: sf_seek (SEEK_END) returned %d instead of %d.\n", __LINE__, k, 5) ;
		exit (1) ;
		} ;

	test_readf_double_or_die (file, 0, data, 1, __LINE__) ;
	if (error_function (data [0], orig [5 * channels], margin))
	{	printf ("\nLine %d: sf_seek (SEEK_END) followed by sf_readf_short failed (%f should be %f).\n", __LINE__, data [0], orig [5 * channels]) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	unlink (filename) ;
	printf ("ok\n") ;
} /* lcomp_test_double */

/*========================================================================================
**	Smoothed differential loss compression tests.
*/

static void
sdlcomp_test_short	(const char *filename, int filetype, int channels, double margin)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	int				k, m, seekpos, half_max_abs ;
	long			datalen ;
	short			*orig, *data, *smooth ;

channels = 1 ;
	print_test_name ("sdlcomp_test_short", filename) ;

	datalen = BUFFER_SIZE ;

	orig = orig_buffer.s ;
	data = data_buffer.s ;
	smooth = smooth_buffer.s ;

	gen_signal_double (orig_buffer.d, 32000.0, channels, datalen) ;
	for (k = 0 ; k < datalen ; k++)
		orig [k] = lrint (orig_buffer.d [k]) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= 123456789 ;	/* Ridiculous value. */
	sfinfo.channels		= channels ;
	sfinfo.format		= filetype ;

	/*	The Vorbis encoder has a bug on PowerPC and X86-64 with sample rates
	**	<= 22050. Increasing the sample rate to 32000 avoids triggering it.
	**	See https://trac.xiph.org/ticket/1229
	*/
	if ((file = sf_open (filename, SFM_WRITE, &sfinfo)) == NULL)
	{	const char * errstr ;

		errstr = sf_strerror (NULL) ;
		if (strstr (errstr, "Sample rate chosen is known to trigger a Vorbis") == NULL)
		{	printf ("Line %d: sf_open_fd (SFM_WRITE) failed : %s\n", __LINE__, errstr) ;
			dump_log_buffer (NULL) ;
			exit (1) ;
			} ;

		printf ("\n                                  Sample rate -> 32kHz    ") ;
		sfinfo.samplerate = 32000 ;

		file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
		} ;

	test_write_short_or_die (file, 0, orig, datalen, __LINE__) ;
	sf_set_string (file, SF_STR_COMMENT, long_comment) ;
	sf_close (file) ;

	memset (data, 0, datalen * sizeof (short)) ;

	if ((filetype & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW)
		memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	if (sfinfo.format != filetype)
	{	printf ("\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < datalen / channels)
	{	printf ("Too few.frames in file. (%ld should be a little more than %ld)\n", datalen, SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	if (sfinfo.frames > (datalen + 400))
	{	printf ("Too many.frames in file. (%ld should be a little more than %ld)\n", SF_COUNT_TO_LONG (sfinfo.frames), datalen) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != channels)
	{	printf ("Incorrect number of channels in file.\n") ;
		exit (1) ;
		} ;

	check_comment (file, filetype, __LINE__) ;

	sf_command (file, SFC_SET_NORM_FLOAT, NULL, SF_FALSE) ;

	check_log_buffer_or_die (file, __LINE__) ;

	test_readf_short_or_die (file, 0, data, datalen, __LINE__) ;

	memcpy (smooth, orig, datalen * sizeof (short)) ;
	smoothed_diff_short (data, datalen) ;
	smoothed_diff_short (smooth, datalen) ;

	half_max_abs = 0.0 ;
	for (k = 0 ; k < datalen ; k++)
	{	if (error_function (1.0 * data [k], 1.0 * smooth [k], margin))
		{	printf ("\nLine %d: Incorrect sample (#%d : %d should be %d).\n", __LINE__, k, data [k], smooth [k]) ;
			oct_save_short (orig, smooth, datalen) ;
			exit (1) ;
			} ;
		half_max_abs = LCT_MAX (half_max_abs, abs (0.5 * data [k])) ;
		} ;

	if (half_max_abs < 1)
	{	printf ("\n\nLine %d: Signal is all zeros.\n", __LINE__) ;
		exit (1) ;
		} ;

	if ((k = sf_read_short (file, data, datalen)) != sfinfo.frames - datalen)
	{	printf ("\n\nLine %d: Incorrect read length (%d should be %ld).\n", __LINE__, k, SF_COUNT_TO_LONG (sfinfo.frames - datalen)) ;
		exit (1) ;
		} ;

	if ((sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_MS_ADPCM &&
		(sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_GSM610)
		for (k = 0 ; k < sfinfo.frames - datalen ; k++)
			if (abs (data [k]) > decay_response (k))
			{	printf ("\n\nLine %d: Incorrect sample (#%ld : abs (%d) should be < %d).\n", __LINE__, datalen + k, data [k], decay_response (k)) ;
				exit (1) ;
				} ;

	/* Now test sf_seek function. */
	if (sfinfo.seekable)
	{	if ((k = sf_seek (file, 0, SEEK_SET)) != 0)
		{	printf ("\n\nLine %d: Seek to start of file failed (%d).\n", __LINE__, k) ;
			exit (1) ;
			} ;

		for (m = 0 ; m < 3 ; m++)
		{	test_readf_short_or_die (file, m, data, datalen / 7, __LINE__) ;

			smoothed_diff_short (data, datalen / 7) ;
			memcpy (smooth, orig + m * datalen / 7, datalen / 7 * sizeof (short)) ;
			smoothed_diff_short (smooth, datalen / 7) ;

			for (k = 0 ; k < datalen / 7 ; k++)
				if (error_function (1.0 * data [k], 1.0 * smooth [k], margin))
				{	printf ("\nLine %d: Incorrect sample C (#%d (%ld) : %d => %d).\n", __LINE__, k, k + m * (datalen / 7), smooth [k], data [k]) ;
					for (m = 0 ; m < 10 ; m++)
						printf ("%d ", data [k]) ;
					printf ("\n") ;
					exit (1) ;
					} ;
			} ; /* for (m = 0 ; m < 3 ; m++) */

		seekpos = BUFFER_SIZE / 10 ;

		/* Check seek from start of file. */
		if ((k = sf_seek (file, seekpos, SEEK_SET)) != seekpos)
		{	printf ("Seek to start of file + %d failed (%d).\n", seekpos, k) ;
			exit (1) ;
			} ;
		test_readf_short_or_die (file, 0, data, 1, __LINE__) ;

		if (error_function (1.0 * data [0], 1.0 * orig [seekpos * channels], margin))
		{	printf ("\nLine %d: sf_seek (SEEK_SET) followed by sf_read_short failed (%d, %d).\n", __LINE__, orig [1], data [0]) ;
			exit (1) ;
			} ;

		if ((k = sf_seek (file, 0, SEEK_CUR)) != seekpos + 1)
		{	printf ("\n\nLine %d: sf_seek (SEEK_CUR) with 0 offset failed (%d should be %d)\n", __LINE__, k, seekpos + 1) ;
			exit (1) ;
			} ;

		seekpos = sf_seek (file, 0, SEEK_CUR) + BUFFER_SIZE / 5 ;
		k = sf_seek (file, BUFFER_SIZE / 5, SEEK_CUR) ;
		test_readf_short_or_die (file, 0, data, 1, __LINE__) ;
		if (error_function (1.0 * data [0], 1.0 * orig [seekpos * channels], margin) || k != seekpos)
		{	printf ("\nLine %d: sf_seek (forwards, SEEK_CUR) followed by sf_read_short failed (%d, %d) (%d, %d).\n", __LINE__, data [0], orig [seekpos * channels], k, seekpos + 1) ;
			exit (1) ;
			} ;

		seekpos = sf_seek (file, 0, SEEK_CUR) - 20 ;
		/* Check seek backward from current position. */
		k = sf_seek (file, -20, SEEK_CUR) ;
		test_readf_short_or_die (file, 0, data, 1, __LINE__) ;
		if (error_function (1.0 * data [0], 1.0 * orig [seekpos * channels], margin) || k != seekpos)
		{	printf ("\nLine %d: sf_seek (backwards, SEEK_CUR) followed by sf_read_short failed (%d, %d) (%d, %d).\n", __LINE__, data [0], orig [seekpos * channels], k, seekpos) ;
			exit (1) ;
			} ;

		/* Check that read past end of file returns number of items. */
		sf_seek (file, sfinfo.frames, SEEK_SET) ;

	 	if ((k = sf_read_short (file, data, datalen)) != 0)
	 	{	printf ("\n\nLine %d: Return value from sf_read_short past end of file incorrect (%d).\n", __LINE__, k) ;
	 		exit (1) ;
	 		} ;

		/* Check seek backward from end. */

		if ((k = sf_seek (file, 5 - sfinfo.frames, SEEK_END)) != 5)
		{	printf ("\n\nLine %d: sf_seek (SEEK_END) returned %d instead of %d.\n", __LINE__, k, 5) ;
			exit (1) ;
			} ;

		test_read_short_or_die (file, 0, data, channels, __LINE__) ;
		if (error_function (1.0 * data [0], 1.0 * orig [5 * channels], margin))
		{	printf ("\nLine %d: sf_seek (SEEK_END) followed by sf_read_short failed (%d should be %d).\n", __LINE__, data [0], orig [5 * channels]) ;
			exit (1) ;
			} ;
		} /* if (sfinfo.seekable) */

	sf_close (file) ;

	unlink (filename) ;
	printf ("ok\n") ;
} /* sdlcomp_test_short */

static	void
sdlcomp_test_int	(const char *filename, int filetype, int channels, double margin)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	int				k, m, seekpos, half_max_abs ;
	long			datalen ;
	int				*orig, *data, *smooth ;
	double			scale ;

channels = 1 ;

	print_test_name ("sdlcomp_test_int", filename) ;

	datalen = BUFFER_SIZE ;
	scale = 1.0 * 0x10000 ;

	orig = orig_buffer.i ;
	data = data_buffer.i ;
	smooth = smooth_buffer.i ;

	gen_signal_double (orig_buffer.d, 32000.0 * scale, channels, datalen) ;
	for (k = 0 ; k < datalen ; k++)
		orig [k] = lrint (orig_buffer.d [k]) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= 123456789 ;	/* Ridiculous value. */
	sfinfo.channels		= channels ;
	sfinfo.format		= filetype ;

	/*	The Vorbis encoder has a bug on PowerPC and X86-64 with sample rates
	**	<= 22050. Increasing the sample rate to 32000 avoids triggering it.
	**	See https://trac.xiph.org/ticket/1229
	*/
	if ((file = sf_open (filename, SFM_WRITE, &sfinfo)) == NULL)
	{	const char * errstr ;

		errstr = sf_strerror (NULL) ;
		if (strstr (errstr, "Sample rate chosen is known to trigger a Vorbis") == NULL)
		{	printf ("Line %d: sf_open_fd (SFM_WRITE) failed : %s\n", __LINE__, errstr) ;
			dump_log_buffer (NULL) ;
			exit (1) ;
			} ;

		printf ("\n                                  Sample rate -> 32kHz    ") ;
		sfinfo.samplerate = 32000 ;

		file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
		} ;

	test_writef_int_or_die (file, 0, orig, datalen, __LINE__) ;
	sf_set_string (file, SF_STR_COMMENT, long_comment) ;
	sf_close (file) ;

	memset (data, 0, datalen * sizeof (int)) ;

	if ((filetype & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW)
		memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	if (sfinfo.format != filetype)
	{	printf ("Returned format incorrect (0x%08X => 0x%08X).\n", filetype, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < datalen / channels)
	{	printf ("Too few.frames in file. (%ld should be a little more than %ld)\n", datalen, SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	if (sfinfo.frames > (datalen + 400))
	{	printf ("Too many.frames in file. (%ld should be a little more than %ld)\n", SF_COUNT_TO_LONG (sfinfo.frames), datalen) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != channels)
	{	printf ("Incorrect number of channels in file.\n") ;
		exit (1) ;
		} ;

	check_log_buffer_or_die (file, __LINE__) ;

	test_readf_int_or_die (file, 0, data, datalen, __LINE__) ;

	memcpy (smooth, orig, datalen * sizeof (int)) ;
	smoothed_diff_int (data, datalen) ;
	smoothed_diff_int (smooth, datalen) ;

	half_max_abs = abs (data [0] >> 16) ;
	for (k = 1 ; k < datalen ; k++)
	{	if (error_function (data [k] / scale, smooth [k] / scale, margin))
		{	printf ("\nLine %d: Incorrect sample (#%d : %d should be %d).\n", __LINE__, k, data [k], smooth [k]) ;
			oct_save_int (orig, smooth, datalen) ;
			exit (1) ;
			} ;
		half_max_abs = LCT_MAX (half_max_abs, abs (data [k] / 2)) ;
		} ;

	if (half_max_abs < 1)
	{	printf ("\n\nLine %d: Signal is all zeros.\n", __LINE__) ;
		exit (1) ;
		} ;

	if ((k = sf_readf_int (file, data, datalen)) != sfinfo.frames - datalen)
	{	printf ("\n\nLine %d: Incorrect read length (%d should be %ld).\n", __LINE__, k, SF_COUNT_TO_LONG (sfinfo.frames - datalen)) ;
		exit (1) ;
		} ;

	if ((sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_IMA_ADPCM &&
		(sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_MS_ADPCM &&
		(sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_GSM610 &&
		(sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_G721_32 &&
		(sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_G723_24)
		for (k = 0 ; k < sfinfo.frames - datalen ; k++)
			if (abs (data [k]) > decay_response (k))
			{	printf ("\n\nLine %d: Incorrect sample (#%ld : abs (%d) should be < %d).\n", __LINE__, datalen + k, data [k], decay_response (k)) ;
				exit (1) ;
				} ;

	/* Now test sf_seek function. */
	if (sfinfo.seekable)
	{	if ((k = sf_seek (file, 0, SEEK_SET)) != 0)
		{	printf ("\n\nLine %d: Seek to start of file failed (%d).\n", __LINE__, k) ;
			exit (1) ;
			} ;

		for (m = 0 ; m < 3 ; m++)
		{	test_readf_int_or_die (file, m, data, datalen / 7, __LINE__) ;

			smoothed_diff_int (data, datalen / 7) ;
			memcpy (smooth, orig + m * datalen / 7, datalen / 7 * sizeof (int)) ;
			smoothed_diff_int (smooth, datalen / 7) ;

			for (k = 0 ; k < datalen / 7 ; k++)
				if (error_function (data [k] / scale, smooth [k] / scale, margin))
				{	printf ("\nLine %d: Incorrect sample (#%d (%ld) : %d => %d).\n", __LINE__, k, k + m * (datalen / 7), smooth [k], data [k]) ;
					for (m = 0 ; m < 10 ; m++)
						printf ("%d ", data [k]) ;
					printf ("\n") ;
					exit (1) ;
					} ;
			} ; /* for (m = 0 ; m < 3 ; m++) */

		seekpos = BUFFER_SIZE / 10 ;

		/* Check seek from start of file. */
		if ((k = sf_seek (file, seekpos, SEEK_SET)) != seekpos)
		{	printf ("Seek to start of file + %d failed (%d).\n", seekpos, k) ;
			exit (1) ;
			} ;
		test_readf_int_or_die (file, 0, data, 1, __LINE__) ;

		if (error_function (1.0 * data [0], 1.0 * orig [seekpos * channels], margin))
		{	printf ("\nLine %d: sf_seek (SEEK_SET) followed by sf_readf_int failed (%d, %d).\n", __LINE__, orig [1], data [0]) ;
			exit (1) ;
			} ;

		if ((k = sf_seek (file, 0, SEEK_CUR)) != seekpos + 1)
		{	printf ("\n\nLine %d: sf_seek (SEEK_CUR) with 0 offset failed (%d should be %d)\n", __LINE__, k, seekpos + 1) ;
			exit (1) ;
			} ;

		seekpos = sf_seek (file, 0, SEEK_CUR) + BUFFER_SIZE / 5 ;
		k = sf_seek (file, BUFFER_SIZE / 5, SEEK_CUR) ;
		test_readf_int_or_die (file, 0, data, 1, __LINE__) ;
		if (error_function (1.0 * data [0], 1.0 * orig [seekpos * channels], margin) || k != seekpos)
		{	printf ("\nLine %d: sf_seek (forwards, SEEK_CUR) followed by sf_readf_int failed (%d, %d) (%d, %d).\n", __LINE__, data [0], orig [seekpos * channels], k, seekpos + 1) ;
			exit (1) ;
			} ;

		seekpos = sf_seek (file, 0, SEEK_CUR) - 20 ;
		/* Check seek backward from current position. */
		k = sf_seek (file, -20, SEEK_CUR) ;
		test_readf_int_or_die (file, 0, data, 1, __LINE__) ;
		if (error_function (1.0 * data [0], 1.0 * orig [seekpos * channels], margin) || k != seekpos)
		{	printf ("\nLine %d: sf_seek (backwards, SEEK_CUR) followed by sf_readf_int failed (%d, %d) (%d, %d).\n", __LINE__, data [0], orig [seekpos * channels], k, seekpos) ;
			exit (1) ;
			} ;

		/* Check that read past end of file returns number of items. */
		sf_seek (file, sfinfo.frames, SEEK_SET) ;

	 	if ((k = sf_readf_int (file, data, datalen)) != 0)
	 	{	printf ("\n\nLine %d: Return value from sf_readf_int past end of file incorrect (%d).\n", __LINE__, k) ;
	 		exit (1) ;
	 		} ;

		/* Check seek backward from end. */

		if ((k = sf_seek (file, 5 - sfinfo.frames, SEEK_END)) != 5)
		{	printf ("\n\nLine %d: sf_seek (SEEK_END) returned %d instead of %d.\n", __LINE__, k, 5) ;
			exit (1) ;
			} ;

		test_readf_int_or_die (file, 0, data, 1, __LINE__) ;
		if (error_function (data [0] / scale, orig [5] / scale, margin))
		{	printf ("\nLine %d: sf_seek (SEEK_END) followed by sf_readf_int failed (%d should be %d).\n", __LINE__, data [0], orig [5]) ;
			exit (1) ;
			} ;
		} /* if (sfinfo.seekable) */

	sf_close (file) ;

	unlink (filename) ;
	printf ("ok\n") ;
} /* sdlcomp_test_int */

static void
sdlcomp_test_float	(const char *filename, int filetype, int channels, double margin)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	int				k, m, seekpos ;
	long			datalen ;
	float			*orig, *data, *smooth ;
	double			half_max_abs ;

channels = 1 ;

	print_test_name ("sdlcomp_test_float", filename) ;

	if ((filetype & SF_FORMAT_SUBMASK) == SF_FORMAT_VORBIS)
	{	puts ("Not working for this format.") ;
		return ;
		} ;

printf ("** fix this ** ") ;

	datalen = BUFFER_SIZE ;

	orig = orig_buffer.f ;
	data = data_buffer.f ;
	smooth = smooth_buffer.f ;

	gen_signal_double (orig_buffer.d, 32000.0, channels, datalen) ;
	for (k = 0 ; k < datalen ; k++)
		orig [k] = lrint (orig_buffer.d [k]) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= 123456789 ;	/* Ridiculous value. */
	sfinfo.channels		= channels ;
	sfinfo.format		= filetype ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	sf_command (file, SFC_SET_NORM_FLOAT, NULL, SF_FALSE) ;
	test_write_float_or_die (file, 0, orig, datalen, __LINE__) ;
	sf_set_string (file, SF_STR_COMMENT, long_comment) ;
	sf_close (file) ;

	memset (data, 0, datalen * sizeof (float)) ;

	if ((filetype & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW)
		memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	if ((sfinfo.format & (SF_FORMAT_TYPEMASK | SF_FORMAT_SUBMASK)) != (filetype & (SF_FORMAT_TYPEMASK | SF_FORMAT_SUBMASK)))
	{	printf ("\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < datalen / channels)
	{	printf ("Too few.frames in file. (%ld should be a little more than %ld)\n", datalen, SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	if (sfinfo.frames > (datalen + 400))
	{	printf ("Too many.frames in file. (%ld should be a little more than %ld)\n", SF_COUNT_TO_LONG (sfinfo.frames), datalen) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != channels)
	{	printf ("Incorrect number of channels in file.\n") ;
		exit (1) ;
		} ;

	check_comment (file, filetype, __LINE__) ;

	sf_command (file, SFC_SET_NORM_FLOAT, NULL, SF_FALSE) ;

	check_log_buffer_or_die (file, __LINE__) ;

	test_read_float_or_die (file, 0, data, datalen, __LINE__) ;

	memcpy (smooth, orig, datalen * sizeof (float)) ;
	smoothed_diff_float (data, datalen) ;
	smoothed_diff_float (smooth, datalen) ;

	half_max_abs = fabs (data [0]) ;
	for (k = 1 ; k < datalen ; k++)
	{	if (error_function (data [k], smooth [k], margin))
		{	printf ("\nLine %d: Incorrect sample (#%d : %d should be %d).\n", __LINE__, k, (int) data [k], (int) smooth [k]) ;
			oct_save_float (orig, smooth, datalen) ;
			exit (1) ;
			} ;
		half_max_abs = LCT_MAX (half_max_abs, abs (0.5 * data [k])) ;
		} ;

	if (half_max_abs <= 0.0)
	{	printf ("\n\nLine %d: Signal is all zeros.\n", __LINE__) ;
		printf ("half_max_abs : % 10.6f\n", half_max_abs) ;
		exit (1) ;
		} ;

	if ((k = sf_read_float (file, data, datalen)) != sfinfo.frames - datalen)
	{	printf ("\n\nLine %d: Incorrect read length (%d should be %ld).\n", __LINE__, k, SF_COUNT_TO_LONG (sfinfo.frames - datalen)) ;
		exit (1) ;
		} ;

	if ((sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_MS_ADPCM &&
		(sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_GSM610)
		for (k = 0 ; k < sfinfo.frames - datalen ; k++)
			if (abs (data [k]) > decay_response (k))
			{	printf ("\n\nLine %d: Incorrect sample (#%ld : abs (%d) should be < %d).\n", __LINE__, datalen + k, (int) data [k], (int) decay_response (k)) ;
				exit (1) ;
				} ;

	/* Now test sf_seek function. */
	if (sfinfo.seekable)
	{	if ((k = sf_seek (file, 0, SEEK_SET)) != 0)
		{	printf ("\n\nLine %d: Seek to start of file failed (%d).\n", __LINE__, k) ;
			exit (1) ;
			} ;

		for (m = 0 ; m < 3 ; m++)
		{	test_read_float_or_die (file, 0, data, datalen / 7, __LINE__) ;

			smoothed_diff_float (data, datalen / 7) ;
			memcpy (smooth, orig + m * datalen / 7, datalen / 7 * sizeof (float)) ;
			smoothed_diff_float (smooth, datalen / 7) ;

			for (k = 0 ; k < datalen / 7 ; k++)
				if (error_function (data [k], smooth [k], margin))
				{	printf ("\nLine %d: Incorrect sample C (#%d (%ld) : %d => %d).\n", __LINE__, k, k + m * (datalen / 7), (int) smooth [k], (int) data [k]) ;
					for (m = 0 ; m < 10 ; m++)
						printf ("%d ", (int) data [k]) ;
					printf ("\n") ;
					exit (1) ;
					} ;
			} ; /* for (m = 0 ; m < 3 ; m++) */

		seekpos = BUFFER_SIZE / 10 ;

		/* Check seek from start of file. */
		if ((k = sf_seek (file, seekpos, SEEK_SET)) != seekpos)
		{	printf ("Seek to start of file + %d failed (%d).\n", seekpos, k) ;
			exit (1) ;
			} ;
		test_read_float_or_die (file, 0, data, channels, __LINE__) ;

		if (error_function (data [0], orig [seekpos * channels], margin))
		{	printf ("\nLine %d: sf_seek (SEEK_SET) followed by sf_read_float failed (%d, %d).\n", __LINE__, (int) orig [1], (int) data [0]) ;
			exit (1) ;
			} ;

		if ((k = sf_seek (file, 0, SEEK_CUR)) != seekpos + 1)
		{	printf ("\n\nLine %d: sf_seek (SEEK_CUR) with 0 offset failed (%d should be %d)\n", __LINE__, k, seekpos + 1) ;
			exit (1) ;
			} ;

		seekpos = sf_seek (file, 0, SEEK_CUR) + BUFFER_SIZE / 5 ;
		k = sf_seek (file, BUFFER_SIZE / 5, SEEK_CUR) ;
		test_read_float_or_die (file, 0, data, channels, __LINE__) ;
		if (error_function (data [0], orig [seekpos * channels], margin) || k != seekpos)
		{	printf ("\nLine %d: sf_seek (forwards, SEEK_CUR) followed by sf_read_float failed (%d, %d) (%d, %d).\n", __LINE__, (int) data [0], (int) orig [seekpos * channels], k, seekpos + 1) ;
			exit (1) ;
			} ;

		seekpos = sf_seek (file, 0, SEEK_CUR) - 20 ;
		/* Check seek backward from current position. */
		k = sf_seek (file, -20, SEEK_CUR) ;
		test_read_float_or_die (file, 0, data, channels, __LINE__) ;
		if (error_function (data [0], orig [seekpos * channels], margin) || k != seekpos)
		{	printf ("\nLine %d: sf_seek (backwards, SEEK_CUR) followed by sf_read_float failed (%d, %d) (%d, %d).\n", __LINE__, (int) data [0], (int) orig [seekpos * channels], k, seekpos) ;
			exit (1) ;
			} ;

		/* Check that read past end of file returns number of items. */
		sf_seek (file, sfinfo.frames, SEEK_SET) ;

	 	if ((k = sf_read_float (file, data, datalen)) != 0)
	 	{	printf ("\n\nLine %d: Return value from sf_read_float past end of file incorrect (%d).\n", __LINE__, k) ;
	 		exit (1) ;
	 		} ;

		/* Check seek backward from end. */

		if ((k = sf_seek (file, 5 - sfinfo.frames, SEEK_END)) != 5)
		{	printf ("\n\nLine %d: sf_seek (SEEK_END) returned %d instead of %d.\n", __LINE__, k, 5) ;
			exit (1) ;
			} ;

		test_read_float_or_die (file, 0, data, channels, __LINE__) ;
		if (error_function (data [0], orig [5 * channels], margin))
		{	printf ("\nLine %d: sf_seek (SEEK_END) followed by sf_read_float failed (%f should be %f).\n", __LINE__, data [0], orig [5 * channels]) ;
			exit (1) ;
			} ;
		} /* if (sfinfo.seekable) */

	sf_close (file) ;

	unlink (filename) ;
	printf ("ok\n") ;
} /* sdlcomp_test_float */

static void
sdlcomp_test_double	(const char *filename, int filetype, int channels, double margin)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	int				k, m, seekpos ;
	long			datalen ;
	double			*orig, *data, *smooth, half_max_abs ;

channels = 1 ;
	print_test_name ("sdlcomp_test_double", filename) ;

	if ((filetype & SF_FORMAT_SUBMASK) == SF_FORMAT_VORBIS)
	{	puts ("Not working for this format.") ;
		return ;
		} ;

	datalen = BUFFER_SIZE ;

	orig = orig_buffer.d ;
	data = data_buffer.d ;
	smooth = smooth_buffer.d ;

	gen_signal_double (orig_buffer.d, 32000.0, channels, datalen) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= 123456789 ;	/* Ridiculous value. */
	sfinfo.channels		= channels ;
	sfinfo.format		= filetype ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	sf_command (file, SFC_SET_NORM_DOUBLE, NULL, SF_FALSE) ;
	test_write_double_or_die (file, 0, orig, datalen, __LINE__) ;
	sf_set_string (file, SF_STR_COMMENT, long_comment) ;
	sf_close (file) ;

	memset (data, 0, datalen * sizeof (double)) ;

	if ((filetype & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW)
		memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	if (sfinfo.format != filetype)
	{	printf ("Returned format incorrect (0x%08X => 0x%08X).\n", filetype, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < datalen / channels)
	{	printf ("Too few.frames in file. (%ld should be a little more than %ld)\n", datalen, SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	if (sfinfo.frames > (datalen + 400))
	{	printf ("Too many.frames in file. (%ld should be a little more than %ld)\n", SF_COUNT_TO_LONG (sfinfo.frames), datalen) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != channels)
	{	printf ("Incorrect number of channels in file.\n") ;
		exit (1) ;
		} ;

	check_comment (file, filetype, __LINE__) ;

	check_comment (file, filetype, __LINE__) ;

	sf_command (file, SFC_SET_NORM_DOUBLE, NULL, SF_FALSE) ;

	check_log_buffer_or_die (file, __LINE__) ;

	test_read_double_or_die (file, 0, data, datalen, __LINE__) ;

	memcpy (smooth, orig, datalen * sizeof (double)) ;
	smoothed_diff_double (data, datalen) ;
	smoothed_diff_double (smooth, datalen) ;

	half_max_abs = 0.0 ;
	for (k = 0 ; k < datalen ; k++)
	{	if (error_function (data [k], smooth [k], margin))
		{	printf ("\n\nLine %d: Incorrect sample (#%d : %d should be %d).\n", __LINE__, k, (int) data [k], (int) smooth [k]) ;
			oct_save_double (orig, smooth, datalen) ;
			exit (1) ;
			} ;
		half_max_abs = LCT_MAX (half_max_abs, 0.5 * fabs (data [k])) ;
		} ;

	if (half_max_abs < 1.0)
	{	printf ("\n\nLine %d: Signal is all zeros.\n", __LINE__) ;
		exit (1) ;
		} ;

	if ((k = sf_read_double (file, data, datalen)) != sfinfo.frames - datalen)
	{	printf ("\n\nLine %d: Incorrect read length (%d should be %ld).\n", __LINE__, k, SF_COUNT_TO_LONG (sfinfo.frames - datalen)) ;
		exit (1) ;
		} ;

	if ((sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_MS_ADPCM &&
		(sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_GSM610)
		for (k = 0 ; k < sfinfo.frames - datalen ; k++)
			if (abs (data [k]) > decay_response (k))
			{	printf ("\n\nLine %d: Incorrect sample (#%ld : abs (%d) should be < %d).\n", __LINE__, datalen + k, (int) data [k], (int) decay_response (k)) ;
				exit (1) ;
				} ;

	/* Now test sf_seek function. */
	if (sfinfo.seekable)
	{	if ((k = sf_seek (file, 0, SEEK_SET)) != 0)
		{	printf ("\n\nLine %d: Seek to start of file failed (%d).\n", __LINE__, k) ;
			exit (1) ;
			} ;

		for (m = 0 ; m < 3 ; m++)
		{	test_read_double_or_die (file, m, data, datalen / 7, __LINE__) ;

			smoothed_diff_double (data, datalen / 7) ;
			memcpy (smooth, orig + m * datalen / 7, datalen / 7 * sizeof (double)) ;
			smoothed_diff_double (smooth, datalen / 7) ;

			for (k = 0 ; k < datalen / 7 ; k++)
				if (error_function (data [k], smooth [k], margin))
				{	printf ("\nLine %d: Incorrect sample C (#%d (%ld) : %d => %d).\n", __LINE__, k, k + m * (datalen / 7), (int) smooth [k], (int) data [k]) ;
					for (m = 0 ; m < 10 ; m++)
						printf ("%d ", (int) data [k]) ;
					printf ("\n") ;
					exit (1) ;
					} ;
			} ; /* for (m = 0 ; m < 3 ; m++) */

		seekpos = BUFFER_SIZE / 10 ;

		/* Check seek from start of file. */
		if ((k = sf_seek (file, seekpos, SEEK_SET)) != seekpos)
		{	printf ("Seek to start of file + %d failed (%d).\n", seekpos, k) ;
			exit (1) ;
			} ;
		test_read_double_or_die (file, 0, data, channels, __LINE__) ;

		if (error_function (data [0], orig [seekpos * channels], margin))
		{	printf ("\nLine %d: sf_seek (SEEK_SET) followed by sf_read_double failed (%d, %d).\n", __LINE__, (int) orig [1], (int) data [0]) ;
			exit (1) ;
			} ;

		if ((k = sf_seek (file, 0, SEEK_CUR)) != seekpos + 1)
		{	printf ("\n\nLine %d: sf_seek (SEEK_CUR) with 0 offset failed (%d should be %d)\n", __LINE__, k, seekpos + 1) ;
			exit (1) ;
			} ;

		seekpos = sf_seek (file, 0, SEEK_CUR) + BUFFER_SIZE / 5 ;
		k = sf_seek (file, BUFFER_SIZE / 5, SEEK_CUR) ;
		test_read_double_or_die (file, 0, data, channels, __LINE__) ;
		if (error_function (data [0], orig [seekpos * channels], margin) || k != seekpos)
		{	printf ("\nLine %d: sf_seek (forwards, SEEK_CUR) followed by sf_read_double failed (%d, %d) (%d, %d).\n", __LINE__, (int) data [0], (int) orig [seekpos * channels], k, seekpos + 1) ;
			exit (1) ;
			} ;

		seekpos = sf_seek (file, 0, SEEK_CUR) - 20 ;
		/* Check seek backward from current position. */
		k = sf_seek (file, -20, SEEK_CUR) ;
		test_read_double_or_die (file, 0, data, channels, __LINE__) ;
		if (error_function (data [0], orig [seekpos * channels], margin) || k != seekpos)
		{	printf ("\nLine %d: sf_seek (backwards, SEEK_CUR) followed by sf_read_double failed (%d, %d) (%d, %d).\n", __LINE__, (int) data [0], (int) orig [seekpos * channels], k, seekpos) ;
			exit (1) ;
			} ;

		/* Check that read past end of file returns number of items. */
		sf_seek (file, sfinfo.frames, SEEK_SET) ;

	 	if ((k = sf_read_double (file, data, datalen)) != 0)
	 	{	printf ("\n\nLine %d: Return value from sf_read_double past end of file incorrect (%d).\n", __LINE__, k) ;
	 		exit (1) ;
	 		} ;

		/* Check seek backward from end. */

		if ((k = sf_seek (file, 5 - sfinfo.frames, SEEK_END)) != 5)
		{	printf ("\n\nLine %d: sf_seek (SEEK_END) returned %d instead of %d.\n", __LINE__, k, 5) ;
			exit (1) ;
			} ;

		test_read_double_or_die (file, 0, data, channels, __LINE__) ;
		if (error_function (data [0], orig [5 * channels], margin))
		{	printf ("\nLine %d: sf_seek (SEEK_END) followed by sf_read_double failed (%f should be %f).\n", __LINE__, data [0], orig [5 * channels]) ;
			exit (1) ;
			} ;
		} /* if (sfinfo.seekable) */

	sf_close (file) ;

	unlink (filename) ;
	printf ("ok\n") ;
} /* sdlcomp_test_double */

static void
read_raw_test (const char *filename, int filetype, int channels)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	sf_count_t		count ;
	long			datalen ;
	short			*orig, *data ;
	int				k ;

	print_test_name ("read_raw_test", filename) ;

	datalen = ARRAY_LEN (orig_buffer.s) / 2 ;

	orig = orig_buffer.s ;
	data = data_buffer.s ;

	gen_signal_double (orig_buffer.d, 32000.0, channels, datalen) ;
	for (k = 0 ; k < datalen ; k++)
		orig [k] = lrint (orig_buffer.d [k]) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= 123456789 ;	/* Ridiculous value. */
	sfinfo.channels		= channels ;
	sfinfo.format		= filetype ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;
	test_write_short_or_die (file, 0, orig, datalen, __LINE__) ;
	sf_set_string (file, SF_STR_COMMENT, long_comment) ;
	sf_close (file) ;

	memset (data, 0, datalen * sizeof (double)) ;

	if ((filetype & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW)
		memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	if (sfinfo.format != filetype)
	{	printf ("Returned format incorrect (0x%08X => 0x%08X).\n", filetype, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < datalen / channels)
	{	printf ("Too few.frames in file. (%ld should be a little more than %ld)\n", datalen, SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	if (sfinfo.frames > (datalen + 400))
	{	printf ("Too many.frames in file. (%ld should be a little more than %ld)\n", SF_COUNT_TO_LONG (sfinfo.frames), datalen) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != channels)
	{	printf ("Incorrect number of channels in file.\n") ;
		exit (1) ;
		} ;

	check_comment (file, filetype, __LINE__) ;

	count = sf_read_raw (file, orig_buffer.c, datalen + 5 * channels) ;
	if (count != sfinfo.channels * sfinfo.frames)
	{	printf ("\nLine %d : sf_read_raw returned %ld should be %ld\n", __LINE__, SF_COUNT_TO_LONG (count), sfinfo.channels * SF_COUNT_TO_LONG (sfinfo.frames)) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	unlink (filename) ;
	printf ("ok\n") ;
} /* read_raw_test */

/*========================================================================================
**	Auxiliary functions
*/

#define		SIGNAL_MAXVAL	30000.0
#define		DECAY_COUNT		1000

static int
decay_response (int k)
{	if (k < 1)
		return (int) (1.2 * SIGNAL_MAXVAL) ;
	if (k > DECAY_COUNT)
		return 0 ;
	return (int) (1.2 * SIGNAL_MAXVAL * (DECAY_COUNT - k) / (1.0 * DECAY_COUNT)) ;
} /* decay_response */

static void
gen_signal_double (double *data, double scale, int channels, int datalen)
{	int		k, ramplen ;
	double	amp = 0.0 ;

	ramplen = DECAY_COUNT ;

	if (channels == 1)
	{	for (k = 0 ; k < datalen ; k++)
		{	if (k <= ramplen)
				amp = scale * k / ((double) ramplen) ;
			else if (k > datalen - ramplen)
				amp = scale * (datalen - k) / ((double) ramplen) ;

/*-printf ("%3d : %g\n", k, amp) ;-*/

			data [k] = amp * (0.4 * sin (33.3 * 2.0 * M_PI * ((double) (k+1)) / ((double) SAMPLE_RATE))
							+ 0.3 * cos (201.1 * 2.0 * M_PI * ((double) (k+1)) / ((double) SAMPLE_RATE))) ;
			} ;
		}
	else
	{	for (k = 0 ; k < datalen ; k ++)
		{	if (k <= ramplen)
				amp = scale * k / ((double) ramplen) ;
			else if (k > datalen - ramplen)
				amp = scale * (datalen - k) / ((double) ramplen) ;

			data [2 * k] = amp * (0.4 * sin (33.3 * 2.0 * M_PI * ((double) (k+1)) / ((double) SAMPLE_RATE))
							+ 0.3 * cos (201.1 * 2.0 * M_PI * ((double) (k+1)) / ((double) SAMPLE_RATE))) ;
			data [2 * k + 1] = amp * (0.4 * sin (55.5 * 2.0 * M_PI * ((double) (k+1)) / ((double) SAMPLE_RATE))
							+ 0.3 * cos (201.1 * 2.0 * M_PI * ((double) (k+1)) / ((double) SAMPLE_RATE))) ;
			} ;
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
	{	printf ("\n\nerror_function (data = %f, orig = %f, margin = %f) -> %f\n", data, orig, margin, error) ;
		return 1 ;
		} ;
	return 0 ;
} /* error_function */

static void
smoothed_diff_short (short *data, unsigned int datalen)
{	unsigned int k ;
	double memory = 0.0 ;

	/* Calculate the smoothed sample-to-sample difference. */
	for (k = 0 ; k < datalen - 1 ; k++)
	{	memory = 0.7 * memory + (1 - 0.7) * (double) (data [k+1] - data [k]) ;
		data [k] = (short) memory ;
		} ;
	data [datalen-1] = data [datalen-2] ;

} /* smoothed_diff_short */

static void
smoothed_diff_int (int *data, unsigned int datalen)
{	unsigned int k ;
	double memory = 0.0 ;

	/* Calculate the smoothed sample-to-sample difference. */
	for (k = 0 ; k < datalen - 1 ; k++)
	{	memory = 0.7 * memory + (1 - 0.7) * (double) (data [k+1] - data [k]) ;
		data [k] = (int) memory ;
		} ;
	data [datalen-1] = data [datalen-2] ;

} /* smoothed_diff_int */

static	void
smoothed_diff_float (float *data, unsigned int datalen)
{	unsigned int k ;
	float memory = 0.0 ;

	/* Calculate the smoothed sample-to-sample difference. */
	for (k = 0 ; k < datalen - 1 ; k++)
	{	memory = 0.7 * memory + (1 - 0.7) * (data [k+1] - data [k]) ;
		data [k] = memory ;
		} ;
	data [datalen-1] = data [datalen-2] ;

} /* smoothed_diff_float */

static	void
smoothed_diff_double (double *data, unsigned int datalen)
{	unsigned int k ;
	double memory = 0.0 ;

	/* Calculate the smoothed sample-to-sample difference. */
	for (k = 0 ; k < datalen - 1 ; k++)
	{	memory = 0.7 * memory + (1 - 0.7) * (data [k+1] - data [k]) ;
		data [k] = memory ;
		} ;
	data [datalen-1] = data [datalen-2] ;

} /* smoothed_diff_double */

static void
check_comment (SNDFILE * file, int format, int lineno)
{	const char		*comment ;

	switch (format & SF_FORMAT_TYPEMASK)
	{	case SF_FORMAT_AIFF :
		case SF_FORMAT_WAV :
		case SF_FORMAT_WAVEX :
			break ;
		default :
			return ;
		} ;

	comment = sf_get_string (file, SF_STR_COMMENT) ;
	if (comment == NULL)
	{	printf ("\n\nLine %d : File does not contain a comment string.\n\n", lineno) ;
		exit (1) ;
		} ;

	if (strcmp (comment, long_comment) != 0)
	{	printf ("\n\nLine %d : File comment does not match comment written.\n\n", lineno) ;
		exit (1) ;
		} ;

	return ;
} /* check_comment */

static int
is_lossy (int filetype)
{
	switch (SF_FORMAT_SUBMASK & filetype)
	{	case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_S8 :
		case SF_FORMAT_PCM_16 :
		case SF_FORMAT_PCM_24 :
		case SF_FORMAT_PCM_32 :
		case SF_FORMAT_FLOAT :
		case SF_FORMAT_DOUBLE :
			return 0 ;

		default :
			break ;
		} ;

	return 1 ;
} /* is_lossy */

