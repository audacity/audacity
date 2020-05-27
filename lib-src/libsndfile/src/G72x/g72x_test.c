/*
** Copyright (C) 1999-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#if HAVE_UNISTD_H
#include <unistd.h>
#else
#include "sf_unistd.h"
#endif
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "g72x.h"
#include "g72x_priv.h"

#ifndef		M_PI
#define		M_PI		3.14159265358979323846264338
#endif

#define		BUFFER_SIZE		(1 << 14)
#define		SAMPLE_RATE		11025


static void g721_test	(void) ;
static void g723_test	(double margin) ;

static void	gen_signal_double (double *data, double scale, int datalen) ;
static int error_function (double data, double orig, double margin) ;

static int	oct_save_short	(short *a, short *b, int len) ;

int
main (int argc, char *argv [])
{	int		bDoAll = 0 ;
	int		nTests = 0 ;

	if (argc != 2)
	{	printf ("Usage : %s <test>\n", argv [0]) ;
		printf ("    Where <test> is one of the following:\n") ;
		printf ("           g721  - test G721 encoder and decoder\n") ;
		printf ("           g723  - test G721 encoder and decoder\n") ;
		printf ("           all   - perform all tests\n") ;
		exit (1) ;
		} ;

	bDoAll = !strcmp (argv [1], "all") ;

	if (bDoAll || ! strcmp (argv [1], "g721"))
	{	g721_test	() ;
		nTests++ ;
		} ;

	if (bDoAll || ! strcmp (argv [1], "g723"))
	{	g723_test	(0.53) ;
		nTests++ ;
		} ;

	if (nTests == 0)
	{	printf ("Mono : ************************************\n") ;
		printf ("Mono : *  No '%s' test defined.\n", argv [1]) ;
		printf ("Mono : ************************************\n") ;
		return 1 ;
		} ;

	return 0 ;
} /* main */

static void
g721_test	(void)
{
	return ;
} /* g721_test */

static void
g723_test	(double margin)
{	static double	orig_buffer [BUFFER_SIZE] ;
	static short 	orig [BUFFER_SIZE] ;
	static short 	data [BUFFER_SIZE] ;

	G72x_STATE encoder_state, decoder_state ;

	long	k ;
	int 	code, position, max_err ;

	private_init_state (&encoder_state) ;
	encoder_state.encoder = g723_24_encoder ;
	encoder_state.codec_bits = 3 ;

	private_init_state (&decoder_state) ;
	decoder_state.decoder = g723_24_decoder ;
	decoder_state.codec_bits = 3 ;

	memset (data, 0, BUFFER_SIZE * sizeof (short)) ;
	memset (orig, 0, BUFFER_SIZE * sizeof (short)) ;

	printf ("    g723_test    : ") ;
	fflush (stdout) ;

	gen_signal_double (orig_buffer, 32000.0, BUFFER_SIZE) ;
	for (k = 0 ; k < BUFFER_SIZE ; k++)
		orig [k] = (short) orig_buffer [k] ;

	/* Write and read data here. */
	position = 0 ;
	max_err = 0 ;
	for (k = 0 ; k < BUFFER_SIZE ; k++)
	{	code = encoder_state.encoder (orig [k], &encoder_state) ;
		data [k] = decoder_state.decoder (code, &decoder_state) ;
		if (abs (orig [k] - data [k]) > max_err)
		{	position = k ;
			max_err = abs (orig [k] - data [k]) ;
			} ;
		} ;

	printf ("\n\nMax error of %d at postion %d.\n", max_err, position) ;

	for (k = 0 ; k < BUFFER_SIZE ; k++)
	{	if (error_function (data [k], orig [k], margin))
		{	printf ("Line %d: Incorrect sample A (#%ld : %d should be %d).\n", __LINE__, k, data [k], orig [k]) ;
			oct_save_short (orig, data, BUFFER_SIZE) ;
			exit (1) ;
			} ;
		} ;


	printf ("ok\n") ;

	return ;
} /* g723_test */


#define		SIGNAL_MAXVAL	30000.0
#define		DECAY_COUNT		1000

static void
gen_signal_double (double *gendata, double scale, int gendatalen)
{	int		k, ramplen ;
	double	amp = 0.0 ;

	ramplen = DECAY_COUNT ;

	for (k = 0 ; k < gendatalen ; k++)
	{	if (k <= ramplen)
			amp = scale * k / ((double) ramplen) ;
		else if (k > gendatalen - ramplen)
			amp = scale * (gendatalen - k) / ((double) ramplen) ;

		gendata [k] = amp * (0.4 * sin (33.3 * 2.0 * M_PI * ((double) (k+1)) / ((double) SAMPLE_RATE))
						+ 0.3 * cos (201.1 * 2.0 * M_PI * ((double) (k+1)) / ((double) SAMPLE_RATE))) ;
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

static int
oct_save_short	(short *a, short *b, int len)
{	FILE 	*file ;
	int		k ;

	if (! (file = fopen ("error.dat", "w")))
		return 1 ;

	fprintf (file, "# Not created by Octave\n") ;

	fprintf (file, "# name: a\n") ;
	fprintf (file, "# type: matrix\n") ;
	fprintf (file, "# rows: %d\n", len) ;
	fprintf (file, "# columns: 1\n") ;

	for (k = 0 ; k < len ; k++)
		fprintf (file, "% d\n", a [k]) ;

	fprintf (file, "# name: b\n") ;
	fprintf (file, "# type: matrix\n") ;
	fprintf (file, "# rows: %d\n", len) ;
	fprintf (file, "# columns: 1\n") ;

	for (k = 0 ; k < len ; k++)
		fprintf (file, "% d\n", b [k]) ;

	fclose (file) ;
	return 0 ;
} /* oct_save_short */

