/*
** Copyright (C) 2002-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include	<math.h>

#include	"dft_cmp.h"
#include	"utils.h"

#ifndef		M_PI
#define		M_PI		3.14159265358979323846264338
#endif

#define		DFT_SPEC_LENGTH		(DFT_DATA_LENGTH / 2)

static void	dft_magnitude (const double *data, double *spectrum) ;
static double calc_max_spectral_difference (const double *spec1, const double *spec2) ;

/*--------------------------------------------------------------------------------
**	Public functions.
*/

double
dft_cmp_float (int linenum, const float *in_data, const float *test_data, int len, double target_snr, int allow_exit)
{	static double orig [DFT_DATA_LENGTH] ;
	static double test [DFT_DATA_LENGTH] ;
	unsigned	k ;

	if (len != DFT_DATA_LENGTH)
	{	printf ("Error (line %d) : dft_cmp_float : Bad input array length.\n", linenum) ;
		return 1 ;
		} ;

	for (k = 0 ; k < ARRAY_LEN (orig) ; k++)
	{	test [k] = test_data [k] ;
		orig [k] = in_data [k] ;
		} ;

	return dft_cmp_double (linenum, orig, test, len, target_snr, allow_exit) ;
} /* dft_cmp_float */

double
dft_cmp_double (int linenum, const double *orig, const double *test, int len, double target_snr, int allow_exit)
{	static double orig_spec [DFT_SPEC_LENGTH] ;
	static double test_spec [DFT_SPEC_LENGTH] ;
	double		snr ;

	if (! orig || ! test)
	{	printf ("Error (line %d) : dft_cmp_double : Bad input arrays.\n", linenum) ;
		return 1 ;
		} ;

	if (len != DFT_DATA_LENGTH)
	{	printf ("Error (line %d) : dft_cmp_double : Bad input array length.\n", linenum) ;
		return 1 ;
		} ;

	dft_magnitude (orig, orig_spec) ;
	dft_magnitude (test, test_spec) ;

	snr = calc_max_spectral_difference (orig_spec, test_spec) ;

	if (snr > target_snr)
	{	printf ("\n\nLine %d: Actual SNR (% 4.1f) > target SNR (% 4.1f).\n\n", linenum, snr, target_snr) ;
		oct_save_double	(orig, test, len) ;
		if (allow_exit)
			exit (1) ;
		} ;

	if (snr < -500.0)
		snr = -500.0 ;

	return snr ;
} /* dft_cmp_double */

/*--------------------------------------------------------------------------------
**	Quick dirty calculation of magnitude spectrum for real valued data using
**	Discrete Fourier Transform. Since the data is real, the DFT is only
**	calculated for positive frequencies.
*/

static void
dft_magnitude (const double *data, double *spectrum)
{	static double cos_angle [DFT_DATA_LENGTH] = { 0.0 } ;
	static double sin_angle [DFT_DATA_LENGTH] ;

	double	real_part, imag_part ;
	int		k, n ;

	/* If sine and cosine tables haven't been initialised, do so. */
	if (cos_angle [0] == 0.0)
		for (n = 0 ; n < DFT_DATA_LENGTH ; n++)
		{	cos_angle [n] = cos (2.0 * M_PI * n / DFT_DATA_LENGTH) ;
			sin_angle [n] = -1.0 * sin (2.0 * M_PI * n / DFT_DATA_LENGTH) ;
			} ;

	/* DFT proper. Since the data is real, only generate a half spectrum. */
	for (k = 1 ; k < DFT_SPEC_LENGTH ; k++)
	{	real_part = 0.0 ;
		imag_part = 0.0 ;

		for (n = 0 ; n < DFT_DATA_LENGTH ; n++)
		{	real_part += data [n] * cos_angle [(k * n) % DFT_DATA_LENGTH] ;
			imag_part += data [n] * sin_angle [(k * n) % DFT_DATA_LENGTH] ;
			} ;

		spectrum [k] = sqrt (real_part * real_part + imag_part * imag_part) ;
		} ;

	spectrum [k] = 0.0 ;

	spectrum [0] = spectrum [1] = spectrum [2] = spectrum [3] = spectrum [4] = 0.0 ;

	return ;
} /* dft_magnitude */

static double
calc_max_spectral_difference (const double *orig, const double *test)
{	double orig_max = 0.0, max_diff = 0.0 ;
	int	k ;

	for (k = 0 ; k < DFT_SPEC_LENGTH ; k++)
	{	if (orig_max < orig [k])
			orig_max = orig [k] ;
		if (max_diff < fabs (orig [k] - test [k]))
			max_diff = fabs (orig [k] - test [k]) ;
		} ;

	if (max_diff < 1e-25)
		return -500.0 ;

	return 20.0 * log10 (max_diff / orig_max) ;
} /* calc_max_spectral_difference */
