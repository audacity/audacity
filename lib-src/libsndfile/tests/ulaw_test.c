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

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <sndfile.h>

#include "utils.h"

#define	BUFFER_SIZE		(65536)

static unsigned char	ulaw_encode (int sample) ;
static int				ulaw_decode (unsigned int ulawbyte) ;

static	short			short_buffer [BUFFER_SIZE] ;
static	unsigned char	ulaw_buffer [BUFFER_SIZE] ;

int
main (void)
{	SNDFILE		*file ;
	SF_INFO 	sfinfo ;
	const char	*filename ;
	int			k ;

	filename = "test.raw" ;

	sfinfo.format		= SF_FORMAT_RAW | SF_FORMAT_ULAW ;
	sfinfo.samplerate	= 44100 ;
	sfinfo.frames		= 123456789 ;
	sfinfo.channels		= 1 ;

	if ((file = sf_open (filename, SFM_WRITE, &sfinfo)) == NULL)
	{	printf ("sf_open_write failed with error : ") ;
		fflush (stdout) ;
		puts (sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	/* Generate a file containing all possible 16 bit sample values
	** and write it to disk as ulaw encoded.frames.
	*/

	for (k = 0 ; k < 0x10000 ; k++)
		short_buffer [k] = k & 0xFFFF ;

	sf_write_short (file, short_buffer, BUFFER_SIZE) ;
	sf_close (file) ;

	/* Now open that file and compare the ulaw encoded sample values
	** with what they should be.
	*/

	if ((file = sf_open (filename, SFM_READ, &sfinfo)) == NULL)
	{	printf ("sf_open_write failed with error : ") ;
		puts (sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	check_log_buffer_or_die (file, __LINE__) ;

	if (sf_read_raw (file, ulaw_buffer, BUFFER_SIZE) != BUFFER_SIZE)
	{	printf ("sf_read_raw : ") ;
		puts (sf_strerror (file)) ;
		exit (1) ;
		} ;

	for (k = 0 ; k < 0x10000 ; k++)
		if (ulaw_encode (short_buffer [k]) != ulaw_buffer [k])
		{	printf ("Encoder error : sample #%d (0x%02X should be 0x%02X)\n", k, ulaw_buffer [k], ulaw_encode (short_buffer [k])) ;
			exit (1) ;
			} ;

	sf_close (file) ;

	printf ("    ulaw_test : encoder ... ok\n") ;

	/* Now generate a file containing all possible 8 bit encoded
	** sample values and write it to disk as ulaw encoded.frames.
	*/

	if (! (file = sf_open (filename, SFM_WRITE, &sfinfo)))
	{	printf ("sf_open_write failed with error : ") ;
		puts (sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	for (k = 0 ; k < 256 ; k++)
		ulaw_buffer [k] = k & 0xFF ;

	sf_write_raw (file, ulaw_buffer, 256) ;
	sf_close (file) ;

	/* Now open that file and compare the ulaw decoded sample values
	** with what they should be.
	*/

	if (! (file = sf_open (filename, SFM_READ, &sfinfo)))
	{	printf ("sf_open_write failed with error : ") ;
		puts (sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	check_log_buffer_or_die (file, __LINE__) ;

	if (sf_read_short (file, short_buffer, 256) != 256)
	{	printf ("sf_read_short : ") ;
		puts (sf_strerror (file)) ;
		exit (1) ;
		} ;


	for (k = 0 ; k < 256 ; k++)
		if (short_buffer [k] != ulaw_decode (ulaw_buffer [k]))
		{	printf ("Decoder error : sample #%d (0x%04X should be 0x%04X)\n", k, short_buffer [k], ulaw_decode (ulaw_buffer [k])) ;
			exit (1) ;
			} ;

	sf_close (file) ;

	printf ("    ulaw_test : decoder ... ok\n") ;

	unlink (filename) ;

	return 0 ;
} /* main */


/*=================================================================================
**	The following routines came from the sox-12.15 (Sound eXcahcnge) distribution.
**
**	This code is not compiled into libsndfile. It is only used to test the
**	libsndfile lookup tables for correctness.
**
**	I have included the original authors comments.
*/

/*
** This routine converts from linear to ulaw.
**
** Craig Reese: IDA/Supercomputing Research Center
** Joe Campbell: Department of Defense
** 29 September 1989
**
** References:
** 1) CCITT Recommendation G.711  (very difficult to follow)
** 2) "A New Digital Technique for Implementation of Any
**     Continuous PCM Companding Law," Villeret, Michel,
**     et al. 1973 IEEE Int. Conf. on Communications, Vol 1,
**     1973, pg. 11.12-11.17
** 3) MIL-STD-188-113,"Interoperability and Performance Standards
**     for Analog-to_Digital Conversion Techniques,"
**     17 February 1987
**
** Input: Signed 16 bit linear sample
** Output: 8 bit ulaw sample
*/

#define uBIAS 0x84		/* define the add-in bias for 16 bit.frames */
#define uCLIP 32635

static
unsigned char ulaw_encode (int sample)
{	static int exp_lut [256] =
	{	0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
		4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
		5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
		5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
		6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
		6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
		6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
		6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
		} ;

    int sign, exponent, mantissa ;
    unsigned char ulawbyte ;

    /* Get the sample into sign-magnitude. */
    sign = (sample >> 8) & 0x80 ;					/* set aside the sign */
    if ( sign != 0 )
		sample = -sample ;							/* get magnitude */
    if ( sample > uCLIP )
		sample = uCLIP ;							/* clip the magnitude */

    /* Convert from 16 bit linear to ulaw. */
    sample = sample + uBIAS ;
    exponent = exp_lut [( sample >> 7 ) & 0xFF] ;
    mantissa = (sample >> ( exponent + 3 ) ) & 0x0F ;
    ulawbyte = ~ (sign | ( exponent << 4 ) | mantissa) ;

	return ulawbyte ;
} /* ulaw_encode */


/*
** This routine converts from ulaw to 16 bit linear.
**
** Craig Reese: IDA/Supercomputing Research Center
** 29 September 1989
**
** References:
** 1) CCITT Recommendation G.711  (very difficult to follow)
** 2) MIL-STD-188-113,"Interoperability and Performance Standards
**     for Analog-to_Digital Conversion Techniques,"
**     17 February 1987
**
** Input: 8 bit ulaw sample
** Output: signed 16 bit linear sample
*/

static
int ulaw_decode (unsigned int ulawbyte)
{	static int exp_lut [8] = { 0, 132, 396, 924, 1980, 4092, 8316, 16764 } ;
    int sign, exponent, mantissa, sample ;

    ulawbyte = ~ ulawbyte ;
    sign = (ulawbyte & 0x80) ;
    exponent = (ulawbyte >> 4) & 0x07 ;
    mantissa = ulawbyte & 0x0F ;
    sample = exp_lut [exponent] + (mantissa << (exponent + 3)) ;
    if (sign != 0)
		sample = -sample ;

    return sample ;
} /* ulaw_decode */

