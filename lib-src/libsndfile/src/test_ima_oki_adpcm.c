/*
** Copyright (C) 2007-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (c) 2007 <robs@users.sourceforge.net>
**
** This library is free software; you can redistribute it and/or modify it
** under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2 of the License, or (at
** your option) any later version.
**
** This library is distributed in the hope that it will be useful, but
** WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
** General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this library.  If not, write to the Free Software Foundation,
** Fifth Floor, 51 Franklin Street, Boston, MA 02111-1301, USA.
*/

#include "sfconfig.h"

#include <stdio.h>

#include "test_main.h"

#include "ima_oki_adpcm.c"

static const unsigned char test_codes [] =
{	0x08, 0x08, 0x04, 0x7f, 0x72, 0xf7, 0x9f, 0x7c, 0xd7, 0xbc, 0x7a, 0xa7, 0xb8,
	0x4b, 0x0b, 0x38, 0xf6, 0x9d, 0x7a, 0xd7, 0xbc, 0x7a, 0xd7, 0xa8, 0x6c, 0x81,
	0x98, 0xe4, 0x0e, 0x7a, 0xd7, 0x9e, 0x7b, 0xc7, 0xab, 0x7a, 0x85, 0xc0, 0xb3,
	0x8f, 0x58, 0xd7, 0xad, 0x7a, 0xd7, 0xad, 0x7a, 0x87, 0xd0, 0x2b, 0x0e, 0x48,
	0xd7, 0xad, 0x78, 0xf7, 0xbc, 0x7a, 0xb7, 0xa8, 0x4b, 0x88, 0x18, 0xd5, 0x8d,
	0x6a, 0xa4, 0x98, 0x08, 0x00, 0x80, 0x88,
} ;

static const short test_pcm [] =
{	32, 0, 32, 0, 32, 320, 880, -336, 2304, 4192, -992, 10128, 5360, -16352,
	30208, 2272, -31872, 14688, -7040, -32432, 14128, -1392, -15488, 22960,
	1232, -1584, 21488, -240, 2576, -15360, 960, -1152, -30032, 10320, 1008,
	-30032, 16528, 1008, -30032, 16528, -5200, -30592, 15968, 448, -30592,
	15968, 448, -2368, 30960, 3024, -80, 8384, 704, -1616, -29168, -1232, 1872,
	-32768, 13792, -1728, -32768, 13792, 4480, -32192, 14368, -7360, -32752,
	13808, -1712, -21456, 16992, 1472, -1344, 26848, -1088, 2016, -17728, 208,
	-2112, -32768, 1376, -1728, -32768, 13792, -1728, -32768, 13792, -1728,
	-32768, 13792, -1728, -32768, 13792, -1728, -4544, 32767, -1377, 1727,
	15823, -2113, 207, -27345, 591, -2513, -32768, 13792, -1728, -32768, 13792,
	10688, -31632, 14928, -6800, -32192, 14368, -1152, -20896, 17552, 2032,
	-784, 22288, 560, -2256, -4816, 2176, 64, -21120, 9920, 6816, -24224, 16128,
	608, -13488, 9584, 272, -2544, 16, -2304, -192, 1728, -16, 1568, 128, -1184,
} ;


static void
test_oki_adpcm (void)
{
	IMA_OKI_ADPCM adpcm ;
	unsigned char code ;
	int i, j ;

	print_test_name ("Testing ima/oki encoder") ;

	ima_oki_adpcm_init (&adpcm, IMA_OKI_ADPCM_TYPE_OKI) ;
	for (i = 0 ; i < ARRAY_LEN (test_codes) ; i++)
		for (j = 0, code = test_codes [i] ; j < 2 ; j++, code <<= 4)
			if (adpcm_decode (&adpcm, code >> 4) != test_pcm [2 * i + j])
			{	printf ("\n\nFail at i = %d, j = %d.\n\n", i, j) ;
				exit (1) ;
				} ;

	puts ("ok") ;

	print_test_name ("Testing ima/oki decoder") ;

	ima_oki_adpcm_init (&adpcm, IMA_OKI_ADPCM_TYPE_OKI) ;
	for (i = 0 ; i < ARRAY_LEN (test_pcm) - 1 ; i += 2)
	{	code = adpcm_encode (&adpcm, test_pcm [i]) ;
		code = (code << 4) | adpcm_encode (&adpcm, test_pcm [i + 1]) ;
		if (code != test_codes [i / 2])
			{	printf ("\n\nFail at i = %d, %d should be %d\n\n", i, code, test_codes [i / 2]) ;
				exit (1) ;
				} ;
		} ;

	puts ("ok") ;
} /* test_oki_adpcm */

static void
test_oki_adpcm_block (void)
{
	IMA_OKI_ADPCM adpcm ;
	int k ;

	if (ARRAY_LEN (adpcm.pcm) < ARRAY_LEN (test_pcm))
	{	printf ("\n\nLine %d : ARRAY_LEN (adpcm->pcm) > ARRAY_LEN (test_pcm) (%d > %d).\n\n", __LINE__, ARRAY_LEN (adpcm.pcm), ARRAY_LEN (test_pcm)) ;
		exit (1) ;
		} ;

	if (ARRAY_LEN (adpcm.codes) < ARRAY_LEN (test_codes))
	{	printf ("\n\nLine %d : ARRAY_LEN (adcodes->codes) > ARRAY_LEN (test_codes).n", __LINE__) ;
		exit (1) ;
		} ;

	print_test_name ("Testing ima/oki block encoder") ;

	ima_oki_adpcm_init (&adpcm, IMA_OKI_ADPCM_TYPE_OKI) ;

	memcpy (adpcm.pcm, test_pcm, sizeof (adpcm.pcm [0]) * ARRAY_LEN (test_pcm)) ;
	adpcm.pcm_count = ARRAY_LEN (test_pcm) ;
	adpcm.code_count = 13 ;

	ima_oki_adpcm_encode_block (&adpcm) ;

	if (adpcm.code_count * 2 != ARRAY_LEN (test_pcm))
	{	printf ("\n\nLine %d : %d * 2 != %d\n\n", __LINE__, adpcm.code_count * 2, ARRAY_LEN (test_pcm)) ;
		exit (1) ;
		} ;

	for (k = 0 ; k < ARRAY_LEN (test_codes) ; k++)
		if (adpcm.codes [k] != test_codes [k])
		{	printf ("\n\nLine %d : Fail at k = %d, %d should be %d\n\n", __LINE__, k, adpcm.codes [k], test_codes [k]) ;
			exit (1) ;
			} ;

	puts ("ok") ;

	print_test_name ("Testing ima/oki block decoder") ;

	ima_oki_adpcm_init (&adpcm, IMA_OKI_ADPCM_TYPE_OKI) ;

	memcpy (adpcm.codes, test_codes, sizeof (adpcm.codes [0]) * ARRAY_LEN (test_codes)) ;
	adpcm.code_count = ARRAY_LEN (test_codes) ;
	adpcm.pcm_count = 13 ;

	ima_oki_adpcm_decode_block (&adpcm) ;

	if (adpcm.pcm_count != 2 * ARRAY_LEN (test_codes))
	{	printf ("\n\nLine %d : %d * 2 != %d\n\n", __LINE__, adpcm.pcm_count, 2 * ARRAY_LEN (test_codes)) ;
		exit (1) ;
		} ;

	for (k = 0 ; k < ARRAY_LEN (test_pcm) ; k++)
		if (adpcm.pcm [k] != test_pcm [k])
		{	printf ("\n\nLine %d : Fail at i = %d, %d should be %d.\n\n", __LINE__, k, adpcm.pcm [k], test_pcm [k]) ;
			exit (1) ;
			} ;

	puts ("ok") ;
} /* test_oki_adpcm_block */

void
test_ima_oki_adpcm (void)
{
	test_oki_adpcm () ;
	test_oki_adpcm_block () ;
} /* main */

