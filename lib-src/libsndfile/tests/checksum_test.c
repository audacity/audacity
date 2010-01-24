/*
** Copyright (C) 2008-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include <sndfile.h>

#include "utils.h"

#define	SAMPLE_RATE	8000

typedef struct
{	int	enc_fmt ;

	const char * enc_name ;
	const char * dec_name ;

	uint64_t	enc_cksum ;
	uint64_t	dec_cksum ;
} CHECKSUM ;

static CHECKSUM
checksum_orig [] =
{
	{	SF_FORMAT_RAW | SF_FORMAT_ULAW,
		"checksum.ulaw", "cksum_ulaw.pcm16",
		0x33aefae029e0c888LL, 0x595cd6e47edd0cffLL
		},
	{	SF_FORMAT_RAW | SF_FORMAT_ALAW,
		"checksum.alaw", "cksum_alaw.pcm16",
		0x48c798da3572d468LL, 0x6837d74869af5bb6LL
		},
	{	SF_FORMAT_RAW | SF_FORMAT_GSM610,
		"checksum.gsm", "cksum_gsm.pcm16",
		0x1b1f64ff2acf858fLL, 0x504179dbadd4bce6LL
		},
	{	SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM,
		"checksum.vox", "cksum_vox.pcm16",
		0xf1147fb3a298f4dfLL, 0xfc9c0cb8b12cb0abLL
		},
} ;

static void checksum_test (const CHECKSUM * cksum) ;

static float orig [1 << 14] ;
static short data [1 << 14] ;

int
main (void)
{	unsigned k ;

	gen_windowed_sine_float (orig, ARRAY_LEN (orig), 0.9) ;

	for (k = 0 ; k < ARRAY_LEN (checksum_orig) ; k++)
		checksum_test (&checksum_orig [k]) ;

	return 0 ;
} /* main */

/*==============================================================================
*/

static void
checksum_test (const CHECKSUM * cksum)
{	SNDFILE * file ;
	SF_INFO info ;

	print_test_name (__func__, cksum->enc_name) ;

	info.format = cksum->enc_fmt ;
	info.channels = 1 ;
	info.samplerate	= SAMPLE_RATE ;

	file = test_open_file_or_die (cksum->enc_name, SFM_WRITE, &info, 0, __LINE__) ;
	test_write_float_or_die (file, 0, orig, ARRAY_LEN (orig), __LINE__) ;
	sf_close (file) ;

	check_file_hash_or_die (cksum->enc_name, cksum->enc_cksum, __LINE__) ;
	puts ("ok") ;

	/*------------------------------------------------------------------------*/

	print_test_name (__func__, cksum->dec_name) ;

	info.format = cksum->enc_fmt ;
	info.channels = 1 ;
	info.samplerate	= SAMPLE_RATE ;

	file = test_open_file_or_die (cksum->enc_name, SFM_READ, &info, 0, __LINE__) ;
	test_read_short_or_die (file, 0, data, ARRAY_LEN (data), __LINE__) ;
	sf_close (file) ;

	info.format = SF_ENDIAN_LITTLE | SF_FORMAT_RAW | SF_FORMAT_PCM_16 ;
	info.channels = 1 ;
	info.samplerate	= SAMPLE_RATE ;

	file = test_open_file_or_die (cksum->dec_name, SFM_WRITE, &info, 0, __LINE__) ;
	test_write_short_or_die (file, 0, data, ARRAY_LEN (data), __LINE__) ;
	sf_close (file) ;

	check_file_hash_or_die (cksum->dec_name, cksum->dec_cksum, __LINE__) ;

	remove (cksum->enc_name) ;
	remove (cksum->dec_name) ;

	puts ("ok") ;
} /* checksum_test */

