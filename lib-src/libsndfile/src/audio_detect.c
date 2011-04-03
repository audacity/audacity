/*
** Copyright (C) 1999-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/


#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <string.h>
#include <errno.h>

#include "common.h"

typedef struct
{	int le_float ;
	int be_float ;
	int le_int_24_32 ;
	int be_int_24_32 ;
} VOTE ;


static void vote_for_format (VOTE * vote, const unsigned char * data, int datalen) ;

int
audio_detect (SF_PRIVATE * psf, AUDIO_DETECT *ad, const unsigned char * data, int datalen)
{	VOTE vote ;

	if (psf == NULL)
		return 0 ;

	if (ad == NULL || datalen < 256)
		return 0 ;

	vote_for_format (&vote, data, datalen) ;

	psf_log_printf (psf, "audio_detect :\n"
			"    le_float     : %d\n"
			"    be_float     : %d\n"
			"    le_int_24_32 : %d\n"
			"    be_int_24_32 : %d\n",
			vote.le_float, vote.be_float, vote.le_int_24_32, vote.be_int_24_32) ;

	if (0) puts (psf->logbuffer) ;

	if (ad->endianness == SF_ENDIAN_LITTLE && vote.le_float > (3 * datalen) / 4)
	{	/* Almost certainly 32 bit floats. */
		return SF_FORMAT_FLOAT ;
		} ;

	if (ad->endianness == SF_ENDIAN_LITTLE && vote.le_int_24_32 > (3 * datalen) / 4)
	{	/* Almost certainly 24 bit data stored in 32 bit ints. */
		return SF_FORMAT_PCM_32 ;
		} ;

	return 0 ;
} /* data_detect */

static void
vote_for_format (VOTE * vote, const unsigned char * data, int datalen)
{
	int k ;

	memset (vote, 0, sizeof (VOTE)) ;

	datalen -= datalen % 4 ;

	for (k = 0 ; k < datalen ; k ++)
	{	if ((k % 4) == 0)
		{	if (data [k] == 0 && data [k + 1] != 0)
				vote->le_int_24_32 += 4 ;

			if (data [2] != 0 && data [3] == 0)
				vote->le_int_24_32 += 4 ;

			if (data [0] != 0 && data [3] > 0x43 && data [3] < 0x4B)
				vote->le_float += 4 ;

			if (data [3] != 0 && data [0] > 0x43 && data [0] < 0x4B)
				vote->be_float += 4 ;
			} ;
		} ;

	return ;
} /* vote_for_format */

