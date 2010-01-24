/*
**	Copyright (C) 2005 Erik de Castro Lopo
**
**	This program is free software; you can redistribute it and/or modify
**	it under the terms of the GNU General Public License as published by
**	the Free Software Foundation; either version 2 of the License, or
**	(at your option) any later version.
**
**	This program is distributed in the hope that it will be useful,
**	but WITHOUT ANY WARRANTY; without even the implied warranty of
**	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**	GNU General Public License for more details.
**
**	You should have received a copy of the GNU General Public License
**	along with this program; if not, write to the Free Software
**	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/*
**	A simple checksum for short, int and float data.
*/

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <sndfile.h>

#include "regtest.h"

#define	BIG_PRIME		999983

#define	ARRAY_LEN(x)	((int) (sizeof (x)) / (sizeof ((x) [0])))

static int short_checksum (SNDFILE * file, int start) ;
static int int_checksum (SNDFILE * file, int start) ;
static int float_checksum (SNDFILE * file, int start) ;

int
calc_checksum (SNDFILE * file, const SF_INFO * info)
{	int start ;

	/* Seed the checksum with data from the SF_INFO struct. */
	start = info->samplerate ;
	start = start * BIG_PRIME + info->channels ;
	start = start * BIG_PRIME + info->format ;

	switch (info->format & SF_FORMAT_SUBMASK)
	{	case SF_FORMAT_FLOAT :
		case SF_FORMAT_DOUBLE :
			return float_checksum (file, start) ;

		case SF_FORMAT_PCM_24 :
		case SF_FORMAT_PCM_32 :
			return int_checksum (file, start) ;

		default :
			return short_checksum (file, start) ;
		} ;

	return 0 ;
} /* calc_checksum */

/*------------------------------------------------------------------------------
*/

static union
{	short	s [1 << 16] ;
	int		i [1 << 15] ;
	float	f [1 << 15] ;
} data ;

static int
short_checksum (SNDFILE * file, int start)
{	int k, count ;

	do
	{	count = (int) sf_read_short (file, data.s, ARRAY_LEN (data.s)) ;
		for (k = 0 ; k < count ; k++)
			start = start * BIG_PRIME + data.s [k] ;
		}
	while (count > 0) ;

	return start ;
} /* short_checksum */

static int
int_checksum (SNDFILE * file, int start)
{	int k, count ;

	do
	{	count = (int) sf_read_int (file, data.i, ARRAY_LEN (data.i)) ;
		for (k = 0 ; k < count ; k++)
			start = start * BIG_PRIME + data.i [k] ;
		}
	while (count > 0) ;

	return start ;
} /* int_checksum */

static int
float_checksum (SNDFILE * file, int start)
{	int k, count ;

	do
	{	count = (int) sf_read_float (file, data.f, ARRAY_LEN (data.f)) ;
		for (k = 0 ; k < count ; k++)
			start = start * BIG_PRIME + lrintf (0x7FFFFFFF * data.f [k]) ;
		}
	while (count > 0) ;

	return start ;
} /* float_checksum */

