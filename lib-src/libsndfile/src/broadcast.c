/*
** Copyright (C) 2006-2016 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2006 Paul Davis <paul@linuxaudiosystems.com>
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
#include <stddef.h>
#include <string.h>

#include "common.h"


static int gen_coding_history (char * added_history, int added_history_max, const SF_INFO * psfinfo) ;

static inline size_t
bc_min_size (const SF_BROADCAST_INFO* info)
{	if (info == NULL)
		return 0 ;

	return offsetof (SF_BROADCAST_INFO, coding_history) + info->coding_history_size ;
} /* bc_min_size */

SF_BROADCAST_INFO_16K*
broadcast_var_alloc (void)
{	return calloc (1, sizeof (SF_BROADCAST_INFO_16K)) ;
} /* broadcast_var_alloc */

int
broadcast_var_set (SF_PRIVATE *psf, const SF_BROADCAST_INFO * info, size_t datasize)
{	size_t len ;

	if (info == NULL)
		return SF_FALSE ;

	if (bc_min_size (info) > datasize)
	{	psf->error = SFE_BAD_BROADCAST_INFO_SIZE ;
		return SF_FALSE ;
		} ;

	if (datasize >= sizeof (SF_BROADCAST_INFO_16K))
	{	psf->error = SFE_BAD_BROADCAST_INFO_TOO_BIG ;
		return SF_FALSE ;
		} ;

	if (psf->broadcast_16k == NULL)
	{	if ((psf->broadcast_16k = broadcast_var_alloc ()) == NULL)
		{	psf->error = SFE_MALLOC_FAILED ;
			return SF_FALSE ;
			} ;
		} ;

	/* Only copy the first part of the struct. */
	memcpy (psf->broadcast_16k, info, offsetof (SF_BROADCAST_INFO, coding_history)) ;

	psf_strlcpy_crlf (psf->broadcast_16k->coding_history, info->coding_history, sizeof (psf->broadcast_16k->coding_history), datasize - offsetof (SF_BROADCAST_INFO, coding_history)) ;
	len = strlen (psf->broadcast_16k->coding_history) ;

	if (len > 0 && psf->broadcast_16k->coding_history [len - 1] != '\n')
		psf_strlcat (psf->broadcast_16k->coding_history, sizeof (psf->broadcast_16k->coding_history), "\r\n") ;

	if (psf->file.mode == SFM_WRITE)
	{	char added_history [256] ;

		gen_coding_history (added_history, sizeof (added_history), &(psf->sf)) ;
		psf_strlcat (psf->broadcast_16k->coding_history, sizeof (psf->broadcast_16k->coding_history), added_history) ;
		} ;

	/* Force coding_history_size to be even. */
	len = strlen (psf->broadcast_16k->coding_history) ;
	len += (len & 1) ? 1 : 0 ;
	psf->broadcast_16k->coding_history_size = len ;

	/* Currently writing this version. */
	psf->broadcast_16k->version = 2 ;

	return SF_TRUE ;
} /* broadcast_var_set */


int
broadcast_var_get (SF_PRIVATE *psf, SF_BROADCAST_INFO * data, size_t datasize)
{	size_t size ;

	if (psf->broadcast_16k == NULL)
		return SF_FALSE ;

	size = SF_MIN (datasize, bc_min_size ((const SF_BROADCAST_INFO *) psf->broadcast_16k)) ;

	memcpy (data, psf->broadcast_16k, size) ;

	return SF_TRUE ;
} /* broadcast_var_get */

/*------------------------------------------------------------------------------
*/

static int
gen_coding_history (char * added_history, int added_history_max, const SF_INFO * psfinfo)
{	char chnstr [16] ;
	int count, width ;

	/*
	**	From : http://www.sr.se/utveckling/tu/bwf/docs/codhist2.htm
	**
	**	Parameter            Variable string <allowed option>                 Unit
	**	==========================================================================================
	**	Coding Algorithm     A=<ANALOGUE, PCM, MPEG1L1, MPEG1L2, MPEG1L3,
	**	                    MPEG2L1, MPEG2L2, MPEG2L3>
	**	Sampling frequency   F=<11000,22050,24000,32000,44100,48000>          [Hz]
	**	Bit-rate             B=<any bit-rate allowed in MPEG 2 (ISO/IEC       [kbit/s per channel]
	**	                    13818-3)>
	**	Word Length          W=<8, 12, 14, 16, 18, 20, 22, 24>                [bits]
	**	Mode                 M=<mono, stereo, dual-mono, joint-stereo>
	**	Text, free string    T=<a free ASCII-text string for in house use.
	**	                    This string should contain no commas (ASCII
	**	                    2Chex). Examples of the contents: ID-No; codec
	**	                    type; A/D type>
	*/

	switch (psfinfo->channels)
	{	case 0 :
			return SF_FALSE ;

		case 1 :
			psf_strlcpy (chnstr, sizeof (chnstr), "mono") ;
			break ;

		case 2 :
			psf_strlcpy (chnstr, sizeof (chnstr), "stereo") ;
			break ;

		default :
			snprintf (chnstr, sizeof (chnstr), "%uchn", psfinfo->channels) ;
			break ;
		} ;

	switch (SF_CODEC (psfinfo->format))
	{	case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_S8 :
			width = 8 ;
			break ;
		case SF_FORMAT_PCM_16 :
			width = 16 ;
			break ;
		case SF_FORMAT_PCM_24 :
			width = 24 ;
			break ;
		case SF_FORMAT_PCM_32 :
			width = 32 ;
			break ;
		case SF_FORMAT_FLOAT :
			width = 24 ; /* Bits in the mantissa + 1 */
			break ;
		case SF_FORMAT_DOUBLE :
			width = 53 ; /* Bits in the mantissa + 1 */
			break ;
		case SF_FORMAT_ULAW :
		case SF_FORMAT_ALAW :
			width = 12 ;
			break ;
		default :
			width = 42 ;
			break ;
		} ;

	count = snprintf (added_history, added_history_max,
							"A=PCM,F=%u,W=%d,M=%s,T=%s-%s\r\n",
							psfinfo->samplerate, width, chnstr, PACKAGE_NAME, PACKAGE_VERSION) ;

	if (count >= added_history_max)
		return 0 ;

	return count ;
} /* gen_coding_history */
