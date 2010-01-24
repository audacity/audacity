/*
** Copyright (C) 2006 Paul Davis <paul@linuxaudiosystems.com>
** Copyright (C) 2006-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include <stdio.h>
#include <stddef.h>
#include <string.h>

#include "common.h"

static void strncpy_crlf (char *dest, const char *src, size_t destmax, size_t srcmax) ;
static int gen_coding_history (char * added_history, int added_history_max, const SF_INFO * psfinfo) ;

static inline size_t
bc_min_size (const SF_BROADCAST_INFO* info)
{	if (info == NULL)
		return 0 ;

	return offsetof (SF_BROADCAST_INFO, coding_history) + info->coding_history_size ;
} /* broadcast_size */


static inline size_t
bc_var_coding_hist_size (const SF_BROADCAST_VAR* var)
{	return var->size - offsetof (SF_BROADCAST_VAR, binfo.coding_history) ;
} /* broadcast_size */

SF_BROADCAST_VAR*
broadcast_var_alloc (size_t datasize)
{	SF_BROADCAST_VAR * data ;

	if ((data = calloc (1, datasize)) != NULL)
		data->size = datasize ;

	return data ;
} /* broadcast_var_alloc */


int
broadcast_var_set (SF_PRIVATE *psf, const SF_BROADCAST_INFO * info, size_t datasize)
{	char added_history [256] ;
	int added_history_len, len ;

	if (info == NULL)
		return SF_FALSE ;

	if (bc_min_size (info) > datasize)
	{	psf->error = SFE_BAD_BROADCAST_INFO_SIZE ;
		return SF_FALSE ;
		} ;

	added_history_len = gen_coding_history (added_history, sizeof (added_history), &(psf->sf)) ;

	if (psf->broadcast_var != NULL)
	{	size_t coding_hist_offset = offsetof (SF_BROADCAST_INFO, coding_history) ;

		if (psf->broadcast_var->binfo.coding_history_size + added_history_len < datasize - coding_hist_offset)
		{	free (psf->broadcast_var) ;
			psf->broadcast_var = NULL ;
			} ;
		} ;

	if (psf->broadcast_var == NULL)
	{	int size = datasize + added_history_len + 512 ;

		psf->broadcast_var = calloc (1, size) ;
		psf->broadcast_var->size = size ;
		} ;

	memcpy (&(psf->broadcast_var->binfo), info, offsetof (SF_BROADCAST_INFO, coding_history)) ;

	strncpy_crlf (psf->broadcast_var->binfo.coding_history, info->coding_history, bc_var_coding_hist_size (psf->broadcast_var), info->coding_history_size) ;
	len = strlen (psf->broadcast_var->binfo.coding_history) ;

	if (len > 0 && psf->broadcast_var->binfo.coding_history [len] != '\n')
		strncat (psf->broadcast_var->binfo.coding_history, "\r\n", 2) ;

	if (psf->mode == SFM_WRITE)
		strncat (psf->broadcast_var->binfo.coding_history, added_history, strlen (added_history)) ;

	psf->broadcast_var->binfo.coding_history_size = strlen (psf->broadcast_var->binfo.coding_history) ;

	/* Fore coding_history_size to be even. */
	psf->broadcast_var->binfo.coding_history_size += (psf->broadcast_var->binfo.coding_history_size & 1) ? 1 : 0 ;

	/* Currently writing this version. */
	psf->broadcast_var->binfo.version = 1 ;

	return SF_TRUE ;
} /* broadcast_var_set */


int
broadcast_var_get (SF_PRIVATE *psf, SF_BROADCAST_INFO * data, size_t datasize)
{	size_t size ;

	if (psf->broadcast_var == NULL)
		return SF_FALSE ;

	size = SF_MIN (datasize, bc_min_size (&(psf->broadcast_var->binfo))) ;

	memcpy (data, &(psf->broadcast_var->binfo), size) ;

	return SF_TRUE ;
} /* broadcast_var_set */

/*------------------------------------------------------------------------------
**	Strncpy which converts all line endings to CR/LF.
*/

static void
strncpy_crlf (char *dest, const char *src, size_t destmax, size_t srcmax)
{	char * destend = dest + destmax - 1 ;
	const char * srcend = src + srcmax ;

	while (dest < destend && src < srcend)
	{	if ((src [0] == '\r' && src [1] == '\n') || (src [0] == '\n' && src [1] == '\r'))
		{	*dest++ = '\r' ;
			*dest++ = '\n' ;
			src += 2 ;
			continue ;
			} ;

		if (src [0] == '\r')
		{	*dest++ = '\r' ;
			*dest++ = '\n' ;
			src += 1 ;
			continue ;
			} ;

		if (src [0] == '\n')
		{	*dest++ = '\r' ;
			*dest++ = '\n' ;
			src += 1 ;
			continue ;
			} ;

		*dest++ = *src++ ;
		} ;

	/* Make sure dest is terminated. */
	*dest = 0 ;
} /* strncpy_crlf */

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
	**	                     MPEG2L1, MPEG2L2, MPEG2L3>
	**	Sampling frequency   F=<11000,22050,24000,32000,44100,48000>          [Hz]
	**	Bit-rate             B=<any bit-rate allowed in MPEG 2 (ISO/IEC       [kbit/s per channel]
	**	                     13818-3)>
	**	Word Length          W=<8, 12, 14, 16, 18, 20, 22, 24>                [bits]
	**	Mode                 M=<mono, stereo, dual-mono, joint-stereo>
	**	Text, free string    T=<a free ASCII-text string for in house use.
	**	                     This string should contain no commas (ASCII
	**	                     2Chex). Examples of the contents: ID-No; codec
	**	                     type; A/D type>
	*/

	switch (psfinfo->channels)
	{	case 0 :
			return SF_FALSE ;

		case 1 :
			strncpy (chnstr, "mono", sizeof (chnstr)) ;
			break ;

		case 2 :
			strncpy (chnstr, "stereo", sizeof (chnstr)) ;
			break ;

		default :
			LSF_SNPRINTF (chnstr, sizeof (chnstr), "%uchn", psfinfo->channels) ;
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

	count = LSF_SNPRINTF (added_history, added_history_max,
							"A=PCM,F=%u,W=%hu,M=%s,T=%s-%s\r\n",
							psfinfo->samplerate, width, chnstr, PACKAGE, VERSION) ;

	if (count >= added_history_max)
		return 0 ;

	return count ;
} /* gen_coding_history */

