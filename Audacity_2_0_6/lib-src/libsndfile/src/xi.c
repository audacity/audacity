/*
** Copyright (C) 2003-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "sndfile.h"
#include "sfendian.h"
#include "common.h"

#define	MAX_XI_SAMPLES	16

/*------------------------------------------------------------------------------
** Private static functions and tyepdefs.
*/

typedef struct
{	/* Warning, this filename is NOT nul terminated. */
	char	filename [22] ;
	char	software [20] ;
	char	sample_name [22] ;

	int		loop_begin, loop_end ;
	int		sample_flags ;

	/* Data for encoder and decoder. */
	short	last_16 ;
} XI_PRIVATE ;

static int	xi_close		(SF_PRIVATE *psf) ;
static int	xi_write_header (SF_PRIVATE *psf, int calc_length) ;
static int	xi_read_header	(SF_PRIVATE *psf) ;
static int	dpcm_init 		(SF_PRIVATE *psf) ;


static sf_count_t	dpcm_seek (SF_PRIVATE *psf, int mode, sf_count_t offset) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
xi_open	(SF_PRIVATE *psf)
{	XI_PRIVATE *pxi ;
	int		subformat, error = 0 ;

	if (psf->is_pipe)
		return SFE_XI_NO_PIPE ;

	if (psf->codec_data)
		pxi = psf->codec_data ;
	else if ((pxi = calloc (1, sizeof (XI_PRIVATE))) == NULL)
		return SFE_MALLOC_FAILED ;

	psf->codec_data = pxi ;

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = xi_read_header (psf)))
			return error ;
		} ;

	subformat = SF_CODEC (psf->sf.format) ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_XI)
			return	SFE_BAD_OPEN_FORMAT ;

		psf->endian = SF_ENDIAN_LITTLE ;
		psf->sf.channels = 1 ; /* Always mono */
		psf->sf.samplerate = 44100 ; /* Always */

		/* Set up default instrument and software name. */
		memcpy (pxi->filename, "Default Name            ", sizeof (pxi->filename)) ;
		memcpy (pxi->software, PACKAGE "-" VERSION "               ", sizeof (pxi->software)) ;

		memset (pxi->sample_name, 0, sizeof (pxi->sample_name)) ;
		snprintf (pxi->sample_name, sizeof (pxi->sample_name), "%s", "Sample #1") ;

		pxi->sample_flags = (subformat == SF_FORMAT_DPCM_16) ? 16 : 0 ;

		if (xi_write_header (psf, SF_FALSE))
			return psf->error ;

		psf->write_header = xi_write_header ;
		} ;

	psf->container_close = xi_close ;
	psf->seek = dpcm_seek ;

	psf->sf.seekable = SF_FALSE ;

	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

	switch (subformat)
	{	case SF_FORMAT_DPCM_8 :		/* 8-bit differential PCM. */
		case SF_FORMAT_DPCM_16 :	/* 16-bit differential PCM. */
				error = dpcm_init (psf) ;
				break ;

		default : break ;
		} ;

	return error ;
} /* xi_open */

/*------------------------------------------------------------------------------
*/

static int
xi_close	(SF_PRIVATE * UNUSED (psf))
{
	return 0 ;
} /* xi_close */

/*==============================================================================
*/

static sf_count_t dpcm_read_dsc2s (SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t dpcm_read_dsc2i (SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t dpcm_read_dsc2f (SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t dpcm_read_dsc2d (SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t dpcm_write_s2dsc (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t dpcm_write_i2dsc (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t dpcm_write_f2dsc (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t dpcm_write_d2dsc (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static sf_count_t dpcm_read_dles2s (SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t dpcm_read_dles2i (SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t dpcm_read_dles2f (SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t dpcm_read_dles2d (SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t dpcm_write_s2dles (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t dpcm_write_i2dles (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t dpcm_write_f2dles (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t dpcm_write_d2dles (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static int
dpcm_init (SF_PRIVATE *psf)
{	if (psf->bytewidth == 0 || psf->sf.channels == 0)
		return SFE_INTERNAL ;

	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

	if (psf->file.mode == SFM_READ || psf->file.mode == SFM_RDWR)
	{	switch (psf->bytewidth)
		{	case 1 :
					psf->read_short		= dpcm_read_dsc2s ;
					psf->read_int		= dpcm_read_dsc2i ;
					psf->read_float		= dpcm_read_dsc2f ;
					psf->read_double	= dpcm_read_dsc2d ;
					break ;
			case 2 :
					psf->read_short		= dpcm_read_dles2s ;
					psf->read_int		= dpcm_read_dles2i ;
					psf->read_float		= dpcm_read_dles2f ;
					psf->read_double	= dpcm_read_dles2d ;
					break ;
			default :
				psf_log_printf (psf, "dpcm_init() returning SFE_UNIMPLEMENTED\n") ;
				return SFE_UNIMPLEMENTED ;
			} ;
		} ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	switch (psf->bytewidth)
		{	case 1 :
					psf->write_short	= dpcm_write_s2dsc ;
					psf->write_int		= dpcm_write_i2dsc ;
					psf->write_float	= dpcm_write_f2dsc ;
					psf->write_double	= dpcm_write_d2dsc ;
					break ;
			case 2 :
					psf->write_short	= dpcm_write_s2dles ;
					psf->write_int		= dpcm_write_i2dles ;
					psf->write_float	= dpcm_write_f2dles ;
					psf->write_double	= dpcm_write_d2dles ;
					break ;
			default :
				psf_log_printf (psf, "dpcm_init() returning SFE_UNIMPLEMENTED\n") ;
				return SFE_UNIMPLEMENTED ;
			} ;
		} ;

	psf->filelength = psf_get_filelen (psf) ;
	psf->datalength = (psf->dataend) ? psf->dataend - psf->dataoffset :
							psf->filelength - psf->dataoffset ;
	psf->sf.frames = psf->datalength / psf->blockwidth ;

	return 0 ;
} /* dpcm_init */

/*==============================================================================
*/

static sf_count_t
dpcm_seek (SF_PRIVATE *psf, int mode, sf_count_t offset)
{	XI_PRIVATE	*pxi ;
	int			total, bufferlen, len ;

	if ((pxi = psf->codec_data) == NULL)
		return SFE_INTERNAL ;

	if (psf->datalength < 0 || psf->dataoffset < 0)
	{	psf->error = SFE_BAD_SEEK ;
		return	PSF_SEEK_ERROR ;
		} ;

	if (offset == 0)
	{	psf_fseek (psf, psf->dataoffset, SEEK_SET) ;
		pxi->last_16 = 0 ;
		return 0 ;
		} ;

	if (offset < 0 || offset > psf->sf.frames)
	{	psf->error = SFE_BAD_SEEK ;
		return	PSF_SEEK_ERROR ;
		} ;

	if (mode != SFM_READ)
	{	/* What to do about write??? */
		psf->error = SFE_BAD_SEEK ;
		return	PSF_SEEK_ERROR ;
		} ;

	psf_fseek (psf, psf->dataoffset, SEEK_SET) ;

	if ((SF_CODEC (psf->sf.format)) == SF_FORMAT_DPCM_16)
	{	total = offset ;
		bufferlen = ARRAY_LEN (psf->u.sbuf) ;
		while (total > 0)
		{	len = (total > bufferlen) ? bufferlen : total ;
			total -= dpcm_read_dles2s (psf, psf->u.sbuf, len) ;
			} ;
		}
	else
	{	total = offset ;
		bufferlen = ARRAY_LEN (psf->u.sbuf) ;
		while (total > 0)
		{	len = (total > bufferlen) ? bufferlen : total ;
			total -= dpcm_read_dsc2s (psf, psf->u.sbuf, len) ;
			} ;
		} ;

	return offset ;
} /* dpcm_seek */


static int
xi_write_header (SF_PRIVATE *psf, int UNUSED (calc_length))
{	XI_PRIVATE	*pxi ;
	sf_count_t	current ;
	const char	*string ;

	if ((pxi = psf->codec_data) == NULL)
		return SFE_INTERNAL ;

	current = psf_ftell (psf) ;

	/* Reset the current header length to zero. */
	psf->header [0] = 0 ;
	psf->headindex = 0 ;
	psf_fseek (psf, 0, SEEK_SET) ;

	string = "Extended Instrument: " ;
	psf_binheader_writef (psf, "b", string, strlen (string)) ;
	psf_binheader_writef (psf, "b1", pxi->filename, sizeof (pxi->filename), 0x1A) ;

	/* Write software version and two byte XI version. */
	psf_binheader_writef (psf, "eb2", pxi->software, sizeof (pxi->software), (1 << 8) + 2) ;

	/*
	** Jump note numbers (96), volume envelope (48), pan envelope (48),
	** volume points (1), pan points (1)
	*/
	psf_binheader_writef (psf, "z", (size_t) (96 + 48 + 48 + 1 + 1)) ;

	/* Jump volume loop (3 bytes), pan loop (3), envelope flags (3), vibrato (3)
	** fade out (2), 22 unknown bytes, and then write sample_count (2 bytes).
	*/
	psf_binheader_writef (psf, "ez2z2", (size_t) (4 * 3), 0x1234, make_size_t (22), 1) ;

	pxi->loop_begin = 0 ;
	pxi->loop_end = 0 ;

	psf_binheader_writef (psf, "et844", psf->sf.frames, pxi->loop_begin, pxi->loop_end) ;

	/* volume, fine tune, flags, pan, note, namelen */
	psf_binheader_writef (psf, "111111", 128, 0, pxi->sample_flags, 128, 0, strlen (pxi->sample_name)) ;

	psf_binheader_writef (psf, "b", pxi->sample_name, sizeof (pxi->sample_name)) ;





	/* Header construction complete so write it out. */
	psf_fwrite (psf->header, psf->headindex, 1, psf) ;

	if (psf->error)
		return psf->error ;

	psf->dataoffset = psf->headindex ;

	if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* xi_write_header */

static int
xi_read_header (SF_PRIVATE *psf)
{	char	buffer [64], name [32] ;
	short	version, fade_out, sample_count ;
	int		k, loop_begin, loop_end ;
	int 	sample_sizes [MAX_XI_SAMPLES] ;

	psf_binheader_readf (psf, "pb", 0, buffer, 21) ;

	memset (sample_sizes, 0, sizeof (sample_sizes)) ;

	buffer [20] = 0 ;
	if (strcmp (buffer, "Extended Instrument:") != 0)
		return SFE_XI_BAD_HEADER ;

	memset (buffer, 0, sizeof (buffer)) ;
	psf_binheader_readf (psf, "b", buffer, 23) ;

	if (buffer [22] != 0x1A)
		return SFE_XI_BAD_HEADER ;

	buffer [22] = 0 ;
	psf_log_printf (psf, "Extended Instrument : %s\n", buffer) ;

	psf_binheader_readf (psf, "be2", buffer, 20, &version) ;
	buffer [19] = 0 ;
	psf_log_printf (psf, "Software : %s\nVersion  : %d.%02d\n", buffer, version / 256, version % 256) ;

	/* Jump note numbers (96), volume envelope (48), pan envelope (48),
	** volume points (1), pan points (1)
	*/
	psf_binheader_readf (psf, "j", 96 + 48 + 48 + 1 + 1) ;

	psf_binheader_readf (psf, "b", buffer, 12) ;
	psf_log_printf (psf, "Volume Loop\n  sustain : %u\n  begin   : %u\n  end     : %u\n",
						buffer [0], buffer [1], buffer [2]) ;
	psf_log_printf (psf, "Pan Loop\n  sustain : %u\n  begin   : %u\n  end     : %u\n",
						buffer [3], buffer [4], buffer [5]) ;
	psf_log_printf (psf, "Envelope Flags\n  volume  : 0x%X\n  pan     : 0x%X\n",
				buffer [6] & 0xFF, buffer [7] & 0xFF) ;

	psf_log_printf (psf, "Vibrato\n  type    : %u\n  sweep   : %u\n  depth   : %u\n  rate    : %u\n",
				buffer [8], buffer [9], buffer [10], buffer [11]) ;

	/*
	** Read fade_out then jump reserved (2 bytes) and ???? (20 bytes) and
	** sample_count.
	*/
	psf_binheader_readf (psf, "e2j2", &fade_out, 2 + 20, &sample_count) ;
	psf_log_printf (psf, "Fade out  : %d\n", fade_out) ;

	/* XI file can contain up to 16 samples. */
	if (sample_count > MAX_XI_SAMPLES)
		return SFE_XI_EXCESS_SAMPLES ;

	if (psf->instrument == NULL && (psf->instrument = psf_instrument_alloc ()) == NULL)
		return SFE_MALLOC_FAILED ;

	/* Log all data for each sample. */
	for (k = 0 ; k < sample_count ; k++)
	{	psf_binheader_readf (psf, "e444", &(sample_sizes [k]), &loop_begin, &loop_end) ;

		/* Read 5 know bytes, 1 unknown byte and 22 name bytes. */
		psf_binheader_readf (psf, "bb", buffer, 6, name, 22) ;
		name [21] = 0 ;

		psf_log_printf (psf, "Sample #%d\n  name    : %s\n", k + 1, name) ;

		psf_log_printf (psf, "  size    : %d\n", sample_sizes [k]) ;



		psf_log_printf (psf, "  loop\n    begin : %d\n    end   : %d\n", loop_begin, loop_end) ;

		psf_log_printf (psf, "  volume  : %u\n  f. tune : %d\n  flags   : 0x%02X ",
					buffer [0] & 0xFF, buffer [1] & 0xFF, buffer [2] & 0xFF) ;

		psf_log_printf (psf, " (") ;
		if (buffer [2] & 1)
			psf_log_printf (psf, " Loop") ;
		if (buffer [2] & 2)
			psf_log_printf (psf, " PingPong") ;
		psf_log_printf (psf, (buffer [2] & 16) ? " 16bit" : " 8bit") ;
		psf_log_printf (psf, " )\n") ;

		psf_log_printf (psf, "  pan     : %u\n  note    : %d\n  namelen : %d\n",
					buffer [3] & 0xFF, buffer [4], buffer [5]) ;

		if (k != 0)
			continue ;

		if (buffer [2] & 16)
		{	psf->sf.format = SF_FORMAT_XI | SF_FORMAT_DPCM_16 ;
			psf->bytewidth = 2 ;
			}
		else
		{	psf->sf.format = SF_FORMAT_XI | SF_FORMAT_DPCM_8 ;
			psf->bytewidth = 1 ;
			} ;
		} ;

	while (sample_count > 1 && sample_sizes [sample_count - 1] == 0)
		sample_count -- ;

	/* Currently, we can only handle 1 sample per file. */

	if (sample_count > 2)
	{	psf_log_printf (psf, "*** Sample count is less than 16 but more than 1.\n") ;
		psf_log_printf (psf, "  sample count : %d    sample_sizes [%d] : %d\n",
						sample_count, sample_count - 1, sample_sizes [sample_count - 1]) ;
		return SFE_XI_EXCESS_SAMPLES ;
		} ;

	psf->datalength = sample_sizes [0] ;

	psf->dataoffset = psf_ftell (psf) ;
	if (psf->dataoffset < 0)
	{	psf_log_printf (psf, "*** Bad Data Offset : %D\n", psf->dataoffset) ;
		return SFE_BAD_OFFSET ;
		} ;
	psf_log_printf (psf, "Data Offset : %D\n", psf->dataoffset) ;

	if (psf->dataoffset + psf->datalength > psf->filelength)
	{	psf_log_printf (psf, "*** File seems to be truncated. Should be at least %D bytes long.\n",
				psf->dataoffset + sample_sizes [0]) ;
		psf->datalength = psf->filelength - psf->dataoffset ;
		} ;

	if (psf_fseek (psf, psf->dataoffset, SEEK_SET) != psf->dataoffset)
		return SFE_BAD_SEEK ;

	psf->endian = SF_ENDIAN_LITTLE ;
	psf->sf.channels = 1 ; /* Always mono */
	psf->sf.samplerate = 44100 ; /* Always */

	psf->blockwidth = psf->sf.channels * psf->bytewidth ;

	if (! psf->sf.frames && psf->blockwidth)
		psf->sf.frames = (psf->filelength - psf->dataoffset) / psf->blockwidth ;

	psf->instrument->basenote = 0 ;
	psf->instrument->gain = 1 ;
	psf->instrument->velocity_lo = psf->instrument->key_lo = 0 ;
	psf->instrument->velocity_hi = psf->instrument->key_hi = 127 ;

	return 0 ;
} /* xi_read_header */

/*==============================================================================
*/

static void dsc2s_array (XI_PRIVATE *pxi, signed char *src, int count, short *dest) ;
static void dsc2i_array (XI_PRIVATE *pxi, signed char *src, int count, int *dest) ;
static void dsc2f_array (XI_PRIVATE *pxi, signed char *src, int count, float *dest, float normfact) ;
static void dsc2d_array (XI_PRIVATE *pxi, signed char *src, int count, double *dest, double normfact) ;

static void dles2s_array (XI_PRIVATE *pxi, short *src, int count, short *dest) ;
static void dles2i_array (XI_PRIVATE *pxi, short *src, int count, int *dest) ;
static void dles2f_array (XI_PRIVATE *pxi, short *src, int count, float *dest, float normfact) ;
static void dles2d_array (XI_PRIVATE *pxi, short *src, int count, double *dest, double normfact) ;

static sf_count_t
dpcm_read_dsc2s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		dsc2s_array (pxi, psf->u.scbuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* dpcm_read_dsc2s */

static sf_count_t
dpcm_read_dsc2i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		dsc2i_array (pxi, psf->u.scbuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* dpcm_read_dsc2i */

static sf_count_t
dpcm_read_dsc2f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x80) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		dsc2f_array (pxi, psf->u.scbuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* dpcm_read_dsc2f */

static sf_count_t
dpcm_read_dsc2d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		normfact ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x80) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		dsc2d_array (pxi, psf->u.scbuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* dpcm_read_dsc2d */

/*------------------------------------------------------------------------------
*/

static sf_count_t
dpcm_read_dles2s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		dles2s_array (pxi, psf->u.sbuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* dpcm_read_dles2s */

static sf_count_t
dpcm_read_dles2i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		dles2i_array (pxi, psf->u.sbuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* dpcm_read_dles2i */

static sf_count_t
dpcm_read_dles2f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x8000) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		dles2f_array (pxi, psf->u.sbuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* dpcm_read_dles2f */

static sf_count_t
dpcm_read_dles2d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		normfact ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x8000) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		dles2d_array (pxi, psf->u.sbuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* dpcm_read_dles2d */

/*==============================================================================
*/

static void s2dsc_array (XI_PRIVATE *pxi, const short *src, signed char *dest, int count) ;
static void i2dsc_array (XI_PRIVATE *pxi, const int *src, signed char *dest, int count) ;
static void f2dsc_array (XI_PRIVATE *pxi, const float *src, signed char *dest, int count, float normfact) ;
static void d2dsc_array (XI_PRIVATE *pxi, const double *src, signed char *dest, int count, double normfact) ;

static void	s2dles_array (XI_PRIVATE *pxi, const short *src, short *dest, int count) ;
static void i2dles_array (XI_PRIVATE *pxi, const int *src, short *dest, int count) ;
static void f2dles_array (XI_PRIVATE *pxi, const float *src, short *dest, int count, float normfact) ;
static void d2dles_array (XI_PRIVATE *pxi, const double *src, short *dest, int count, double normfact) ;


static sf_count_t
dpcm_write_s2dsc (SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		s2dsc_array (pxi, ptr + total, psf->u.scbuf, bufferlen) ;
		writecount = psf_fwrite (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* dpcm_write_s2dsc */

static sf_count_t
dpcm_write_i2dsc (SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		i2dsc_array (pxi, ptr + total, psf->u.scbuf, bufferlen) ;
		writecount = psf_fwrite (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* dpcm_write_i2dsc */

static sf_count_t
dpcm_write_f2dsc (SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	normfact = (psf->norm_float == SF_TRUE) ? (1.0 * 0x7F) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		f2dsc_array (pxi, ptr + total, psf->u.scbuf, bufferlen, normfact) ;
		writecount = psf_fwrite (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* dpcm_write_f2dsc */

static sf_count_t
dpcm_write_d2dsc (SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;
	double		normfact ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	normfact = (psf->norm_double == SF_TRUE) ? (1.0 * 0x7F) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		d2dsc_array (pxi, ptr + total, psf->u.scbuf, bufferlen, normfact) ;
		writecount = psf_fwrite (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* dpcm_write_d2dsc */


static sf_count_t
dpcm_write_s2dles (SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		s2dles_array (pxi, ptr + total, psf->u.sbuf, bufferlen) ;
		writecount = psf_fwrite (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* dpcm_write_s2dles */

static sf_count_t
dpcm_write_i2dles (SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		i2dles_array (pxi, ptr + total, psf->u.sbuf, bufferlen) ;
		writecount = psf_fwrite (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* dpcm_write_i2dles */

static sf_count_t
dpcm_write_f2dles (SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	normfact = (psf->norm_float == SF_TRUE) ? (1.0 * 0x7FFF) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		f2dles_array (pxi, ptr + total, psf->u.sbuf, bufferlen, normfact) ;
		writecount = psf_fwrite (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* dpcm_write_f2dles */

static sf_count_t
dpcm_write_d2dles (SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	XI_PRIVATE	*pxi ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;
	double		normfact ;

	if ((pxi = psf->codec_data) == NULL)
		return 0 ;

	normfact = (psf->norm_double == SF_TRUE) ? (1.0 * 0x7FFF) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		d2dles_array (pxi, ptr + total, psf->u.sbuf, bufferlen, normfact) ;
		writecount = psf_fwrite (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* dpcm_write_d2dles */


/*==============================================================================
*/

static void
dsc2s_array (XI_PRIVATE *pxi, signed char *src, int count, short *dest)
{	signed char	last_val ;
	int			k ;

	last_val = pxi->last_16 >> 8 ;

	for (k = 0 ; k < count ; k++)
	{	last_val += src [k] ;
		dest [k] = last_val << 8 ;
		} ;

	pxi->last_16 = last_val << 8 ;
} /* dsc2s_array */

static void
dsc2i_array (XI_PRIVATE *pxi, signed char *src, int count, int *dest)
{	signed char	last_val ;
	int			k ;

	last_val = pxi->last_16 >> 8 ;

	for (k = 0 ; k < count ; k++)
	{	last_val += src [k] ;
		dest [k] = last_val << 24 ;
		} ;

	pxi->last_16 = last_val << 8 ;
} /* dsc2i_array */

static void
dsc2f_array (XI_PRIVATE *pxi, signed char *src, int count, float *dest, float normfact)
{	signed char	last_val ;
	int			k ;

	last_val = pxi->last_16 >> 8 ;

	for (k = 0 ; k < count ; k++)
	{	last_val += src [k] ;
		dest [k] = last_val * normfact ;
		} ;

	pxi->last_16 = last_val << 8 ;
} /* dsc2f_array */

static void
dsc2d_array (XI_PRIVATE *pxi, signed char *src, int count, double *dest, double normfact)
{	signed char	last_val ;
	int			k ;

	last_val = pxi->last_16 >> 8 ;

	for (k = 0 ; k < count ; k++)
	{	last_val += src [k] ;
		dest [k] = last_val * normfact ;
		} ;

	pxi->last_16 = last_val << 8 ;
} /* dsc2d_array */

/*------------------------------------------------------------------------------
*/

static void
s2dsc_array (XI_PRIVATE *pxi, const short *src, signed char *dest, int count)
{	signed char	last_val, current ;
	int			k ;

	last_val = pxi->last_16 >> 8 ;

	for (k = 0 ; k < count ; k++)
	{	current = src [k] >> 8 ;
		dest [k] = current - last_val ;
		last_val = current ;
		} ;

	pxi->last_16 = last_val << 8 ;
} /* s2dsc_array */

static void
i2dsc_array (XI_PRIVATE *pxi, const int *src, signed char *dest, int count)
{	signed char	last_val, current ;
	int			k ;

	last_val = pxi->last_16 >> 8 ;

	for (k = 0 ; k < count ; k++)
	{	current = src [k] >> 24 ;
		dest [k] = current - last_val ;
		last_val = current ;
		} ;

	pxi->last_16 = last_val << 8 ;
} /* i2dsc_array */

static void
f2dsc_array (XI_PRIVATE *pxi, const float *src, signed char *dest, int count, float normfact)
{	signed char	last_val, current ;
	int			k ;

	last_val = pxi->last_16 >> 8 ;

	for (k = 0 ; k < count ; k++)
	{	current = lrintf (src [k] * normfact) ;
		dest [k] = current - last_val ;
		last_val = current ;
		} ;

	pxi->last_16 = last_val << 8 ;
} /* f2dsc_array */

static void
d2dsc_array (XI_PRIVATE *pxi, const double *src, signed char *dest, int count, double normfact)
{	signed char	last_val, current ;
	int			k ;

	last_val = pxi->last_16 >> 8 ;

	for (k = 0 ; k < count ; k++)
	{	current = lrint (src [k] * normfact) ;
		dest [k] = current - last_val ;
		last_val = current ;
		} ;

	pxi->last_16 = last_val << 8 ;
} /* d2dsc_array */

/*==============================================================================
*/

static void
dles2s_array (XI_PRIVATE *pxi, short *src, int count, short *dest)
{	short	last_val ;
	int		k ;

	last_val = pxi->last_16 ;

	for (k = 0 ; k < count ; k++)
	{	last_val += LES2H_SHORT (src [k]) ;
		dest [k] = last_val ;
		} ;

	pxi->last_16 = last_val ;
} /* dles2s_array */

static void
dles2i_array (XI_PRIVATE *pxi, short *src, int count, int *dest)
{	short	last_val ;
	int		k ;

	last_val = pxi->last_16 ;

	for (k = 0 ; k < count ; k++)
	{	last_val += LES2H_SHORT (src [k]) ;
		dest [k] = last_val << 16 ;
		} ;

	pxi->last_16 = last_val ;
} /* dles2i_array */

static void
dles2f_array (XI_PRIVATE *pxi, short *src, int count, float *dest, float normfact)
{	short	last_val ;
	int		k ;

	last_val = pxi->last_16 ;

	for (k = 0 ; k < count ; k++)
	{	last_val += LES2H_SHORT (src [k]) ;
		dest [k] = last_val * normfact ;
		} ;

	pxi->last_16 = last_val ;
} /* dles2f_array */

static void
dles2d_array (XI_PRIVATE *pxi, short *src, int count, double *dest, double normfact)
{	short	last_val ;
	int		k ;

	last_val = pxi->last_16 ;

	for (k = 0 ; k < count ; k++)
	{	last_val += LES2H_SHORT (src [k]) ;
		dest [k] = last_val * normfact ;
		} ;

	pxi->last_16 = last_val ;
} /* dles2d_array */

/*------------------------------------------------------------------------------
*/

static void
s2dles_array (XI_PRIVATE *pxi, const short *src, short *dest, int count)
{	short	diff, last_val ;
	int		k ;

	last_val = pxi->last_16 ;

	for (k = 0 ; k < count ; k++)
	{	diff = src [k] - last_val ;
		dest [k] = LES2H_SHORT (diff) ;
		last_val = src [k] ;
		} ;

	pxi->last_16 = last_val ;
} /* s2dles_array */

static void
i2dles_array (XI_PRIVATE *pxi, const int *src, short *dest, int count)
{	short	diff, last_val ;
	int		k ;

	last_val = pxi->last_16 ;

	for (k = 0 ; k < count ; k++)
	{	diff = (src [k] >> 16) - last_val ;
		dest [k] = LES2H_SHORT (diff) ;
		last_val = src [k] >> 16 ;
		} ;

	pxi->last_16 = last_val ;
} /* i2dles_array */

static void
f2dles_array (XI_PRIVATE *pxi, const float *src, short *dest, int count, float normfact)
{	short	diff, last_val, current ;
	int		k ;

	last_val = pxi->last_16 ;

	for (k = 0 ; k < count ; k++)
	{	current = lrintf (src [k] * normfact) ;
		diff = current - last_val ;
		dest [k] = LES2H_SHORT (diff) ;
		last_val = current ;
		} ;

	pxi->last_16 = last_val ;
} /* f2dles_array */

static void
d2dles_array (XI_PRIVATE *pxi, const double *src, short *dest, int count, double normfact)
{	short	diff, last_val, current ;
	int		k ;

	last_val = pxi->last_16 ;

	for (k = 0 ; k < count ; k++)
	{	current = lrint (src [k] * normfact) ;
		diff = current - last_val ;
		dest [k] = LES2H_SHORT (diff) ;
		last_val = current ;
		} ;

	pxi->last_16 = last_val ;
} /* d2dles_array */

