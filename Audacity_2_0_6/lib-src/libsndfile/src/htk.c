/*
** Copyright (C) 2002-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	"sfconfig.h"

#include	<stdio.h>
#include	<fcntl.h>
#include	<string.h>
#include	<ctype.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

/*------------------------------------------------------------------------------
** Macros to handle big/little endian issues.
*/

#define	SFE_HTK_BAD_FILE_LEN 	1666
#define	SFE_HTK_NOT_WAVEFORM	1667

/*------------------------------------------------------------------------------
** Private static functions.
*/

static	int		htk_close		(SF_PRIVATE *psf) ;

static int		htk_write_header (SF_PRIVATE *psf, int calc_length) ;
static int		htk_read_header (SF_PRIVATE *psf) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
htk_open	(SF_PRIVATE *psf)
{	int		subformat ;
	int		error = 0 ;

	if (psf->is_pipe)
		return SFE_HTK_NO_PIPE ;

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = htk_read_header (psf)))
			return error ;
		} ;

	subformat = SF_CODEC (psf->sf.format) ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_HTK)
			return	SFE_BAD_OPEN_FORMAT ;

		psf->endian = SF_ENDIAN_BIG ;

		if (htk_write_header (psf, SF_FALSE))
			return psf->error ;

		psf->write_header = htk_write_header ;
		} ;

	psf->container_close = htk_close ;

	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

	switch (subformat)
	{	case SF_FORMAT_PCM_16 :	/* 16-bit linear PCM. */
				error = pcm_init (psf) ;
				break ;

		default : break ;
		} ;

	return error ;
} /* htk_open */

/*------------------------------------------------------------------------------
*/

static int
htk_close	(SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
		htk_write_header (psf, SF_TRUE) ;

	return 0 ;
} /* htk_close */

static int
htk_write_header (SF_PRIVATE *psf, int calc_length)
{	sf_count_t	current ;
	int			sample_count, sample_period ;

	current = psf_ftell (psf) ;

	if (calc_length)
		psf->filelength = psf_get_filelen (psf) ;

	/* Reset the current header length to zero. */
	psf->header [0] = 0 ;
	psf->headindex = 0 ;
	psf_fseek (psf, 0, SEEK_SET) ;

	if (psf->filelength > 12)
		sample_count = (psf->filelength - 12) / 2 ;
	else
		sample_count = 0 ;

	sample_period = 10000000 / psf->sf.samplerate ;

	psf_binheader_writef (psf, "E444", sample_count, sample_period, 0x20000) ;

	/* Header construction complete so write it out. */
	psf_fwrite (psf->header, psf->headindex, 1, psf) ;

	if (psf->error)
		return psf->error ;

	psf->dataoffset = psf->headindex ;

	if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* htk_write_header */

/*
** Found the following info in a comment block within Bill Schottstaedt's
** sndlib library.
**
** HTK format files consist of a contiguous sequence of samples preceded by a
** header. Each sample is a vector of either 2-byte integers or 4-byte floats.
** 2-byte integers are used for compressed forms as described below and for
** vector quantised data as described later in section 5.11. HTK format data
** files can also be used to store speech waveforms as described in section 5.8.
**
** The HTK file format header is 12 bytes long and contains the following data
**   nSamples   -- number of samples in file (4-byte integer)
**   sampPeriod -- sample period in 100ns units (4-byte integer)
**   sampSize   -- number of bytes per sample (2-byte integer)
**   parmKind   -- a code indicating the sample kind (2-byte integer)
**
** The parameter kind  consists of a 6 bit code representing the basic
** parameter kind plus additional bits for each of the possible qualifiers.
** The basic parameter kind codes are
**
**  0    WAVEFORM    sampled waveform
**  1    LPC         linear prediction filter coefficients
**  2    LPREFC      linear prediction reflection coefficients
**  3    LPCEPSTRA   LPC cepstral coefficients
**  4    LPDELCEP    LPC cepstra plus delta coefficients
**  5    IREFC       LPC reflection coef in 16 bit integer format
**  6    MFCC        mel-frequency cepstral coefficients
**  7    FBANK       log mel-filter bank channel outputs
**  8    MELSPEC     linear mel-filter bank channel outputs
**  9    USER        user defined sample kind
**  10   DISCRETE    vector quantised data
**
** and the bit-encoding for the qualifiers (in octal) is
**   _E   000100      has energy
**   _N   000200      absolute energy suppressed
**   _D   000400      has delta coefficients
**   _A   001000      has acceleration coefficients
**   _C   002000      is compressed
**   _Z   004000      has zero mean static coef.
**   _K   010000      has CRC checksum
**   _O   020000      has 0'th cepstral coef.
*/

static int
htk_read_header (SF_PRIVATE *psf)
{	int		sample_count, sample_period, marker ;

	psf_binheader_readf (psf, "pE444", 0, &sample_count, &sample_period, &marker) ;

	if (2 * sample_count + 12 != psf->filelength)
		return SFE_HTK_BAD_FILE_LEN ;

	if (marker != 0x20000)
		return SFE_HTK_NOT_WAVEFORM ;

	psf->sf.channels = 1 ;

	if (sample_period > 0)
	{	psf->sf.samplerate = 10000000 / sample_period ;
		psf_log_printf (psf, "HTK Waveform file\n  Sample Count  : %d\n  Sample Period : %d => %d Hz\n",
					sample_count, sample_period, psf->sf.samplerate) ;
		}
	else
	{	psf->sf.samplerate = 16000 ;
		psf_log_printf (psf, "HTK Waveform file\n  Sample Count  : %d\n  Sample Period : %d (should be > 0) => Guessed sample rate %d Hz\n",
					sample_count, sample_period, psf->sf.samplerate) ;
		} ;

	psf->sf.format = SF_FORMAT_HTK | SF_FORMAT_PCM_16 ;
	psf->bytewidth = 2 ;

	/* HTK always has a 12 byte header. */
	psf->dataoffset = 12 ;
	psf->endian = SF_ENDIAN_BIG ;

	psf->datalength = psf->filelength - psf->dataoffset ;

	psf->blockwidth = psf->sf.channels * psf->bytewidth ;

	if (! psf->sf.frames && psf->blockwidth)
		psf->sf.frames = (psf->filelength - psf->dataoffset) / psf->blockwidth ;

	return 0 ;
} /* htk_read_header */

