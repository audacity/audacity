/*
** Copyright (C) 2004-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <string.h>

#include "sndfile.h"
#include "sfendian.h"
#include "common.h"

#define TWOBIT_MARKER	(MAKE_MARKER ('2', 'B', 'I', 'T'))
#define	AVR_HDR_SIZE	128

#define	SFE_AVR_X	666

/*
** From: hyc@hanauma.Jpl.Nasa.Gov (Howard Chu)
**
** A lot of PD software exists to play Mac .snd files on the ST. One other
** format that seems pretty popular (used by a number of commercial packages)
** is the AVR format (from Audio Visual Research). This format has a 128 byte
** header that looks like this (its actually packed, but thats not portable):
*/

typedef struct
{	int		marker ;	/* 2BIT */
	char	name [8] ;	/* null-padded sample name */
	short	mono ;		/* 0 = mono, 0xffff = stereo */
	short	rez ;		/* 8 = 8 bit, 16 = 16 bit */
	short	sign ;		/* 0 = unsigned, 0xffff = signed */

	short	loop ;		/* 0 = no loop, 0xffff = looping sample */
	short	midi ;		/* 0xffff = no MIDI note assigned,  */
						/*	0xffXX = single key note assignment */
						/*	0xLLHH = key split, low/hi note */
	int		srate ;		/* sample frequency in hertz */
	int		frames ;	/* sample length in bytes or words (see rez) */
	int		lbeg ;		/* offset to start of loop in bytes or words. */
						/* set to zero if unused */
	int		lend ;		/* offset to end of loop in bytes or words. */
						/* set to sample length if unused */
	short	res1 ;		/* Reserved, MIDI keyboard split */
	short	res2 ;		/* Reserved, sample compression */
	short	res3 ;		/* Reserved */
	char	ext [20] ;	/* Additional filename space, used if (name[7] != 0) */
	char	user [64] ; /* User defined. Typically ASCII message */
} AVR_HEADER ;

/*------------------------------------------------------------------------------
** Private static functions.
*/

static int		avr_close (SF_PRIVATE *psf) ;

static int		avr_read_header (SF_PRIVATE *psf) ;
static int		avr_write_header (SF_PRIVATE *psf, int calc_length) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
avr_open	(SF_PRIVATE *psf)
{	int		error = 0 ;

	if (psf->mode == SFM_READ || (psf->mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = avr_read_header (psf)))
			return error ;
		} ;

	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_AVR)
		return	SFE_BAD_OPEN_FORMAT ;

	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	psf->endian = SF_ENDIAN (psf->sf.format) ;
		psf->endian = SF_ENDIAN_BIG ;

		if (avr_write_header (psf, SF_FALSE))
			return psf->error ;

		psf->write_header = avr_write_header ;
		} ;

	psf->container_close = avr_close ;

	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

	error = pcm_init (psf) ;

	return error ;
} /* avr_open */

static int
avr_read_header (SF_PRIVATE *psf)
{	AVR_HEADER	hdr ;

	memset (&hdr, 0, sizeof (hdr)) ;

	psf_binheader_readf (psf, "pmb", 0, &hdr.marker, &hdr.name, sizeof (hdr.name)) ;
	psf_log_printf (psf, "%M\n", hdr.marker) ;

	if (hdr.marker != TWOBIT_MARKER)
		return SFE_AVR_X ;

	psf_log_printf (psf, "  Name        : %s\n", hdr.name) ;

	psf_binheader_readf (psf, "E22222", &hdr.mono, &hdr.rez, &hdr.sign, &hdr.loop, &hdr.midi) ;

	psf->sf.channels = (hdr.mono & 1) + 1 ;

	psf_log_printf (psf, "  Channels    : %d\n  Bit width   : %d\n  Signed      : %s\n",
			(hdr.mono & 1) + 1, hdr.rez, hdr.sign ? "yes" : "no") ;

	switch ((hdr.rez << 16) + (hdr.sign & 1))
	{	case ((8 << 16) + 0) :
			psf->sf.format = SF_FORMAT_AVR | SF_FORMAT_PCM_U8 ;
			psf->bytewidth = 1 ;
			break ;

		case ((8 << 16) + 1) :
			psf->sf.format = SF_FORMAT_AVR | SF_FORMAT_PCM_S8 ;
			psf->bytewidth = 1 ;
			break ;

		case ((16 << 16) + 1) :
			psf->sf.format = SF_FORMAT_AVR | SF_FORMAT_PCM_16 ;
			psf->bytewidth = 2 ;
			break ;

		default :
			psf_log_printf (psf, "Error : bad rez/sign combination.\n") ;
			return SFE_AVR_X ;
		} ;

	psf_binheader_readf (psf, "E4444", &hdr.srate, &hdr.frames, &hdr.lbeg, &hdr.lend) ;

	psf->sf.frames = hdr.frames ;
	psf->sf.samplerate = hdr.srate ;

	psf_log_printf (psf, "  Frames      : %D\n", psf->sf.frames) ;
	psf_log_printf (psf, "  Sample rate : %d\n", psf->sf.samplerate) ;

	psf_binheader_readf (psf, "E222", &hdr.res1, &hdr.res2, &hdr.res3) ;
	psf_binheader_readf (psf, "bb", hdr.ext, sizeof (hdr.ext), hdr.user, sizeof (hdr.user)) ;

	psf_log_printf (psf, "  Ext         : %s\n  User        : %s\n", hdr.ext, hdr.user) ;

	psf->endian = SF_ENDIAN_BIG ;

 	psf->dataoffset = AVR_HDR_SIZE ;
	psf->datalength = hdr.frames * (hdr.rez / 8) ;

	if (psf->fileoffset > 0)
		psf->filelength = AVR_HDR_SIZE + psf->datalength ;

	if (psf_ftell (psf) != psf->dataoffset)
		psf_binheader_readf (psf, "j", psf->dataoffset - psf_ftell (psf)) ;

	psf->blockwidth = psf->sf.channels * psf->bytewidth ;

	if (psf->sf.frames == 0 && psf->blockwidth)
		psf->sf.frames = (psf->filelength - psf->dataoffset) / psf->blockwidth ;

	return 0 ;
} /* avr_read_header */

static int
avr_write_header (SF_PRIVATE *psf, int calc_length)
{	sf_count_t	current ;
	int			sign ;

	if (psf->pipeoffset > 0)
		return 0 ;

	current = psf_ftell (psf) ;

	if (calc_length)
	{	psf->filelength = psf_get_filelen (psf) ;

		psf->datalength = psf->filelength - psf->dataoffset ;
		if (psf->dataend)
			psf->datalength -= psf->filelength - psf->dataend ;

		psf->sf.frames = psf->datalength / (psf->bytewidth * psf->sf.channels) ;
		} ;

	/* Reset the current header length to zero. */
	psf->header [0] = 0 ;
	psf->headindex = 0 ;

	/*
	** Only attempt to seek if we are not writng to a pipe. If we are
	** writing to a pipe we shouldn't be here anyway.
	*/
	if (psf->is_pipe == SF_FALSE)
		psf_fseek (psf, 0, SEEK_SET) ;

	psf_binheader_writef (psf, "Emz22", TWOBIT_MARKER, make_size_t (8),
			psf->sf.channels == 2 ? 0xFFFF : 0, psf->bytewidth * 8) ;

	sign = ((SF_CODEC (psf->sf.format)) == SF_FORMAT_PCM_U8) ? 0 : 0xFFFF ;

	psf_binheader_writef (psf, "E222", sign, 0, 0xFFFF) ;
	psf_binheader_writef (psf, "E4444", psf->sf.samplerate, psf->sf.frames, 0, 0) ;

	psf_binheader_writef (psf, "E222zz", 0, 0, 0, make_size_t (20), make_size_t (64)) ;

	/* Header construction complete so write it out. */
	psf_fwrite (psf->header, psf->headindex, 1, psf) ;

	if (psf->error)
		return psf->error ;

	psf->dataoffset = psf->headindex ;

	if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* avr_write_header */

static int
avr_close (SF_PRIVATE *psf)
{
	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
		avr_write_header (psf, SF_TRUE) ;

	return 0 ;
} /* avr_close */

