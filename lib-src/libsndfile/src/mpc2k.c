/*
** Copyright (C) 2008-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <fcntl.h>
#include <string.h>
#include <ctype.h>

#include "sndfile.h"
#include "sfendian.h"
#include "common.h"

/*
**	Info from Olivier Tristan <o.tristan@ultimatesoundbank.com>
**
**	HEADER
**	2 magic bytes: 1 and 4.
**	17 char for the name of the sample.
**	3 bytes: level, tune and channels (0 for channels is mono while 1 is stereo)
**	4 uint32: sampleStart, loopEnd, sampleFrames and loopLength
**	1 byte: loopMode (0 no loop, 1 forward looping)
**	1 byte: number of beat in loop
**	1 uint16: sampleRate
**
**	DATA
**	Data are always non compressed 16 bits interleaved
*/

#define HEADER_LENGTH		42	/* Sum of above data fields. */
#define HEADER_NAME_LEN		17	/* Length of name string. */

#define	SFE_MPC_NO_MARKER	666

/*------------------------------------------------------------------------------
** Private static functions.
*/

static	int		mpc2k_close		(SF_PRIVATE *psf) ;

static int		mpc2k_write_header (SF_PRIVATE *psf, int calc_length) ;
static int		mpc2k_read_header (SF_PRIVATE *psf) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
mpc2k_open	(SF_PRIVATE *psf)
{	int		error = 0 ;

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = mpc2k_read_header (psf)))
			return error ;
		} ;

	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_MPC2K)
		return	SFE_BAD_OPEN_FORMAT ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	if (mpc2k_write_header (psf, SF_FALSE))
			return psf->error ;

		psf->write_header = mpc2k_write_header ;
		} ;

	psf->container_close = mpc2k_close ;

	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

	error = pcm_init (psf) ;

	return error ;
} /* mpc2k_open */

/*------------------------------------------------------------------------------
*/

static int
mpc2k_close	(SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
		mpc2k_write_header (psf, SF_TRUE) ;

	return 0 ;
} /* mpc2k_close */

static int
mpc2k_write_header (SF_PRIVATE *psf, int calc_length)
{	char sample_name [HEADER_NAME_LEN + 1] ;
	sf_count_t	current ;

	if (psf->pipeoffset > 0)
		return 0 ;

	current = psf_ftell (psf) ;

	if (calc_length)
	{	psf->filelength = psf_get_filelen (psf) ;

		psf->dataoffset = HEADER_LENGTH ;
		psf->datalength = psf->filelength - psf->dataoffset ;

		psf->sf.frames = psf->datalength / (psf->bytewidth * psf->sf.channels) ;
		} ;

	/* Reset the current header length to zero. */
	psf->header.ptr [0] = 0 ;
	psf->header.indx = 0 ;

	/*
	** Only attempt to seek if we are not writng to a pipe. If we are
	** writing to a pipe we shouldn't be here anyway.
	*/
	if (psf->is_pipe == SF_FALSE)
		psf_fseek (psf, 0, SEEK_SET) ;

	snprintf (sample_name, sizeof (sample_name), "%-*.*s", HEADER_NAME_LEN, HEADER_NAME_LEN, psf->file.name.c) ;

	psf_binheader_writef (psf, "e11b", BHW1 (1), BHW1 (4), BHWv (sample_name), BHWz (HEADER_NAME_LEN)) ;
	psf_binheader_writef (psf, "e111", BHW1 (100), BHW1 (0), BHW1 ((psf->sf.channels - 1) & 1)) ;
	psf_binheader_writef (psf, "et4888", BHW4 (0), BHW8 (psf->sf.frames), BHW8 (psf->sf.frames), BHW8 (psf->sf.frames)) ;
	psf_binheader_writef (psf, "e112", BHW1 (0), BHW1 (1), BHW2 ((uint16_t) psf->sf.samplerate)) ;

	/* Always 16 bit little endian data. */
	psf->bytewidth = 2 ;
	psf->endian = SF_ENDIAN_LITTLE ;

	psf_fwrite (psf->header.ptr, psf->header.indx, 1, psf) ;

	if (psf->error)
		return psf->error ;

	psf->dataoffset = psf->header.indx ;

	if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* mpc2k_write_header */

static int
mpc2k_read_header (SF_PRIVATE *psf)
{	char sample_name [HEADER_NAME_LEN + 1] ;
	unsigned char bytes [4] ;
	uint32_t sample_start, loop_end, sample_frames, loop_length ;
	uint16_t sample_rate ;

	psf_binheader_readf (psf, "pebb", 0, bytes, 2, sample_name, make_size_t (HEADER_NAME_LEN)) ;

	if (bytes [0] != 1 || bytes [1] != 4)
		return SFE_MPC_NO_MARKER ;

	sample_name [HEADER_NAME_LEN] = 0 ;

	psf_log_printf (psf, "MPC2000\n  Name         : %s\n", sample_name) ;

	psf_binheader_readf (psf, "eb4444", bytes, 3, &sample_start, &loop_end, &sample_frames, &loop_length) ;

	psf->sf.channels = bytes [2] ? 2 : 1 ;

	psf_log_printf (psf, "  Level        : %d\n  Tune         : %d\n  Stereo       : %s\n", bytes [0], bytes [1], bytes [2] ? "Yes" : "No") ;

	psf_log_printf (psf, "  Sample start : %d\n  Loop end     : %d\n  Frames       : %d\n  Length       : %d\n", sample_start, loop_end, sample_frames, loop_length) ;

	psf_binheader_readf (psf, "eb2", bytes, 2, &sample_rate) ;

	psf_log_printf (psf, "  Loop mode    : %s\n  Beats        : %d\n  Sample rate  : %d\nEnd\n", bytes [0] ? "None" : "Fwd", bytes [1], sample_rate) ;

	psf->sf.samplerate = sample_rate ;

	psf->sf.format = SF_FORMAT_MPC2K | SF_FORMAT_PCM_16 ;

	psf->dataoffset = psf_ftell (psf) ;

	/* Always 16 bit little endian data. */
	psf->bytewidth = 2 ;
	psf->endian = SF_ENDIAN_LITTLE ;

	psf->datalength = psf->filelength - psf->dataoffset ;
	psf->blockwidth = psf->sf.channels * psf->bytewidth ;
	psf->sf.frames = psf->datalength / psf->blockwidth ;

	psf->sf.frames = (psf->filelength - psf->dataoffset) / psf->blockwidth ;

	return 0 ;
} /* mpc2k_read_header */

