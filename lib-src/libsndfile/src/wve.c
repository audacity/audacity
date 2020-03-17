/*
** Copyright (C) 2002-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2007 Reuben Thomas
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
** Macros to handle big/little endian issues, and other magic numbers.
*/

#define ALAW_MARKER			MAKE_MARKER ('A', 'L', 'a', 'w')
#define SOUN_MARKER			MAKE_MARKER ('S', 'o', 'u', 'n')
#define DFIL_MARKER			MAKE_MARKER ('d', 'F', 'i', 'l')
#define ESSN_MARKER			MAKE_MARKER ('e', '*', '*', '\0')
#define PSION_VERSION		((unsigned short) 3856)
#define PSION_DATAOFFSET	0x20

/*------------------------------------------------------------------------------
** Private static functions.
*/

static int	wve_read_header (SF_PRIVATE *psf) ;
static int	wve_write_header (SF_PRIVATE *psf, int calc_length) ;
static int	wve_close (SF_PRIVATE *psf) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
wve_open (SF_PRIVATE *psf)
{	int	error = 0 ;

	if (psf->is_pipe)
		return SFE_WVE_NO_PIPE ;

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = wve_read_header (psf)))
			return error ;
		} ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_WVE)
			return	SFE_BAD_OPEN_FORMAT ;

		psf->endian = SF_ENDIAN_BIG ;

		if ((error = wve_write_header (psf, SF_FALSE)))
			return error ;

		psf->write_header = wve_write_header ;
		} ;

	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

	psf->container_close = wve_close ;

	error = alaw_init (psf) ;

	return error ;
} /* wve_open */

/*------------------------------------------------------------------------------
*/

static int
wve_read_header (SF_PRIVATE *psf)
{	int marker ;
	unsigned short version, padding, repeats, trash ;
	unsigned datalength ;

	/* Set position to start of file to begin reading header. */
	psf_binheader_readf (psf, "pm", 0, &marker) ;
	if (marker != ALAW_MARKER)
	{	psf_log_printf (psf, "Could not find '%M'\n", ALAW_MARKER) ;
		return SFE_WVE_NOT_WVE ;
		} ;

	psf_binheader_readf (psf, "m", &marker) ;
	if (marker != SOUN_MARKER)
	{	psf_log_printf (psf, "Could not find '%M'\n", SOUN_MARKER) ;
		return SFE_WVE_NOT_WVE ;
		} ;

	psf_binheader_readf (psf, "m", &marker) ;
	if (marker != DFIL_MARKER)
	{	psf_log_printf (psf, "Could not find '%M'\n", DFIL_MARKER) ;
		return SFE_WVE_NOT_WVE ;
		} ;

	psf_binheader_readf (psf, "m", &marker) ;
	if (marker != ESSN_MARKER)
	{	psf_log_printf (psf, "Could not find '%M'\n", ESSN_MARKER) ;
		return SFE_WVE_NOT_WVE ;
		} ;

	psf_binheader_readf (psf, "E2", &version) ;

	psf_log_printf (psf, "Psion Palmtop Alaw (.wve)\n"
			"  Sample Rate : 8000\n"
			"  Channels    : 1\n"
			"  Encoding    : A-law\n") ;

	if (version != PSION_VERSION)
		psf_log_printf (psf, "Psion version %d should be %d\n", version, PSION_VERSION) ;

	psf_binheader_readf (psf, "E4", &datalength) ;
	psf->dataoffset = PSION_DATAOFFSET ;
	if (datalength != psf->filelength - psf->dataoffset)
	{	psf->datalength = psf->filelength - psf->dataoffset ;
		psf_log_printf (psf, "Data length %d should be %D\n", datalength, psf->datalength) ;
		}
	else
		psf->datalength = datalength ;

	psf_binheader_readf (psf, "E22222", &padding, &repeats, &trash, &trash, &trash) ;

	psf->sf.format		= SF_FORMAT_WVE | SF_FORMAT_ALAW ;
	psf->sf.samplerate	= 8000 ;
	psf->sf.frames		= psf->datalength ;
	psf->sf.channels	= 1 ;

	return SFE_NO_ERROR ;
} /* wve_read_header */

/*------------------------------------------------------------------------------
*/

static int
wve_write_header (SF_PRIVATE *psf, int calc_length)
{	sf_count_t	current ;
	unsigned datalen ;

	current = psf_ftell (psf) ;

	if (calc_length)
	{	psf->filelength = psf_get_filelen (psf) ;

		psf->datalength = psf->filelength - psf->dataoffset ;
		if (psf->dataend)
			psf->datalength -= psf->filelength - psf->dataend ;

		psf->sf.frames = psf->datalength / (psf->bytewidth * psf->sf.channels) ;
		} ;

	/* Reset the current header length to zero. */
	psf->header.ptr [0] = 0 ;
	psf->header.indx = 0 ;
	psf_fseek (psf, 0, SEEK_SET) ;

	/* Write header. */
	datalen = psf->datalength ;
	psf_binheader_writef (psf, "Emmmm", BHWm (ALAW_MARKER), BHWm (SOUN_MARKER), BHWm (DFIL_MARKER), BHWm (ESSN_MARKER)) ;
	psf_binheader_writef (psf, "E2422222", BHW2 (PSION_VERSION), BHW4 (datalen), BHW2 (0), BHW2 (0), BHW2 (0), BHW2 (0), BHW2 (0)) ;
	psf_fwrite (psf->header.ptr, psf->header.indx, 1, psf) ;

	if (psf->sf.channels != 1)
		return SFE_CHANNEL_COUNT ;

	if (psf->error)
		return psf->error ;

	psf->dataoffset = psf->header.indx ;

	if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* wve_write_header */

/*------------------------------------------------------------------------------
*/

static int
wve_close (SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	/*  Now we know for certain the length of the file we can re-write
		**	the header.
		*/
		wve_write_header (psf, SF_TRUE) ;
		} ;

	return 0 ;
} /* wve_close */
