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
#include	<math.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

/*------------------------------------------------------------------------------
** Information on how to decode and encode this file was obtained in a PDF
** file which I found on http://www.wotsit.org/.
** Also did a lot of testing with GNU Octave but do not have access to
** Matlab (tm) and so could not test it there.
*/

/*------------------------------------------------------------------------------
** Macros to handle big/little endian issues.
*/

#define MATL_MARKER	(MAKE_MARKER ('M', 'A', 'T', 'L'))

#define IM_MARKER	(('I' << 8) + 'M')
#define MI_MARKER	(('M' << 8) + 'I')

/*------------------------------------------------------------------------------
** Enums and typedefs.
*/

enum
{	MAT5_TYPE_SCHAR			= 0x1,
	MAT5_TYPE_UCHAR			= 0x2,
	MAT5_TYPE_INT16			= 0x3,
	MAT5_TYPE_UINT16		= 0x4,
	MAT5_TYPE_INT32			= 0x5,
	MAT5_TYPE_UINT32		= 0x6,
	MAT5_TYPE_FLOAT			= 0x7,
	MAT5_TYPE_DOUBLE		= 0x9,
	MAT5_TYPE_ARRAY			= 0xE,

	MAT5_TYPE_COMP_USHORT	= 0x00020004,
	MAT5_TYPE_COMP_UINT		= 0x00040006
} ;

typedef struct
{	sf_count_t	size ;
	int			rows, cols ;
	char		name [32] ;
} MAT5_MATRIX ;

/*------------------------------------------------------------------------------
** Private static functions.
*/

static int		mat5_close		(SF_PRIVATE *psf) ;

static int		mat5_write_header (SF_PRIVATE *psf, int calc_length) ;
static int		mat5_read_header (SF_PRIVATE *psf) ;

/*------------------------------------------------------------------------------
** Public function.
*/

int
mat5_open	(SF_PRIVATE *psf)
{	int		subformat, error = 0 ;

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = mat5_read_header (psf)))
			return error ;
		} ;

	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_MAT5)
		return	SFE_BAD_OPEN_FORMAT ;

	subformat = SF_CODEC (psf->sf.format) ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	if (psf->is_pipe)
			return SFE_NO_PIPE_WRITE ;

		psf->endian = SF_ENDIAN (psf->sf.format) ;
		if (CPU_IS_LITTLE_ENDIAN && (psf->endian == SF_ENDIAN_CPU || psf->endian == 0))
			psf->endian = SF_ENDIAN_LITTLE ;
		else if (CPU_IS_BIG_ENDIAN && (psf->endian == SF_ENDIAN_CPU || psf->endian == 0))
			psf->endian = SF_ENDIAN_BIG ;

		if ((error = mat5_write_header (psf, SF_FALSE)))
			return error ;

		psf->write_header = mat5_write_header ;
		} ;

	psf->container_close = mat5_close ;

	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

	switch (subformat)
	{	case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_16 :
		case SF_FORMAT_PCM_32 :
				error = pcm_init (psf) ;
				break ;

		case SF_FORMAT_FLOAT :
				error = float32_init (psf) ;
				break ;

		case SF_FORMAT_DOUBLE :
				error = double64_init (psf) ;
				break ;

		default : break ;
		} ;

	return error ;
} /* mat5_open */

/*------------------------------------------------------------------------------
*/

static int
mat5_close	(SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
		mat5_write_header (psf, SF_TRUE) ;

	return 0 ;
} /* mat5_close */

/*------------------------------------------------------------------------------
*/

static int
mat5_write_header (SF_PRIVATE *psf, int calc_length)
{	static const char	*filename = "MATLAB 5.0 MAT-file, written by " PACKAGE "-" VERSION ", " ;
	static const char	*sr_name = "samplerate\0\0\0\0\0\0\0\0\0\0\0" ;
	static const char	*wd_name = "wavedata\0" ;
	sf_count_t	current, datasize ;
	int			encoding ;

	current = psf_ftell (psf) ;

	if (calc_length)
	{	psf_fseek (psf, 0, SEEK_END) ;
		psf->filelength = psf_ftell (psf) ;
		psf_fseek (psf, 0, SEEK_SET) ;

		psf->datalength = psf->filelength - psf->dataoffset ;
		if (psf->dataend)
			psf->datalength -= psf->filelength - psf->dataend ;

		psf->sf.frames = psf->datalength / (psf->bytewidth * psf->sf.channels) ;
		} ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_U8 :
				encoding = MAT5_TYPE_UCHAR ;
				break ;

		case SF_FORMAT_PCM_16 :
				encoding = MAT5_TYPE_INT16 ;
				break ;

		case SF_FORMAT_PCM_32 :
				encoding = MAT5_TYPE_INT32 ;
				break ;

		case SF_FORMAT_FLOAT :
				encoding = MAT5_TYPE_FLOAT ;
				break ;

		case SF_FORMAT_DOUBLE :
				encoding = MAT5_TYPE_DOUBLE ;
				break ;

		default :
				return SFE_BAD_OPEN_FORMAT ;
		} ;

	/* Reset the current header length to zero. */
	psf->header [0] = 0 ;
	psf->headindex = 0 ;
	psf_fseek (psf, 0, SEEK_SET) ;

	psf_get_date_str (psf->u.cbuf, sizeof (psf->u.scbuf)) ;
	psf_binheader_writef (psf, "bb", filename, strlen (filename), psf->u.cbuf, strlen (psf->u.cbuf) + 1) ;

	memset (psf->u.scbuf, ' ', 124 - psf->headindex) ;
	psf_binheader_writef (psf, "b", psf->u.scbuf, make_size_t (124 - psf->headindex)) ;

	psf->rwf_endian = psf->endian ;

	if (psf->rwf_endian == SF_ENDIAN_BIG)
		psf_binheader_writef (psf, "2b", 0x0100, "MI", make_size_t (2)) ;
	else
		psf_binheader_writef (psf, "2b", 0x0100, "IM", make_size_t (2)) ;

	psf_binheader_writef (psf, "444444", MAT5_TYPE_ARRAY, 64, MAT5_TYPE_UINT32, 8, 6, 0) ;
	psf_binheader_writef (psf, "4444", MAT5_TYPE_INT32, 8, 1, 1) ;
	psf_binheader_writef (psf, "44b", MAT5_TYPE_SCHAR, strlen (sr_name), sr_name, make_size_t (16)) ;

	if (psf->sf.samplerate > 0xFFFF)
		psf_binheader_writef (psf, "44", MAT5_TYPE_COMP_UINT, psf->sf.samplerate) ;
	else
	{	unsigned short samplerate = psf->sf.samplerate ;

		psf_binheader_writef (psf, "422", MAT5_TYPE_COMP_USHORT, samplerate, 0) ;
		} ;

	datasize = psf->sf.frames * psf->sf.channels * psf->bytewidth ;

	psf_binheader_writef (psf, "t484444", MAT5_TYPE_ARRAY, datasize + 64, MAT5_TYPE_UINT32, 8, 6, 0) ;
	psf_binheader_writef (psf, "t4448", MAT5_TYPE_INT32, 8, psf->sf.channels, psf->sf.frames) ;
	psf_binheader_writef (psf, "44b", MAT5_TYPE_SCHAR, strlen (wd_name), wd_name, strlen (wd_name)) ;

	datasize = psf->sf.frames * psf->sf.channels * psf->bytewidth ;
	if (datasize > 0x7FFFFFFF)
		datasize = 0x7FFFFFFF ;

	psf_binheader_writef (psf, "t48", encoding, datasize) ;

	/* Header construction complete so write it out. */
	psf_fwrite (psf->header, psf->headindex, 1, psf) ;

	if (psf->error)
		return psf->error ;

	psf->dataoffset = psf->headindex ;

	if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* mat5_write_header */

static int
mat5_read_header (SF_PRIVATE *psf)
{	char	name [32] ;
	short	version, endian ;
	int		type, flags1, flags2, rows, cols ;
	unsigned size ;

	psf_binheader_readf (psf, "pb", 0, psf->u.cbuf, 124) ;

	psf->u.scbuf [125] = 0 ;

	if (strlen (psf->u.cbuf) >= 124)
		return SFE_UNIMPLEMENTED ;

	if (strstr (psf->u.cbuf, "MATLAB 5.0 MAT-file") == psf->u.cbuf)
		psf_log_printf (psf, "%s\n", psf->u.scbuf) ;


	psf_binheader_readf (psf, "E22", &version, &endian) ;

	if (endian == MI_MARKER)
	{	psf->endian = psf->rwf_endian = SF_ENDIAN_BIG ;
		if (CPU_IS_LITTLE_ENDIAN) version = ENDSWAP_SHORT (version) ;
		}
	else if (endian == IM_MARKER)
	{	psf->endian = psf->rwf_endian = SF_ENDIAN_LITTLE ;
		if (CPU_IS_BIG_ENDIAN) version = ENDSWAP_SHORT (version) ;
		}
	else
		return SFE_MAT5_BAD_ENDIAN ;

	if ((CPU_IS_LITTLE_ENDIAN && endian == IM_MARKER) ||
			(CPU_IS_BIG_ENDIAN && endian == MI_MARKER))
		version = ENDSWAP_SHORT (version) ;

	psf_log_printf (psf, "Version : 0x%04X\n", version) ;
	psf_log_printf (psf, "Endian  : 0x%04X => %s\n", endian,
				(psf->endian == SF_ENDIAN_LITTLE) ? "Little" : "Big") ;

	/*========================================================*/
	psf_binheader_readf (psf, "44", &type, &size) ;
	psf_log_printf (psf, "Block\n Type : %X    Size : %d\n", type, size) ;

	if (type != MAT5_TYPE_ARRAY)
		return SFE_MAT5_NO_BLOCK ;

	psf_binheader_readf (psf, "44", &type, &size) ;
	psf_log_printf (psf, "    Type : %X    Size : %d\n", type, size) ;

	if (type != MAT5_TYPE_UINT32)
		return SFE_MAT5_NO_BLOCK ;

	psf_binheader_readf (psf, "44", &flags1, &flags2) ;
	psf_log_printf (psf, "    Flg1 : %X    Flg2 : %d\n", flags1, flags2) ;

	psf_binheader_readf (psf, "44", &type, &size) ;
	psf_log_printf (psf, "    Type : %X    Size : %d\n", type, size) ;

	if (type != MAT5_TYPE_INT32)
		return SFE_MAT5_NO_BLOCK ;

	psf_binheader_readf (psf, "44", &rows, &cols) ;
	psf_log_printf (psf, "    Rows : %X    Cols : %d\n", rows, cols) ;

	if (rows != 1 || cols != 1)
		return SFE_MAT5_SAMPLE_RATE ;

	psf_binheader_readf (psf, "4", &type) ;

	if (type == MAT5_TYPE_SCHAR)
	{	psf_binheader_readf (psf, "4", &size) ;
		psf_log_printf (psf, "    Type : %X    Size : %d\n", type, size) ;
		if (size > SIGNED_SIZEOF (name) - 1)
		{	psf_log_printf (psf, "Error : Bad name length.\n") ;
			return SFE_MAT5_NO_BLOCK ;
			} ;

		psf_binheader_readf (psf, "bj", name, size, (8 - (size % 8)) % 8) ;
		name [size] = 0 ;
		}
	else if ((type & 0xFFFF) == MAT5_TYPE_SCHAR)
	{	size = type >> 16 ;
		if (size > 4)
		{	psf_log_printf (psf, "Error : Bad name length.\n") ;
			return SFE_MAT5_NO_BLOCK ;
			} ;

		psf_log_printf (psf, "    Type : %X\n", type) ;
		psf_binheader_readf (psf, "4", &name) ;
		name [size] = 0 ;
		}
	else
		return SFE_MAT5_NO_BLOCK ;

	psf_log_printf (psf, "    Name : %s\n", name) ;

	/*-----------------------------------------*/

	psf_binheader_readf (psf, "44", &type, &size) ;

	switch (type)
	{	case MAT5_TYPE_DOUBLE :
				{	double	samplerate ;

					psf_binheader_readf (psf, "d", &samplerate) ;
					snprintf (name, sizeof (name), "%f\n", samplerate) ;
					psf_log_printf (psf, "    Val  : %s\n", name) ;

					psf->sf.samplerate = lrint (samplerate) ;
					} ;
				break ;

		case MAT5_TYPE_COMP_USHORT :
				{	unsigned short samplerate ;

					psf_binheader_readf (psf, "j2j", -4, &samplerate, 2) ;
					psf_log_printf (psf, "    Val  : %u\n", samplerate) ;
					psf->sf.samplerate = samplerate ;
					}
				break ;

		case MAT5_TYPE_COMP_UINT :
				psf_log_printf (psf, "    Val  : %u\n", size) ;
				psf->sf.samplerate = size ;
				break ;

		default :
			psf_log_printf (psf, "    Type : %X    Size : %d  ***\n", type, size) ;
			return SFE_MAT5_SAMPLE_RATE ;
		} ;

	/*-----------------------------------------*/


	psf_binheader_readf (psf, "44", &type, &size) ;
	psf_log_printf (psf, " Type : %X    Size : %d\n", type, size) ;

	if (type != MAT5_TYPE_ARRAY)
		return SFE_MAT5_NO_BLOCK ;

	psf_binheader_readf (psf, "44", &type, &size) ;
	psf_log_printf (psf, "    Type : %X    Size : %d\n", type, size) ;

	if (type != MAT5_TYPE_UINT32)
		return SFE_MAT5_NO_BLOCK ;

	psf_binheader_readf (psf, "44", &flags1, &flags2) ;
	psf_log_printf (psf, "    Flg1 : %X    Flg2 : %d\n", flags1, flags2) ;

	psf_binheader_readf (psf, "44", &type, &size) ;
	psf_log_printf (psf, "    Type : %X    Size : %d\n", type, size) ;

	if (type != MAT5_TYPE_INT32)
		return SFE_MAT5_NO_BLOCK ;

	psf_binheader_readf (psf, "44", &rows, &cols) ;
	psf_log_printf (psf, "    Rows : %X    Cols : %d\n", rows, cols) ;

	psf_binheader_readf (psf, "4", &type) ;

	if (type == MAT5_TYPE_SCHAR)
	{	psf_binheader_readf (psf, "4", &size) ;
		psf_log_printf (psf, "    Type : %X    Size : %d\n", type, size) ;
		if (size > SIGNED_SIZEOF (name) - 1)
		{	psf_log_printf (psf, "Error : Bad name length.\n") ;
			return SFE_MAT5_NO_BLOCK ;
			} ;

		psf_binheader_readf (psf, "bj", name, size, (8 - (size % 8)) % 8) ;
		name [size] = 0 ;
		}
	else if ((type & 0xFFFF) == MAT5_TYPE_SCHAR)
	{	size = type >> 16 ;
		if (size > 4)
		{	psf_log_printf (psf, "Error : Bad name length.\n") ;
			return SFE_MAT5_NO_BLOCK ;
			} ;

		psf_log_printf (psf, "    Type : %X\n", type) ;
		psf_binheader_readf (psf, "4", &name) ;
		name [size] = 0 ;
		}
	else
		return SFE_MAT5_NO_BLOCK ;

	psf_log_printf (psf, "    Name : %s\n", name) ;

	psf_binheader_readf (psf, "44", &type, &size) ;
	psf_log_printf (psf, "    Type : %X    Size : %d\n", type, size) ;

	/*++++++++++++++++++++++++++++++++++++++++++++++++++*/

	if (rows == 0 && cols == 0)
	{	psf_log_printf (psf, "*** Error : zero channel count.\n") ;
		return SFE_CHANNEL_COUNT_ZERO ;
		} ;

	psf->sf.channels	= rows ;
	psf->sf.frames		= cols ;

	psf->sf.format = psf->endian | SF_FORMAT_MAT5 ;

	switch (type)
	{	case MAT5_TYPE_DOUBLE :
				psf_log_printf (psf, "Data type : double\n") ;
				psf->sf.format |= SF_FORMAT_DOUBLE ;
				psf->bytewidth = 8 ;
				break ;

		case MAT5_TYPE_FLOAT :
				psf_log_printf (psf, "Data type : float\n") ;
				psf->sf.format |= SF_FORMAT_FLOAT ;
				psf->bytewidth = 4 ;
				break ;

		case MAT5_TYPE_INT32 :
				psf_log_printf (psf, "Data type : 32 bit PCM\n") ;
				psf->sf.format |= SF_FORMAT_PCM_32 ;
				psf->bytewidth = 4 ;
				break ;

		case MAT5_TYPE_INT16 :
				psf_log_printf (psf, "Data type : 16 bit PCM\n") ;
				psf->sf.format |= SF_FORMAT_PCM_16 ;
				psf->bytewidth = 2 ;
				break ;

		case MAT5_TYPE_UCHAR :
				psf_log_printf (psf, "Data type : unsigned 8 bit PCM\n") ;
				psf->sf.format |= SF_FORMAT_PCM_U8 ;
				psf->bytewidth = 1 ;
				break ;

		default :
				psf_log_printf (psf, "*** Error : Bad marker %08X\n", type) ;
				return SFE_UNIMPLEMENTED ;
		} ;

	psf->dataoffset = psf_ftell (psf) ;
	psf->datalength = psf->filelength - psf->dataoffset ;

	return 0 ;
} /* mat5_read_header */

