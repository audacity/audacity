/*
** Copyright (C) 2001-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

/*	RANT:
**	The VOC file format is the most brain damaged format I have yet had to deal
**	with. No one programmer could have bee stupid enough to put this together.
**	Instead it looks like a series of manic, dyslexic assembly language programmers
**	hacked it to fit their needs.
**	Utterly woeful.
*/

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sndfile.h"
#include "sfendian.h"
#include "common.h"


/*------------------------------------------------------------------------------
 * Typedefs for file chunks.
*/

#define	VOC_MAX_SECTIONS	200

enum
{	VOC_TERMINATOR		= 0,
	VOC_SOUND_DATA		= 1,
	VOC_SOUND_CONTINUE	= 2,
	VOC_SILENCE			= 3,
	VOC_MARKER			= 4,
	VOC_ASCII			= 5,
	VOC_REPEAT			= 6,
	VOC_END_REPEAT		= 7,
	VOC_EXTENDED		= 8,
	VOC_EXTENDED_II		= 9
} ;

typedef struct
{	int 	samples ;
	int		offset ;	/* Offset of zero => silence. */
} SND_DATA_BLOCKS ;

typedef struct
{	unsigned int 	sections, section_types ;
	int				samplerate, channels, bitwidth ;
	SND_DATA_BLOCKS	blocks [VOC_MAX_SECTIONS] ;
} VOC_DATA ;

/*------------------------------------------------------------------------------
 * Private static functions.
*/

static	int	voc_close	(SF_PRIVATE *psf) ;
static	int voc_write_header (SF_PRIVATE *psf, int calc_length) ;
static	int voc_read_header	(SF_PRIVATE *psf) ;

static const char* voc_encoding2str (int encoding) ;

#if 0

/*	These functions would be required for files with more than one VOC_SOUND_DATA
**	segment. Not sure whether to bother implementing this.
*/

static int	voc_multi_init (SF_PRIVATE *psf, VOC_DATA *pvoc) ;

static int	voc_multi_read_uc2s		(SF_PRIVATE *psf, short *ptr, int len) ;
static int	voc_multi_read_les2s	(SF_PRIVATE *psf, short *ptr, int len) ;

static int	voc_multi_read_uc2i		(SF_PRIVATE *psf, int *ptr, int len) ;
static int	voc_multi_read_les2i	(SF_PRIVATE *psf, int *ptr, int len) ;

static int	voc_multi_read_uc2f		(SF_PRIVATE *psf, float *ptr, int len) ;
static int	voc_multi_read_les2f	(SF_PRIVATE *psf, float *ptr, int len) ;

static int	voc_multi_read_uc2d		(SF_PRIVATE *psf, double *ptr, int len) ;
static int	voc_multi_read_les2d	(SF_PRIVATE *psf, double *ptr, int len) ;
#endif

/*------------------------------------------------------------------------------
** Public function.
*/

int
voc_open	(SF_PRIVATE *psf)
{	int subformat, error = 0 ;

	if (psf->is_pipe)
		return SFE_VOC_NO_PIPE ;

	if (psf->mode == SFM_READ || (psf->mode == SFM_RDWR && psf->filelength > 0))
	{	if ((error = voc_read_header (psf)))
			return error ;
		} ;

	subformat = SF_CODEC (psf->sf.format) ;

	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_VOC)
			return	SFE_BAD_OPEN_FORMAT ;

		psf->endian = SF_ENDIAN_LITTLE ;

		if ((error = voc_write_header (psf, SF_FALSE)))
			return error ;

		psf->write_header = voc_write_header ;
		} ;

	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

	psf->container_close = voc_close ;

	switch (subformat)
	{	case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_16 :
				error = pcm_init (psf) ;
				break ;

		case SF_FORMAT_ALAW :
				error = alaw_init (psf) ;
				break ;

		case SF_FORMAT_ULAW :
				error = ulaw_init (psf) ;
				break ;

		default : return SFE_UNIMPLEMENTED ;
		} ;

	return error ;
} /* voc_open */

/*------------------------------------------------------------------------------
*/

static int
voc_read_header	(SF_PRIVATE *psf)
{	VOC_DATA	*pvoc ;
	char	creative [20] ;
	unsigned char block_type, rate_byte ;
	short	version, checksum, encoding, dataoffset ;
	int		offset ;

	/* Set position to start of file to begin reading header. */
	offset = psf_binheader_readf (psf, "pb", 0, creative, SIGNED_SIZEOF (creative)) ;

	if (creative [sizeof (creative) - 1] != 0x1A)
		return SFE_VOC_NO_CREATIVE ;

	/* Terminate the string. */
	creative [sizeof (creative) - 1] = 0 ;

	if (strcmp ("Creative Voice File", creative))
		return SFE_VOC_NO_CREATIVE ;

	psf_log_printf (psf, "%s\n", creative) ;

	offset += psf_binheader_readf (psf, "e222", &dataoffset, &version, &checksum) ;

	psf->dataoffset = dataoffset ;

	psf_log_printf (psf, 	"dataoffset : %d\n"
							"version    : 0x%X\n"
							"checksum   : 0x%X\n", psf->dataoffset, version, checksum) ;

	if (version != 0x010A && version != 0x0114)
		return SFE_VOC_BAD_VERSION ;

	if (! (psf->codec_data = malloc (sizeof (VOC_DATA))))
		return SFE_MALLOC_FAILED ;

	pvoc = (VOC_DATA*) psf->codec_data ;

	memset (pvoc, 0, sizeof (VOC_DATA)) ;

	/* Set the default encoding now. */
	psf->sf.format = SF_FORMAT_VOC ; /* Major format */
	encoding = SF_FORMAT_PCM_U8 ; /* Minor format */
	psf->endian = SF_ENDIAN_LITTLE ;

	while (1)
	{	int size ;
		short count ;

		block_type = 0 ;
		offset += psf_binheader_readf (psf, "1", &block_type) ;

		switch (block_type)
		{	case VOC_ASCII :
					offset += psf_binheader_readf (psf, "e3", &size) ;

					psf_log_printf (psf, " ASCII : %d\n", size) ;

					offset += psf_binheader_readf (psf, "b", psf->header, size) ;
					psf->header [size] = 0 ;
					psf_log_printf (psf, "  text : %s\n", psf->header) ;
					continue ;

			case VOC_REPEAT :
					offset += psf_binheader_readf (psf, "e32", &size, &count) ;
					psf_log_printf (psf, " Repeat : %d\n", count) ;
					continue ;

			case VOC_SOUND_DATA :
			case VOC_EXTENDED :
			case VOC_EXTENDED_II :
					break ;

			default : psf_log_printf (psf, "*** Weird block marker (%d)\n", block_type) ;
			} ;

		break ;
		} ;

	if (block_type == VOC_SOUND_DATA)
	{	unsigned char compression ;
		int 	size ;

		offset += psf_binheader_readf (psf, "e311", &size, &rate_byte, &compression) ;

		psf->sf.samplerate = 1000000 / (256 - (rate_byte & 0xFF)) ;

		psf_log_printf (psf, " Sound Data : %d\n  sr   : %d => %dHz\n  comp : %d\n",
								size, rate_byte, psf->sf.samplerate, compression) ;

		if (offset + size - 1 > psf->filelength)
		{	psf_log_printf (psf, "Seems to be a truncated file.\n") ;
			psf_log_printf (psf, "offset: %d    size: %d    sum: %d    filelength: %D\n", offset, size, offset + size, psf->filelength) ;
			return SFE_VOC_BAD_SECTIONS ;
			}
		else if (psf->filelength - offset - size > 4)
		{	psf_log_printf (psf, "Seems to be a multi-segment file (#1).\n") ;
			psf_log_printf (psf, "offset: %d    size: %d    sum: %d    filelength: %D\n", offset, size, offset + size, psf->filelength) ;
			return SFE_VOC_BAD_SECTIONS ;
			} ;

		psf->dataoffset = offset ;
		psf->dataend	= psf->filelength - 1 ;

		psf->sf.channels = 1 ;
		psf->bytewidth = 1 ;

		psf->sf.format = SF_FORMAT_VOC | SF_FORMAT_PCM_U8 ;

		return 0 ;
		} ;

	if (block_type == VOC_EXTENDED)
	{	unsigned char pack, stereo, compression ;
		unsigned short rate_short ;
		int		size ;

		offset += psf_binheader_readf (psf, "e3211", &size, &rate_short, &pack, &stereo) ;

		psf_log_printf (psf, " Extended : %d\n", size) ;
		if (size == 4)
			psf_log_printf (psf, "  size   : 4\n") ;
		else
			psf_log_printf (psf, "  size   : %d (should be 4)\n", size) ;

		psf_log_printf (psf,	"  pack   : %d\n"
								"  stereo : %s\n", pack, (stereo ? "yes" : "no")) ;

		if (stereo)
		{	psf->sf.channels = 2 ;
			psf->sf.samplerate = 128000000 / (65536 - rate_short) ;
			}
		else
		{	psf->sf.channels = 1 ;
			psf->sf.samplerate = 256000000 / (65536 - rate_short) ;
			} ;

		psf_log_printf (psf, "  sr     : %d => %dHz\n", (rate_short & 0xFFFF), psf->sf.samplerate) ;

		offset += psf_binheader_readf (psf, "1", &block_type) ;

		if (block_type != VOC_SOUND_DATA)
		{	psf_log_printf (psf, "*** Expecting VOC_SOUND_DATA section.\n") ;
			return SFE_VOC_BAD_FORMAT ;
			} ;

		offset += psf_binheader_readf (psf, "e311", &size, &rate_byte, &compression) ;

		psf_log_printf (psf,	" Sound Data : %d\n"
								"  sr     : %d\n"
								"  comp   : %d\n", size, rate_byte, compression) ;


		if (offset + size - 1 > psf->filelength)
		{	psf_log_printf (psf, "Seems to be a truncated file.\n") ;
			psf_log_printf (psf, "offset: %d    size: %d    sum: %d    filelength: %D\n", offset, size, offset + size, psf->filelength) ;
			return SFE_VOC_BAD_SECTIONS ;
			}
		else if (offset + size - 1 < psf->filelength)
		{	psf_log_printf (psf, "Seems to be a multi-segment file (#2).\n") ;
			psf_log_printf (psf, "offset: %d    size: %d    sum: %d    filelength: %D\n", offset, size, offset + size, psf->filelength) ;
			return SFE_VOC_BAD_SECTIONS ;
			} ;

		psf->dataoffset = offset ;
		psf->dataend = psf->filelength - 1 ;

		psf->bytewidth = 1 ;

		psf->sf.format = SF_FORMAT_VOC | SF_FORMAT_PCM_U8 ;

		return 0 ;
		}

	if (block_type == VOC_EXTENDED_II)
	{	unsigned char bitwidth, channels ;
		int size, fourbytes ;

		offset += psf_binheader_readf (psf, "e341124", &size, &psf->sf.samplerate,
								&bitwidth, &channels, &encoding, &fourbytes) ;

		if (size * 2 == psf->filelength - 39)
		{	int temp_size = psf->filelength - 31 ;

			psf_log_printf (psf, " Extended II : %d (SoX bug: should be %d)\n", size, temp_size) ;
			size = temp_size ;
			}
		else
			psf_log_printf (psf, " Extended II : %d\n", size) ;

		psf_log_printf (psf,	"  sample rate : %d\n"
								"  bit width   : %d\n"
								"  channels    : %d\n", psf->sf.samplerate, bitwidth, channels) ;

		if (bitwidth == 16 && encoding == 0)
		{	encoding = 4 ;
			psf_log_printf (psf, "  encoding    : 0 (SoX bug: should be 4 for 16 bit signed PCM)\n") ;
			}
		else
			psf_log_printf (psf, "  encoding    : %d => %s\n", encoding, voc_encoding2str (encoding)) ;


		psf_log_printf (psf, "  fourbytes   : %X\n", fourbytes) ;

		psf->sf.channels = channels ;

		psf->dataoffset = offset ;
		psf->dataend	= psf->filelength - 1 ;

		if (size + 31 == psf->filelength + 1)
		{	/* Hack for reading files produced using
			** sf_command (SFC_UPDATE_HEADER_NOW).
			*/
			psf_log_printf (psf, "Missing zero byte at end of file.\n") ;
			size = psf->filelength - 30 ;
			psf->dataend = 0 ;
			}
		else if (size + 31 > psf->filelength)
		{	psf_log_printf (psf, "Seems to be a truncated file.\n") ;
			size = psf->filelength - 31 ;
			}
		else if (size + 31 < psf->filelength)
			psf_log_printf (psf, "Seems to be a multi-segment file (#3).\n") ;

		switch (encoding)
		{	case 0 :
					psf->sf.format = SF_FORMAT_VOC | SF_FORMAT_PCM_U8 ;
					psf->bytewidth = 1 ;
					break ;

			case 4 :
					psf->sf.format = SF_FORMAT_VOC | SF_FORMAT_PCM_16 ;
					psf->bytewidth = 2 ;
					break ;

			case 6 :
					psf->sf.format = SF_FORMAT_VOC | SF_FORMAT_ALAW ;
					psf->bytewidth = 1 ;
					break ;

			case 7 :
					psf->sf.format = SF_FORMAT_VOC | SF_FORMAT_ULAW ;
					psf->bytewidth = 1 ;
					break ;

			default : /* Unknown */
					return SFE_UNKNOWN_FORMAT ;
					break ;
			} ;

		} ;

	return 0 ;
} /* voc_read_header */

/*====================================================================================
*/

static int
voc_write_header (SF_PRIVATE *psf, int calc_length)
{	sf_count_t	current ;
	int			rate_const, subformat ;

	current = psf_ftell (psf) ;

	if (calc_length)
	{	psf->filelength = psf_get_filelen (psf) ;

		psf->datalength = psf->filelength - psf->dataoffset ;
		if (psf->dataend)
			psf->datalength -= psf->filelength - psf->dataend ;

		psf->sf.frames = psf->datalength / (psf->bytewidth * psf->sf.channels) ;
		} ;

	subformat = SF_CODEC (psf->sf.format) ;
	/* Reset the current header length to zero. */
	psf->header [0] = 0 ;
	psf->headindex = 0 ;
	psf_fseek (psf, 0, SEEK_SET) ;

	/* VOC marker and 0x1A byte. */
	psf_binheader_writef (psf, "eb1", "Creative Voice File", make_size_t (19), 0x1A) ;

	/* Data offset, version and other. */
	psf_binheader_writef (psf, "e222", 26, 0x0114, 0x111F) ;

	/*	Use same logic as SOX.
	**	If the file is mono 8 bit data, use VOC_SOUND_DATA.
	**	If the file is mono 16 bit data, use VOC_EXTENED.
	**	Otherwise use VOC_EXTENED_2.
	*/

	if (subformat == SF_FORMAT_PCM_U8 && psf->sf.channels == 1)
	{	/* samplerate = 1000000 / (256 - rate_const) ; */
		rate_const = 256 - 1000000 / psf->sf.samplerate ;

		/* First type marker, length, rate_const and compression */
		psf_binheader_writef (psf, "e1311", VOC_SOUND_DATA, (int) (psf->datalength + 1), rate_const, 0) ;
		}
	else if (subformat == SF_FORMAT_PCM_U8 && psf->sf.channels == 2)
	{	/* sample_rate = 128000000 / (65536 - rate_short) ; */
		rate_const = 65536 - 128000000 / psf->sf.samplerate ;

		/* First write the VOC_EXTENDED section
		** 		marker, length, rate_const and compression
		*/
		psf_binheader_writef (psf, "e13211", VOC_EXTENDED, 4, rate_const, 0, 1) ;

		/* samplerate = 1000000 / (256 - rate_const) ; */
		rate_const = 256 - 1000000 / psf->sf.samplerate ;

		/*	Now write the VOC_SOUND_DATA section
		** 		marker, length, rate_const and compression
		*/
		psf_binheader_writef (psf, "e1311", VOC_SOUND_DATA, (int) (psf->datalength + 1), rate_const, 0) ;
		}
	else
	{	int length ;

		if (psf->sf.channels < 1 || psf->sf.channels > 2)
			return SFE_CHANNEL_COUNT ;

		switch (subformat)
		{	case SF_FORMAT_PCM_U8 :
					psf->bytewidth = 1 ;
					length = psf->sf.frames * psf->sf.channels * psf->bytewidth + 12 ;
					/* Marker, length, sample rate, bitwidth, stereo flag, encoding and fourt zero bytes. */
					psf_binheader_writef (psf, "e1341124", VOC_EXTENDED_II, length, psf->sf.samplerate, 16, psf->sf.channels, 4, 0) ;
					break ;

			case SF_FORMAT_PCM_16 :
					psf->bytewidth = 2 ;
					length = psf->sf.frames * psf->sf.channels * psf->bytewidth + 12 ;
					/* Marker, length, sample rate, bitwidth, stereo flag, encoding and fourt zero bytes. */
					psf_binheader_writef (psf, "e1341124", VOC_EXTENDED_II, length, psf->sf.samplerate, 16, psf->sf.channels, 4, 0) ;
					break ;

			case SF_FORMAT_ALAW :
					psf->bytewidth = 1 ;
					length = psf->sf.frames * psf->sf.channels * psf->bytewidth + 12 ;
					psf_binheader_writef (psf, "e1341124", VOC_EXTENDED_II, length, psf->sf.samplerate, 8, psf->sf.channels, 6, 0) ;
					break ;

			case SF_FORMAT_ULAW :
					psf->bytewidth = 1 ;
					length = psf->sf.frames * psf->sf.channels * psf->bytewidth + 12 ;
					psf_binheader_writef (psf, "e1341124", VOC_EXTENDED_II, length, psf->sf.samplerate, 8, psf->sf.channels, 7, 0) ;
					break ;

			default : return SFE_UNIMPLEMENTED ;
			} ;
		} ;

	psf_fwrite (psf->header, psf->headindex, 1, psf) ;

	if (psf->error)
		return psf->error ;

	psf->dataoffset = psf->headindex ;

	if (current > 0)
		psf_fseek (psf, current, SEEK_SET) ;

	return psf->error ;
} /* voc_write_header */

static int
voc_close	(SF_PRIVATE *psf)
{
	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	/*  Now we know for certain the length of the file we can re-write
		**	correct values for the FORM, 8SVX and BODY chunks.
		*/
		unsigned byte = VOC_TERMINATOR ;


		psf_fseek (psf, 0, SEEK_END) ;

		/* Write terminator */
		psf_fwrite (&byte, 1, 1, psf) ;

		voc_write_header (psf, SF_TRUE) ;
		} ;

	return 0 ;
} /* voc_close */

static const 	char*
voc_encoding2str (int encoding)
{
	switch (encoding)
	{	case 0 :	return "8 bit unsigned PCM" ;
		case 4 :	return "16 bit signed PCM" ;
		case 6 :	return "A-law" ;
		case 7 :	return "u-law" ;
		default :	break ;
		}
	return "*** Unknown ***" ;
} /* voc_encoding2str */

/*====================================================================================
*/

#if 0
static int
voc_multi_init (SF_PRIVATE *psf, VOC_DATA *pvoc)
{
	psf->sf.frames = 0 ;

	if (pvoc->bitwidth == 8)
	{	psf->read_short		= voc_multi_read_uc2s ;
		psf->read_int		= voc_multi_read_uc2i ;
		psf->read_float		= voc_multi_read_uc2f ;
		psf->read_double	= voc_multi_read_uc2d ;
		return 0 ;
		} ;

	if (pvoc->bitwidth == 16)
	{	psf->read_short		= voc_multi_read_les2s ;
		psf->read_int		= voc_multi_read_les2i ;
		psf->read_float		= voc_multi_read_les2f ;
		psf->read_double	= voc_multi_read_les2d ;
		return 0 ;
		} ;

	psf_log_printf (psf, "Error : bitwith != 8 && bitwidth != 16.\n") ;

	return SFE_UNIMPLEMENTED ;
} /* voc_multi_read_int */

/*------------------------------------------------------------------------------------
*/

static int
voc_multi_read_uc2s (SF_PRIVATE *psf, short *ptr, int len)
{

	return 0 ;
} /* voc_multi_read_uc2s */

static int
voc_multi_read_les2s (SF_PRIVATE *psf, short *ptr, int len)
{

	return 0 ;
} /* voc_multi_read_les2s */


static int
voc_multi_read_uc2i (SF_PRIVATE *psf, int *ptr, int len)
{

	return 0 ;
} /* voc_multi_read_uc2i */

static int
voc_multi_read_les2i (SF_PRIVATE *psf, int *ptr, int len)
{

	return 0 ;
} /* voc_multi_read_les2i */


static int
voc_multi_read_uc2f (SF_PRIVATE *psf, float *ptr, int len)
{

	return 0 ;
} /* voc_multi_read_uc2f */

static int
voc_multi_read_les2f (SF_PRIVATE *psf, float *ptr, int len)
{

	return 0 ;
} /* voc_multi_read_les2f */


static int
voc_multi_read_uc2d (SF_PRIVATE *psf, double *ptr, int len)
{

	return 0 ;
} /* voc_multi_read_uc2d */

static int
voc_multi_read_les2d (SF_PRIVATE *psf, double *ptr, int len)
{

	return 0 ;
} /* voc_multi_read_les2d */

#endif

/*------------------------------------------------------------------------------------

Creative Voice (VOC) file format
--------------------------------

~From: galt@dsd.es.com

(byte numbers are hex!)

    HEADER (bytes 00-19)
    Series of DATA BLOCKS (bytes 1A+) [Must end w/ Terminator Block]

- ---------------------------------------------------------------

HEADER:
=======
     byte #     Description
     ------     ------------------------------------------
     00-12      "Creative Voice File"
     13         1A (eof to abort printing of file)
     14-15      Offset of first datablock in .voc file (std 1A 00
                in Intel Notation)
     16-17      Version number (minor,major) (VOC-HDR puts 0A 01)
     18-19      1's Comp of Ver. # + 1234h (VOC-HDR puts 29 11)

- ---------------------------------------------------------------

DATA BLOCK:
===========

   Data Block:  TYPE(1-byte), SIZE(3-bytes), INFO(0+ bytes)
   NOTE: Terminator Block is an exception -- it has only the TYPE byte.

      TYPE   Description     Size (3-byte int)   Info
      ----   -----------     -----------------   -----------------------
      00     Terminator      (NONE)              (NONE)
      01     Sound data      2+length of data    *
      02     Sound continue  length of data      Voice Data
      03     Silence         3                   **
      04     Marker          2                   Marker# (2 bytes)
      05     ASCII           length of string    null terminated string
      06     Repeat          2                   Count# (2 bytes)
      07     End repeat      0                   (NONE)
      08     Extended        4                   ***

      *Sound Info Format:
       ---------------------
       00   Sample Rate
       01   Compression Type
       02+  Voice Data

      **Silence Info Format:
      ----------------------------
      00-01  Length of silence - 1
      02     Sample Rate


    ***Extended Info Format:
       ---------------------
       00-01  Time Constant: Mono: 65536 - (256000000/sample_rate)
                             Stereo: 65536 - (25600000/(2*sample_rate))
       02     Pack
       03     Mode: 0 = mono
                    1 = stereo


  Marker#           -- Driver keeps the most recent marker in a status byte
  Count#            -- Number of repetitions + 1
                         Count# may be 1 to FFFE for 0 - FFFD repetitions
                         or FFFF for endless repetitions
  Sample Rate       -- SR byte = 256-(1000000/sample_rate)
  Length of silence -- in units of sampling cycle
  Compression Type  -- of voice data
                         8-bits    = 0
                         4-bits    = 1
                         2.6-bits  = 2
                         2-bits    = 3
                         Multi DAC = 3+(# of channels) [interesting--
                                       this isn't in the developer's manual]


---------------------------------------------------------------------------------
Addendum submitted by Votis Kokavessis:

After some experimenting with .VOC files I found out that there is a Data Block
Type 9, which is not covered in the VOC.TXT file. Here is what I was able to discover
about this block type:


TYPE: 09
SIZE: 12 + length of data
INFO: 12 (twelve) bytes

INFO STRUCTURE:

Bytes 0-1: (Word) Sample Rate (e.g. 44100)
Bytes 2-3: zero (could be that bytes 0-3 are a DWord for Sample Rate)
Byte 4: Sample Size in bits (e.g. 16)
Byte 5: Number of channels (e.g. 1 for mono, 2 for stereo)
Byte 6: Unknown (equal to 4 in all files I examined)
Bytes 7-11: zero


-------------------------------------------------------------------------------------*/

/*=====================================================================================
**=====================================================================================
**=====================================================================================
**=====================================================================================
*/

/*------------------------------------------------------------------------
The following is taken from the Audio File Formats FAQ dated 2-Jan-1995
and submitted by Guido van Rossum <guido@cwi.nl>.
--------------------------------------------------------------------------
Creative Voice (VOC) file format
--------------------------------

From: galt@dsd.es.com

(byte numbers are hex!)

    HEADER (bytes 00-19)
    Series of DATA BLOCKS (bytes 1A+) [Must end w/ Terminator Block]

- ---------------------------------------------------------------

HEADER:
-------
     byte #     Description
     ------     ------------------------------------------
     00-12      "Creative Voice File"
     13         1A (eof to abort printing of file)
     14-15      Offset of first datablock in .voc file (std 1A 00
                in Intel Notation)
     16-17      Version number (minor,major) (VOC-HDR puts 0A 01)
     18-19      2's Comp of Ver. # + 1234h (VOC-HDR puts 29 11)

- ---------------------------------------------------------------

DATA BLOCK:
-----------

   Data Block:  TYPE(1-byte), SIZE(3-bytes), INFO(0+ bytes)
   NOTE: Terminator Block is an exception -- it has only the TYPE byte.

      TYPE   Description     Size (3-byte int)   Info
      ----   -----------     -----------------   -----------------------
      00     Terminator      (NONE)              (NONE)
      01     Sound data      2+length of data    *
      02     Sound continue  length of data      Voice Data
      03     Silence         3                   **
      04     Marker          2                   Marker# (2 bytes)
      05     ASCII           length of string    null terminated string
      06     Repeat          2                   Count# (2 bytes)
      07     End repeat      0                   (NONE)
      08     Extended        4                   ***

      *Sound Info Format:       **Silence Info Format:
       ---------------------      ----------------------------
       00   Sample Rate           00-01  Length of silence - 1
       01   Compression Type      02     Sample Rate
       02+  Voice Data

    ***Extended Info Format:
       ---------------------
       00-01  Time Constant: Mono: 65536 - (256000000/sample_rate)
                             Stereo: 65536 - (25600000/(2*sample_rate))
       02     Pack
       03     Mode: 0 = mono
                    1 = stereo


  Marker#           -- Driver keeps the most recent marker in a status byte
  Count#            -- Number of repetitions + 1
                         Count# may be 1 to FFFE for 0 - FFFD repetitions
                         or FFFF for endless repetitions
  Sample Rate       -- SR byte = 256-(1000000/sample_rate)
  Length of silence -- in units of sampling cycle
  Compression Type  -- of voice data
                         8-bits    = 0
                         4-bits    = 1
                         2.6-bits  = 2
                         2-bits    = 3
                         Multi DAC = 3+(# of channels) [interesting--
                                       this isn't in the developer's manual]

Detailed description of new data blocks (VOC files version 1.20 and above):

        (Source is fax from Barry Boone at Creative Labs, 405/742-6622)

BLOCK 8 - digitized sound attribute extension, must preceed block 1.
          Used to define stereo, 8 bit audio
        BYTE bBlockID;       // = 8
        BYTE nBlockLen[3];   // 3 byte length
        WORD wTimeConstant;  // time constant = same as block 1
        BYTE bPackMethod;    // same as in block 1
        BYTE bVoiceMode;     // 0-mono, 1-stereo

        Data is stored left, right

BLOCK 9 - data block that supersedes blocks 1 and 8.
          Used for stereo, 16 bit.

        BYTE bBlockID;          // = 9
        BYTE nBlockLen[3];      // length 12 plus length of sound
        DWORD dwSamplesPerSec;  // samples per second, not time const.
        BYTE bBitsPerSample;    // e.g., 8 or 16
        BYTE bChannels;         // 1 for mono, 2 for stereo
        WORD wFormat;           // see below
        BYTE reserved[4];       // pad to make block w/o data
                                // have a size of 16 bytes

        Valid values of wFormat are:

                0x0000  8-bit unsigned PCM
                0x0001  Creative 8-bit to 4-bit ADPCM
                0x0002  Creative 8-bit to 3-bit ADPCM
                0x0003  Creative 8-bit to 2-bit ADPCM
                0x0004  16-bit signed PCM
                0x0006  CCITT a-Law
                0x0007  CCITT u-Law
                0x02000 Creative 16-bit to 4-bit ADPCM

        Data is stored left, right

------------------------------------------------------------------------*/
