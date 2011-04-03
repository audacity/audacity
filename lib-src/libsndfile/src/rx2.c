/*
** Copyright (C) 2001-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include	<string.h>
#include	<ctype.h>
#include	<stdarg.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

#if (ENABLE_EXPERIMENTAL_CODE == 0)

int
rx2_open	(SF_PRIVATE *psf)
{	if (psf)
		return SFE_UNIMPLEMENTED ;
	return 0 ;
} /* rx2_open */

#else

/*------------------------------------------------------------------------------
 * Macros to handle big/little endian issues.
*/

#define	CAT_MARKER	(MAKE_MARKER ('C', 'A', 'T', ' '))
#define	GLOB_MARKER (MAKE_MARKER ('G', 'L', 'O', 'B'))

#define	RECY_MARKER (MAKE_MARKER ('R', 'E', 'C', 'Y'))

#define	SLCL_MARKER (MAKE_MARKER ('S', 'L', 'C', 'L'))
#define	SLCE_MARKER (MAKE_MARKER ('S', 'L', 'C', 'E'))

#define	DEVL_MARKER	(MAKE_MARKER ('D', 'E', 'V', 'L'))
#define	TRSH_MARKER	(MAKE_MARKER ('T', 'R', 'S', 'H'))

#define	EQ_MARKER	(MAKE_MARKER ('E', 'Q', ' ', ' '))
#define	COMP_MARKER (MAKE_MARKER ('C', 'O', 'M', 'P'))

#define	SINF_MARKER (MAKE_MARKER ('S', 'I', 'N', 'F'))
#define	SDAT_MARKER (MAKE_MARKER ('S', 'D', 'A', 'T'))

/*------------------------------------------------------------------------------
 * Typedefs for file chunks.
*/


/*------------------------------------------------------------------------------
 * Private static functions.
*/
static int	rx2_close	(SF_PRIVATE *psf) ;

/*------------------------------------------------------------------------------
** Public functions.
*/

int
rx2_open	(SF_PRIVATE *psf)
{	static const char *marker_type [4] =
	{	"Original Enabled", "Enabled Hidden",
		"Additional/PencilTool", "Disabled"
		} ;

	int error, marker, length, glob_offset, slce_count, frames ;

	int sdat_length = 0, slce_total = 0 ;

	int n_channels ;


	/* So far only doing read. */

	psf_binheader_readf (psf, "Epm4", 0, &marker, &length) ;

	if (marker != CAT_MARKER)
	{	psf_log_printf (psf, "length : %d\n", length) ;
		return -1000 ;
		} ;

	if (length != psf->filelength - 8)
		psf_log_printf (psf, "%M : %d (should be %d)\n", marker, length, psf->filelength - 8) ;
	else
		psf_log_printf (psf, "%M : %d\n", marker, length) ;

	/* 'REX2' marker */
	psf_binheader_readf (psf, "m", &marker) ;
	psf_log_printf (psf, "%M", marker) ;

	/* 'HEAD' marker */
	psf_binheader_readf (psf, "m", &marker) ;
	psf_log_printf (psf, "%M\n", marker) ;

	/* Grab 'GLOB' offset. */
	psf_binheader_readf (psf, "E4", &glob_offset) ;
	glob_offset += 0x14 ;	/* Add the current file offset. */

	/* Jump to offset 0x30 */
	psf_binheader_readf (psf, "p", 0x30) ;

	/* Get name length */
	length = 0 ;
	psf_binheader_readf (psf, "1", &length) ;
	if (length >= SIGNED_SIZEOF (psf->u.cbuf))
	{	psf_log_printf (psf, "  Text : %d *** Error : Too sf_count_t!\n") ;
		return -1001 ;
		}

	memset (psf->u.cbuf, 0, sizeof (psf->u.cbuf)) ;
	psf_binheader_readf (psf, "b", psf->u.cbuf, length) ;
	psf_log_printf (psf, " Text : \"%s\"\n", psf->u.cbuf) ;

	/* Jump to GLOB offset position. */
	if (glob_offset & 1)
		glob_offset ++ ;

	psf_binheader_readf (psf, "p", glob_offset) ;

	slce_count = 0 ;
	/* GLOB */
	while (1)
	{	psf_binheader_readf (psf, "m", &marker) ;

		if (marker != SLCE_MARKER && slce_count > 0)
		{	psf_log_printf (psf, "   SLCE count : %d\n", slce_count) ;
			slce_count = 0 ;
			}
		switch (marker)
		{	case GLOB_MARKER:
					psf_binheader_readf (psf, "E4", &length) ;
					psf_log_printf (psf, " %M : %d\n", marker, length) ;
					psf_binheader_readf (psf, "j", length) ;
					break ;

			case RECY_MARKER:
					psf_binheader_readf (psf, "E4", &length) ;
					psf_log_printf (psf, " %M : %d\n", marker, length) ;
					psf_binheader_readf (psf, "j", (length+1) & 0xFFFFFFFE) ; /* ?????? */
					break ;

			case CAT_MARKER:
					psf_binheader_readf (psf, "E4", &length) ;
					psf_log_printf (psf, " %M : %d\n", marker, length) ;
					/*-psf_binheader_readf (psf, "j", length) ;-*/
					break ;

			case DEVL_MARKER:
					psf_binheader_readf (psf, "mE4", &marker, &length) ;
					psf_log_printf (psf, "  DEVL%M : %d\n", marker, length) ;
					if (length & 1)
						length ++ ;
					psf_binheader_readf (psf, "j", length) ;
					break ;

			case EQ_MARKER:
			case COMP_MARKER:
					psf_binheader_readf (psf, "E4", &length) ;
					psf_log_printf (psf, "   %M : %d\n", marker, length) ;
					/* This is weird!!!! why make this (length - 1) */
					if (length & 1)
						length ++ ;
					psf_binheader_readf (psf, "j", length) ;
					break ;

			case SLCL_MARKER:
					psf_log_printf (psf, "  %M\n    (Offset, Next Offset, Type)\n", marker) ;
					slce_count = 0 ;
					break ;

			case SLCE_MARKER:
					{	int len [4], indx ;

						psf_binheader_readf (psf, "E4444", &len [0], &len [1], &len [2], &len [3]) ;

						indx = ((len [3] & 0x0000FFFF) >> 8) & 3 ;

						if (len [2] == 1)
						{	if (indx != 1)
								indx = 3 ;	/* 2 cases, where next slice offset = 1 -> disabled & enabled/hidden */

							psf_log_printf (psf, "   %M : (%6d, ?: 0x%X, %s)\n", marker, len [1], (len [3] & 0xFFFF0000) >> 16, marker_type [indx]) ;
							}
						else
						{	slce_total += len [2] ;

							psf_log_printf (psf, "   %M : (%6d, SLCE_next_ofs:%d, ?: 0x%X, %s)\n", marker, len [1], len [2], (len [3] & 0xFFFF0000) >> 16, marker_type [indx]) ;
							} ;

						slce_count ++ ;
						} ;
					break ;

			case SINF_MARKER:
					psf_binheader_readf (psf, "E4", &length) ;
					psf_log_printf (psf, " %M : %d\n", marker, length) ;

					psf_binheader_readf (psf, "E2", &n_channels) ;
					n_channels = (n_channels & 0x0000FF00) >> 8 ;
					psf_log_printf (psf, "  Channels    : %d\n", n_channels) ;

					psf_binheader_readf (psf, "E44", &psf->sf.samplerate, &frames) ;
					psf->sf.frames = frames ;
					psf_log_printf (psf, "  Sample Rate : %d\n", psf->sf.samplerate) ;
					psf_log_printf (psf, "  Frames      : %D\n", psf->sf.frames) ;

					psf_binheader_readf (psf, "E4", &length) ;
					psf_log_printf (psf, "  ??????????? : %d\n", length) ;

					psf_binheader_readf (psf, "E4", &length) ;
					psf_log_printf (psf, "  ??????????? : %d\n", length) ;
					break ;

			case SDAT_MARKER:
					psf_binheader_readf (psf, "E4", &length) ;

				sdat_length = length ;

					/* Get the current offset. */
					psf->dataoffset = psf_binheader_readf (psf, NULL) ;

					if (psf->dataoffset + length != psf->filelength)
						psf_log_printf (psf, " %M : %d (should be %d)\n", marker, length, psf->dataoffset + psf->filelength) ;
					else
						psf_log_printf (psf, " %M : %d\n", marker, length) ;
					break ;

			default :
					psf_log_printf (psf, "Unknown marker : 0x%X %M", marker, marker) ;
					return -1003 ;
					break ;
			} ;

		/* SDAT always last marker in file. */
		if (marker == SDAT_MARKER)
			break ;
		} ;

	puts (psf->logbuffer) ;
	puts ("-----------------------------------") ;

	printf ("SDAT length  : %d\n", sdat_length) ;
	printf ("SLCE count   : %d\n", slce_count) ;

	/* Hack for zero slice count. */
	if (slce_count == 0 && slce_total == 1)
		slce_total = frames ;

	printf ("SLCE samples : %d\n", slce_total) ;

	/* Two bytes per sample. */
	printf ("Comp Ratio   : %f:1\n", (2.0 * slce_total * n_channels) / sdat_length) ;

	puts (" ") ;

	psf->logbuffer [0] = 0 ;

	/* OK, have the header although not too sure what it all means. */

	psf->endian = SF_ENDIAN_BIG ;

	psf->datalength = psf->filelength - psf->dataoffset ;

 	if (psf_fseek (psf, psf->dataoffset, SEEK_SET))
		return SFE_BAD_SEEK ;

	psf->sf.format = (SF_FORMAT_REX2 | SF_FORMAT_DWVW_12) ;

	psf->sf.channels	= 1 ;
	psf->bytewidth		= 2 ;
	psf->blockwidth		= psf->sf.channels * psf->bytewidth ;

	if ((error = dwvw_init (psf, 16)))
		return error ;

	psf->container_close = rx2_close ;

	if (! psf->sf.frames && psf->blockwidth)
		psf->sf.frames = psf->datalength / psf->blockwidth ;

	/* All done. */

	return 0 ;
} /* rx2_open */

/*------------------------------------------------------------------------------
*/

static int
rx2_close	(SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_WRITE)
	{	/*  Now we know for certain the length of the file we can re-write
		**	correct values for the FORM, 8SVX and BODY chunks.
		*/

		} ;

	return 0 ;
} /* rx2_close */

#endif
