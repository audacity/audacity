/*
** Copyright (C) 2001-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2004 Paavo Jumppanen
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

/*
** The sd2 support implemented in this file was partially sponsored
** (financially) by Paavo Jumppanen.
*/

/*
** Documentation on the Mac resource fork was obtained here :
** http://developer.apple.com/documentation/mac/MoreToolbox/MoreToolbox-99.html
*/

#include	"sfconfig.h"

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<ctype.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

/*------------------------------------------------------------------------------
 * Markers.
*/

#define	Sd2f_MARKER			MAKE_MARKER ('S', 'd', '2', 'f')
#define	Sd2a_MARKER			MAKE_MARKER ('S', 'd', '2', 'a')
#define	ALCH_MARKER			MAKE_MARKER ('A', 'L', 'C', 'H')
#define lsf1_MARKER			MAKE_MARKER ('l', 's', 'f', '1')

#define STR_MARKER			MAKE_MARKER ('S', 'T', 'R', ' ')
#define sdML_MARKER			MAKE_MARKER ('s', 'd', 'M', 'L')

enum
{	RSRC_STR = 111,
	RSRC_BIN
} ;

typedef struct
{	unsigned char * rsrc_data ;
	int rsrc_len ;
	int need_to_free_rsrc_data ;

	int data_offset, data_length ;
	int map_offset, map_length ;

	int type_count, type_offset ;
	int item_offset ;

	int str_index, str_count ;

	int string_offset ;

	/* All the above just to get these three. */
	int sample_size, sample_rate, channels ;
} SD2_RSRC ;

typedef struct
{	int type ;
	int id ;
	char name [32] ;
	char value [32] ;
	int value_len ;
} STR_RSRC ;

/*------------------------------------------------------------------------------
 * Private static functions.
*/

static int sd2_close	(SF_PRIVATE *psf) ;

static int sd2_parse_rsrc_fork (SF_PRIVATE *psf) ;
static int parse_str_rsrc (SF_PRIVATE *psf, SD2_RSRC * rsrc) ;

static int sd2_write_rsrc_fork (SF_PRIVATE *psf, int calc_length) ;

/*------------------------------------------------------------------------------
** Public functions.
*/

int
sd2_open (SF_PRIVATE *psf)
{	int subformat, error = 0, valid ;

	/* SD2 is always big endian. */
	psf->endian = SF_ENDIAN_BIG ;

	if (psf->file.mode == SFM_READ || (psf->file.mode == SFM_RDWR && psf->rsrclength > 0))
	{	psf_use_rsrc (psf, SF_TRUE) ;
		valid = psf_file_valid (psf) ;
		psf_use_rsrc (psf, SF_FALSE) ;
		if (! valid)
		{	psf_log_printf (psf, "sd2_open : psf->rsrc.filedes < 0\n") ;
			return SFE_SD2_BAD_RSRC ;
			} ;

		error = sd2_parse_rsrc_fork (psf) ;

		if (error)
			goto error_cleanup ;
		} ;

	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_SD2)
	{	error = SFE_BAD_OPEN_FORMAT ;
		goto error_cleanup ;
		} ;

	subformat = SF_CODEC (psf->sf.format) ;
	psf->dataoffset = 0 ;

	/* Only open and write the resource in RDWR mode is its current length is zero. */
	if (psf->file.mode == SFM_WRITE || (psf->file.mode == SFM_RDWR && psf->rsrclength == 0))
	{	psf->rsrc.mode = psf->file.mode ;
		psf_open_rsrc (psf) ;

		error = sd2_write_rsrc_fork (psf, SF_FALSE) ;

		if (error)
			goto error_cleanup ;

		/* Not needed. */
		psf->write_header = NULL ;
		} ;

	psf->container_close = sd2_close ;

	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

	switch (subformat)
	{	case SF_FORMAT_PCM_S8 :	/* 8-bit linear PCM. */
		case SF_FORMAT_PCM_16 :	/* 16-bit linear PCM. */
		case SF_FORMAT_PCM_24 :	/* 24-bit linear PCM */
				error = pcm_init (psf) ;
				break ;

		default :
				error = SFE_UNIMPLEMENTED ;
				break ;
		} ;

	psf_fseek (psf, psf->dataoffset, SEEK_SET) ;

error_cleanup:

	/* Close the resource fork regardless. We won't need it again. */
	psf_close_rsrc (psf) ;

	return error ;
} /* sd2_open */

/*------------------------------------------------------------------------------
*/

static int
sd2_close	(SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_WRITE)
	{	/*  Now we know for certain the audio_length of the file we can re-write
		**	correct values for the FORM, 8SVX and BODY chunks.
		*/

		} ;

	return 0 ;
} /* sd2_close */

/*------------------------------------------------------------------------------
*/

static inline void
write_char (unsigned char * data, int offset, char value)
{	data [offset] = value ;
} /* write_char */

static inline void
write_short (unsigned char * data, int offset, short value)
{	data [offset] = value >> 8 ;
	data [offset + 1] = value ;
} /* write_char */

static inline void
write_int (unsigned char * data, int offset, int value)
{	data [offset] = value >> 24 ;
	data [offset + 1] = value >> 16 ;
	data [offset + 2] = value >> 8 ;
	data [offset + 3] = value ;
} /* write_int */

static inline void
write_marker (unsigned char * data, int offset, int value)
{
	if (CPU_IS_BIG_ENDIAN)
	{	data [offset] = value >> 24 ;
		data [offset + 1] = value >> 16 ;
		data [offset + 2] = value >> 8 ;
		data [offset + 3] = value ;
		}
	else
	{	data [offset] = value ;
		data [offset + 1] = value >> 8 ;
		data [offset + 2] = value >> 16 ;
		data [offset + 3] = value >> 24 ;
		} ;
} /* write_marker */

static void
write_str (unsigned char * data, int offset, const char * buffer, int buffer_len)
{	memcpy (data + offset, buffer, buffer_len) ;
} /* write_str */

static int
sd2_write_rsrc_fork (SF_PRIVATE *psf, int UNUSED (calc_length))
{	SD2_RSRC rsrc ;
	STR_RSRC str_rsrc [] =
	{	{ RSRC_STR, 1000, "_sample-size", "", 0 },
		{ RSRC_STR, 1001, "_sample-rate", "", 0 },
		{ RSRC_STR, 1002, "_channels", "", 0 },
		{ RSRC_BIN, 1000, "_Markers", "", 8 }
		} ;

	int k, str_offset, data_offset, next_str ;

	psf_use_rsrc (psf, SF_TRUE) ;

	memset (&rsrc, 0, sizeof (rsrc)) ;

	rsrc.sample_rate = psf->sf.samplerate ;
	rsrc.sample_size = psf->bytewidth ;
	rsrc.channels = psf->sf.channels ;

	rsrc.rsrc_data = psf->header ;
	rsrc.rsrc_len = sizeof (psf->header) ;
	memset (rsrc.rsrc_data, 0xea, rsrc.rsrc_len) ;

	snprintf (str_rsrc [0].value, sizeof (str_rsrc [0].value), "_%d", rsrc.sample_size) ;
	snprintf (str_rsrc [1].value, sizeof (str_rsrc [1].value), "_%d.000000", rsrc.sample_rate) ;
	snprintf (str_rsrc [2].value, sizeof (str_rsrc [2].value), "_%d", rsrc.channels) ;

	for (k = 0 ; k < ARRAY_LEN (str_rsrc) ; k++)
	{	if (str_rsrc [k].value_len == 0)
		{	str_rsrc [k].value_len = strlen (str_rsrc [k].value) ;
			str_rsrc [k].value [0] = str_rsrc [k].value_len - 1 ;
			} ;

		/* Turn name string into a pascal string. */
		str_rsrc [k].name [0] = strlen (str_rsrc [k].name) - 1 ;
		} ;

	rsrc.data_offset = 0x100 ;

	/*
	** Calculate data length :
	**		length of strings, plus the length of the sdML chunk.
	*/
	rsrc.data_length = 0 ;
	for (k = 0 ; k < ARRAY_LEN (str_rsrc) ; k++)
		rsrc.data_length += str_rsrc [k].value_len + 4 ;

	rsrc.map_offset = rsrc.data_offset + rsrc.data_length ;

	/* Very start of resource fork. */
	write_int (rsrc.rsrc_data, 0, rsrc.data_offset) ;
	write_int (rsrc.rsrc_data, 4, rsrc.map_offset) ;
	write_int (rsrc.rsrc_data, 8, rsrc.data_length) ;

	write_char (rsrc.rsrc_data, 0x30, strlen (psf->file.name.c)) ;
	write_str (rsrc.rsrc_data, 0x31, psf->file.name.c, strlen (psf->file.name.c)) ;

	write_short (rsrc.rsrc_data, 0x50, 0) ;
	write_marker (rsrc.rsrc_data, 0x52, Sd2f_MARKER) ;
	write_marker (rsrc.rsrc_data, 0x56, lsf1_MARKER) ;

	/* Very start of resource map. */
	write_int (rsrc.rsrc_data, rsrc.map_offset + 0, rsrc.data_offset) ;
	write_int (rsrc.rsrc_data, rsrc.map_offset + 4, rsrc.map_offset) ;
	write_int (rsrc.rsrc_data, rsrc.map_offset + 8, rsrc.data_length) ;

	/* These I don't currently understand. */
	if (1)
	{	write_char (rsrc.rsrc_data, rsrc.map_offset+ 16, 1) ;
		/* Next resource map. */
		write_int (rsrc.rsrc_data, rsrc.map_offset + 17, 0x12345678) ;
		/* File ref number. */
		write_short (rsrc.rsrc_data, rsrc.map_offset + 21, 0xabcd) ;
		/* Fork attributes. */
		write_short (rsrc.rsrc_data, rsrc.map_offset + 23, 0) ;
		} ;

	/* Resource type offset. */
	rsrc.type_offset = rsrc.map_offset + 30 ;
	write_short (rsrc.rsrc_data, rsrc.map_offset + 24, rsrc.type_offset - rsrc.map_offset - 2) ;

	/* Type index max. */
	rsrc.type_count = 2 ;
	write_short (rsrc.rsrc_data, rsrc.map_offset + 28, rsrc.type_count - 1) ;

	rsrc.item_offset = rsrc.type_offset + rsrc.type_count * 8 ;

	rsrc.str_count = ARRAY_LEN (str_rsrc) ;
	rsrc.string_offset = rsrc.item_offset + (rsrc.str_count + 1) * 12 - rsrc.map_offset ;
	write_short (rsrc.rsrc_data, rsrc.map_offset + 26, rsrc.string_offset) ;

	/* Write 'STR ' resource type. */
	rsrc.str_count = 3 ;
	write_marker (rsrc.rsrc_data, rsrc.type_offset, STR_MARKER) ;
	write_short (rsrc.rsrc_data, rsrc.type_offset + 4, rsrc.str_count - 1) ;
	write_short (rsrc.rsrc_data, rsrc.type_offset + 6, 0x12) ;

	/* Write 'sdML' resource type. */
	write_marker (rsrc.rsrc_data, rsrc.type_offset + 8, sdML_MARKER) ;
	write_short (rsrc.rsrc_data, rsrc.type_offset + 12, 0) ;
	write_short (rsrc.rsrc_data, rsrc.type_offset + 14, 0x36) ;

	str_offset = rsrc.map_offset + rsrc.string_offset ;
	next_str = 0 ;
	data_offset = rsrc.data_offset ;
	for (k = 0 ; k < ARRAY_LEN (str_rsrc) ; k++)
	{	write_str (rsrc.rsrc_data, str_offset, str_rsrc [k].name, strlen (str_rsrc [k].name)) ;

		write_short (rsrc.rsrc_data, rsrc.item_offset + k * 12, str_rsrc [k].id) ;
		write_short (rsrc.rsrc_data, rsrc.item_offset + k * 12 + 2, next_str) ;

		str_offset += strlen (str_rsrc [k].name) ;
		next_str += strlen (str_rsrc [k].name) ;

		write_int (rsrc.rsrc_data, rsrc.item_offset + k * 12 + 4, data_offset - rsrc.data_offset) ;

		write_int (rsrc.rsrc_data, data_offset, str_rsrc [k].value_len) ;
		write_str (rsrc.rsrc_data, data_offset + 4, str_rsrc [k].value, str_rsrc [k].value_len) ;
		data_offset += 4 + str_rsrc [k].value_len ;
		} ;

	/* Finally, calculate and set map length. */
	rsrc.map_length = str_offset - rsrc.map_offset ;
	write_int (rsrc.rsrc_data, 12, rsrc.map_length) ;
	write_int (rsrc.rsrc_data, rsrc.map_offset + 12, rsrc.map_length) ;

	rsrc.rsrc_len = rsrc.map_offset + rsrc.map_length ;

	psf_fwrite (rsrc.rsrc_data, rsrc.rsrc_len, 1, psf) ;

	psf_use_rsrc (psf, SF_FALSE) ;

	if (psf->error)
		return psf->error ;

	return 0 ;
} /* sd2_write_rsrc_fork */

/*------------------------------------------------------------------------------
*/

static inline int
read_char (const unsigned char * data, int offset)
{	return data [offset] ;
} /* read_char */

static inline int
read_short (const unsigned char * data, int offset)
{	return (data [offset] << 8) + data [offset + 1] ;
} /* read_short */

static inline int
read_int (const unsigned char * data, int offset)
{	return (data [offset] << 24) + (data [offset + 1] << 16) + (data [offset + 2] << 8) + data [offset + 3] ;
} /* read_int */

static inline int
read_marker (const unsigned char * data, int offset)
{
	if (CPU_IS_BIG_ENDIAN)
		return (data [offset] << 24) + (data [offset + 1] << 16) + (data [offset + 2] << 8) + data [offset + 3] ;
	else if (CPU_IS_LITTLE_ENDIAN)
		return data [offset] + (data [offset + 1] << 8) + (data [offset + 2] << 16) + (data [offset + 3] << 24) ;
	else
		return 0x666 ;
} /* read_marker */

static void
read_str (const unsigned char * data, int offset, char * buffer, int buffer_len)
{	int k ;

	memset (buffer, 0, buffer_len) ;

	for (k = 0 ; k < buffer_len - 1 ; k++)
	{	if (psf_isprint (data [offset + k]) == 0)
			return ;
		buffer [k] = data [offset + k] ;
		} ;
	return ;
} /* read_str */

static int
sd2_parse_rsrc_fork (SF_PRIVATE *psf)
{	SD2_RSRC rsrc ;
	int k, marker, error = 0 ;

	psf_use_rsrc (psf, SF_TRUE) ;

	memset (&rsrc, 0, sizeof (rsrc)) ;

	rsrc.rsrc_len = psf_get_filelen (psf) ;
	psf_log_printf (psf, "Resource length : %d (0x%04X)\n", rsrc.rsrc_len, rsrc.rsrc_len) ;

	if (rsrc.rsrc_len > SIGNED_SIZEOF (psf->header))
	{	rsrc.rsrc_data = calloc (1, rsrc.rsrc_len) ;
		rsrc.need_to_free_rsrc_data = SF_TRUE ;
		}
	else
		rsrc.rsrc_data = psf->header ;

	/* Read in the whole lot. */
	psf_fread (rsrc.rsrc_data, rsrc.rsrc_len, 1, psf) ;

	/* Reset the header storage because we have changed to the rsrcdes. */
	psf->headindex = psf->headend = rsrc.rsrc_len ;

	rsrc.data_offset = read_int (rsrc.rsrc_data, 0) ;
	rsrc.map_offset = read_int (rsrc.rsrc_data, 4) ;
	rsrc.data_length = read_int (rsrc.rsrc_data, 8) ;
	rsrc.map_length = read_int (rsrc.rsrc_data, 12) ;

	if (rsrc.data_offset == 0x51607 && rsrc.map_offset == 0x20000)
	{	psf_log_printf (psf, "Trying offset of 0x52 bytes.\n") ;
		rsrc.data_offset = read_int (rsrc.rsrc_data, 0x52 + 0) + 0x52 ;
		rsrc.map_offset = read_int (rsrc.rsrc_data, 0x52 + 4) + 0x52 ;
		rsrc.data_length = read_int (rsrc.rsrc_data, 0x52 + 8) ;
		rsrc.map_length = read_int (rsrc.rsrc_data, 0x52 + 12) ;
		} ;

	psf_log_printf (psf, "  data offset : 0x%04X\n  map  offset : 0x%04X\n"
				"  data length : 0x%04X\n  map  length : 0x%04X\n",
				rsrc.data_offset, rsrc.map_offset, rsrc.data_length, rsrc.map_length) ;

	if (rsrc.data_offset > rsrc.rsrc_len)
	{	psf_log_printf (psf, "Error : rsrc.data_offset (%d, 0x%x) > len\n", rsrc.data_offset, rsrc.data_offset) ;
		error = SFE_SD2_BAD_DATA_OFFSET ;
		goto parse_rsrc_fork_cleanup ;
		} ;

	if (rsrc.map_offset > rsrc.rsrc_len)
	{	psf_log_printf (psf, "Error : rsrc.map_offset > len\n") ;
		error = SFE_SD2_BAD_MAP_OFFSET ;
		goto parse_rsrc_fork_cleanup ;
		} ;

	if (rsrc.data_length > rsrc.rsrc_len)
	{	psf_log_printf (psf, "Error : rsrc.data_length > len\n") ;
		error = SFE_SD2_BAD_DATA_LENGTH ;
		goto parse_rsrc_fork_cleanup ;
		} ;

	if (rsrc.map_length > rsrc.rsrc_len)
	{	psf_log_printf (psf, "Error : rsrc.map_length > len\n") ;
		error = SFE_SD2_BAD_MAP_LENGTH ;
		goto parse_rsrc_fork_cleanup ;
		} ;

	if (rsrc.data_offset + rsrc.data_length != rsrc.map_offset || rsrc.map_offset + rsrc.map_length != rsrc.rsrc_len)
	{	psf_log_printf (psf, "Error : This does not look like a MacOSX resource fork.\n") ;
		error = SFE_SD2_BAD_RSRC ;
		goto parse_rsrc_fork_cleanup ;
		} ;

	if (rsrc.map_offset + 28 >= rsrc.rsrc_len)
	{	psf_log_printf (psf, "Bad map offset (%d + 28 > %d).\n", rsrc.map_offset, rsrc.rsrc_len) ;
		error = SFE_SD2_BAD_RSRC ;
		goto parse_rsrc_fork_cleanup ;
		} ;

	rsrc.string_offset = rsrc.map_offset + read_short (rsrc.rsrc_data, rsrc.map_offset + 26) ;
	if (rsrc.string_offset > rsrc.rsrc_len)
	{	psf_log_printf (psf, "Bad string offset (%d).\n", rsrc.string_offset) ;
		error = SFE_SD2_BAD_RSRC ;
		goto parse_rsrc_fork_cleanup ;
		} ;

	rsrc.type_offset = rsrc.map_offset + 30 ;

	rsrc.type_count = read_short (rsrc.rsrc_data, rsrc.map_offset + 28) + 1 ;
	if (rsrc.type_count < 1)
	{	psf_log_printf (psf, "Bad type count.\n") ;
		error = SFE_SD2_BAD_RSRC ;
		goto parse_rsrc_fork_cleanup ;
		} ;

	rsrc.item_offset = rsrc.type_offset + rsrc.type_count * 8 ;
	if (rsrc.item_offset < 0 || rsrc.item_offset > rsrc.rsrc_len)
	{	psf_log_printf (psf, "Bad item offset (%d).\n", rsrc.item_offset) ;
		error = SFE_SD2_BAD_RSRC ;
		goto parse_rsrc_fork_cleanup ;
		} ;

	rsrc.str_index = -1 ;
	for (k = 0 ; k < rsrc.type_count ; k ++)
	{	marker = read_marker (rsrc.rsrc_data, rsrc.type_offset + k * 8) ;

		if (marker == STR_MARKER)
		{	rsrc.str_index = k ;
			rsrc.str_count = read_short (rsrc.rsrc_data, rsrc.type_offset + k * 8 + 4) + 1 ;
			error = parse_str_rsrc (psf, &rsrc) ;
			goto parse_rsrc_fork_cleanup ;
			} ;
		} ;

	psf_log_printf (psf, "No 'STR ' resource.\n") ;
	error = SFE_SD2_BAD_RSRC ;

parse_rsrc_fork_cleanup :

	psf_use_rsrc (psf, SF_FALSE) ;

	if (rsrc.need_to_free_rsrc_data)
		free (rsrc.rsrc_data) ;

	return error ;
} /* sd2_parse_rsrc_fork */

static int
parse_str_rsrc (SF_PRIVATE *psf, SD2_RSRC * rsrc)
{	char name [32], value [32] ;
	int k, str_offset, rsrc_id, data_offset = 0, data_len = 0 ;

	psf_log_printf (psf, "Finding parameters :\n") ;

	str_offset = rsrc->string_offset ;
	psf_log_printf (psf, "  Offset    RsrcId    dlen    slen    Value\n") ;

	for (k = 0 ; data_offset + data_len < rsrc->rsrc_len ; k++)
	{	int slen ;

		slen = read_char (rsrc->rsrc_data, str_offset) ;
		read_str (rsrc->rsrc_data, str_offset + 1, name, SF_MIN (SIGNED_SIZEOF (name), slen + 1)) ;
		str_offset += slen + 1 ;

		rsrc_id = read_short (rsrc->rsrc_data, rsrc->item_offset + k * 12) ;

		data_offset = rsrc->data_offset + read_int (rsrc->rsrc_data, rsrc->item_offset + k * 12 + 4) ;
		if (data_offset < 0 || data_offset > rsrc->rsrc_len)
		{	psf_log_printf (psf, "Exiting parser on data offset of %d.\n", data_offset) ;
			break ;
			} ;

		data_len = read_int (rsrc->rsrc_data, data_offset) ;
		if (data_len < 0 || data_len > rsrc->rsrc_len)
		{	psf_log_printf (psf, "Exiting parser on data length of %d.\n", data_len) ;
			break ;
			} ;

		slen = read_char (rsrc->rsrc_data, data_offset + 4) ;
		read_str (rsrc->rsrc_data, data_offset + 5, value, SF_MIN (SIGNED_SIZEOF (value), slen + 1)) ;

		psf_log_printf (psf, "  0x%04x     %4d     %4d     %3d    '%s'\n", data_offset, rsrc_id, data_len, slen, value) ;

		if (rsrc_id == 1000 && rsrc->sample_size == 0)
			rsrc->sample_size = strtol (value, NULL, 10) ;
		else if (rsrc_id == 1001 && rsrc->sample_rate == 0)
			rsrc->sample_rate = strtol (value, NULL, 10) ;
		else if (rsrc_id == 1002 && rsrc->channels == 0)
			rsrc->channels = strtol (value, NULL, 10) ;
		} ;

	psf_log_printf (psf, "Found Parameters :\n") ;
	psf_log_printf (psf, "  sample-size : %d\n", rsrc->sample_size) ;
	psf_log_printf (psf, "  sample-rate : %d\n", rsrc->sample_rate) ;
	psf_log_printf (psf, "  channels    : %d\n", rsrc->channels) ;

	if (rsrc->sample_rate <= 4 && rsrc->sample_size > 4)
	{	int temp ;

		psf_log_printf (psf, "Geez!! Looks like sample rate and sample size got switched.\nCorrecting this screw up.\n") ;
		temp = rsrc->sample_rate ;
		rsrc->sample_rate = rsrc->sample_size ;
		rsrc->sample_size = temp ;
		} ;

	if (rsrc->sample_rate < 0)
	{	psf_log_printf (psf, "Bad sample rate (%d)\n", rsrc->sample_rate) ;
		return SFE_SD2_BAD_RSRC ;
		} ;

	if (rsrc->channels < 0)
	{	psf_log_printf (psf, "Bad channel count (%d)\n", rsrc->channels) ;
		return SFE_SD2_BAD_RSRC ;
		} ;

	psf->sf.samplerate = rsrc->sample_rate ;
	psf->sf.channels = rsrc->channels ;
	psf->bytewidth = rsrc->sample_size ;

	switch (rsrc->sample_size)
	{	case 1 :
			psf->sf.format = SF_FORMAT_SD2 | SF_FORMAT_PCM_S8 ;
			break ;

		case 2 :
			psf->sf.format = SF_FORMAT_SD2 | SF_FORMAT_PCM_16 ;
			break ;

		case 3 :
			psf->sf.format = SF_FORMAT_SD2 | SF_FORMAT_PCM_24 ;
			break ;

		default :
			psf_log_printf (psf, "Bad sample size (%d)\n", rsrc->sample_size) ;
			return SFE_SD2_BAD_SAMPLE_SIZE ;
		} ;

	psf_log_printf (psf, "ok\n") ;

	return 0 ;
} /* parse_str_rsrc */

