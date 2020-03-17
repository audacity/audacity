/*
** Copyright (C) 2004-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2004 Tobias Gehrig <tgehrig@ira.uka.de>
**
** This program is free software ; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation ; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY ; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program ; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include	"sfconfig.h"

#include	<stdio.h>
#include	<stdlib.h>
#include	<fcntl.h>
#include	<string.h>
#include	<ctype.h>
#include	<math.h>

#include	"sndfile.h"
#include	"common.h"

#if HAVE_EXTERNAL_XIPH_LIBS

#include	<FLAC/stream_decoder.h>
#include	<FLAC/stream_encoder.h>
#include	<FLAC/metadata.h>

/*------------------------------------------------------------------------------
** Private static functions.
*/

#define	FLAC_DEFAULT_COMPRESSION_LEVEL	5

#define ENC_BUFFER_SIZE 8192

typedef enum
{	PFLAC_PCM_SHORT = 50,
	PFLAC_PCM_INT = 51,
	PFLAC_PCM_FLOAT = 52,
	PFLAC_PCM_DOUBLE = 53
} PFLAC_PCM ;

typedef struct
{
	FLAC__StreamDecoder *fsd ;
	FLAC__StreamEncoder *fse ;

	PFLAC_PCM pcmtype ;
	void* ptr ;
	unsigned pos, len, remain ;

	FLAC__StreamMetadata *metadata ;

	const int32_t * const * wbuffer ;
	int32_t * rbuffer [FLAC__MAX_CHANNELS] ;

	int32_t* encbuffer ;
	unsigned bufferpos ;

	const FLAC__Frame *frame ;

	unsigned compression ;

} FLAC_PRIVATE ;

typedef struct
{	const char *tag ;
	int type ;
} FLAC_TAG ;

static sf_count_t	flac_seek (SF_PRIVATE *psf, int mode, sf_count_t offset) ;
static int			flac_byterate (SF_PRIVATE *psf) ;
static int			flac_close (SF_PRIVATE *psf) ;

static int			flac_enc_init (SF_PRIVATE *psf) ;
static int			flac_read_header (SF_PRIVATE *psf) ;

static sf_count_t	flac_read_flac2s (SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	flac_read_flac2i (SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	flac_read_flac2f (SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	flac_read_flac2d (SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t	flac_write_s2flac (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	flac_write_i2flac (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	flac_write_f2flac (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	flac_write_d2flac (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static void		f2flac8_array (const float *src, int32_t *dest, int count, int normalize) ;
static void		f2flac16_array (const float *src, int32_t *dest, int count, int normalize) ;
static void		f2flac24_array (const float *src, int32_t *dest, int count, int normalize) ;
static void		f2flac8_clip_array (const float *src, int32_t *dest, int count, int normalize) ;
static void		f2flac16_clip_array (const float *src, int32_t *dest, int count, int normalize) ;
static void		f2flac24_clip_array (const float *src, int32_t *dest, int count, int normalize) ;
static void		d2flac8_array (const double *src, int32_t *dest, int count, int normalize) ;
static void		d2flac16_array (const double *src, int32_t *dest, int count, int normalize) ;
static void		d2flac24_array (const double *src, int32_t *dest, int count, int normalize) ;
static void		d2flac8_clip_array (const double *src, int32_t *dest, int count, int normalize) ;
static void		d2flac16_clip_array (const double *src, int32_t *dest, int count, int normalize) ;
static void		d2flac24_clip_array (const double *src, int32_t *dest, int count, int normalize) ;

static int flac_command (SF_PRIVATE *psf, int command, void *data, int datasize) ;

/* Decoder Callbacks */
static FLAC__StreamDecoderReadStatus sf_flac_read_callback (const FLAC__StreamDecoder *decoder, FLAC__byte buffer [], size_t *bytes, void *client_data) ;
static FLAC__StreamDecoderSeekStatus sf_flac_seek_callback (const FLAC__StreamDecoder *decoder, FLAC__uint64 absolute_byte_offset, void *client_data) ;
static FLAC__StreamDecoderTellStatus sf_flac_tell_callback (const FLAC__StreamDecoder *decoder, FLAC__uint64 *absolute_byte_offset, void *client_data) ;
static FLAC__StreamDecoderLengthStatus sf_flac_length_callback (const FLAC__StreamDecoder *decoder, FLAC__uint64 *stream_length, void *client_data) ;
static FLAC__bool sf_flac_eof_callback (const FLAC__StreamDecoder *decoder, void *client_data) ;
static FLAC__StreamDecoderWriteStatus sf_flac_write_callback (const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame, const int32_t * const buffer [], void *client_data) ;
static void sf_flac_meta_callback (const FLAC__StreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data) ;
static void sf_flac_error_callback (const FLAC__StreamDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data) ;

/* Encoder Callbacks */
static FLAC__StreamEncoderSeekStatus sf_flac_enc_seek_callback (const FLAC__StreamEncoder *encoder, FLAC__uint64 absolute_byte_offset, void *client_data) ;
static FLAC__StreamEncoderTellStatus sf_flac_enc_tell_callback (const FLAC__StreamEncoder *encoder, FLAC__uint64 *absolute_byte_offset, void *client_data) ;
static FLAC__StreamEncoderWriteStatus sf_flac_enc_write_callback (const FLAC__StreamEncoder *encoder, const FLAC__byte buffer [], size_t bytes, unsigned samples, unsigned current_frame, void *client_data) ;

static void
s2flac8_array (const short *src, int32_t *dest, int count)
{	while (--count >= 0)
		dest [count] = src [count] >> 8 ;
} /* s2flac8_array */

static void
s2flac16_array (const short *src, int32_t *dest, int count)
{	while (--count >= 0)
		dest [count] = src [count] ;
} /* s2flac16_array */

static void
s2flac24_array (const short *src, int32_t *dest, int count)
{	while (--count >= 0)
		dest [count] = src [count] << 8 ;
} /* s2flac24_array */

static void
i2flac8_array (const int *src, int32_t *dest, int count)
{	while (--count >= 0)
		dest [count] = src [count] >> 24 ;
} /* i2flac8_array */

static void
i2flac16_array (const int *src, int32_t *dest, int count)
{
	while (--count >= 0)
		dest [count] = src [count] >> 16 ;
} /* i2flac16_array */

static void
i2flac24_array (const int *src, int32_t *dest, int count)
{	while (--count >= 0)
		dest [count] = src [count] >> 8 ;
} /* i2flac24_array */

static sf_count_t
flac_buffer_copy (SF_PRIVATE *psf)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;
	const FLAC__Frame *frame = pflac->frame ;
	const int32_t* const *buffer = pflac->wbuffer ;
	unsigned i = 0, j, offset, channels, len ;

	if (psf->sf.channels != (int) frame->header.channels)
	{	psf_log_printf (psf, "Error: FLAC frame changed from %d to %d channels\n"
									"Nothing to do but to error out.\n" ,
									psf->sf.channels, frame->header.channels) ;
		psf->error = SFE_FLAC_CHANNEL_COUNT_CHANGED ;
		return 0 ;
		} ;

	/*
	**	frame->header.blocksize is variable and we're using a constant blocksize
	**	of FLAC__MAX_BLOCK_SIZE.
	**	Check our assumptions here.
	*/
	if (frame->header.blocksize > FLAC__MAX_BLOCK_SIZE)
	{	psf_log_printf (psf, "Ooops : frame->header.blocksize (%d) > FLAC__MAX_BLOCK_SIZE (%d)\n", __func__, __LINE__, frame->header.blocksize, FLAC__MAX_BLOCK_SIZE) ;
		psf->error = SFE_INTERNAL ;
		return 0 ;
		} ;

	if (frame->header.channels > FLAC__MAX_CHANNELS)
		psf_log_printf (psf, "Ooops : frame->header.channels (%d) > FLAC__MAX_BLOCK_SIZE (%d)\n", __func__, __LINE__, frame->header.channels, FLAC__MAX_CHANNELS) ;

	channels = SF_MIN (frame->header.channels, FLAC__MAX_CHANNELS) ;

	if (pflac->ptr == NULL)
	{	/*
		** This pointer is reset to NULL each time the current frame has been
		** decoded. Somehow its used during encoding and decoding.
		*/
		for (i = 0 ; i < channels ; i++)
		{
			if (pflac->rbuffer [i] == NULL)
				pflac->rbuffer [i] = calloc (FLAC__MAX_BLOCK_SIZE, sizeof (int32_t)) ;

			memcpy (pflac->rbuffer [i], buffer [i], frame->header.blocksize * sizeof (int32_t)) ;
			} ;
		pflac->wbuffer = (const int32_t* const*) pflac->rbuffer ;

		return 0 ;
		} ;

	len = SF_MIN (pflac->len, frame->header.blocksize) ;

	if (pflac->remain % channels != 0)
	{	psf_log_printf (psf, "Error: pflac->remain %u    channels %u\n", pflac->remain, channels) ;
		return 0 ;
		} ;

	switch (pflac->pcmtype)
	{	case PFLAC_PCM_SHORT :
			{	short *retpcm = (short*) pflac->ptr ;
				int shift = 16 - frame->header.bits_per_sample ;
				if (shift < 0)
				{	shift = abs (shift) ;
					for (i = 0 ; i < len && pflac->remain > 0 ; i++)
					{	offset = pflac->pos + i * channels ;

						if (pflac->bufferpos >= frame->header.blocksize)
							break ;

						if (offset + channels > pflac->len)
							break ;

						for (j = 0 ; j < channels ; j++)
							retpcm [offset + j] = buffer [j][pflac->bufferpos] >> shift ;
						pflac->remain -= channels ;
						pflac->bufferpos ++ ;
						}
					}
				else
				{	for (i = 0 ; i < len && pflac->remain > 0 ; i++)
					{	offset = pflac->pos + i * channels ;

						if (pflac->bufferpos >= frame->header.blocksize)
							break ;

						if (offset + channels > pflac->len)
							break ;

						for (j = 0 ; j < channels ; j++)
							retpcm [offset + j] = ((uint16_t) buffer [j][pflac->bufferpos]) << shift ;

						pflac->remain -= channels ;
						pflac->bufferpos ++ ;
						} ;
					} ;
				} ;
			break ;

		case PFLAC_PCM_INT :
			{	int *retpcm = (int*) pflac->ptr ;
				int shift = 32 - frame->header.bits_per_sample ;
				for (i = 0 ; i < len && pflac->remain > 0 ; i++)
				{	offset = pflac->pos + i * channels ;

					if (pflac->bufferpos >= frame->header.blocksize)
						break ;

					if (offset + channels > pflac->len)
						break ;

					for (j = 0 ; j < channels ; j++)
						retpcm [offset + j] = ((uint32_t) buffer [j][pflac->bufferpos]) << shift ;
					pflac->remain -= channels ;
					pflac->bufferpos++ ;
					} ;
				} ;
			break ;

		case PFLAC_PCM_FLOAT :
			{	float *retpcm = (float*) pflac->ptr ;
				float norm = (psf->norm_float == SF_TRUE) ? 1.0 / (1 << (frame->header.bits_per_sample - 1)) : 1.0 ;

				for (i = 0 ; i < len && pflac->remain > 0 ; i++)
				{	offset = pflac->pos + i * channels ;

					if (pflac->bufferpos >= frame->header.blocksize)
						break ;

					if (offset + channels > pflac->len)
						break ;

					for (j = 0 ; j < channels ; j++)
						retpcm [offset + j] = buffer [j][pflac->bufferpos] * norm ;
					pflac->remain -= channels ;
					pflac->bufferpos++ ;
					} ;
				} ;
			break ;

		case PFLAC_PCM_DOUBLE :
			{	double *retpcm = (double*) pflac->ptr ;
				double norm = (psf->norm_double == SF_TRUE) ? 1.0 / (1 << (frame->header.bits_per_sample - 1)) : 1.0 ;

				for (i = 0 ; i < len && pflac->remain > 0 ; i++)
				{	offset = pflac->pos + i * channels ;

					if (pflac->bufferpos >= frame->header.blocksize)
						break ;

					if (offset + channels > pflac->len)
						break ;

					for (j = 0 ; j < channels ; j++)
						retpcm [offset + j] = buffer [j][pflac->bufferpos] * norm ;
					pflac->remain -= channels ;
					pflac->bufferpos++ ;
					} ;
				} ;
			break ;

		default :
			return 0 ;
		} ;

	offset = i * channels ;
	pflac->pos += i * channels ;

	return offset ;
} /* flac_buffer_copy */


static FLAC__StreamDecoderReadStatus
sf_flac_read_callback (const FLAC__StreamDecoder * UNUSED (decoder), FLAC__byte buffer [], size_t *bytes, void *client_data)
{	SF_PRIVATE *psf = (SF_PRIVATE*) client_data ;

	*bytes = psf_fread (buffer, 1, *bytes, psf) ;
	if (*bytes > 0 && psf->error == 0)
		return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE ;

	return FLAC__STREAM_DECODER_READ_STATUS_ABORT ;
} /* sf_flac_read_callback */

static FLAC__StreamDecoderSeekStatus
sf_flac_seek_callback (const FLAC__StreamDecoder * UNUSED (decoder), FLAC__uint64 absolute_byte_offset, void *client_data)
{	SF_PRIVATE *psf = (SF_PRIVATE*) client_data ;

	psf_fseek (psf, absolute_byte_offset, SEEK_SET) ;
	if (psf->error)
		return FLAC__STREAM_DECODER_SEEK_STATUS_ERROR ;

	return FLAC__STREAM_DECODER_SEEK_STATUS_OK ;
} /* sf_flac_seek_callback */

static FLAC__StreamDecoderTellStatus
sf_flac_tell_callback (const FLAC__StreamDecoder * UNUSED (decoder), FLAC__uint64 *absolute_byte_offset, void *client_data)
{	SF_PRIVATE *psf = (SF_PRIVATE*) client_data ;

	*absolute_byte_offset = psf_ftell (psf) ;
	if (psf->error)
		return FLAC__STREAM_DECODER_TELL_STATUS_ERROR ;

	return FLAC__STREAM_DECODER_TELL_STATUS_OK ;
} /* sf_flac_tell_callback */

static FLAC__StreamDecoderLengthStatus
sf_flac_length_callback (const FLAC__StreamDecoder * UNUSED (decoder), FLAC__uint64 *stream_length, void *client_data)
{	SF_PRIVATE *psf = (SF_PRIVATE*) client_data ;

	if ((*stream_length = psf->filelength) == 0)
		return FLAC__STREAM_DECODER_LENGTH_STATUS_ERROR ;

	return FLAC__STREAM_DECODER_LENGTH_STATUS_OK ;
} /* sf_flac_length_callback */

static FLAC__bool
sf_flac_eof_callback (const FLAC__StreamDecoder *UNUSED (decoder), void *client_data)
{	SF_PRIVATE *psf = (SF_PRIVATE*) client_data ;

	if (psf_ftell (psf) == psf->filelength)
		return SF_TRUE ;

	return SF_FALSE ;
} /* sf_flac_eof_callback */

static FLAC__StreamDecoderWriteStatus
sf_flac_write_callback (const FLAC__StreamDecoder * UNUSED (decoder), const FLAC__Frame *frame, const int32_t * const buffer [], void *client_data)
{	SF_PRIVATE *psf = (SF_PRIVATE*) client_data ;
	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;

	pflac->frame = frame ;
	pflac->bufferpos = 0 ;

	pflac->wbuffer = buffer ;

	flac_buffer_copy (psf) ;

	return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE ;
} /* sf_flac_write_callback */

static void
sf_flac_meta_get_vorbiscomments (SF_PRIVATE *psf, const FLAC__StreamMetadata *metadata)
{	static FLAC_TAG tags [] =
	{ 	{ "title", SF_STR_TITLE },
		{ "copyright", SF_STR_COPYRIGHT },
		{ "software", SF_STR_SOFTWARE },
		{ "artist", SF_STR_ARTIST },
		{ "comment", SF_STR_COMMENT },
		{ "date", SF_STR_DATE },
		{ "album", SF_STR_ALBUM },
		{ "license", SF_STR_LICENSE },
		{ "tracknumber", SF_STR_TRACKNUMBER },
		{ "genre", SF_STR_GENRE }
		} ;

	const char *value, *cptr ;
	int k, tag_num ;

	for (k = 0 ; k < ARRAY_LEN (tags) ; k++)
	{	tag_num = FLAC__metadata_object_vorbiscomment_find_entry_from (metadata, 0, tags [k].tag) ;

		if (tag_num < 0)
			continue ;

		value = (const char*) metadata->data.vorbis_comment.comments [tag_num].entry ;
		if ((cptr = strchr (value, '=')) != NULL)
			value = cptr + 1 ;

		psf_log_printf (psf, "  %-12s : %s\n", tags [k].tag, value) ;
		psf_store_string (psf, tags [k].type, value) ;
		} ;

	return ;
} /* sf_flac_meta_get_vorbiscomments */

static void
sf_flac_meta_callback (const FLAC__StreamDecoder * UNUSED (decoder), const FLAC__StreamMetadata *metadata, void *client_data)
{	SF_PRIVATE *psf = (SF_PRIVATE*) client_data ;
	int bitwidth = 0 ;

	switch (metadata->type)
	{	case FLAC__METADATA_TYPE_STREAMINFO :
			if (psf->sf.channels > 0 && psf->sf.channels != (int) metadata->data.stream_info.channels)
			{	psf_log_printf (psf, "Error: FLAC stream changed from %d to %d channels\n"
									"Nothing to do but to error out.\n" ,
									psf->sf.channels, metadata->data.stream_info.channels) ;
				psf->error = SFE_FLAC_CHANNEL_COUNT_CHANGED ;
				return ;
				} ;

			if (psf->sf.channels > 0 && psf->sf.samplerate != (int) metadata->data.stream_info.sample_rate)
			{	psf_log_printf (psf, "Warning: FLAC stream changed sample rates from %d to %d.\n"
									"Carrying on as if nothing happened.",
									psf->sf.samplerate, metadata->data.stream_info.sample_rate) ;
				} ;
			psf->sf.channels = metadata->data.stream_info.channels ;
			psf->sf.samplerate = metadata->data.stream_info.sample_rate ;
			psf->sf.frames = metadata->data.stream_info.total_samples ;

			psf_log_printf (psf, "FLAC Stream Metadata\n  Channels    : %d\n  Sample rate : %d\n", psf->sf.channels, psf->sf.samplerate) ;

			if (psf->sf.frames == 0)
			{	psf_log_printf (psf, "  Frames      : 0 (bumping to SF_COUNT_MAX)\n") ;
				psf->sf.frames = SF_COUNT_MAX ;
				}
			else
				psf_log_printf (psf, "  Frames      : %D\n", psf->sf.frames) ;

			switch (metadata->data.stream_info.bits_per_sample)
			{	case 8 :
					psf->sf.format |= SF_FORMAT_PCM_S8 ;
					bitwidth = 8 ;
					break ;
				case 16 :
					psf->sf.format |= SF_FORMAT_PCM_16 ;
					bitwidth = 16 ;
					break ;
				case 24 :
					psf->sf.format |= SF_FORMAT_PCM_24 ;
					bitwidth = 24 ;
					break ;
				default :
					psf_log_printf (psf, "sf_flac_meta_callback : bits_per_sample %d not yet implemented.\n", metadata->data.stream_info.bits_per_sample) ;
					break ;
				} ;

			if (bitwidth > 0)
				psf_log_printf (psf, "  Bit width   : %d\n", bitwidth) ;
			break ;

		case FLAC__METADATA_TYPE_VORBIS_COMMENT :
			psf_log_printf (psf, "Vorbis Comment Metadata\n") ;
			sf_flac_meta_get_vorbiscomments (psf, metadata) ;
			break ;

		case FLAC__METADATA_TYPE_PADDING :
			psf_log_printf (psf, "Padding Metadata\n") ;
			break ;

		case FLAC__METADATA_TYPE_APPLICATION :
			psf_log_printf (psf, "Application Metadata\n") ;
			break ;

		case FLAC__METADATA_TYPE_SEEKTABLE :
			psf_log_printf (psf, "Seektable Metadata\n") ;
			break ;

		case FLAC__METADATA_TYPE_CUESHEET :
			psf_log_printf (psf, "Cuesheet Metadata\n") ;
			break ;

		case FLAC__METADATA_TYPE_PICTURE :
			psf_log_printf (psf, "Picture Metadata\n") ;
			break ;

		case FLAC__METADATA_TYPE_UNDEFINED :
			psf_log_printf (psf, "Undefined Metadata\n") ;
			break ;

		default :
			psf_log_printf (psf, "sf_flac_meta_callback : metadata-type %d not yet implemented.\n", metadata->type) ;
			break ;
		} ;

	return ;
} /* sf_flac_meta_callback */

static void
sf_flac_error_callback (const FLAC__StreamDecoder * UNUSED (decoder), FLAC__StreamDecoderErrorStatus status, void *client_data)
{	SF_PRIVATE *psf = (SF_PRIVATE*) client_data ;

	psf_log_printf (psf, "ERROR : %s\n", FLAC__StreamDecoderErrorStatusString [status]) ;

	switch (status)
	{	case FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC :
			psf->error = SFE_FLAC_LOST_SYNC ;
			break ;
		case FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER :
			psf->error = SFE_FLAC_BAD_HEADER ;
			break ;
		default :
			psf->error = SFE_FLAC_UNKOWN_ERROR ;
			break ;
		} ;

	return ;
} /* sf_flac_error_callback */

static FLAC__StreamEncoderSeekStatus
sf_flac_enc_seek_callback (const FLAC__StreamEncoder * UNUSED (encoder), FLAC__uint64 absolute_byte_offset, void *client_data)
{	SF_PRIVATE *psf = (SF_PRIVATE*) client_data ;

	psf_fseek (psf, absolute_byte_offset, SEEK_SET) ;
	if (psf->error)
		return FLAC__STREAM_ENCODER_SEEK_STATUS_ERROR ;

	return FLAC__STREAM_ENCODER_SEEK_STATUS_OK ;
} /* sf_flac_enc_seek_callback */

static FLAC__StreamEncoderTellStatus
sf_flac_enc_tell_callback (const FLAC__StreamEncoder *UNUSED (encoder), FLAC__uint64 *absolute_byte_offset, void *client_data)
{	SF_PRIVATE *psf = (SF_PRIVATE*) client_data ;

	*absolute_byte_offset = psf_ftell (psf) ;
	if (psf->error)
		return FLAC__STREAM_ENCODER_TELL_STATUS_ERROR ;

	return FLAC__STREAM_ENCODER_TELL_STATUS_OK ;
} /* sf_flac_enc_tell_callback */

static FLAC__StreamEncoderWriteStatus
sf_flac_enc_write_callback (const FLAC__StreamEncoder * UNUSED (encoder), const FLAC__byte buffer [], size_t bytes, unsigned UNUSED (samples), unsigned UNUSED (current_frame), void *client_data)
{	SF_PRIVATE *psf = (SF_PRIVATE*) client_data ;

	if (psf_fwrite (buffer, 1, bytes, psf) == (sf_count_t) bytes && psf->error == 0)
		return FLAC__STREAM_ENCODER_WRITE_STATUS_OK ;

	return FLAC__STREAM_ENCODER_WRITE_STATUS_FATAL_ERROR ;
} /* sf_flac_enc_write_callback */

static void
flac_write_strings (SF_PRIVATE *psf, FLAC_PRIVATE* pflac)
{	FLAC__StreamMetadata_VorbisComment_Entry entry ;
	int	k, string_count = 0 ;

	for (k = 0 ; k < SF_MAX_STRINGS ; k++)
	{	if (psf->strings.data [k].type != 0)
			string_count ++ ;
		} ;

	if (string_count == 0)
		return ;

	if (pflac->metadata == NULL && (pflac->metadata = FLAC__metadata_object_new (FLAC__METADATA_TYPE_VORBIS_COMMENT)) == NULL)
	{	psf_log_printf (psf, "FLAC__metadata_object_new returned NULL\n") ;
		return ;
		} ;

	for (k = 0 ; k < SF_MAX_STRINGS && psf->strings.data [k].type != 0 ; k++)
	{	const char * key, * value ;

		switch (psf->strings.data [k].type)
		{	case SF_STR_SOFTWARE :
				key = "software" ;
				break ;
			case SF_STR_TITLE :
				key = "title" ;
				break ;
			case SF_STR_COPYRIGHT :
				key = "copyright" ;
				break ;
			case SF_STR_ARTIST :
				key = "artist" ;
				break ;
			case SF_STR_COMMENT :
				key = "comment" ;
				break ;
			case SF_STR_DATE :
				key = "date" ;
				break ;
			case SF_STR_ALBUM :
				key = "album" ;
				break ;
			case SF_STR_LICENSE :
				key = "license" ;
				break ;
			case SF_STR_TRACKNUMBER :
				key = "tracknumber" ;
				break ;
			case SF_STR_GENRE :
				key = "genre" ;
				break ;
			default :
				continue ;
			} ;

		value = psf->strings.storage + psf->strings.data [k].offset ;

		FLAC__metadata_object_vorbiscomment_entry_from_name_value_pair (&entry, key, value) ;
		FLAC__metadata_object_vorbiscomment_append_comment (pflac->metadata, entry, /* copy */ SF_FALSE) ;
		} ;

	if (! FLAC__stream_encoder_set_metadata (pflac->fse, &pflac->metadata, 1))
	{	printf ("%s %d : fail\n", __func__, __LINE__) ;
		return ;
		} ;

	return ;
} /* flac_write_strings */

static int
flac_write_header (SF_PRIVATE *psf, int UNUSED (calc_length))
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;
	int err ;

	flac_write_strings (psf, pflac) ;

	if ((err = FLAC__stream_encoder_init_stream (pflac->fse, sf_flac_enc_write_callback, sf_flac_enc_seek_callback, sf_flac_enc_tell_callback, NULL, psf)) != FLAC__STREAM_DECODER_INIT_STATUS_OK)
	{	psf_log_printf (psf, "Error : FLAC encoder init returned error : %s\n", FLAC__StreamEncoderInitStatusString [err]) ;
		return SFE_FLAC_INIT_DECODER ;
		} ;

	if (psf->error == 0)
		psf->dataoffset = psf_ftell (psf) ;
	pflac->encbuffer = calloc (ENC_BUFFER_SIZE, sizeof (int32_t)) ;

	/* can only call init_stream once */
	psf->write_header = NULL ;

	return psf->error ;
} /* flac_write_header */

/*------------------------------------------------------------------------------
** Public function.
*/

int
flac_open	(SF_PRIVATE *psf)
{	int		subformat ;
	int		error = 0 ;

	FLAC_PRIVATE* pflac = calloc (1, sizeof (FLAC_PRIVATE)) ;
	psf->codec_data = pflac ;

	/* Set the default value here. Over-ridden later if necessary. */
	pflac->compression = FLAC_DEFAULT_COMPRESSION_LEVEL ;

	if (psf->file.mode == SFM_RDWR)
		return SFE_BAD_MODE_RW ;

	if (psf->file.mode == SFM_READ)
	{	if ((error = flac_read_header (psf)))
			return error ;
		} ;

	subformat = SF_CODEC (psf->sf.format) ;

	if (psf->file.mode == SFM_WRITE)
	{	if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_FLAC)
			return	SFE_BAD_OPEN_FORMAT ;

		psf->endian = SF_ENDIAN_BIG ;
		psf->sf.seekable = 0 ;

		psf->strings.flags = SF_STR_ALLOW_START ;

		if ((error = flac_enc_init (psf)))
			return error ;

		/* In an ideal world we would write the header at this point. Unfortunately
		** that would prevent string metadata being added so we have to hold off.
		*/

		psf->write_header = flac_write_header ;
		} ;

	psf->datalength = psf->filelength ;
	psf->dataoffset = 0 ;

	psf->container_close = flac_close ;
	psf->seek = flac_seek ;
	psf->byterate = flac_byterate ;

	psf->command = flac_command ;

	switch (subformat)
	{	case SF_FORMAT_PCM_S8 :	/* 8-bit FLAC.  */
		case SF_FORMAT_PCM_16 :	/* 16-bit FLAC. */
		case SF_FORMAT_PCM_24 :	/* 24-bit FLAC. */
			error = flac_init (psf) ;
			break ;

		default : return SFE_UNIMPLEMENTED ;
		} ;

	return error ;
} /* flac_open */

/*------------------------------------------------------------------------------
*/

static int
flac_close	(SF_PRIVATE *psf)
{	FLAC_PRIVATE* pflac ;
	int k ;

	if ((pflac = (FLAC_PRIVATE*) psf->codec_data) == NULL)
		return 0 ;

	if (pflac->metadata != NULL)
		FLAC__metadata_object_delete (pflac->metadata) ;

	if (psf->file.mode == SFM_WRITE)
	{	FLAC__stream_encoder_finish (pflac->fse) ;
		FLAC__stream_encoder_delete (pflac->fse) ;
		free (pflac->encbuffer) ;
		} ;

	if (psf->file.mode == SFM_READ)
	{	FLAC__stream_decoder_finish (pflac->fsd) ;
		FLAC__stream_decoder_delete (pflac->fsd) ;
		} ;

	for (k = 0 ; k < ARRAY_LEN (pflac->rbuffer) ; k++)
		free (pflac->rbuffer [k]) ;

	free (pflac) ;
	psf->codec_data = NULL ;

	return 0 ;
} /* flac_close */

static int
flac_enc_init (SF_PRIVATE *psf)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;
	unsigned bps ;

	/* To cite the flac FAQ at
	** http://flac.sourceforge.net/faq.html#general__samples
	**     "FLAC supports linear sample rates from 1Hz - 655350Hz in 1Hz
	**     increments."
	*/
	if (psf->sf.samplerate < 1 || psf->sf.samplerate > 655350)
	{	psf_log_printf (psf, "flac sample rate out of range.\n", psf->sf.samplerate) ;
		return SFE_FLAC_BAD_SAMPLE_RATE ;
		} ;

	psf_fseek (psf, 0, SEEK_SET) ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_S8 :
			bps = 8 ;
			break ;
		case SF_FORMAT_PCM_16 :
			bps = 16 ;
			break ;
		case SF_FORMAT_PCM_24 :
			bps = 24 ;
			break ;

		default :
			bps = 0 ;
			break ;
		} ;

	if (pflac->fse)
		FLAC__stream_encoder_delete (pflac->fse) ;
	if ((pflac->fse = FLAC__stream_encoder_new ()) == NULL)
		return SFE_FLAC_NEW_DECODER ;

	if (! FLAC__stream_encoder_set_channels (pflac->fse, psf->sf.channels))
	{	psf_log_printf (psf, "FLAC__stream_encoder_set_channels (%d) return false.\n", psf->sf.channels) ;
		return SFE_FLAC_INIT_DECODER ;
		} ;

	if (! FLAC__stream_encoder_set_sample_rate (pflac->fse, psf->sf.samplerate))
	{	psf_log_printf (psf, "FLAC__stream_encoder_set_sample_rate (%d) returned false.\n", psf->sf.samplerate) ;
		return SFE_FLAC_BAD_SAMPLE_RATE ;
		} ;

	if (! FLAC__stream_encoder_set_bits_per_sample (pflac->fse, bps))
	{	psf_log_printf (psf, "FLAC__stream_encoder_set_bits_per_sample (%d) return false.\n", bps) ;
		return SFE_FLAC_INIT_DECODER ;
		} ;

	if (! FLAC__stream_encoder_set_compression_level (pflac->fse, pflac->compression))
	{	psf_log_printf (psf, "FLAC__stream_encoder_set_compression_level (%d) return false.\n", pflac->compression) ;
		return SFE_FLAC_INIT_DECODER ;
		} ;

	return 0 ;
} /* flac_enc_init */

static int
flac_read_header (SF_PRIVATE *psf)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;

	psf_fseek (psf, 0, SEEK_SET) ;
	if (pflac->fsd)
		FLAC__stream_decoder_delete (pflac->fsd) ;
	if ((pflac->fsd = FLAC__stream_decoder_new ()) == NULL)
		return SFE_FLAC_NEW_DECODER ;

	FLAC__stream_decoder_set_metadata_respond_all (pflac->fsd) ;

	if (FLAC__stream_decoder_init_stream (pflac->fsd, sf_flac_read_callback, sf_flac_seek_callback, sf_flac_tell_callback, sf_flac_length_callback, sf_flac_eof_callback, sf_flac_write_callback, sf_flac_meta_callback, sf_flac_error_callback, psf) != FLAC__STREAM_DECODER_INIT_STATUS_OK)
		return SFE_FLAC_INIT_DECODER ;

	FLAC__stream_decoder_process_until_end_of_metadata (pflac->fsd) ;

	psf_log_printf (psf, "End\n") ;

	if (psf->error != 0)
		FLAC__stream_decoder_delete (pflac->fsd) ;
	else
	{	FLAC__uint64 position ;

		FLAC__stream_decoder_get_decode_position (pflac->fsd, &position) ;
		psf->dataoffset = position ;
		} ;

	return psf->error ;
} /* flac_read_header */

static int
flac_command (SF_PRIVATE * psf, int command, void * data, int datasize)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;
	double quality ;

	switch (command)
	{	case SFC_SET_COMPRESSION_LEVEL :
			if (data == NULL || datasize != sizeof (double))
				return SF_FALSE ;

			if (psf->have_written)
				return SF_FALSE ;

			/* FLAC compression level is in the range [0, 8] while libsndfile takes
			** values in the range [0.0, 1.0]. Massage the libsndfile value here.
			*/
			quality = (*((double *) data)) * 8.0 ;
			/* Clip range. */
			pflac->compression = lrint (SF_MAX (0.0, SF_MIN (8.0, quality))) ;

			psf_log_printf (psf, "%s : Setting SFC_SET_COMPRESSION_LEVEL to %u.\n", __func__, pflac->compression) ;

			if (flac_enc_init (psf))
				return SF_FALSE ;

			return SF_TRUE ;

		default :
			return SF_FALSE ;
		} ;

	return SF_FALSE ;
} /* flac_command */

int
flac_init (SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_RDWR)
		return SFE_BAD_MODE_RW ;

	if (psf->file.mode == SFM_READ)
	{	psf->read_short		= flac_read_flac2s ;
		psf->read_int		= flac_read_flac2i ;
		psf->read_float		= flac_read_flac2f ;
		psf->read_double	= flac_read_flac2d ;
		} ;

	if (psf->file.mode == SFM_WRITE)
	{	psf->write_short	= flac_write_s2flac ;
		psf->write_int		= flac_write_i2flac ;
		psf->write_float	= flac_write_f2flac ;
		psf->write_double	= flac_write_d2flac ;
		} ;

	if (psf->filelength > psf->dataoffset)
		psf->datalength = (psf->dataend) ? psf->dataend - psf->dataoffset : psf->filelength - psf->dataoffset ;
	else
		psf->datalength = 0 ;

	return 0 ;
} /* flac_init */

static unsigned
flac_read_loop (SF_PRIVATE *psf, unsigned len)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;
	FLAC__StreamDecoderState state ;

	pflac->pos = 0 ;
	pflac->len = len ;
	pflac->remain = len ;

	state = FLAC__stream_decoder_get_state (pflac->fsd) ;
	if (state > FLAC__STREAM_DECODER_END_OF_STREAM)
	{	psf_log_printf (psf, "FLAC__stream_decoder_get_state returned %s\n", FLAC__StreamDecoderStateString [state]) ;
		/* Current frame is busted, so NULL the pointer. */
		pflac->frame = NULL ;
		} ;

	/* First copy data that has already been decoded and buffered. */
	if (pflac->frame != NULL && pflac->bufferpos < pflac->frame->header.blocksize)
		flac_buffer_copy (psf) ;

	/* Decode some more. */
	while (pflac->pos < pflac->len)
	{	if (FLAC__stream_decoder_process_single (pflac->fsd) == 0)
			break ;
		state = FLAC__stream_decoder_get_state (pflac->fsd) ;
		if (state >= FLAC__STREAM_DECODER_END_OF_STREAM)
		{	psf_log_printf (psf, "FLAC__stream_decoder_get_state returned %s\n", FLAC__StreamDecoderStateString [state]) ;
			/* Current frame is busted, so NULL the pointer. */
			pflac->frame = NULL ;
			break ;
			} ;
		} ;

	pflac->ptr = NULL ;

	return pflac->pos ;
} /* flac_read_loop */

static sf_count_t
flac_read_flac2s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;
	sf_count_t total = 0, current ;
	unsigned readlen ;

	pflac->pcmtype = PFLAC_PCM_SHORT ;

	while (total < len)
	{	pflac->ptr = ptr + total ;
		readlen = (len - total > 0x1000000) ? 0x1000000 : (unsigned) (len - total) ;
		current = flac_read_loop (psf, readlen) ;
		if (current == 0)
			break ;
		total += current ;
		} ;

	return total ;
} /* flac_read_flac2s */

static sf_count_t
flac_read_flac2i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;
	sf_count_t total = 0, current ;
	unsigned readlen ;

	pflac->pcmtype = PFLAC_PCM_INT ;

	while (total < len)
	{	pflac->ptr = ptr + total ;
		readlen = (len - total > 0x1000000) ? 0x1000000 : (unsigned) (len - total) ;
		current = flac_read_loop (psf, readlen) ;
		if (current == 0)
			break ;
		total += current ;
		} ;

	return total ;
} /* flac_read_flac2i */

static sf_count_t
flac_read_flac2f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;
	sf_count_t total = 0, current ;
	unsigned readlen ;

	pflac->pcmtype = PFLAC_PCM_FLOAT ;

	while (total < len)
	{	pflac->ptr = ptr + total ;
		readlen = (len - total > 0x1000000) ? 0x1000000 : (unsigned) (len - total) ;
		current = flac_read_loop (psf, readlen) ;
		if (current == 0)
			break ;
		total += current ;
		} ;

	return total ;
} /* flac_read_flac2f */

static sf_count_t
flac_read_flac2d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;
	sf_count_t total = 0, current ;
	unsigned readlen ;

	pflac->pcmtype = PFLAC_PCM_DOUBLE ;

	while (total < len)
	{	pflac->ptr = ptr + total ;
		readlen = (len - total > 0x1000000) ? 0x1000000 : (unsigned) (len - total) ;

		current = flac_read_loop (psf, readlen) ;
		if (current == 0)
			break ;
		total += current ;
		} ;

	return total ;
} /* flac_read_flac2d */

static sf_count_t
flac_write_s2flac (SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;
	void (*convert) (const short *, int32_t *, int) ;
	int bufferlen, writecount, thiswrite ;
	sf_count_t	total = 0 ;
	int32_t* buffer = pflac->encbuffer ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_S8 :
			convert = s2flac8_array ;
			break ;
		case SF_FORMAT_PCM_16 :
			convert = s2flac16_array ;
			break ;
		case SF_FORMAT_PCM_24 :
			convert = s2flac24_array ;
			break ;
		default :
			return -1 ;
		} ;

	bufferlen = ENC_BUFFER_SIZE / (sizeof (int32_t) * psf->sf.channels) ;
	bufferlen *= psf->sf.channels ;

	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : (int) len ;
		convert (ptr + total, buffer, writecount) ;
		if (FLAC__stream_encoder_process_interleaved (pflac->fse, buffer, writecount / psf->sf.channels))
			thiswrite = writecount ;
		else
			break ;
		total += thiswrite ;
		if (thiswrite < writecount)
			break ;

		len -= thiswrite ;
		} ;

	return total ;
} /* flac_write_s2flac */

static sf_count_t
flac_write_i2flac (SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;
	void (*convert) (const int *, int32_t *, int) ;
	int bufferlen, writecount, thiswrite ;
	sf_count_t	total = 0 ;
	int32_t* buffer = pflac->encbuffer ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_S8 :
			convert = i2flac8_array ;
			break ;
		case SF_FORMAT_PCM_16 :
			convert = i2flac16_array ;
			break ;
		case SF_FORMAT_PCM_24 :
			convert = i2flac24_array ;
			break ;
		default :
			return -1 ;
		} ;

	bufferlen = ENC_BUFFER_SIZE / (sizeof (int32_t) * psf->sf.channels) ;
	bufferlen *= psf->sf.channels ;

	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : (int) len ;
		convert (ptr + total, buffer, writecount) ;
		if (FLAC__stream_encoder_process_interleaved (pflac->fse, buffer, writecount / psf->sf.channels))
			thiswrite = writecount ;
		else
			break ;
		total += thiswrite ;
		if (thiswrite < writecount)
			break ;

		len -= thiswrite ;
		} ;

	return total ;
} /* flac_write_i2flac */

static sf_count_t
flac_write_f2flac (SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;
	void (*convert) (const float *, int32_t *, int, int) ;
	int bufferlen, writecount, thiswrite ;
	sf_count_t	total = 0 ;
	int32_t* buffer = pflac->encbuffer ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_S8 :
			convert = (psf->add_clipping) ? f2flac8_clip_array : f2flac8_array ;
			break ;
		case SF_FORMAT_PCM_16 :
			convert = (psf->add_clipping) ? f2flac16_clip_array : f2flac16_array ;
			break ;
		case SF_FORMAT_PCM_24 :
			convert = (psf->add_clipping) ? f2flac24_clip_array : f2flac24_array ;
			break ;
		default :
			return -1 ;
		} ;

	bufferlen = ENC_BUFFER_SIZE / (sizeof (int32_t) * psf->sf.channels) ;
	bufferlen *= psf->sf.channels ;

	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : (int) len ;
		convert (ptr + total, buffer, writecount, psf->norm_float) ;
		if (FLAC__stream_encoder_process_interleaved (pflac->fse, buffer, writecount / psf->sf.channels))
			thiswrite = writecount ;
		else
			break ;
		total += thiswrite ;
		if (thiswrite < writecount)
			break ;

		len -= thiswrite ;
		} ;

	return total ;
} /* flac_write_f2flac */

static void
f2flac8_clip_array (const float *src, int32_t *dest, int count, int normalize)
{	float normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x10) : 1.0 ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7F))
		{	dest [count] = 0x7F ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10))
		{	dest [count] = -0x80 ;
			continue ;
			} ;
		dest [count] = lrintf (scaled_value) ;
		} ;

	return ;
} /* f2flac8_clip_array */

static void
f2flac16_clip_array (const float *src, int32_t *dest, int count, int normalize)
{	float normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x1000) : 1.0 ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFF))
		{	dest [count] = 0x7FFF ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x1000))
		{	dest [count] = -0x8000 ;
			continue ;
			} ;
		dest [count] = lrintf (scaled_value) ;
		} ;
} /* f2flac16_clip_array */

static void
f2flac24_clip_array (const float *src, int32_t *dest, int count, int normalize)
{	float normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x100000) : 1.0 ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFF))
		{	dest [count] = 0x7FFFFF ;
			continue ;
			} ;

		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x100000))
		{	dest [count] = -0x800000 ;
			continue ;
			}
		dest [count] = lrintf (scaled_value) ;
		} ;

	return ;
} /* f2flac24_clip_array */

static void
f2flac8_array (const float *src, int32_t *dest, int count, int normalize)
{	float normfact = normalize ? (1.0 * 0x7F) : 1.0 ;

	while (--count >= 0)
		dest [count] = lrintf (src [count] * normfact) ;
} /* f2flac8_array */

static void
f2flac16_array (const float *src, int32_t *dest, int count, int normalize)
{	float normfact = normalize ? (1.0 * 0x7FFF) : 1.0 ;

	while (--count >= 0)
		dest [count] = lrintf (src [count] * normfact) ;
} /* f2flac16_array */

static void
f2flac24_array (const float *src, int32_t *dest, int count, int normalize)
{	float normfact = normalize ? (1.0 * 0x7FFFFF) : 1.0 ;

	while (--count >= 0)
		dest [count] = lrintf (src [count] * normfact) ;
} /* f2flac24_array */

static sf_count_t
flac_write_d2flac (SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;
	void (*convert) (const double *, int32_t *, int, int) ;
	int bufferlen, writecount, thiswrite ;
	sf_count_t	total = 0 ;
	int32_t* buffer = pflac->encbuffer ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_S8 :
			convert = (psf->add_clipping) ? d2flac8_clip_array : d2flac8_array ;
			break ;
		case SF_FORMAT_PCM_16 :
			convert = (psf->add_clipping) ? d2flac16_clip_array : d2flac16_array ;
			break ;
		case SF_FORMAT_PCM_24 :
			convert = (psf->add_clipping) ? d2flac24_clip_array : d2flac24_array ;
			break ;
		default :
			return -1 ;
		} ;

	bufferlen = ENC_BUFFER_SIZE / (sizeof (int32_t) * psf->sf.channels) ;
	bufferlen *= psf->sf.channels ;

	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : (int) len ;
		convert (ptr + total, buffer, writecount, psf->norm_double) ;
		if (FLAC__stream_encoder_process_interleaved (pflac->fse, buffer, writecount / psf->sf.channels))
			thiswrite = writecount ;
		else
			break ;
		total += thiswrite ;
		if (thiswrite < writecount)
			break ;

		len -= thiswrite ;
		} ;

	return total ;
} /* flac_write_d2flac */

static void
d2flac8_clip_array (const double *src, int32_t *dest, int count, int normalize)
{	double normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x10) : 1.0 ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7F))
		{	dest [count] = 0x7F ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10))
		{	dest [count] = -0x80 ;
			continue ;
			} ;
		dest [count] = lrint (scaled_value) ;
		} ;

	return ;
} /* d2flac8_clip_array */

static void
d2flac16_clip_array (const double *src, int32_t *dest, int count, int normalize)
{	double normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x1000) : 1.0 ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFF))
		{	dest [count] = 0x7FFF ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x1000))
		{	dest [count] = -0x8000 ;
			continue ;
			} ;
		dest [count] = lrint (scaled_value) ;
		} ;

	return ;
} /* d2flac16_clip_array */

static void
d2flac24_clip_array (const double *src, int32_t *dest, int count, int normalize)
{	double normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x100000) : 1.0 ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFF))
		{	dest [count] = 0x7FFFFF ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x100000))
		{	dest [count] = -0x800000 ;
			continue ;
			} ;
		dest [count] = lrint (scaled_value) ;
		} ;

	return ;
} /* d2flac24_clip_array */

static void
d2flac8_array (const double *src, int32_t *dest, int count, int normalize)
{	double normfact = normalize ? (1.0 * 0x7F) : 1.0 ;

	while (--count >= 0)
		dest [count] = lrint (src [count] * normfact) ;
} /* d2flac8_array */

static void
d2flac16_array (const double *src, int32_t *dest, int count, int normalize)
{	double normfact = normalize ? (1.0 * 0x7FFF) : 1.0 ;

	while (--count >= 0)
		dest [count] = lrint (src [count] * normfact) ;
} /* d2flac16_array */

static void
d2flac24_array (const double *src, int32_t *dest, int count, int normalize)
{	double normfact = normalize ? (1.0 * 0x7FFFFF) : 1.0 ;

	while (--count >= 0)
		dest [count] = lrint (src [count] * normfact) ;
} /* d2flac24_array */

static sf_count_t
flac_seek (SF_PRIVATE *psf, int UNUSED (mode), sf_count_t offset)
{	FLAC_PRIVATE* pflac = (FLAC_PRIVATE*) psf->codec_data ;

	if (pflac == NULL)
		return 0 ;

	if (psf->dataoffset < 0)
	{	psf->error = SFE_BAD_SEEK ;
		return ((sf_count_t) -1) ;
		} ;

	pflac->frame = NULL ;

	if (psf->file.mode == SFM_READ)
	{	if (FLAC__stream_decoder_seek_absolute (pflac->fsd, offset))
			return offset ;

		if (offset == psf->sf.frames)
		{	/*
			** If we've been asked to seek to the very end of the file, libFLAC
			** will return an error. However, we know the length of the file so
			** instead of returning an error, we can return the offset.
			*/
			return offset ;
			} ;

		psf->error = SFE_BAD_SEEK ;
		return ((sf_count_t) -1) ;
		} ;

	/* Seeking in write mode not yet supported. */
	psf->error = SFE_BAD_SEEK ;

	return ((sf_count_t) -1) ;
} /* flac_seek */

static int
flac_byterate (SF_PRIVATE *psf)
{
	if (psf->file.mode == SFM_READ)
		return (psf->datalength * psf->sf.samplerate) / psf->sf.frames ;

	return -1 ;
} /* flac_byterate */


#else /* HAVE_EXTERNAL_XIPH_LIBS */

int
flac_open	(SF_PRIVATE *psf)
{
	psf_log_printf (psf, "This version of libsndfile was compiled without FLAC support.\n") ;
	return SFE_UNIMPLEMENTED ;
} /* flac_open */

#endif
