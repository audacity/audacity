/* flac - Command-line FLAC encoder/decoder
 * Copyright (C) 2000-2009  Josh Coalson
 * Copyright (C) 2011-2014  Xiph.Org Foundation
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <errno.h>
#include <limits.h> /* for LONG_MAX */
#include <math.h> /* for floor() */
#include <stdio.h> /* for FILE etc. */
#include <stdlib.h> /* for malloc */
#include <string.h> /* for strcmp(), strerror() */
#include <sys/stat.h>
#include "FLAC/all.h"
#include "share/alloc.h"
#include "share/grabbag.h"
#include "share/compat.h"
#include "share/private.h"
#include "share/safe_str.h"
#include "encode.h"

#ifdef min
#undef min
#endif
#define min(x,y) ((x)<(y)?(x):(y))
#ifdef max
#undef max
#endif
#define max(x,y) ((x)>(y)?(x):(y))

/* this MUST be >= 588 so that sector aligning can take place with one read */
/* this MUST be < 2^sizeof(size_t) / ( FLAC__MAX_CHANNELS * (FLAC__MAX_BITS_PER_SAMPLE/8) ) */
#define CHUNK_OF_SAMPLES 2048

typedef struct {
	unsigned sample_rate;
	unsigned channels;
	unsigned bits_per_sample; /* width of sample point, including 'shift' bits, valid bps is bits_per_sample-shift */
	unsigned shift; /* # of LSBs samples have been shifted left by */
	unsigned bytes_per_wide_sample; /* for convenience, always == channels*((bps+7)/8), or 0 if N/A to input format (like FLAC) */
	FLAC__bool is_unsigned_samples;
	FLAC__bool is_big_endian;
	FLAC__uint32 channel_mask;
} SampleInfo;

/* this is the client_data attached to the FLAC decoder when encoding from a FLAC file */
typedef struct {
	FLAC__off_t filesize;
	const FLAC__byte *lookahead;
	unsigned lookahead_length;
	size_t num_metadata_blocks;
	FLAC__StreamMetadata *metadata_blocks[1024]; /*@@@ BAD MAGIC number */
	FLAC__uint64 samples_left_to_process;
	FLAC__bool fatal_error;
} FLACDecoderData;

typedef struct {
#if FLAC__HAS_OGG
	FLAC__bool use_ogg;
#endif
	FLAC__bool verify;
	FLAC__bool is_stdout;
	FLAC__bool outputfile_opened; /* true if we successfully opened the output file and we want it to be deleted if there is an error */
	const char *inbasefilename;
	const char *infilename;
	const char *outfilename;

	FLAC__bool treat_warnings_as_errors;
	FLAC__bool continue_through_decode_errors;
	FLAC__bool replay_gain;
	FLAC__uint64 total_samples_to_encode; /* (i.e. "wide samples" aka "sample frames") WATCHOUT: may be 0 to mean 'unknown' */
	FLAC__uint64 unencoded_size; /* an estimate of the input size, only used in the progress indicator */
	FLAC__uint64 bytes_written;
	FLAC__uint64 samples_written;
	unsigned stats_frames_interval;
	unsigned old_frames_written;

	SampleInfo info;

	FileFormat format;
	union {
		struct {
			FLAC__uint64 data_bytes;
		} iff;
		struct {
			FLAC__StreamDecoder *decoder;
			FLACDecoderData client_data;
		} flac;
	} fmt;

	FLAC__StreamEncoder *encoder;

	FILE *fin;
	FLAC__StreamMetadata *seek_table_template;
	double progress, compression_ratio;
} EncoderSession;

const int FLAC_ENCODE__DEFAULT_PADDING = 8192;

static FLAC__bool is_big_endian_host_;

static FLAC__int8 static_buffer[CHUNK_OF_SAMPLES*FLAC__MAX_CHANNELS*((FLAC__REFERENCE_CODEC_MAX_BITS_PER_SAMPLE+7)/8)];

static union {
	FLAC__int8 *s8;
	FLAC__uint8 *u8;
	FLAC__int16 *s16;
	FLAC__uint16 *u16;
} ubuffer = { static_buffer };


static FLAC__int32 in_[FLAC__MAX_CHANNELS][CHUNK_OF_SAMPLES];
static FLAC__int32 *input_[FLAC__MAX_CHANNELS];


/*
 * local routines
 */
static FLAC__bool EncoderSession_construct(EncoderSession *e, encode_options_t options, FLAC__off_t infilesize, FILE *infile, const char *infilename, const char *outfilename, const FLAC__byte *lookahead, unsigned lookahead_length);
static void EncoderSession_destroy(EncoderSession *e);
static int EncoderSession_finish_ok(EncoderSession *e, int info_align_carry, int info_align_zero, foreign_metadata_t *foreign_metadata, FLAC__bool error_on_compression_fail);
static int EncoderSession_finish_error(EncoderSession *e);
static FLAC__bool EncoderSession_init_encoder(EncoderSession *e, encode_options_t options);
static FLAC__bool EncoderSession_process(EncoderSession *e, const FLAC__int32 * const buffer[], unsigned samples);
static FLAC__bool EncoderSession_format_is_iff(const EncoderSession *e);
static FLAC__bool convert_to_seek_table_template(const char *requested_seek_points, int num_requested_seek_points, FLAC__StreamMetadata *cuesheet, EncoderSession *e);
static FLAC__bool canonicalize_until_specification(utils__SkipUntilSpecification *spec, const char *inbasefilename, unsigned sample_rate, FLAC__uint64 skip, FLAC__uint64 total_samples_in_input);
static FLAC__bool verify_metadata(const EncoderSession *e, FLAC__StreamMetadata **metadata, unsigned num_metadata);
static FLAC__bool format_input(FLAC__int32 *dest[], unsigned wide_samples, FLAC__bool is_big_endian, FLAC__bool is_unsigned_samples, unsigned channels, unsigned bps, unsigned shift, size_t *channel_map);
static void encoder_progress_callback(const FLAC__StreamEncoder *encoder, FLAC__uint64 bytes_written, FLAC__uint64 samples_written, unsigned frames_written, unsigned total_frames_estimate, void *client_data);
static FLAC__StreamDecoderReadStatus flac_decoder_read_callback(const FLAC__StreamDecoder *decoder, FLAC__byte buffer[], size_t *bytes, void *client_data);
static FLAC__StreamDecoderSeekStatus flac_decoder_seek_callback(const FLAC__StreamDecoder *decoder, FLAC__uint64 absolute_byte_offset, void *client_data);
static FLAC__StreamDecoderTellStatus flac_decoder_tell_callback(const FLAC__StreamDecoder *decoder, FLAC__uint64 *absolute_byte_offset, void *client_data);
static FLAC__StreamDecoderLengthStatus flac_decoder_length_callback(const FLAC__StreamDecoder *decoder, FLAC__uint64 *stream_length, void *client_data);
static FLAC__bool flac_decoder_eof_callback(const FLAC__StreamDecoder *decoder, void *client_data);
static FLAC__StreamDecoderWriteStatus flac_decoder_write_callback(const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 * const buffer[], void *client_data);
static void flac_decoder_metadata_callback(const FLAC__StreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data);
static void flac_decoder_error_callback(const FLAC__StreamDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data);
static FLAC__bool parse_cuesheet(FLAC__StreamMetadata **cuesheet, const char *cuesheet_filename, const char *inbasefilename, unsigned sample_rate, FLAC__bool is_cdda, FLAC__uint64 lead_out_offset, FLAC__bool treat_warnings_as_errors);
static void print_stats(const EncoderSession *encoder_session);
static void print_error_with_init_status(const EncoderSession *e, const char *message, FLAC__StreamEncoderInitStatus init_status);
static void print_error_with_state(const EncoderSession *e, const char *message);
static void print_verify_error(EncoderSession *e);
static FLAC__bool read_bytes(FILE *f, FLAC__byte *buf, size_t n, FLAC__bool eof_ok, const char *fn);
static FLAC__bool read_uint16(FILE *f, FLAC__bool big_endian, FLAC__uint16 *val, const char *fn);
static FLAC__bool read_uint32(FILE *f, FLAC__bool big_endian, FLAC__uint32 *val, const char *fn);
static FLAC__bool read_uint64(FILE *f, FLAC__bool big_endian, FLAC__uint64 *val, const char *fn);
static FLAC__bool read_sane_extended(FILE *f, FLAC__uint32 *val, const char *fn);
static FLAC__bool fskip_ahead(FILE *f, FLAC__uint64 offset);
static unsigned count_channel_mask_bits(FLAC__uint32 mask);
#if 0
static FLAC__uint32 limit_channel_mask(FLAC__uint32 mask, unsigned channels);
#endif

static FLAC__bool get_sample_info_raw(EncoderSession *e, encode_options_t options)
{
	e->info.sample_rate = options.format_options.raw.sample_rate;
	e->info.channels = options.format_options.raw.channels;
	e->info.bits_per_sample = options.format_options.raw.bps;
	e->info.shift = 0;
	e->info.bytes_per_wide_sample = options.format_options.raw.channels * ((options.format_options.raw.bps+7)/8);
	e->info.is_unsigned_samples = options.format_options.raw.is_unsigned_samples;
	e->info.is_big_endian = options.format_options.raw.is_big_endian;
	e->info.channel_mask = 0;

	return true;
}

static FLAC__bool get_sample_info_wave(EncoderSession *e, encode_options_t options)
{
	FLAC__bool got_fmt_chunk = false, got_data_chunk = false, got_ds64_chunk = false;
	unsigned sample_rate = 0, channels = 0, bps = 0, shift = 0;
	FLAC__uint32 channel_mask = 0;
	FLAC__uint64 ds64_data_size = 0;

	e->info.is_unsigned_samples = false;
	e->info.is_big_endian = false;

	if(e->format == FORMAT_WAVE64) {
		/*
		 * lookahead[] already has "riff\x2E\x91\xCF\x11\xA5\xD6\x28\xDB", skip over remaining header
		 */
		if(!fskip_ahead(e->fin, 16+8+16-12)) { /* riff GUID + riff size + WAVE GUID - lookahead */
			flac__utils_printf(stderr, 1, "%s: ERROR during read while skipping over remaining \"riff\" header\n", e->inbasefilename);
			return false;
		}
	}
	/* else lookahead[] already has "RIFFxxxxWAVE" or "RF64xxxxWAVE" */

	while(!feof(e->fin) && !got_data_chunk) {
		/* chunk IDs are 4 bytes for WAVE/RF64, 16 for Wave64 */
		/* for WAVE/RF64 we want the 5th char zeroed so we can treat it like a C string */
		char chunk_id[16] = { '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0' };

		if(!read_bytes(e->fin, (FLAC__byte*)chunk_id, e->format==FORMAT_WAVE64?16:4, /*eof_ok=*/true, e->inbasefilename)) {
			flac__utils_printf(stderr, 1, "%s: ERROR: incomplete chunk identifier\n", e->inbasefilename);
			return false;
		}
		if(feof(e->fin))
			break;

		if(e->format == FORMAT_RF64 && !memcmp(chunk_id, "ds64", 4)) { /* RF64 64-bit sizes chunk */
			FLAC__uint32 xx, data_bytes;

			if(got_ds64_chunk) {
				flac__utils_printf(stderr, 1, "%s: ERROR: file has multiple 'ds64' chunks\n", e->inbasefilename);
				return false;
			}
			if(got_fmt_chunk || got_data_chunk) {
				flac__utils_printf(stderr, 1, "%s: ERROR: 'ds64' chunk appears after 'fmt ' or 'data' chunk\n", e->inbasefilename);
				return false;
			}

			/* ds64 chunk size */
			if(!read_uint32(e->fin, /*big_endian=*/false, &xx, e->inbasefilename))
				return false;
			data_bytes = xx;
			if(data_bytes < 28) {
				flac__utils_printf(stderr, 1, "%s: ERROR: non-standard 'ds64' chunk has length = %u\n", e->inbasefilename, (unsigned)data_bytes);
				return false;
			}
			if(data_bytes & 1) /* should never happen, but enforce WAVE alignment rules */
				data_bytes++;

			/* RIFF 64-bit size, lo/hi */
			if(!read_uint32(e->fin, /*big_endian=*/false, &xx, e->inbasefilename))
				return false;
			if(!read_uint32(e->fin, /*big_endian=*/false, &xx, e->inbasefilename))
				return false;

			/* 'data' 64-bit size */
			if(!read_uint64(e->fin, /*big_endian=*/false, &ds64_data_size, e->inbasefilename))
				return false;

			data_bytes -= 16;

			/* skip any extra data in the ds64 chunk */
			if(!fskip_ahead(e->fin, data_bytes)) {
				flac__utils_printf(stderr, 1, "%s: ERROR during read while skipping over extra 'ds64' data\n", e->inbasefilename);
				return false;
			}

			got_ds64_chunk = true;
		}
		else if(
			!memcmp(chunk_id, "fmt ", 4) &&
			(e->format!=FORMAT_WAVE64 || !memcmp(chunk_id, "fmt \xF3\xAC\xD3\x11\x8C\xD1\x00\xC0\x4F\x8E\xDB\x8A", 16))
		) { /* format chunk */
			FLAC__uint16 x;
			FLAC__uint32 xx, data_bytes;
			FLAC__uint16 wFormatTag; /* wFormatTag word from the 'fmt ' chunk */

			if(got_fmt_chunk) {
				flac__utils_printf(stderr, 1, "%s: ERROR: file has multiple 'fmt ' chunks\n", e->inbasefilename);
				return false;
			}

			/* see
			 *   http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html
			 *   http://windowssdk.msdn.microsoft.com/en-us/library/ms713497.aspx
			 *   http://msdn.microsoft.com/library/default.asp?url=/library/en-us/audio_r/hh/Audio_r/aud-prop_d40f094e-44f9-4baa-8a15-03e4fb369501.xml.asp
			 *
			 * WAVEFORMAT is
			 * 4 byte: chunk size
			 * 2 byte: format type: 1 for WAVE_FORMAT_PCM, 65534 for WAVE_FORMAT_EXTENSIBLE
			 * 2 byte: # channels
			 * 4 byte: sample rate (Hz)
			 * 4 byte: avg bytes per sec
			 * 2 byte: block align
			 * 2 byte: bits per sample (not necessarily all significant)
			 * WAVEFORMATEX adds
			 * 2 byte: extension size in bytes (usually 0 for WAVEFORMATEX and 22 for WAVEFORMATEXTENSIBLE with PCM)
			 * WAVEFORMATEXTENSIBLE adds
			 * 2 byte: valid bits per sample
			 * 4 byte: channel mask
			 * 16 byte: subformat GUID, first 2 bytes have format type, 1 being PCM
			 *
			 * Current spec says WAVEFORMATEX with PCM must have bps == 8 or 16, or any multiple of 8 for WAVEFORMATEXTENSIBLE.
			 * Lots of old broken WAVEs/apps have don't follow it, e.g. 20 bps but a block align of 3/6 for mono/stereo.
			 *
			 * Block align for WAVE_FORMAT_PCM or WAVE_FORMAT_EXTENSIBLE is also supposed to be channels*bps/8
			 *
			 * If the channel mask has more set bits than # of channels, the extra MSBs are ignored.
			 * If the channel mask has less set bits than # of channels, the extra channels are unassigned to any speaker.
			 *
			 * Data is supposed to be unsigned for bps <= 8 else signed.
			 */

			/* fmt chunk size */
			if(!read_uint32(e->fin, /*big_endian=*/false, &xx, e->inbasefilename))
				return false;
			data_bytes = xx;
			if(e->format == FORMAT_WAVE64) {
				/* other half of the size field should be 0 */
				if(!read_uint32(e->fin, /*big_endian=*/false, &xx, e->inbasefilename))
					return false;
				if(xx) {
					flac__utils_printf(stderr, 1, "%s: ERROR: freakishly large Wave64 'fmt ' chunk has length = 0x%08X%08X\n", e->inbasefilename, (unsigned)xx, (unsigned)data_bytes);
					return false;
				}
				/* subtract size of header */
				if (data_bytes < 16+8) {
					flac__utils_printf(stderr, 1, "%s: ERROR: freakishly small Wave64 'fmt ' chunk has length = 0x%08X%08X\n", e->inbasefilename, (unsigned)xx, (unsigned)data_bytes);
					return false;
				}
				data_bytes -= (16+8);
			}
			if(data_bytes < 16) {
				flac__utils_printf(stderr, 1, "%s: ERROR: non-standard 'fmt ' chunk has length = %u\n", e->inbasefilename, (unsigned)data_bytes);
				return false;
			}
			if(e->format != FORMAT_WAVE64) {
				if(data_bytes & 1) /* should never happen, but enforce WAVE alignment rules */
					data_bytes++;
			}
			else { /* Wave64 */
				data_bytes = (data_bytes+7) & (~7u); /* should never happen, but enforce Wave64 alignment rules */
			}

			/* format code */
			if(!read_uint16(e->fin, /*big_endian=*/false, &wFormatTag, e->inbasefilename))
				return false;
			if(wFormatTag != 1 /*WAVE_FORMAT_PCM*/ && wFormatTag != 65534 /*WAVE_FORMAT_EXTENSIBLE*/) {
				flac__utils_printf(stderr, 1, "%s: ERROR: unsupported format type %u\n", e->inbasefilename, (unsigned)wFormatTag);
				return false;
			}

			/* number of channels */
			if(!read_uint16(e->fin, /*big_endian=*/false, &x, e->inbasefilename))
				return false;
			channels = (unsigned)x;

			/* sample rate */
			if(!read_uint32(e->fin, /*big_endian=*/false, &xx, e->inbasefilename))
				return false;
			sample_rate = xx;

			/* avg bytes per second (ignored) */
			if(!read_uint32(e->fin, /*big_endian=*/false, &xx, e->inbasefilename))
				return false;
			/* block align */
			if(!read_uint16(e->fin, /*big_endian=*/false, &x, e->inbasefilename))
				return false;

			/* bits per sample */
			if(!read_uint16(e->fin, /*big_endian=*/false, &x, e->inbasefilename))
				return false;
			bps = (unsigned)x;

			e->info.is_unsigned_samples = (bps <= 8);

			if(wFormatTag == 1) {
				if(bps != 8 && bps != 16) {
					if(bps == 24 || bps == 32) {
						/* let these slide with a warning since they're unambiguous */
						flac__utils_printf(stderr, 1, "%s: WARNING: legacy WAVE file has format type %u but bits-per-sample=%u\n", e->inbasefilename, (unsigned)wFormatTag, bps);
						if(e->treat_warnings_as_errors)
							return false;
					}
					else {
						/* @@@ we could add an option to specify left- or right-justified blocks so we knew how to set 'shift' */
						flac__utils_printf(stderr, 1, "%s: ERROR: legacy WAVE file has format type %u but bits-per-sample=%u\n", e->inbasefilename, (unsigned)wFormatTag, bps);
						return false;
					}
				}
#if 0 /* @@@ reinstate once we can get an answer about whether the samples are left- or right-justified */
				if((bps+7)/8 * channels == block_align) {
					if(bps % 8) {
						/* assume legacy file is byte aligned with some LSBs zero; this is double-checked in format_input() */
						flac__utils_printf(stderr, 1, "%s: WARNING: legacy WAVE file (format type %d) has block alignment=%u, bits-per-sample=%u, channels=%u\n", e->inbasefilename, (unsigned)wFormatTag, block_align, bps, channels);
						if(e->treat_warnings_as_errors)
							return false;
						shift = 8 - (bps % 8);
						bps += shift;
					}
					else
						shift = 0;
				}
				else {
					flac__utils_printf(stderr, 1, "%s: ERROR: illegal WAVE file (format type %d) has block alignment=%u, bits-per-sample=%u, channels=%u\n", e->inbasefilename, (unsigned)wFormatTag, block_align, bps, channels);
					return false;
				}
#else
				shift = 0;
#endif
				if(channels > 2 && !options.channel_map_none) {
					flac__utils_printf(stderr, 1, "%s: ERROR: WAVE has >2 channels but is not WAVE_FORMAT_EXTENSIBLE; cannot assign channels\n", e->inbasefilename);
					return false;
				}
				FLAC__ASSERT(data_bytes >= 16);
				data_bytes -= 16;
			}
			else {
				if(data_bytes < 40) {
					flac__utils_printf(stderr, 1, "%s: ERROR: invalid WAVEFORMATEXTENSIBLE chunk with size %u\n", e->inbasefilename, (unsigned)data_bytes);
					return false;
				}
				/* cbSize */
				if(!read_uint16(e->fin, /*big_endian=*/false, &x, e->inbasefilename))
					return false;
				if(x < 22) {
					flac__utils_printf(stderr, 1, "%s: ERROR: invalid WAVEFORMATEXTENSIBLE chunk with cbSize %u\n", e->inbasefilename, (unsigned)x);
					return false;
				}
				/* valid bps */
				if(!read_uint16(e->fin, /*big_endian=*/false, &x, e->inbasefilename))
					return false;
				if((unsigned)x > bps) {
					flac__utils_printf(stderr, 1, "%s: ERROR: invalid WAVEFORMATEXTENSIBLE chunk with wValidBitsPerSample (%u) > wBitsPerSample (%u)\n", e->inbasefilename, (unsigned)x, bps);
					return false;
				}
				shift = bps - (unsigned)x;
				/* channel mask */
				if(!read_uint32(e->fin, /*big_endian=*/false, &channel_mask, e->inbasefilename))
					return false;
				/* for mono/stereo and unassigned channels, we fake the mask */
				if(channel_mask == 0) {
					if(channels == 1)
						channel_mask = 0x0004;
					else if(channels == 2)
						channel_mask = 0x0003;
				}
				/* set channel mapping */
				/* FLAC order follows SMPTE and WAVEFORMATEXTENSIBLE but with fewer channels, which are: */
				/* front left, front right, front center, LFE, back left, back right, back center, side left, side right */
				/* the default mapping is sufficient for 1-8 channels */
#if 0
				/* @@@ example for dolby/vorbis order, for reference later in case it becomes important */
				if(
					options.channel_map_none ||
					channel_mask == 0x0001 || /* 1 channel: (mono) */
					channel_mask == 0x0003 || /* 2 channels: front left, front right */
					channel_mask == 0x0033 || /* 4 channels: front left, front right, back left, back right */
					channel_mask == 0x0603    /* 4 channels: front left, front right, side left, side right */
				) {
					/* keep default channel order */
				}
				else if(
					channel_mask == 0x0007 || /* 3 channels: front left, front right, front center */
					channel_mask == 0x0037 || /* 5 channels: front left, front right, front center, back left, back right */
					channel_mask == 0x0607    /* 5 channels: front left, front right, front center, side left, side right */
				) {
					/* to dolby order: front left, center, front right [, surround left, surround right ] */
					channel_map[1] = 2;
					channel_map[2] = 1;
				}
				else if(
					channel_mask == 0x003f || /* 6 channels: front left, front right, front center, LFE, back left, back right */
					channel_mask == 0x060f || /* 6 channels: front left, front right, front center, LFE, side left, side right */
					channel_mask == 0x070f || /* 7 channels: front left, front right, front center, LFE, back center, side left, side right */
					channel_mask == 0x063f    /* 8 channels: front left, front right, front center, LFE, back left, back right, side left, side right */
				) {
					/* to dolby order: front left, center, front right, surround left, surround right, LFE */
					channel_map[1] = 2;
					channel_map[2] = 1;
					channel_map[3] = 5;
					channel_map[4] = 3;
					channel_map[5] = 4;
				}
#else
				if(
					options.channel_map_none ||
					channel_mask == 0x0001 || /* 1 channel: front left */
					channel_mask == 0x0002 || /* 1 channel: front right */
					channel_mask == 0x0004 || /* 1 channel: mono or front center */
					channel_mask == 0x0003 || /* 2 channels: front left, front right */
					channel_mask == 0x0007 || /* 3 channels: front left, front right, front center */
					channel_mask == 0x0033 || /* 4 channels: front left, front right, back left, back right */
					channel_mask == 0x0603 || /* 4 channels: front left, front right, side left, side right */
					channel_mask == 0x0037 || /* 5 channels: front left, front right, front center, back left, back right */
					channel_mask == 0x0607 || /* 5 channels: front left, front right, front center, side left, side right */
					channel_mask == 0x003f || /* 6 channels: front left, front right, front center, LFE, back left, back right */
					channel_mask == 0x060f || /* 6 channels: front left, front right, front center, LFE, side left, side right */
					channel_mask == 0x070f || /* 7 channels: front left, front right, front center, LFE, back center, side left, side right */
					channel_mask == 0x063f    /* 8 channels: front left, front right, front center, LFE, back left, back right, side left, side right */
				) {
					/* keep default channel order */
				}
#endif
				else {
					flac__utils_printf(stderr, 1, "%s: ERROR: WAVEFORMATEXTENSIBLE chunk with unsupported channel mask=0x%04X\n\nUse --channel-map=none option to encode the input\n", e->inbasefilename, (unsigned)channel_mask);
					return false;
				}
				if(!options.channel_map_none) {
					if(count_channel_mask_bits(channel_mask) < channels) {
						flac__utils_printf(stderr, 1, "%s: ERROR: WAVEFORMATEXTENSIBLE chunk: channel mask 0x%04X has unassigned channels (#channels=%u)\n", e->inbasefilename, (unsigned)channel_mask, channels);
						return false;
					}
#if 0
					/* supporting this is too difficult with channel mapping; e.g. what if mask is 0x003f but #channels=4?
					 * there would be holes in the order that would have to be filled in, or the mask would have to be
					 * limited and the logic above rerun to see if it still fits into the FLAC mapping.
					 */
					else if(count_channel_mask_bits(channel_mask) > channels)
						channel_mask = limit_channel_mask(channel_mask, channels);
#else
					else if(count_channel_mask_bits(channel_mask) > channels) {
						flac__utils_printf(stderr, 1, "%s: ERROR: WAVEFORMATEXTENSIBLE chunk: channel mask 0x%04X has extra bits for non-existant channels (#channels=%u)\n", e->inbasefilename, (unsigned)channel_mask, channels);
						return false;
					}
#endif
				}
				/* first part of GUID */
				if(!read_uint16(e->fin, /*big_endian=*/false, &x, e->inbasefilename))
					return false;
				if(x != 1) {
					flac__utils_printf(stderr, 1, "%s: ERROR: unsupported WAVEFORMATEXTENSIBLE chunk with non-PCM format %u\n", e->inbasefilename, (unsigned)x);
					return false;
				}
				data_bytes -= 26;
			}

			e->info.bytes_per_wide_sample = channels * (bps / 8);

			/* skip any extra data in the fmt chunk */
			if(!fskip_ahead(e->fin, data_bytes)) {
				flac__utils_printf(stderr, 1, "%s: ERROR during read while skipping over extra 'fmt' data\n", e->inbasefilename);
				return false;
			}

			got_fmt_chunk = true;
		}
		else if(
			!memcmp(chunk_id, "data", 4) &&
			(e->format!=FORMAT_WAVE64 || !memcmp(chunk_id, "data\xF3\xAC\xD3\x11\x8C\xD1\x00\xC0\x4F\x8E\xDB\x8A", 16))
		) { /* data chunk */
			FLAC__uint32 xx;
			FLAC__uint64 data_bytes;

			if(!got_fmt_chunk) {
				flac__utils_printf(stderr, 1, "%s: ERROR: got 'data' chunk before 'fmt' chunk\n", e->inbasefilename);
				return false;
			}

			/* data size */
			if(e->format != FORMAT_WAVE64) {
				if(!read_uint32(e->fin, /*big_endian=*/false, &xx, e->inbasefilename))
					return false;
				data_bytes = xx;
			}
			else { /* Wave64 */
				if(!read_uint64(e->fin, /*big_endian=*/false, &data_bytes, e->inbasefilename))
					return false;
				/* subtract size of header */
				if (data_bytes < 16+8) {
					flac__utils_printf(stderr, 1, "%s: ERROR: freakishly small Wave64 'data' chunk has length = 0x00000000%08X\n", e->inbasefilename, (unsigned)data_bytes);
					return false;
				}
				data_bytes -= (16+8);
			}
			if(e->format == FORMAT_RF64) {
				if(!got_ds64_chunk) {
					flac__utils_printf(stderr, 1, "%s: ERROR: RF64 file has no 'ds64' chunk before 'data' chunk\n", e->inbasefilename);
					return false;
				}
				if(data_bytes == 0xffffffff)
					data_bytes = ds64_data_size;
			}
			if(options.ignore_chunk_sizes) {
				FLAC__ASSERT(!options.sector_align);
				if(data_bytes) {
					flac__utils_printf(stderr, 1, "%s: WARNING: 'data' chunk has non-zero size, using --ignore-chunk-sizes is probably a bad idea\n", e->inbasefilename, chunk_id);
					if(e->treat_warnings_as_errors)
						return false;
				}
				data_bytes = (FLAC__uint64)0 - (FLAC__uint64)e->info.bytes_per_wide_sample; /* max out data_bytes; we'll use EOF as signal to stop reading */
			}
			else if(0 == data_bytes) {
				flac__utils_printf(stderr, 1, "%s: ERROR: 'data' chunk has size of 0\n", e->inbasefilename);
				return false;
			}

			e->fmt.iff.data_bytes = data_bytes;

			got_data_chunk = true;
			break;
		}
		else {
			FLAC__uint32 xx;
			FLAC__uint64 skip;
			if(!options.format_options.iff.foreign_metadata) {
				if(e->format != FORMAT_WAVE64)
					flac__utils_printf(stderr, 1, "%s: WARNING: skipping unknown chunk '%s' (use --keep-foreign-metadata to keep)\n", e->inbasefilename, chunk_id);
				else
					flac__utils_printf(stderr, 1, "%s: WARNING: skipping unknown chunk %02X%02X%02X%02X-%02X%02X-%02X%02X-%02X%02X-%02X%02X%02X%02X%02X%02X (use --keep-foreign-metadata to keep)\n",
						e->inbasefilename,
						(unsigned)((const unsigned char *)chunk_id)[3],
						(unsigned)((const unsigned char *)chunk_id)[2],
						(unsigned)((const unsigned char *)chunk_id)[1],
						(unsigned)((const unsigned char *)chunk_id)[0],
						(unsigned)((const unsigned char *)chunk_id)[5],
						(unsigned)((const unsigned char *)chunk_id)[4],
						(unsigned)((const unsigned char *)chunk_id)[7],
						(unsigned)((const unsigned char *)chunk_id)[6],
						(unsigned)((const unsigned char *)chunk_id)[9],
						(unsigned)((const unsigned char *)chunk_id)[8],
						(unsigned)((const unsigned char *)chunk_id)[10],
						(unsigned)((const unsigned char *)chunk_id)[11],
						(unsigned)((const unsigned char *)chunk_id)[12],
						(unsigned)((const unsigned char *)chunk_id)[13],
						(unsigned)((const unsigned char *)chunk_id)[14],
						(unsigned)((const unsigned char *)chunk_id)[15]
					);
				if(e->treat_warnings_as_errors)
					return false;
			}

			/* chunk size */
			if(e->format != FORMAT_WAVE64) {
				if(!read_uint32(e->fin, /*big_endian=*/false, &xx, e->inbasefilename))
					return false;
				skip = xx;
				skip += skip & 1;
			}
			else { /* Wave64 */
				if(!read_uint64(e->fin, /*big_endian=*/false, &skip, e->inbasefilename))
					return false;
				skip = (skip+7) & (~(FLAC__uint64)7);
				/* subtract size of header */
				if (skip < 16+8) {
					flac__utils_printf(stderr, 1, "%s: ERROR: freakishly small Wave64 chunk has length = 0x00000000%08X\n", e->inbasefilename, (unsigned)skip);
					return false;
				}
				skip -= (16+8);
			}
			if(skip) {
				if(!fskip_ahead(e->fin, skip)) {
					flac__utils_printf(stderr, 1, "%s: ERROR during read while skipping over chunk\n", e->inbasefilename);
					return false;
				}
			}
		}
	}

	if(!got_fmt_chunk) {
		flac__utils_printf(stderr, 1, "%s: ERROR: didn't find fmt chunk\n", e->inbasefilename);
		return false;
	}
	if(!got_data_chunk) {
		flac__utils_printf(stderr, 1, "%s: ERROR: didn't find data chunk\n", e->inbasefilename);
		return false;
	}

	e->info.sample_rate = sample_rate;
	e->info.channels = channels;
	e->info.bits_per_sample = bps;
	e->info.shift = shift;
	e->info.channel_mask = channel_mask;

	return true;
}

static FLAC__bool get_sample_info_aiff(EncoderSession *e, encode_options_t options)
{
	FLAC__bool got_comm_chunk = false, got_ssnd_chunk = false;
	unsigned sample_rate = 0, channels = 0, bps = 0, shift = 0;
	FLAC__uint64 sample_frames = 0;
	FLAC__uint32 channel_mask = 0;

	e->info.is_unsigned_samples = false;
	e->info.is_big_endian = true;

	/*
	 * lookahead[] already has "FORMxxxxAIFF", do chunks
	 */
	while(!feof(e->fin) && !got_ssnd_chunk) {
		char chunk_id[5] = { '\0', '\0', '\0', '\0', '\0' }; /* one extra byte for terminating NUL so we can also treat it like a C string */
		if(!read_bytes(e->fin, (FLAC__byte*)chunk_id, 4, /*eof_ok=*/true, e->inbasefilename)) {
			flac__utils_printf(stderr, 1, "%s: ERROR: incomplete chunk identifier\n", e->inbasefilename);
			return false;
		}
		if(feof(e->fin))
			break;

		if(!memcmp(chunk_id, "COMM", 4)) { /* common chunk */
			FLAC__uint16 x;
			FLAC__uint32 xx;
			unsigned long skip;
			const FLAC__bool is_aifc = e->format == FORMAT_AIFF_C;
			const FLAC__uint32 minimum_comm_size = (is_aifc? 22 : 18);

			if(got_comm_chunk) {
				flac__utils_printf(stderr, 1, "%s: ERROR: file has multiple 'COMM' chunks\n", e->inbasefilename);
				return false;
			}

			/* COMM chunk size */
			if(!read_uint32(e->fin, /*big_endian=*/true, &xx, e->inbasefilename))
				return false;
			else if(xx < minimum_comm_size) {
				flac__utils_printf(stderr, 1, "%s: ERROR: non-standard %s 'COMM' chunk has length = %u\n", e->inbasefilename, is_aifc? "AIFF-C" : "AIFF", (unsigned int)xx);
				return false;
			}
			else if(!is_aifc && xx != minimum_comm_size) {
				flac__utils_printf(stderr, 1, "%s: WARNING: non-standard %s 'COMM' chunk has length = %u, expected %u\n", e->inbasefilename, is_aifc? "AIFF-C" : "AIFF", (unsigned int)xx, minimum_comm_size);
				if(e->treat_warnings_as_errors)
					return false;
			}
			skip = (xx-minimum_comm_size)+(xx & 1);

			/* number of channels */
			if(!read_uint16(e->fin, /*big_endian=*/true, &x, e->inbasefilename))
				return false;
			channels = (unsigned)x;
			if(channels > 2 && !options.channel_map_none) {
				flac__utils_printf(stderr, 1, "%s: ERROR: unsupported number of channels %u for AIFF\n", e->inbasefilename, channels);
				return false;
			}

			/* number of sample frames */
			if(!read_uint32(e->fin, /*big_endian=*/true, &xx, e->inbasefilename))
				return false;
			sample_frames = xx;

			/* bits per sample */
			if(!read_uint16(e->fin, /*big_endian=*/true, &x, e->inbasefilename))
				return false;
			bps = (unsigned)x;
			shift = (bps%8)? 8-(bps%8) : 0; /* SSND data is always byte-aligned, left-justified but format_input() will double-check */
			bps += shift;

			/* sample rate */
			if(!read_sane_extended(e->fin, &xx, e->inbasefilename))
				return false;
			sample_rate = xx;

			/* check compression type for AIFF-C */
			if(is_aifc) {
				if(!read_uint32(e->fin, /*big_endian=*/true, &xx, e->inbasefilename))
					return false;
				if(xx == 0x736F7774) /* "sowt" */
					e->info.is_big_endian = false;
				else if(xx == 0x4E4F4E45) /* "NONE" */
					; /* nothing to do, we already default to big-endian */
				else {
					flac__utils_printf(stderr, 1, "%s: ERROR: can't handle AIFF-C compression type \"%c%c%c%c\"\n", e->inbasefilename, (char)(xx>>24), (char)((xx>>16)&8), (char)((xx>>8)&8), (char)(xx&8));
					return false;
				}
			}

			/* set channel mapping */
			/* FLAC order follows SMPTE and WAVEFORMATEXTENSIBLE but with fewer channels, which are: */
			/* front left, front right, center, LFE, back left, back right, surround left, surround right */
			/* specs say the channel ordering is:
			 *                             1     2   3   4   5   6
			 * ___________________________________________________
			 * 2         stereo            l     r
			 * 3                           l     r   c
			 * 4                           l     c   r   S
			 * quad (ambiguous with 4ch)  Fl    Fr   Bl  Br
			 * 5                          Fl     Fr  Fc  Sl  Sr
			 * 6                           l     lc  c   r   rc  S
			 * l:left r:right c:center Fl:front-left Fr:front-right Bl:back-left Br:back-right Lc:left-center Rc:right-center S:surround
			 * so we only have unambiguous mappings for 2, 3, and 5 channels
			 */
			if(
				options.channel_map_none ||
				channels == 1 || /* 1 channel: (mono) */
				channels == 2 || /* 2 channels: left, right */
				channels == 3 || /* 3 channels: left, right, center */
				channels == 5    /* 5 channels: front left, front right, center, surround left, surround right */
			) {
				/* keep default channel order */
			}
			else {
				flac__utils_printf(stderr, 1, "%s: ERROR: unsupported number of channels %u for AIFF\n", e->inbasefilename, channels);
				return false;
			}

			e->info.bytes_per_wide_sample = channels * (bps / 8);

			/* skip any extra data in the COMM chunk */
			if(!fskip_ahead(e->fin, skip)) {
				flac__utils_printf(stderr, 1, "%s: ERROR during read while skipping over extra COMM data\n", e->inbasefilename);
				return false;
			}

			got_comm_chunk = true;
		}
		else if(!memcmp(chunk_id, "SSND", 4) && !got_ssnd_chunk) { /* sound data chunk */
			FLAC__uint32 xx;
			FLAC__uint64 data_bytes;
			unsigned offset = 0;

			if(!got_comm_chunk) {
				flac__utils_printf(stderr, 1, "%s: ERROR: got 'SSND' chunk before 'COMM' chunk\n", e->inbasefilename);
				return false;
			}

			/* SSND chunk size */
			if(!read_uint32(e->fin, /*big_endian=*/true, &xx, e->inbasefilename))
				return false;
			data_bytes = xx;
			if(options.ignore_chunk_sizes) {
				FLAC__ASSERT(!options.sector_align);
				if(data_bytes) {
					flac__utils_printf(stderr, 1, "%s: WARNING: 'SSND' chunk has non-zero size, using --ignore-chunk-sizes is probably a bad idea\n", e->inbasefilename, chunk_id);
					if(e->treat_warnings_as_errors)
						return false;
				}
				data_bytes = (FLAC__uint64)0 - (FLAC__uint64)e->info.bytes_per_wide_sample; /* max out data_bytes; we'll use EOF as signal to stop reading */
			}
			else if(data_bytes <= 8) {
				flac__utils_printf(stderr, 1, "%s: ERROR: 'SSND' chunk has size <= 8\n", e->inbasefilename);
				return false;
			}
			else {
				data_bytes -= 8; /* discount the offset and block size fields */
			}

			/* offset */
			if(!read_uint32(e->fin, /*big_endian=*/true, &xx, e->inbasefilename))
				return false;
			offset = xx;
			data_bytes -= offset;

			/* block size */
			if(!read_uint32(e->fin, /*big_endian=*/true, &xx, e->inbasefilename))
				return false;
			if(xx && !options.ignore_chunk_sizes)
				data_bytes -= (xx - (data_bytes % xx));
			if(options.ignore_chunk_sizes) {
				if(xx) {
					flac__utils_printf(stderr, 1, "%s: WARNING: 'SSND' chunk has non-zero blocksize, using --ignore-chunk-sizes is probably a bad idea\n", e->inbasefilename, chunk_id);
					if(e->treat_warnings_as_errors)
						return false;
				}
			}

			/* skip any SSND offset bytes */
			if(!fskip_ahead(e->fin, offset)) {
				flac__utils_printf(stderr, 1, "%s: ERROR: skipping offset in SSND chunk\n", e->inbasefilename);
				return false;
			}

			e->fmt.iff.data_bytes = data_bytes;

			got_ssnd_chunk = true;
		}
		else {
			FLAC__uint32 xx;
			if(!options.format_options.iff.foreign_metadata) {
				flac__utils_printf(stderr, 1, "%s: WARNING: skipping unknown chunk '%s' (use --keep-foreign-metadata to keep)\n", e->inbasefilename, chunk_id);
				if(e->treat_warnings_as_errors)
					return false;
			}

			/* chunk size */
			if(!read_uint32(e->fin, /*big_endian=*/true, &xx, e->inbasefilename))
				return false;
			else {
				unsigned long skip = xx + (xx & 1);

				FLAC__ASSERT(skip <= LONG_MAX);
				if(!fskip_ahead(e->fin, skip)) {
					flac__utils_printf(stderr, 1, "%s: ERROR during read while skipping over chunk\n", e->inbasefilename);
					return false;
				}
			}
		}
	}

	if(!got_comm_chunk) {
		flac__utils_printf(stderr, 1, "%s: ERROR: didn't find COMM chunk\n", e->inbasefilename);
		return false;
	}
	if(!got_ssnd_chunk && sample_frames) {
		flac__utils_printf(stderr, 1, "%s: ERROR: didn't find SSND chunk\n", e->inbasefilename);
		return false;
	}

	e->info.sample_rate = sample_rate;
	e->info.channels = channels;
	e->info.bits_per_sample = bps;
	e->info.shift = shift;
	e->info.channel_mask = channel_mask;

	return true;
}

static FLAC__bool get_sample_info_flac(EncoderSession *e)
{
	if (!(
		FLAC__stream_decoder_set_md5_checking(e->fmt.flac.decoder, false) &&
		FLAC__stream_decoder_set_metadata_respond_all(e->fmt.flac.decoder)
	)) {
		flac__utils_printf(stderr, 1, "%s: ERROR: setting up decoder for FLAC input\n", e->inbasefilename);
		return false;
	}

	if (e->format == FORMAT_OGGFLAC) {
		if (FLAC__stream_decoder_init_ogg_stream(e->fmt.flac.decoder, flac_decoder_read_callback, flac_decoder_seek_callback, flac_decoder_tell_callback, flac_decoder_length_callback, flac_decoder_eof_callback, flac_decoder_write_callback, flac_decoder_metadata_callback, flac_decoder_error_callback, /*client_data=*/e) != FLAC__STREAM_DECODER_INIT_STATUS_OK) {
			flac__utils_printf(stderr, 1, "%s: ERROR: initializing decoder for Ogg FLAC input, state = %s\n", e->inbasefilename, FLAC__stream_decoder_get_resolved_state_string(e->fmt.flac.decoder));
			return false;
		}
	}
	else if (FLAC__stream_decoder_init_stream(e->fmt.flac.decoder, flac_decoder_read_callback, flac_decoder_seek_callback, flac_decoder_tell_callback, flac_decoder_length_callback, flac_decoder_eof_callback, flac_decoder_write_callback, flac_decoder_metadata_callback, flac_decoder_error_callback, /*client_data=*/e) != FLAC__STREAM_DECODER_INIT_STATUS_OK) {
		flac__utils_printf(stderr, 1, "%s: ERROR: initializing decoder for FLAC input, state = %s\n", e->inbasefilename, FLAC__stream_decoder_get_resolved_state_string(e->fmt.flac.decoder));
		return false;
	}

	if (!FLAC__stream_decoder_process_until_end_of_metadata(e->fmt.flac.decoder) || e->fmt.flac.client_data.fatal_error) {
		if (e->fmt.flac.client_data.fatal_error)
			flac__utils_printf(stderr, 1, "%s: ERROR: out of memory or too many metadata blocks while reading metadata in FLAC input\n", e->inbasefilename);
		else
			flac__utils_printf(stderr, 1, "%s: ERROR: reading metadata in FLAC input, state = %s\n", e->inbasefilename, FLAC__stream_decoder_get_resolved_state_string(e->fmt.flac.decoder));
		return false;
	}

	if (e->fmt.flac.client_data.num_metadata_blocks == 0) {
		flac__utils_printf(stderr, 1, "%s: ERROR: reading metadata in FLAC input, got no metadata blocks\n", e->inbasefilename);
		return false;
	}
	else if (e->fmt.flac.client_data.metadata_blocks[0]->type != FLAC__METADATA_TYPE_STREAMINFO) {
		flac__utils_printf(stderr, 1, "%s: ERROR: reading metadata in FLAC input, first metadata block is not STREAMINFO\n", e->inbasefilename);
		return false;
	}
	else if (e->fmt.flac.client_data.metadata_blocks[0]->data.stream_info.total_samples == 0) {
		flac__utils_printf(stderr, 1, "%s: ERROR: FLAC input has STREAMINFO with unknown total samples which is not supported\n", e->inbasefilename);
		return false;
	}

	e->info.sample_rate = e->fmt.flac.client_data.metadata_blocks[0]->data.stream_info.sample_rate;
	e->info.channels = e->fmt.flac.client_data.metadata_blocks[0]->data.stream_info.channels;
	e->info.bits_per_sample = e->fmt.flac.client_data.metadata_blocks[0]->data.stream_info.bits_per_sample;
	e->info.shift = 0;
	e->info.bytes_per_wide_sample = 0;
	e->info.is_unsigned_samples = false; /* not applicable for FLAC input */
	e->info.is_big_endian = false; /* not applicable for FLAC input */
	e->info.channel_mask = 0;

	return true;
}

/*
 * public routines
 */
int flac__encode_file(FILE *infile, FLAC__off_t infilesize, const char *infilename, const char *outfilename, const FLAC__byte *lookahead, unsigned lookahead_length, encode_options_t options)
{
	EncoderSession encoder_session;
	size_t channel_map[FLAC__MAX_CHANNELS];
	int info_align_carry = -1, info_align_zero = -1;

	if(!EncoderSession_construct(&encoder_session, options, infilesize, infile, infilename, outfilename, lookahead, lookahead_length))
		return 1;

	/* initialize default channel map that preserves channel order */
	{
		size_t i;
		for(i = 0; i < sizeof(channel_map)/sizeof(channel_map[0]); i++)
			channel_map[i] = i;
	}

	/* read foreign metadata if requested */
	if(EncoderSession_format_is_iff(&encoder_session) && options.format_options.iff.foreign_metadata) {
		const char *error;
		if(!(
			options.format == FORMAT_WAVE || options.format == FORMAT_RF64?
				flac__foreign_metadata_read_from_wave(options.format_options.iff.foreign_metadata, infilename, &error) :
			options.format == FORMAT_WAVE64?
				flac__foreign_metadata_read_from_wave64(options.format_options.iff.foreign_metadata, infilename, &error) :
				flac__foreign_metadata_read_from_aiff(options.format_options.iff.foreign_metadata, infilename, &error)
		)) {
			flac__utils_printf(stderr, 1, "%s: ERROR reading foreign metadata: %s\n", encoder_session.inbasefilename, error);
			return EncoderSession_finish_error(&encoder_session);
		}
	}

	/* initialize encoder session with info about the audio (channels/bps/resolution/endianness/etc) */
	switch(options.format) {
		case FORMAT_RAW:
			if(!get_sample_info_raw(&encoder_session, options))
				return EncoderSession_finish_error(&encoder_session);
			break;
		case FORMAT_WAVE:
		case FORMAT_WAVE64:
		case FORMAT_RF64:
			if(!get_sample_info_wave(&encoder_session, options))
				return EncoderSession_finish_error(&encoder_session);
			break;
		case FORMAT_AIFF:
		case FORMAT_AIFF_C:
			if(!get_sample_info_aiff(&encoder_session, options))
				return EncoderSession_finish_error(&encoder_session);
			break;
		case FORMAT_FLAC:
		case FORMAT_OGGFLAC:
			/*
			 * set up FLAC decoder for the input
			 */
			if (0 == (encoder_session.fmt.flac.decoder = FLAC__stream_decoder_new())) {
				flac__utils_printf(stderr, 1, "%s: ERROR: creating decoder for FLAC input\n", encoder_session.inbasefilename);
				return EncoderSession_finish_error(&encoder_session);
			}
			if(!get_sample_info_flac(&encoder_session))
				return EncoderSession_finish_error(&encoder_session);
			break;
		default:
			FLAC__ASSERT(0);
			/* double protection */
			return EncoderSession_finish_error(&encoder_session);
	}

	/* some more checks */
	if(encoder_session.info.channels == 0 || encoder_session.info.channels > FLAC__MAX_CHANNELS) {
		flac__utils_printf(stderr, 1, "%s: ERROR: unsupported number of channels %u\n", encoder_session.inbasefilename, encoder_session.info.channels);
		return EncoderSession_finish_error(&encoder_session);
	}
	if(!FLAC__format_sample_rate_is_valid(encoder_session.info.sample_rate)) {
		flac__utils_printf(stderr, 1, "%s: ERROR: unsupported sample rate %u\n", encoder_session.inbasefilename, encoder_session.info.sample_rate);
		return EncoderSession_finish_error(&encoder_session);
	}
	if(encoder_session.info.bits_per_sample-encoder_session.info.shift < 4 || encoder_session.info.bits_per_sample-encoder_session.info.shift > 24) {
		flac__utils_printf(stderr, 1, "%s: ERROR: unsupported bits-per-sample %u\n", encoder_session.inbasefilename, encoder_session.info.bits_per_sample-encoder_session.info.shift);
		return EncoderSession_finish_error(&encoder_session);
	}
	if(options.sector_align) {
		if(encoder_session.info.channels != 2) {
			flac__utils_printf(stderr, 1, "%s: ERROR: file has %u channels, must be 2 for --sector-align\n", encoder_session.inbasefilename, encoder_session.info.channels);
			return EncoderSession_finish_error(&encoder_session);
		}
		if(encoder_session.info.sample_rate != 44100) {
			flac__utils_printf(stderr, 1, "%s: ERROR: file's sample rate is %u, must be 44100 for --sector-align\n", encoder_session.inbasefilename, encoder_session.info.sample_rate);
			return EncoderSession_finish_error(&encoder_session);
		}
		if(encoder_session.info.bits_per_sample-encoder_session.info.shift != 16) {
			flac__utils_printf(stderr, 1, "%s: ERROR: file has %u bits-per-sample, must be 16 for --sector-align\n", encoder_session.inbasefilename, encoder_session.info.bits_per_sample-encoder_session.info.shift);
			return EncoderSession_finish_error(&encoder_session);
		}
	}

	{
		FLAC__uint64 total_samples_in_input; /* WATCHOUT: may be 0 to mean "unknown" */
		FLAC__uint64 skip;
		FLAC__uint64 until; /* a value of 0 mean end-of-stream (i.e. --until=-0) */
		unsigned align_remainder = 0;

		switch(options.format) {
			case FORMAT_RAW:
				if(infilesize < 0)
					total_samples_in_input = 0;
				else
					total_samples_in_input = (FLAC__uint64)infilesize / encoder_session.info.bytes_per_wide_sample + *options.align_reservoir_samples;
				break;
			case FORMAT_WAVE:
			case FORMAT_WAVE64:
			case FORMAT_RF64:
			case FORMAT_AIFF:
			case FORMAT_AIFF_C:
				/* truncation in the division removes any padding byte that was counted in encoder_session.fmt.iff.data_bytes */
				total_samples_in_input = encoder_session.fmt.iff.data_bytes / encoder_session.info.bytes_per_wide_sample + *options.align_reservoir_samples;
				break;
			case FORMAT_FLAC:
			case FORMAT_OGGFLAC:
				total_samples_in_input = encoder_session.fmt.flac.client_data.metadata_blocks[0]->data.stream_info.total_samples + *options.align_reservoir_samples;
				break;
			default:
				FLAC__ASSERT(0);
				/* double protection */
				return EncoderSession_finish_error(&encoder_session);
		}

		/*
		 * now that we know the sample rate, canonicalize the
		 * --skip string to an absolute sample number:
		 */
		flac__utils_canonicalize_skip_until_specification(&options.skip_specification, encoder_session.info.sample_rate);
		FLAC__ASSERT(options.skip_specification.value.samples >= 0);
		skip = (FLAC__uint64)options.skip_specification.value.samples;
		FLAC__ASSERT(!options.sector_align || (options.format != FORMAT_FLAC && options.format != FORMAT_OGGFLAC && skip == 0));
		/* *options.align_reservoir_samples will be 0 unless --sector-align is used */
		FLAC__ASSERT(options.sector_align || *options.align_reservoir_samples == 0);

		/*
		 * now that we possibly know the input size, canonicalize the
		 * --until string to an absolute sample number:
		 */
		if(!canonicalize_until_specification(&options.until_specification, encoder_session.inbasefilename, encoder_session.info.sample_rate, skip, total_samples_in_input))
			return EncoderSession_finish_error(&encoder_session);
		until = (FLAC__uint64)options.until_specification.value.samples;
		FLAC__ASSERT(!options.sector_align || until == 0);

		/* adjust encoding parameters based on skip and until values */
		switch(options.format) {
			case FORMAT_RAW:
				infilesize -= (FLAC__off_t)skip * encoder_session.info.bytes_per_wide_sample;
				encoder_session.total_samples_to_encode = total_samples_in_input - skip;
				break;
			case FORMAT_WAVE:
			case FORMAT_WAVE64:
			case FORMAT_RF64:
			case FORMAT_AIFF:
			case FORMAT_AIFF_C:
				encoder_session.fmt.iff.data_bytes -= skip * encoder_session.info.bytes_per_wide_sample;
				if(options.ignore_chunk_sizes) {
					encoder_session.total_samples_to_encode = 0;
					FLAC__ASSERT(0 == until);
				}
				else {
					encoder_session.total_samples_to_encode = total_samples_in_input - skip;
				}
				break;
			case FORMAT_FLAC:
			case FORMAT_OGGFLAC:
				encoder_session.total_samples_to_encode = total_samples_in_input - skip;
				break;
			default:
				FLAC__ASSERT(0);
				/* double protection */
				return EncoderSession_finish_error(&encoder_session);
		}
		if(until > 0) {
			const FLAC__uint64 trim = total_samples_in_input - until;
			FLAC__ASSERT(total_samples_in_input > 0);
			FLAC__ASSERT(!options.sector_align);
			if(options.format == FORMAT_RAW)
				infilesize -= (FLAC__off_t)trim * encoder_session.info.bytes_per_wide_sample;
			else if(EncoderSession_format_is_iff(&encoder_session))
				encoder_session.fmt.iff.data_bytes -= trim * encoder_session.info.bytes_per_wide_sample;
			encoder_session.total_samples_to_encode -= trim;
		}
		if(options.sector_align && (options.format != FORMAT_RAW || infilesize >=0)) { /* for RAW, need to know the filesize */
			FLAC__ASSERT(skip == 0); /* asserted above too, but lest we forget */
			align_remainder = (unsigned)(encoder_session.total_samples_to_encode % 588);
			if(options.is_last_file)
				encoder_session.total_samples_to_encode += (588-align_remainder); /* will pad with zeroes */
			else
				encoder_session.total_samples_to_encode -= align_remainder; /* will stop short and carry over to next file */
		}
		switch(options.format) {
			case FORMAT_RAW:
				encoder_session.unencoded_size = encoder_session.total_samples_to_encode * encoder_session.info.bytes_per_wide_sample;
				break;
			case FORMAT_WAVE:
				/* +44 for the size of the WAVE headers; this is just an estimate for the progress indicator and doesn't need to be exact */
				encoder_session.unencoded_size = encoder_session.total_samples_to_encode * encoder_session.info.bytes_per_wide_sample + 44;
				break;
			case FORMAT_WAVE64:
				/* +44 for the size of the WAVE headers; this is just an estimate for the progress indicator and doesn't need to be exact */
				encoder_session.unencoded_size = encoder_session.total_samples_to_encode * encoder_session.info.bytes_per_wide_sample + 104;
				break;
			case FORMAT_RF64:
				/* +72 for the size of the RF64 headers; this is just an estimate for the progress indicator and doesn't need to be exact */
				encoder_session.unencoded_size = encoder_session.total_samples_to_encode * encoder_session.info.bytes_per_wide_sample + 80;
				break;
			case FORMAT_AIFF:
			case FORMAT_AIFF_C:
				/* +54 for the size of the AIFF headers; this is just an estimate for the progress indicator and doesn't need to be exact */
				encoder_session.unencoded_size = encoder_session.total_samples_to_encode * encoder_session.info.bytes_per_wide_sample + 54;
				break;
			case FORMAT_FLAC:
			case FORMAT_OGGFLAC:
				if(infilesize < 0)
					/* if we don't know, use 0 as hint to progress indicator (which is the only place this is used): */
					encoder_session.unencoded_size = 0;
				else if(skip == 0 && until == 0)
					encoder_session.unencoded_size = (FLAC__uint64)infilesize;
				else if(total_samples_in_input)
					encoder_session.unencoded_size = (FLAC__uint64)infilesize * encoder_session.total_samples_to_encode / total_samples_in_input;
				else
					encoder_session.unencoded_size = (FLAC__uint64)infilesize;
				break;
			default:
				FLAC__ASSERT(0);
				/* double protection */
				return EncoderSession_finish_error(&encoder_session);
		}

		if(encoder_session.total_samples_to_encode == 0) {
			encoder_session.unencoded_size = 0;
			flac__utils_printf(stderr, 2, "(No runtime statistics possible; please wait for encoding to finish...)\n");
		}

		if(options.format == FORMAT_FLAC || options.format == FORMAT_OGGFLAC)
			encoder_session.fmt.flac.client_data.samples_left_to_process = encoder_session.total_samples_to_encode;

		stats_new_file();
		/* init the encoder */
		if(!EncoderSession_init_encoder(&encoder_session, options))
			return EncoderSession_finish_error(&encoder_session);

		/* skip over any samples as requested */
		if(skip > 0) {
			switch(options.format) {
				case FORMAT_RAW:
					{
						unsigned skip_bytes = encoder_session.info.bytes_per_wide_sample * (unsigned)skip;
						if(skip_bytes > lookahead_length) {
							skip_bytes -= lookahead_length;
							lookahead_length = 0;
							if(!fskip_ahead(encoder_session.fin, skip_bytes)) {
								flac__utils_printf(stderr, 1, "%s: ERROR during read while skipping samples\n", encoder_session.inbasefilename);
								return EncoderSession_finish_error(&encoder_session);
							}
						}
						else {
							lookahead += skip_bytes;
							lookahead_length -= skip_bytes;
						}
					}
					break;
				case FORMAT_WAVE:
				case FORMAT_WAVE64:
				case FORMAT_RF64:
				case FORMAT_AIFF:
				case FORMAT_AIFF_C:
					if(!fskip_ahead(encoder_session.fin, skip * encoder_session.info.bytes_per_wide_sample)) {
						flac__utils_printf(stderr, 1, "%s: ERROR during read while skipping samples\n", encoder_session.inbasefilename);
						return EncoderSession_finish_error(&encoder_session);
					}
					break;
				case FORMAT_FLAC:
				case FORMAT_OGGFLAC:
					/*
					 * have to wait until the FLAC encoder is set up for writing
					 * before any seeking in the input FLAC file, because the seek
					 * itself will usually call the decoder's write callback, and
					 * our decoder's write callback passes samples to our FLAC
					 * encoder
					 */
					if(!FLAC__stream_decoder_seek_absolute(encoder_session.fmt.flac.decoder, skip)) {
						flac__utils_printf(stderr, 1, "%s: ERROR while skipping samples, FLAC decoder state = %s\n", encoder_session.inbasefilename, FLAC__stream_decoder_get_resolved_state_string(encoder_session.fmt.flac.decoder));
						return EncoderSession_finish_error(&encoder_session);
					}
					break;
				default:
					FLAC__ASSERT(0);
					/* double protection */
					return EncoderSession_finish_error(&encoder_session);
			}
		}

		/*
		 * first do any samples in the reservoir
		 */
		if(options.sector_align && *options.align_reservoir_samples > 0) {
			FLAC__ASSERT(options.format != FORMAT_FLAC && options.format != FORMAT_OGGFLAC); /* check again */
			if(!EncoderSession_process(&encoder_session, (const FLAC__int32 * const *)options.align_reservoir, *options.align_reservoir_samples)) {
				print_error_with_state(&encoder_session, "ERROR during encoding");
				return EncoderSession_finish_error(&encoder_session);
			}
		}

		/*
		 * decrement infilesize or the data_bytes counter if we need to align the file
		 */
		if(options.sector_align) {
			if(options.is_last_file) {
				*options.align_reservoir_samples = 0;
			}
			else {
				*options.align_reservoir_samples = align_remainder;
				if(options.format == FORMAT_RAW) {
					FLAC__ASSERT(infilesize >= 0);
					infilesize -= (FLAC__off_t)((*options.align_reservoir_samples) * encoder_session.info.bytes_per_wide_sample);
					FLAC__ASSERT(infilesize >= 0);
				}
				else if(EncoderSession_format_is_iff(&encoder_session))
					encoder_session.fmt.iff.data_bytes -= (*options.align_reservoir_samples) * encoder_session.info.bytes_per_wide_sample;
			}
		}

		/*
		 * now do samples from the file
		 */
		switch(options.format) {
			case FORMAT_RAW:
				if(infilesize < 0) {
					size_t bytes_read;
					while(!feof(infile)) {
						if(lookahead_length > 0) {
							FLAC__ASSERT(lookahead_length < CHUNK_OF_SAMPLES * encoder_session.info.bytes_per_wide_sample);
							memcpy(ubuffer.u8, lookahead, lookahead_length);
							bytes_read = fread(ubuffer.u8+lookahead_length, sizeof(unsigned char), CHUNK_OF_SAMPLES * encoder_session.info.bytes_per_wide_sample - lookahead_length, infile) + lookahead_length;
							if(ferror(infile)) {
								flac__utils_printf(stderr, 1, "%s: ERROR during read\n", encoder_session.inbasefilename);
								return EncoderSession_finish_error(&encoder_session);
							}
							lookahead_length = 0;
						}
						else
							bytes_read = fread(ubuffer.u8, sizeof(unsigned char), CHUNK_OF_SAMPLES * encoder_session.info.bytes_per_wide_sample, infile);

						if(bytes_read == 0) {
							if(ferror(infile)) {
								flac__utils_printf(stderr, 1, "%s: ERROR during read\n", encoder_session.inbasefilename);
								return EncoderSession_finish_error(&encoder_session);
							}
						}
						else if(bytes_read % encoder_session.info.bytes_per_wide_sample != 0) {
							flac__utils_printf(stderr, 1, "%s: ERROR: got partial sample\n", encoder_session.inbasefilename);
							return EncoderSession_finish_error(&encoder_session);
						}
						else {
							unsigned wide_samples = bytes_read / encoder_session.info.bytes_per_wide_sample;
							if(!format_input(input_, wide_samples, encoder_session.info.is_big_endian, encoder_session.info.is_unsigned_samples, encoder_session.info.channels, encoder_session.info.bits_per_sample, encoder_session.info.shift, channel_map))
								return EncoderSession_finish_error(&encoder_session);

							if(!EncoderSession_process(&encoder_session, (const FLAC__int32 * const *)input_, wide_samples)) {
								print_error_with_state(&encoder_session, "ERROR during encoding");
								return EncoderSession_finish_error(&encoder_session);
							}
						}
					}
				}
				else {
					size_t bytes_read;
					const FLAC__uint64 max_input_bytes = infilesize;
					FLAC__uint64 total_input_bytes_read = 0;
					while(total_input_bytes_read < max_input_bytes) {
						{
							size_t wanted = (CHUNK_OF_SAMPLES * encoder_session.info.bytes_per_wide_sample);
							wanted = (size_t) min((FLAC__uint64)wanted, max_input_bytes - total_input_bytes_read);

							if(lookahead_length > 0) {
								FLAC__ASSERT(lookahead_length <= wanted);
								memcpy(ubuffer.u8, lookahead, lookahead_length);
								wanted -= lookahead_length;
								bytes_read = lookahead_length;
								if(wanted > 0) {
									bytes_read += fread(ubuffer.u8+lookahead_length, sizeof(unsigned char), wanted, infile);
									if(ferror(infile)) {
										flac__utils_printf(stderr, 1, "%s: ERROR during read\n", encoder_session.inbasefilename);
										return EncoderSession_finish_error(&encoder_session);
									}
								}
								lookahead_length = 0;
							}
							else
								bytes_read = fread(ubuffer.u8, sizeof(unsigned char), wanted, infile);
						}

						if(bytes_read == 0) {
							if(ferror(infile)) {
								flac__utils_printf(stderr, 1, "%s: ERROR during read\n", encoder_session.inbasefilename);
								return EncoderSession_finish_error(&encoder_session);
							}
							else if(feof(infile)) {
								flac__utils_printf(stderr, 1, "%s: WARNING: unexpected EOF; expected %" PRIu64 " samples, got %" PRIu64 " samples\n", encoder_session.inbasefilename, encoder_session.total_samples_to_encode, encoder_session.samples_written);
								if(encoder_session.treat_warnings_as_errors)
									return EncoderSession_finish_error(&encoder_session);
								total_input_bytes_read = max_input_bytes;
							}
						}
						else {
							if(bytes_read % encoder_session.info.bytes_per_wide_sample != 0) {
								flac__utils_printf(stderr, 1, "%s: ERROR: got partial sample\n", encoder_session.inbasefilename);
								return EncoderSession_finish_error(&encoder_session);
							}
							else {
								unsigned wide_samples = bytes_read / encoder_session.info.bytes_per_wide_sample;
								if(!format_input(input_, wide_samples, encoder_session.info.is_big_endian, encoder_session.info.is_unsigned_samples, encoder_session.info.channels, encoder_session.info.bits_per_sample, encoder_session.info.shift, channel_map))
									return EncoderSession_finish_error(&encoder_session);

								if(!EncoderSession_process(&encoder_session, (const FLAC__int32 * const *)input_, wide_samples)) {
									print_error_with_state(&encoder_session, "ERROR during encoding");
									return EncoderSession_finish_error(&encoder_session);
								}
								total_input_bytes_read += bytes_read;
							}
						}
					}
				}
				break;
			case FORMAT_WAVE:
			case FORMAT_WAVE64:
			case FORMAT_RF64:
			case FORMAT_AIFF:
			case FORMAT_AIFF_C:
				while(encoder_session.fmt.iff.data_bytes > 0) {
					const size_t bytes_to_read = (size_t)min(
						encoder_session.fmt.iff.data_bytes,
						(FLAC__uint64)CHUNK_OF_SAMPLES * (FLAC__uint64)encoder_session.info.bytes_per_wide_sample
					);
					size_t bytes_read = fread(ubuffer.u8, sizeof(unsigned char), bytes_to_read, infile);
					if(bytes_read == 0) {
						if(ferror(infile)) {
							flac__utils_printf(stderr, 1, "%s: ERROR during read\n", encoder_session.inbasefilename);
							return EncoderSession_finish_error(&encoder_session);
						}
						else if(feof(infile)) {
							if(options.ignore_chunk_sizes) {
								flac__utils_printf(stderr, 1, "%s: INFO: hit EOF with --ignore-chunk-sizes, got %" PRIu64 " samples\n", encoder_session.inbasefilename, encoder_session.samples_written);
							}
							else {
								flac__utils_printf(stderr, 1, "%s: WARNING: unexpected EOF; expected %" PRIu64 " samples, got %" PRIu64 " samples\n", encoder_session.inbasefilename, encoder_session.total_samples_to_encode, encoder_session.samples_written);
								if(encoder_session.treat_warnings_as_errors)
									return EncoderSession_finish_error(&encoder_session);
							}
							encoder_session.fmt.iff.data_bytes = 0;
						}
					}
					else {
						if(bytes_read % encoder_session.info.bytes_per_wide_sample != 0) {
							flac__utils_printf(stderr, 1, "%s: ERROR: got partial sample\n", encoder_session.inbasefilename);
							return EncoderSession_finish_error(&encoder_session);
						}
						else {
							unsigned wide_samples = bytes_read / encoder_session.info.bytes_per_wide_sample;
							if(!format_input(input_, wide_samples, encoder_session.info.is_big_endian, encoder_session.info.is_unsigned_samples, encoder_session.info.channels, encoder_session.info.bits_per_sample, encoder_session.info.shift, channel_map))
								return EncoderSession_finish_error(&encoder_session);

							if(!EncoderSession_process(&encoder_session, (const FLAC__int32 * const *)input_, wide_samples)) {
								print_error_with_state(&encoder_session, "ERROR during encoding");
								return EncoderSession_finish_error(&encoder_session);
							}
							encoder_session.fmt.iff.data_bytes -= bytes_read;
						}
					}
				}
				break;
			case FORMAT_FLAC:
			case FORMAT_OGGFLAC:
				while(!encoder_session.fmt.flac.client_data.fatal_error && encoder_session.fmt.flac.client_data.samples_left_to_process > 0) {
					/* We can also hit the end of stream without samples_left_to_process
					 * going to 0 if there are errors and continue_through_decode_errors
					 * is on, so we want to break in that case too:
					 */
					if(encoder_session.continue_through_decode_errors && FLAC__stream_decoder_get_state(encoder_session.fmt.flac.decoder) == FLAC__STREAM_DECODER_END_OF_STREAM)
						break;
					if(!FLAC__stream_decoder_process_single(encoder_session.fmt.flac.decoder)) {
						flac__utils_printf(stderr, 1, "%s: ERROR: while decoding FLAC input, state = %s\n", encoder_session.inbasefilename, FLAC__stream_decoder_get_resolved_state_string(encoder_session.fmt.flac.decoder));
						return EncoderSession_finish_error(&encoder_session);
					}
				}
				if(encoder_session.fmt.flac.client_data.fatal_error) {
					flac__utils_printf(stderr, 1, "%s: ERROR: while decoding FLAC input, state = %s\n", encoder_session.inbasefilename, FLAC__stream_decoder_get_resolved_state_string(encoder_session.fmt.flac.decoder));
					return EncoderSession_finish_error(&encoder_session);
				}
				break;
			default:
				FLAC__ASSERT(0);
				/* double protection */
				return EncoderSession_finish_error(&encoder_session);
		}

		/*
		 * now read unaligned samples into reservoir or pad with zeroes if necessary
		 */
		if(options.sector_align) {
			if(options.is_last_file) {
				unsigned wide_samples = 588 - align_remainder;
				if(wide_samples < 588) {
					unsigned channel;

					info_align_zero = wide_samples;
					for(channel = 0; channel < encoder_session.info.channels; channel++)
						memset(input_[channel], 0, sizeof(input_[0][0]) * wide_samples);

					if(!EncoderSession_process(&encoder_session, (const FLAC__int32 * const *)input_, wide_samples)) {
						print_error_with_state(&encoder_session, "ERROR during encoding");
						return EncoderSession_finish_error(&encoder_session);
					}
				}
			}
			else {
				if(*options.align_reservoir_samples > 0) {
					size_t bytes_read;
					FLAC__ASSERT(CHUNK_OF_SAMPLES >= 588);
					bytes_read = fread(ubuffer.u8, sizeof(unsigned char), (*options.align_reservoir_samples) * encoder_session.info.bytes_per_wide_sample, infile);
					if(bytes_read == 0 && ferror(infile)) {
						flac__utils_printf(stderr, 1, "%s: ERROR during read\n", encoder_session.inbasefilename);
						return EncoderSession_finish_error(&encoder_session);
					}
					else if(bytes_read != (*options.align_reservoir_samples) * encoder_session.info.bytes_per_wide_sample) {
						flac__utils_printf(stderr, 1, "%s: WARNING: unexpected EOF; read %" PRIu64 " bytes; expected %" PRIu64 " samples, got %" PRIu64 " samples\n", encoder_session.inbasefilename, bytes_read, encoder_session.total_samples_to_encode, encoder_session.samples_written);
						if(encoder_session.treat_warnings_as_errors)
							return EncoderSession_finish_error(&encoder_session);
					}
					else {
						info_align_carry = *options.align_reservoir_samples;
						if(!format_input(options.align_reservoir, *options.align_reservoir_samples, encoder_session.info.is_big_endian, encoder_session.info.is_unsigned_samples, encoder_session.info.channels, encoder_session.info.bits_per_sample, encoder_session.info.shift, channel_map))
							return EncoderSession_finish_error(&encoder_session);
					}
				}
			}
		}
	}

	return EncoderSession_finish_ok(
		&encoder_session,
		info_align_carry,
		info_align_zero,
		EncoderSession_format_is_iff(&encoder_session)? options.format_options.iff.foreign_metadata : 0,
		options.error_on_compression_fail
	);
}

FLAC__bool EncoderSession_construct(EncoderSession *e, encode_options_t options, FLAC__off_t infilesize, FILE *infile, const char *infilename, const char *outfilename, const FLAC__byte *lookahead, unsigned lookahead_length)
{
	unsigned i;
	FLAC__uint32 test = 1;

	/*
	 * initialize globals
	 */

	is_big_endian_host_ = (*((FLAC__byte*)(&test)))? false : true;

	for(i = 0; i < FLAC__MAX_CHANNELS; i++)
		input_[i] = &(in_[i][0]);


	/*
	 * initialize instance
	 */

#if FLAC__HAS_OGG
	e->use_ogg = options.use_ogg;
#endif
	e->verify = options.verify;
	e->treat_warnings_as_errors = options.treat_warnings_as_errors;
	e->continue_through_decode_errors = options.continue_through_decode_errors;

	e->is_stdout = (0 == strcmp(outfilename, "-"));
	e->outputfile_opened = false;

	e->inbasefilename = grabbag__file_get_basename(infilename);
	e->infilename = infilename;
	e->outfilename = outfilename;

	e->total_samples_to_encode = 0;
	e->unencoded_size = 0;
	e->bytes_written = 0;
	e->samples_written = 0;
	e->stats_frames_interval = 0;
	e->old_frames_written = 0;

	memset(&e->info, 0, sizeof(e->info));

	e->format = options.format;

	switch(options.format) {
		case FORMAT_RAW:
			break;
		case FORMAT_WAVE:
		case FORMAT_WAVE64:
		case FORMAT_RF64:
		case FORMAT_AIFF:
		case FORMAT_AIFF_C:
			e->fmt.iff.data_bytes = 0;
			break;
		case FORMAT_FLAC:
		case FORMAT_OGGFLAC:
			e->fmt.flac.decoder = 0;
			e->fmt.flac.client_data.filesize = infilesize;
			e->fmt.flac.client_data.lookahead = lookahead;
			e->fmt.flac.client_data.lookahead_length = lookahead_length;
			e->fmt.flac.client_data.num_metadata_blocks = 0;
			e->fmt.flac.client_data.samples_left_to_process = 0;
			e->fmt.flac.client_data.fatal_error = false;
			break;
		default:
			FLAC__ASSERT(0);
			/* double protection */
			return false;
	}

	e->encoder = 0;

	e->fin = infile;
	e->seek_table_template = 0;

	if(0 == (e->seek_table_template = FLAC__metadata_object_new(FLAC__METADATA_TYPE_SEEKTABLE))) {
		flac__utils_printf(stderr, 1, "%s: ERROR allocating memory for seek table\n", e->inbasefilename);
		return false;
	}

	e->encoder = FLAC__stream_encoder_new();
	if(0 == e->encoder) {
		flac__utils_printf(stderr, 1, "%s: ERROR creating the encoder instance\n", e->inbasefilename);
		EncoderSession_destroy(e);
		return false;
	}

	return true;
}

void EncoderSession_destroy(EncoderSession *e)
{
	if(e->format == FORMAT_FLAC || e->format == FORMAT_OGGFLAC) {
		size_t i;
		if(e->fmt.flac.decoder)
			FLAC__stream_decoder_delete(e->fmt.flac.decoder);
		e->fmt.flac.decoder = 0;
		for(i = 0; i < e->fmt.flac.client_data.num_metadata_blocks; i++)
			FLAC__metadata_object_delete(e->fmt.flac.client_data.metadata_blocks[i]);
		e->fmt.flac.client_data.num_metadata_blocks = 0;
	}

	if(e->fin != stdin)
		fclose(e->fin);

	if(0 != e->encoder) {
		FLAC__stream_encoder_delete(e->encoder);
		e->encoder = 0;
	}

	if(0 != e->seek_table_template) {
		FLAC__metadata_object_delete(e->seek_table_template);
		e->seek_table_template = 0;
	}
}

int EncoderSession_finish_ok(EncoderSession *e, int info_align_carry, int info_align_zero, foreign_metadata_t *foreign_metadata, FLAC__bool error_on_compression_fail)
{
	FLAC__StreamEncoderState fse_state = FLAC__STREAM_ENCODER_OK;
	int ret = 0;
	FLAC__bool verify_error = false;

	if(e->encoder) {
		fse_state = FLAC__stream_encoder_get_state(e->encoder);
		ret = FLAC__stream_encoder_finish(e->encoder)? 0 : 1;
		verify_error =
			fse_state == FLAC__STREAM_ENCODER_VERIFY_MISMATCH_IN_AUDIO_DATA ||
			FLAC__stream_encoder_get_state(e->encoder) == FLAC__STREAM_ENCODER_VERIFY_MISMATCH_IN_AUDIO_DATA
		;
	}
	/* all errors except verify errors should interrupt the stats */
	if(ret && !verify_error)
		print_error_with_state(e, "ERROR during encoding");
	else if(e->total_samples_to_encode > 0) {
		print_stats(e);
		flac__utils_printf(stderr, 2, "\n");
	}

	if(verify_error) {
		print_verify_error(e);
		ret = 1;
	}
	else {
		if(info_align_carry >= 0) {
			flac__utils_printf(stderr, 1, "%s: INFO: sector alignment causing %d samples to be carried over\n", e->inbasefilename, info_align_carry);
		}
		if(info_align_zero >= 0) {
			flac__utils_printf(stderr, 1, "%s: INFO: sector alignment causing %d zero samples to be appended\n", e->inbasefilename, info_align_zero);
		}
	}

	/*@@@@@@ should this go here or somewhere else? */
	if(ret == 0 && foreign_metadata) {
		const char *error;
		if(!flac__foreign_metadata_write_to_flac(foreign_metadata, e->infilename, e->outfilename, &error)) {
			flac__utils_printf(stderr, 1, "%s: ERROR: updating foreign metadata in FLAC file: %s\n", e->inbasefilename, error);
			ret = 1;
		}
	}

	if (e->compression_ratio >= 1.0) {
		flac__utils_printf(stderr, 1,
			"FAILURE: Compression failed (ratio %0.3f, should be < 1.0).\n"
			"This happens for some files for one or more of the following reasons:\n"
			" * Recompressing an existing FLAC from a higher to a lower compression setting.\n"
			" * Insufficient input data  (eg, very short files, < 10000 frames).\n"
			" * The audio data is not compressable (eg a full range white noise signal).\n"
			, e->compression_ratio);
		if (error_on_compression_fail)
			ret = 1;
	}

	EncoderSession_destroy(e);

	return ret;
}

int EncoderSession_finish_error(EncoderSession *e)
{
	FLAC__ASSERT(e->encoder);

	if(e->total_samples_to_encode > 0)
		flac__utils_printf(stderr, 2, "\n");

	if(FLAC__stream_encoder_get_state(e->encoder) == FLAC__STREAM_ENCODER_VERIFY_MISMATCH_IN_AUDIO_DATA)
		print_verify_error(e);
	else if(e->outputfile_opened)
		/* only want to delete the file if we opened it; otherwise it could be an existing file and our overwrite failed */
		flac_unlink(e->outfilename);

	EncoderSession_destroy(e);

	return 1;
}

typedef struct {
	unsigned num_metadata;
	FLAC__bool *needs_delete;
	FLAC__StreamMetadata **metadata;
	FLAC__StreamMetadata *cuesheet; /* always needs to be deleted */
} static_metadata_t;

static void static_metadata_init(static_metadata_t *m)
{
	m->num_metadata = 0;
	m->needs_delete = 0;
	m->metadata = 0;
	m->cuesheet = 0;
}

static void static_metadata_clear(static_metadata_t *m)
{
	unsigned i;
	for(i = 0; i < m->num_metadata; i++)
		if(m->needs_delete[i])
			FLAC__metadata_object_delete(m->metadata[i]);
	if(m->metadata)
		free(m->metadata);
	if(m->needs_delete)
		free(m->needs_delete);
	if(m->cuesheet)
		FLAC__metadata_object_delete(m->cuesheet);
	static_metadata_init(m);
}

static FLAC__bool static_metadata_append(static_metadata_t *m, FLAC__StreamMetadata *d, FLAC__bool needs_delete)
{
	void *x;
	if(0 == (x = safe_realloc_muladd2_(m->metadata, sizeof(*m->metadata), /*times (*/m->num_metadata, /*+*/1/*)*/)))
		return false;
	m->metadata = (FLAC__StreamMetadata**)x;
	if(0 == (x = safe_realloc_muladd2_(m->needs_delete, sizeof(*m->needs_delete), /*times (*/m->num_metadata, /*+*/1/*)*/)))
		return false;
	m->needs_delete = (FLAC__bool*)x;
	m->metadata[m->num_metadata] = d;
	m->needs_delete[m->num_metadata] = needs_delete;
	m->num_metadata++;
	return true;
}

FLAC__bool EncoderSession_init_encoder(EncoderSession *e, encode_options_t options)
{
	const unsigned channels = e->info.channels;
	const unsigned bps = e->info.bits_per_sample - e->info.shift;
	const unsigned sample_rate = e->info.sample_rate;
	FLACDecoderData *flac_decoder_data = (e->format == FORMAT_FLAC || e->format == FORMAT_OGGFLAC)? &e->fmt.flac.client_data : 0;
	FLAC__StreamMetadata padding;
	FLAC__StreamMetadata **metadata = 0;
	static_metadata_t static_metadata;
	unsigned num_metadata = 0, ic;
	FLAC__StreamEncoderInitStatus init_status;
	const FLAC__bool is_cdda = (channels == 1 || channels == 2) && (bps == 16) && (sample_rate == 44100);
	char apodizations[2000];

	FLAC__ASSERT(sizeof(options.pictures)/sizeof(options.pictures[0]) <= 64);

	static_metadata_init(&static_metadata);

	e->replay_gain = options.replay_gain;

	apodizations[0] = '\0';

	if(e->replay_gain) {
		if(channels != 1 && channels != 2) {
			flac__utils_printf(stderr, 1, "%s: ERROR, number of channels (%u) must be 1 or 2 for --replay-gain\n", e->inbasefilename, channels);
			return false;
		}
		if(!grabbag__replaygain_is_valid_sample_frequency(sample_rate)) {
			flac__utils_printf(stderr, 1, "%s: ERROR, invalid sample rate (%u) for --replay-gain\n", e->inbasefilename, sample_rate);
			return false;
		}
		if(options.is_first_file) {
			if(!grabbag__replaygain_init(sample_rate)) {
				flac__utils_printf(stderr, 1, "%s: ERROR initializing ReplayGain stage\n", e->inbasefilename);
				return false;
			}
		}
	}

	if(!parse_cuesheet(&static_metadata.cuesheet, options.cuesheet_filename, e->inbasefilename, sample_rate, is_cdda, e->total_samples_to_encode, e->treat_warnings_as_errors))
		return false;

	if(!convert_to_seek_table_template(options.requested_seek_points, options.num_requested_seek_points, options.cued_seekpoints? static_metadata.cuesheet : 0, e)) {
		flac__utils_printf(stderr, 1, "%s: ERROR allocating memory for seek table\n", e->inbasefilename);
		static_metadata_clear(&static_metadata);
		return false;
	}

	/* build metadata */
	if(flac_decoder_data) {
		/*
		 * we're encoding from FLAC so we will use the FLAC file's
		 * metadata as the basis for the encoded file
		 */
		{
			unsigned i;
			/*
			 * first handle pictures: simple append any --pictures
			 * specified.
			 */
			for(i = 0; i < options.num_pictures; i++) {
				FLAC__StreamMetadata *pic = FLAC__metadata_object_clone(options.pictures[i]);
				if(0 == pic) {
					flac__utils_printf(stderr, 1, "%s: ERROR allocating memory for PICTURE block\n", e->inbasefilename);
					static_metadata_clear(&static_metadata);
					return false;
				}
				flac_decoder_data->metadata_blocks[flac_decoder_data->num_metadata_blocks++] = pic;
			}
		}
		{
			/*
			 * next handle vorbis comment: if any tags were specified
			 * or there is no existing vorbis comment, we create a
			 * new vorbis comment (discarding any existing one); else
			 * we keep the existing one.  also need to make sure to
			 * propagate any channel mask tag.
			 */
			/* @@@ change to append -T values from options.vorbis_comment if input has VC already? */
			size_t i, j;
			FLAC__bool vc_found = false;
			for(i = 0, j = 0; i < flac_decoder_data->num_metadata_blocks; i++) {
				if(flac_decoder_data->metadata_blocks[i]->type == FLAC__METADATA_TYPE_VORBIS_COMMENT)
					vc_found = true;
				if(flac_decoder_data->metadata_blocks[i]->type == FLAC__METADATA_TYPE_VORBIS_COMMENT && options.vorbis_comment->data.vorbis_comment.num_comments > 0) {
					(void) flac__utils_get_channel_mask_tag(flac_decoder_data->metadata_blocks[i], &e->info.channel_mask);
					flac__utils_printf(stderr, 1, "%s: WARNING, replacing tags from input FLAC file with those given on the command-line\n", e->inbasefilename);
					if(e->treat_warnings_as_errors) {
						static_metadata_clear(&static_metadata);
						return false;
					}
					FLAC__metadata_object_delete(flac_decoder_data->metadata_blocks[i]);
					flac_decoder_data->metadata_blocks[i] = 0;
				}
				else
					flac_decoder_data->metadata_blocks[j++] = flac_decoder_data->metadata_blocks[i];
			}
			flac_decoder_data->num_metadata_blocks = j;
			if((!vc_found || options.vorbis_comment->data.vorbis_comment.num_comments > 0) && flac_decoder_data->num_metadata_blocks < sizeof(flac_decoder_data->metadata_blocks)/sizeof(flac_decoder_data->metadata_blocks[0])) {
				/* prepend ours */
				FLAC__StreamMetadata *vc = FLAC__metadata_object_clone(options.vorbis_comment);
				if(0 == vc || (e->info.channel_mask && !flac__utils_set_channel_mask_tag(vc, e->info.channel_mask))) {
					flac__utils_printf(stderr, 1, "%s: ERROR allocating memory for VORBIS_COMMENT block\n", e->inbasefilename);
					static_metadata_clear(&static_metadata);
					return false;
				}
				for(i = flac_decoder_data->num_metadata_blocks; i > 1; i--)
					flac_decoder_data->metadata_blocks[i] = flac_decoder_data->metadata_blocks[i-1];
				flac_decoder_data->metadata_blocks[1] = vc;
				flac_decoder_data->num_metadata_blocks++;
			}
		}
		{
			/*
			 * next handle cuesheet: if --cuesheet was specified, use
			 * it; else if file has existing CUESHEET and cuesheet's
			 * lead-out offset is correct, keep it; else no CUESHEET
			 */
			size_t i, j;
			for(i = 0, j = 0; i < flac_decoder_data->num_metadata_blocks; i++) {
				FLAC__bool existing_cuesheet_is_bad = false;
				/* check if existing cuesheet matches the input audio */
				if(flac_decoder_data->metadata_blocks[i]->type == FLAC__METADATA_TYPE_CUESHEET && 0 == static_metadata.cuesheet) {
					const FLAC__StreamMetadata_CueSheet *cs = &flac_decoder_data->metadata_blocks[i]->data.cue_sheet;
					if(e->total_samples_to_encode == 0) {
						flac__utils_printf(stderr, 1, "%s: WARNING, cuesheet in input FLAC file cannot be kept if input size is not known, dropping it...\n", e->inbasefilename);
						if(e->treat_warnings_as_errors) {
							static_metadata_clear(&static_metadata);
							return false;
						}
						existing_cuesheet_is_bad = true;
					}
					else if(e->total_samples_to_encode != cs->tracks[cs->num_tracks-1].offset) {
						flac__utils_printf(stderr, 1, "%s: WARNING, lead-out offset of cuesheet in input FLAC file does not match input length, dropping existing cuesheet...\n", e->inbasefilename);
						if(e->treat_warnings_as_errors) {
							static_metadata_clear(&static_metadata);
							return false;
						}
						existing_cuesheet_is_bad = true;
					}
				}
				if(flac_decoder_data->metadata_blocks[i]->type == FLAC__METADATA_TYPE_CUESHEET && (existing_cuesheet_is_bad || 0 != static_metadata.cuesheet)) {
					if(0 != static_metadata.cuesheet) {
						flac__utils_printf(stderr, 1, "%s: WARNING, replacing cuesheet in input FLAC file with the one given on the command-line\n", e->inbasefilename);
						if(e->treat_warnings_as_errors) {
							static_metadata_clear(&static_metadata);
							return false;
						}
					}
					FLAC__metadata_object_delete(flac_decoder_data->metadata_blocks[i]);
					flac_decoder_data->metadata_blocks[i] = 0;
				}
				else
					flac_decoder_data->metadata_blocks[j++] = flac_decoder_data->metadata_blocks[i];
			}
			flac_decoder_data->num_metadata_blocks = j;
			if(0 != static_metadata.cuesheet && flac_decoder_data->num_metadata_blocks < sizeof(flac_decoder_data->metadata_blocks)/sizeof(flac_decoder_data->metadata_blocks[0])) {
				/* prepend ours */
				FLAC__StreamMetadata *cs = FLAC__metadata_object_clone(static_metadata.cuesheet);
				if(0 == cs) {
					flac__utils_printf(stderr, 1, "%s: ERROR allocating memory for CUESHEET block\n", e->inbasefilename);
					static_metadata_clear(&static_metadata);
					return false;
				}
				for(i = flac_decoder_data->num_metadata_blocks; i > 1; i--)
					flac_decoder_data->metadata_blocks[i] = flac_decoder_data->metadata_blocks[i-1];
				flac_decoder_data->metadata_blocks[1] = cs;
				flac_decoder_data->num_metadata_blocks++;
			}
		}
		{
			/*
			 * next handle seektable: if -S- was specified, no
			 * SEEKTABLE; else if -S was specified, use it/them;
			 * else if file has existing SEEKTABLE and input size is
			 * preserved (no --skip/--until/etc specified), keep it;
			 * else use default seektable options
			 *
			 * note: meanings of num_requested_seek_points:
			 *  -1 : no -S option given, default to some value
			 *   0 : -S- given (no seektable)
			 *  >0 : one or more -S options given
			 */
			size_t i, j;
			FLAC__bool existing_seektable = false;
			for(i = 0, j = 0; i < flac_decoder_data->num_metadata_blocks; i++) {
				if(flac_decoder_data->metadata_blocks[i]->type == FLAC__METADATA_TYPE_SEEKTABLE)
					existing_seektable = true;
				if(flac_decoder_data->metadata_blocks[i]->type == FLAC__METADATA_TYPE_SEEKTABLE && (e->total_samples_to_encode != flac_decoder_data->metadata_blocks[0]->data.stream_info.total_samples || options.num_requested_seek_points >= 0)) {
					if(options.num_requested_seek_points > 0) {
						flac__utils_printf(stderr, 1, "%s: WARNING, replacing seektable in input FLAC file with the one given on the command-line\n", e->inbasefilename);
						if(e->treat_warnings_as_errors) {
							static_metadata_clear(&static_metadata);
							return false;
						}
					}
					else if(options.num_requested_seek_points == 0)
						; /* no warning, silently delete existing SEEKTABLE since user specified --no-seektable (-S-) */
					else {
						flac__utils_printf(stderr, 1, "%s: WARNING, can't use existing seektable in input FLAC since the input size is changing or unknown, dropping existing SEEKTABLE block...\n", e->inbasefilename);
						if(e->treat_warnings_as_errors) {
							static_metadata_clear(&static_metadata);
							return false;
						}
					}
					FLAC__metadata_object_delete(flac_decoder_data->metadata_blocks[i]);
					flac_decoder_data->metadata_blocks[i] = 0;
					existing_seektable = false;
				}
				else
					flac_decoder_data->metadata_blocks[j++] = flac_decoder_data->metadata_blocks[i];
			}
			flac_decoder_data->num_metadata_blocks = j;
			if((options.num_requested_seek_points > 0 || (options.num_requested_seek_points < 0 && !existing_seektable)) && flac_decoder_data->num_metadata_blocks < sizeof(flac_decoder_data->metadata_blocks)/sizeof(flac_decoder_data->metadata_blocks[0])) {
				/* prepend ours */
				FLAC__StreamMetadata *st = FLAC__metadata_object_clone(e->seek_table_template);
				if(0 == st) {
					flac__utils_printf(stderr, 1, "%s: ERROR allocating memory for SEEKTABLE block\n", e->inbasefilename);
					static_metadata_clear(&static_metadata);
					return false;
				}
				for(i = flac_decoder_data->num_metadata_blocks; i > 1; i--)
					flac_decoder_data->metadata_blocks[i] = flac_decoder_data->metadata_blocks[i-1];
				flac_decoder_data->metadata_blocks[1] = st;
				flac_decoder_data->num_metadata_blocks++;
			}
		}
		{
			/*
			 * finally handle padding: if --no-padding was specified,
			 * then delete all padding; else if -P was specified,
			 * use that instead of existing padding (if any); else
			 * if existing file has padding, move all existing
			 * padding blocks to one padding block at the end; else
			 * use default padding.
			 */
			int p = -1;
			size_t i, j;
			for(i = 0, j = 0; i < flac_decoder_data->num_metadata_blocks; i++) {
				if(flac_decoder_data->metadata_blocks[i]->type == FLAC__METADATA_TYPE_PADDING) {
					if(p < 0)
						p = 0;
					p += flac_decoder_data->metadata_blocks[i]->length;
					FLAC__metadata_object_delete(flac_decoder_data->metadata_blocks[i]);
					flac_decoder_data->metadata_blocks[i] = 0;
				}
				else
					flac_decoder_data->metadata_blocks[j++] = flac_decoder_data->metadata_blocks[i];
			}
			flac_decoder_data->num_metadata_blocks = j;
			if(options.padding > 0)
				p = options.padding;
			if(p < 0)
				p = e->total_samples_to_encode / sample_rate < 20*60? FLAC_ENCODE__DEFAULT_PADDING : FLAC_ENCODE__DEFAULT_PADDING*8;
			if(p > 0)
				p += (e->replay_gain ? GRABBAG__REPLAYGAIN_MAX_TAG_SPACE_REQUIRED : 0);
			if(options.padding != 0) {
				if(p > 0 && flac_decoder_data->num_metadata_blocks < sizeof(flac_decoder_data->metadata_blocks)/sizeof(flac_decoder_data->metadata_blocks[0])) {
					flac_decoder_data->metadata_blocks[flac_decoder_data->num_metadata_blocks] = FLAC__metadata_object_new(FLAC__METADATA_TYPE_PADDING);
					if(0 == flac_decoder_data->metadata_blocks[flac_decoder_data->num_metadata_blocks]) {
						flac__utils_printf(stderr, 1, "%s: ERROR allocating memory for PADDING block\n", e->inbasefilename);
						static_metadata_clear(&static_metadata);
						return false;
					}
					flac_decoder_data->metadata_blocks[flac_decoder_data->num_metadata_blocks]->is_last = false; /* the encoder will set this for us */
					flac_decoder_data->metadata_blocks[flac_decoder_data->num_metadata_blocks]->length = p;
					flac_decoder_data->num_metadata_blocks++;
				}
			}
		}
		metadata = &flac_decoder_data->metadata_blocks[1]; /* don't include STREAMINFO */
		num_metadata = flac_decoder_data->num_metadata_blocks - 1;
	}
	else {
		/*
		 * we're not encoding from FLAC so we will build the metadata
		 * from scratch
		 */
		const foreign_metadata_t *foreign_metadata = EncoderSession_format_is_iff(e)? options.format_options.iff.foreign_metadata : 0;
		unsigned i;

		if(e->seek_table_template->data.seek_table.num_points > 0) {
			e->seek_table_template->is_last = false; /* the encoder will set this for us */
			static_metadata_append(&static_metadata, e->seek_table_template, /*needs_delete=*/false);
		}
		if(0 != static_metadata.cuesheet)
			static_metadata_append(&static_metadata, static_metadata.cuesheet, /*needs_delete=*/false);
		if(e->info.channel_mask) {
			if(!flac__utils_set_channel_mask_tag(options.vorbis_comment, e->info.channel_mask)) {
				flac__utils_printf(stderr, 1, "%s: ERROR adding channel mask tag\n", e->inbasefilename);
				static_metadata_clear(&static_metadata);
				return false;
			}
		}
		static_metadata_append(&static_metadata, options.vorbis_comment, /*needs_delete=*/false);
		for(i = 0; i < options.num_pictures; i++)
			static_metadata_append(&static_metadata, options.pictures[i], /*needs_delete=*/false);
		if(foreign_metadata) {
			for(i = 0; i < foreign_metadata->num_blocks; i++) {
				FLAC__StreamMetadata *p = FLAC__metadata_object_new(FLAC__METADATA_TYPE_PADDING);
				if(!p) {
					flac__utils_printf(stderr, 1, "%s: ERROR: out of memory\n", e->inbasefilename);
					static_metadata_clear(&static_metadata);
					return false;
				}
				static_metadata_append(&static_metadata, p, /*needs_delete=*/true);
				static_metadata.metadata[static_metadata.num_metadata-1]->length = FLAC__STREAM_METADATA_APPLICATION_ID_LEN/8 + foreign_metadata->blocks[i].size;
			}
		}
		if(options.padding != 0) {
			padding.is_last = false; /* the encoder will set this for us */
			padding.type = FLAC__METADATA_TYPE_PADDING;
			padding.length = (unsigned)(options.padding>0? options.padding : (e->total_samples_to_encode / sample_rate < 20*60? FLAC_ENCODE__DEFAULT_PADDING : FLAC_ENCODE__DEFAULT_PADDING*8)) + (e->replay_gain ? GRABBAG__REPLAYGAIN_MAX_TAG_SPACE_REQUIRED : 0);
			static_metadata_append(&static_metadata, &padding, /*needs_delete=*/false);
		}
		metadata = static_metadata.metadata;
		num_metadata = static_metadata.num_metadata;
	}

	/* check for a few things that have not already been checked.  the
	 * FLAC__stream_encoder_init*() will check it but only return
	 * FLAC__STREAM_ENCODER_INIT_STATUS_INVALID_METADATA so we check some
	 * up front to give a better error message.
	 */
	if(!verify_metadata(e, metadata, num_metadata)) {
		static_metadata_clear(&static_metadata);
		return false;
	}

	FLAC__stream_encoder_set_verify(e->encoder, options.verify);
	FLAC__stream_encoder_set_streamable_subset(e->encoder, !options.lax);
	FLAC__stream_encoder_set_channels(e->encoder, channels);
	FLAC__stream_encoder_set_bits_per_sample(e->encoder, bps);
	FLAC__stream_encoder_set_sample_rate(e->encoder, sample_rate);
	for(ic = 0; ic < options.num_compression_settings; ic++) {
		switch(options.compression_settings[ic].type) {
			case CST_BLOCKSIZE:
				FLAC__stream_encoder_set_blocksize(e->encoder, options.compression_settings[ic].value.t_unsigned);
				break;
			case CST_COMPRESSION_LEVEL:
				FLAC__stream_encoder_set_compression_level(e->encoder, options.compression_settings[ic].value.t_unsigned);
				apodizations[0] = '\0';
				break;
			case CST_DO_MID_SIDE:
				FLAC__stream_encoder_set_do_mid_side_stereo(e->encoder, options.compression_settings[ic].value.t_bool);
				break;
			case CST_LOOSE_MID_SIDE:
				FLAC__stream_encoder_set_loose_mid_side_stereo(e->encoder, options.compression_settings[ic].value.t_bool);
				break;
			case CST_APODIZATION:
				if(strlen(apodizations)+strlen(options.compression_settings[ic].value.t_string)+2 >= sizeof(apodizations)) {
					flac__utils_printf(stderr, 1, "%s: ERROR: too many apodization functions requested\n", e->inbasefilename);
					static_metadata_clear(&static_metadata);
					return false;
				}
				else {
					safe_strncat(apodizations, options.compression_settings[ic].value.t_string, sizeof(apodizations));
					safe_strncat(apodizations, ";", sizeof(apodizations));
				}
				break;
			case CST_MAX_LPC_ORDER:
				FLAC__stream_encoder_set_max_lpc_order(e->encoder, options.compression_settings[ic].value.t_unsigned);
				break;
			case CST_QLP_COEFF_PRECISION:
				FLAC__stream_encoder_set_qlp_coeff_precision(e->encoder, options.compression_settings[ic].value.t_unsigned);
				break;
			case CST_DO_QLP_COEFF_PREC_SEARCH:
				FLAC__stream_encoder_set_do_qlp_coeff_prec_search(e->encoder, options.compression_settings[ic].value.t_bool);
				break;
			case CST_DO_ESCAPE_CODING:
				FLAC__stream_encoder_set_do_escape_coding(e->encoder, options.compression_settings[ic].value.t_bool);
				break;
			case CST_DO_EXHAUSTIVE_MODEL_SEARCH:
				FLAC__stream_encoder_set_do_exhaustive_model_search(e->encoder, options.compression_settings[ic].value.t_bool);
				break;
			case CST_MIN_RESIDUAL_PARTITION_ORDER:
				FLAC__stream_encoder_set_min_residual_partition_order(e->encoder, options.compression_settings[ic].value.t_unsigned);
				break;
			case CST_MAX_RESIDUAL_PARTITION_ORDER:
				FLAC__stream_encoder_set_max_residual_partition_order(e->encoder, options.compression_settings[ic].value.t_unsigned);
				break;
			case CST_RICE_PARAMETER_SEARCH_DIST:
				FLAC__stream_encoder_set_rice_parameter_search_dist(e->encoder, options.compression_settings[ic].value.t_unsigned);
				break;
		}
	}
	if(*apodizations)
		FLAC__stream_encoder_set_apodization(e->encoder, apodizations);
	FLAC__stream_encoder_set_total_samples_estimate(e->encoder, e->total_samples_to_encode);
	FLAC__stream_encoder_set_metadata(e->encoder, (num_metadata > 0)? metadata : 0, num_metadata);

	FLAC__stream_encoder_disable_constant_subframes(e->encoder, options.debug.disable_constant_subframes);
	FLAC__stream_encoder_disable_fixed_subframes(e->encoder, options.debug.disable_fixed_subframes);
	FLAC__stream_encoder_disable_verbatim_subframes(e->encoder, options.debug.disable_verbatim_subframes);
	if(!options.debug.do_md5) {
		flac__utils_printf(stderr, 1, "%s: WARNING, MD5 computation disabled, resulting file will not have MD5 sum\n", e->inbasefilename);
		if(e->treat_warnings_as_errors) {
			static_metadata_clear(&static_metadata);
			return false;
		}
		FLAC__stream_encoder_set_do_md5(e->encoder, false);
	}

#if FLAC__HAS_OGG
	if(e->use_ogg) {
		FLAC__stream_encoder_set_ogg_serial_number(e->encoder, options.serial_number);

		init_status = FLAC__stream_encoder_init_ogg_file(e->encoder, e->is_stdout? 0 : e->outfilename, encoder_progress_callback, /*client_data=*/e);
	}
	else
#endif
	{
		init_status = FLAC__stream_encoder_init_file(e->encoder, e->is_stdout? 0 : e->outfilename, encoder_progress_callback, /*client_data=*/e);
	}

	if(init_status != FLAC__STREAM_ENCODER_INIT_STATUS_OK) {
		print_error_with_init_status(e, "ERROR initializing encoder", init_status);
		if(FLAC__stream_encoder_get_state(e->encoder) != FLAC__STREAM_ENCODER_IO_ERROR)
			e->outputfile_opened = true;
		static_metadata_clear(&static_metadata);
		return false;
	}
	else
		e->outputfile_opened = true;

	e->stats_frames_interval =
		(FLAC__stream_encoder_get_do_exhaustive_model_search(e->encoder) && FLAC__stream_encoder_get_do_qlp_coeff_prec_search(e->encoder))? 0x1f :
		(FLAC__stream_encoder_get_do_exhaustive_model_search(e->encoder) || FLAC__stream_encoder_get_do_qlp_coeff_prec_search(e->encoder))? 0x3f :
		0xff;

	static_metadata_clear(&static_metadata);

	return true;
}

FLAC__bool EncoderSession_process(EncoderSession *e, const FLAC__int32 * const buffer[], unsigned samples)
{
	if(e->replay_gain) {
		if(!grabbag__replaygain_analyze(buffer, e->info.channels==2, e->info.bits_per_sample, samples)) {
			flac__utils_printf(stderr, 1, "%s: WARNING, error while calculating ReplayGain\n", e->inbasefilename);
			if(e->treat_warnings_as_errors)
				return false;
		}
	}

	return FLAC__stream_encoder_process(e->encoder, buffer, samples);
}

FLAC__bool EncoderSession_format_is_iff(const EncoderSession *e)
{
	return
		e->format == FORMAT_WAVE ||
		e->format == FORMAT_WAVE64 ||
		e->format == FORMAT_RF64 ||
		e->format == FORMAT_AIFF ||
		e->format == FORMAT_AIFF_C;
}

FLAC__bool convert_to_seek_table_template(const char *requested_seek_points, int num_requested_seek_points, FLAC__StreamMetadata *cuesheet, EncoderSession *e)
{
	const FLAC__bool only_placeholders = e->is_stdout;
	FLAC__bool has_real_points;

	if(num_requested_seek_points == 0 && 0 == cuesheet)
		return true;

	if(num_requested_seek_points < 0) {
#if FLAC__HAS_OGG
		/*@@@@@@ workaround ogg bug: too many seekpoints makes table not fit in one page */
		if(e->use_ogg && e->total_samples_to_encode > 0 && e->total_samples_to_encode / e->info.sample_rate / 10 > 230)
			requested_seek_points = "230x;";
		else
#endif
			requested_seek_points = "10s;";
		num_requested_seek_points = 1;
	}

	if(num_requested_seek_points > 0) {
		if(!grabbag__seektable_convert_specification_to_template(requested_seek_points, only_placeholders, e->total_samples_to_encode, e->info.sample_rate, e->seek_table_template, &has_real_points))
			return false;
	}

	if(0 != cuesheet) {
		unsigned i, j;
		const FLAC__StreamMetadata_CueSheet *cs = &cuesheet->data.cue_sheet;
		for(i = 0; i < cs->num_tracks; i++) {
			const FLAC__StreamMetadata_CueSheet_Track *tr = cs->tracks+i;
			for(j = 0; j < tr->num_indices; j++) {
				if(!FLAC__metadata_object_seektable_template_append_point(e->seek_table_template, tr->offset + tr->indices[j].offset))
					return false;
				has_real_points = true;
			}
		}
		if(has_real_points)
			if(!FLAC__metadata_object_seektable_template_sort(e->seek_table_template, /*compact=*/true))
				return false;
	}

	if(has_real_points) {
		if(e->is_stdout) {
			flac__utils_printf(stderr, 1, "%s: WARNING, cannot write back seekpoints when encoding to stdout\n", e->inbasefilename);
			if(e->treat_warnings_as_errors)
				return false;
		}
	}

	return true;
}

FLAC__bool canonicalize_until_specification(utils__SkipUntilSpecification *spec, const char *inbasefilename, unsigned sample_rate, FLAC__uint64 skip, FLAC__uint64 total_samples_in_input)
{
	/* convert from mm:ss.sss to sample number if necessary */
	flac__utils_canonicalize_skip_until_specification(spec, sample_rate);

	/* special case: if "--until=-0", use the special value '0' to mean "end-of-stream" */
	if(spec->is_relative && spec->value.samples == 0) {
		spec->is_relative = false;
		return true;
	}

	/* in any other case the total samples in the input must be known */
	if(total_samples_in_input == 0) {
		flac__utils_printf(stderr, 1, "%s: ERROR, cannot use --until when input length is unknown\n", inbasefilename);
		return false;
	}

	FLAC__ASSERT(spec->value_is_samples);

	/* convert relative specifications to absolute */
	if(spec->is_relative) {
		if(spec->value.samples <= 0)
			spec->value.samples += (FLAC__int64)total_samples_in_input;
		else
			spec->value.samples += skip;
		spec->is_relative = false;
	}

	/* error check */
	if(spec->value.samples < 0) {
		flac__utils_printf(stderr, 1, "%s: ERROR, --until value is before beginning of input\n", inbasefilename);
		return false;
	}
	if((FLAC__uint64)spec->value.samples <= skip) {
		flac__utils_printf(stderr, 1, "%s: ERROR, --until value is before --skip point\n", inbasefilename);
		return false;
	}
	if((FLAC__uint64)spec->value.samples > total_samples_in_input) {
		flac__utils_printf(stderr, 1, "%s: ERROR, --until value is after end of input\n", inbasefilename);
		return false;
	}

	return true;
}

FLAC__bool verify_metadata(const EncoderSession *e, FLAC__StreamMetadata **metadata, unsigned num_metadata)
{
	FLAC__bool metadata_picture_has_type1 = false;
	FLAC__bool metadata_picture_has_type2 = false;
	unsigned i;

	FLAC__ASSERT(0 != metadata);
	for(i = 0; i < num_metadata; i++) {
		const FLAC__StreamMetadata *m = metadata[i];
		if(m->type == FLAC__METADATA_TYPE_SEEKTABLE) {
			if(!FLAC__format_seektable_is_legal(&m->data.seek_table)) {
				flac__utils_printf(stderr, 1, "%s: ERROR: SEEKTABLE metadata block is invalid\n", e->inbasefilename);
				return false;
			}
		}
		else if(m->type == FLAC__METADATA_TYPE_CUESHEET) {
			if(!FLAC__format_cuesheet_is_legal(&m->data.cue_sheet, m->data.cue_sheet.is_cd, /*violation=*/0)) {
				flac__utils_printf(stderr, 1, "%s: ERROR: CUESHEET metadata block is invalid\n", e->inbasefilename);
				return false;
			}
		}
		else if(m->type == FLAC__METADATA_TYPE_PICTURE) {
			const char *error = 0;
			if(!FLAC__format_picture_is_legal(&m->data.picture, &error)) {
				flac__utils_printf(stderr, 1, "%s: ERROR: PICTURE metadata block is invalid: %s\n", e->inbasefilename, error);
				return false;
			}
			if(m->data.picture.type == FLAC__STREAM_METADATA_PICTURE_TYPE_FILE_ICON_STANDARD) {
				if(metadata_picture_has_type1) {
					flac__utils_printf(stderr, 1, "%s: ERROR: there may only be one picture of type 1 (32x32 icon) in the file\n", e->inbasefilename);
					return false;
				}
				metadata_picture_has_type1 = true;
			}
			else if(m->data.picture.type == FLAC__STREAM_METADATA_PICTURE_TYPE_FILE_ICON) {
				if(metadata_picture_has_type2) {
					flac__utils_printf(stderr, 1, "%s: ERROR: there may only be one picture of type 2 (icon) in the file\n", e->inbasefilename);
					return false;
				}
				metadata_picture_has_type2 = true;
			}
		}
	}

	return true;
}

FLAC__bool format_input(FLAC__int32 *dest[], unsigned wide_samples, FLAC__bool is_big_endian, FLAC__bool is_unsigned_samples, unsigned channels, unsigned bps, unsigned shift, size_t *channel_map)
{
	unsigned wide_sample, sample, channel;
	FLAC__int32 *out[FLAC__MAX_CHANNELS];

	if(0 == channel_map) {
		for(channel = 0; channel < channels; channel++)
			out[channel] = dest[channel];
	}
	else {
		for(channel = 0; channel < channels; channel++)
			out[channel] = dest[channel_map[channel]];
	}

	if(bps == 8) {
		if(is_unsigned_samples) {
			for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
				for(channel = 0; channel < channels; channel++, sample++)
					out[channel][wide_sample] = (FLAC__int32)ubuffer.u8[sample] - 0x80;
		}
		else {
			for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
				for(channel = 0; channel < channels; channel++, sample++)
					out[channel][wide_sample] = (FLAC__int32)ubuffer.s8[sample];
		}
	}
	else if(bps == 16) {
		if(is_big_endian != is_big_endian_host_) {
			unsigned char tmp;
			const unsigned bytes = wide_samples * channels * (bps >> 3);
			unsigned b;
			for(b = 0; b < bytes; b += 2) {
				tmp = ubuffer.u8[b];
				ubuffer.u8[b] = ubuffer.u8[b+1];
				ubuffer.u8[b+1] = tmp;
			}
		}
		if(is_unsigned_samples) {
			for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
				for(channel = 0; channel < channels; channel++, sample++)
					out[channel][wide_sample] = ubuffer.u16[sample] - 0x8000;
		}
		else {
			for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
				for(channel = 0; channel < channels; channel++, sample++)
					out[channel][wide_sample] = ubuffer.s16[sample];
		}
	}
	else if(bps == 24) {
		if(!is_big_endian) {
			unsigned char tmp;
			const unsigned bytes = wide_samples * channels * (bps >> 3);
			unsigned b;
			for(b = 0; b < bytes; b += 3) {
				tmp = ubuffer.u8[b];
				ubuffer.u8[b] = ubuffer.u8[b+2];
				ubuffer.u8[b+2] = tmp;
			}
		}
		if(is_unsigned_samples) {
			unsigned b;
			for(b = sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
				for(channel = 0; channel < channels; channel++, sample++) {
					FLAC__int32 t;
					t  = ubuffer.u8[b++]; t <<= 8;
					t |= ubuffer.u8[b++]; t <<= 8;
					t |= ubuffer.u8[b++];
					t -= 0x800000;
					out[channel][wide_sample] = t;
				}
		}
		else {
			unsigned b;
			for(b = sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
				for(channel = 0; channel < channels; channel++, sample++) {
					FLAC__int32 t;
					t  = ubuffer.s8[b++]; t <<= 8;
					t |= ubuffer.u8[b++]; t <<= 8;
					t |= ubuffer.u8[b++];
					out[channel][wide_sample] = t;
				}
		}
	}
	else {
		FLAC__ASSERT(0);
	}
	if(shift > 0) {
		FLAC__int32 mask = (1<<shift)-1;
		for(wide_sample = 0; wide_sample < wide_samples; wide_sample++)
			for(channel = 0; channel < channels; channel++) {
				if(out[channel][wide_sample] & mask) {
					flac__utils_printf(stderr, 1, "ERROR during read, sample data (channel#%u sample#%u = %d) has non-zero least-significant bits\n  WAVE/AIFF header said the last %u bits are not significant and should be zero.\n", channel, wide_sample, out[channel][wide_sample], shift);
					return false;
				}
				out[channel][wide_sample] >>= shift;
			}
	}
	return true;
}

void encoder_progress_callback(const FLAC__StreamEncoder *encoder, FLAC__uint64 bytes_written, FLAC__uint64 samples_written, unsigned frames_written, unsigned total_frames_estimate, void *client_data)
{
	EncoderSession *e = (EncoderSession*)client_data;

	const FLAC__uint64 uesize = e->unencoded_size;

	e->progress = e->total_samples_to_encode ? (double)samples_written / (double)e->total_samples_to_encode : 0;
	e->compression_ratio = (e->progress && uesize) ? (double)e->bytes_written / ((double)uesize * min(1.0, e->progress)) : 0;

	(void)encoder, (void)total_frames_estimate;

	e->bytes_written = bytes_written;
	e->samples_written = samples_written;

	if(e->total_samples_to_encode > 0 && frames_written - e->old_frames_written > e->stats_frames_interval) {
		print_stats(e);
		e->old_frames_written = frames_written;
	}
}

FLAC__StreamDecoderReadStatus flac_decoder_read_callback(const FLAC__StreamDecoder *decoder, FLAC__byte buffer[], size_t *bytes, void *client_data)
{
	size_t n = 0;
	EncoderSession *e = (EncoderSession*)client_data;
    FLACDecoderData *data = &e->fmt.flac.client_data;

	(void)decoder;

	if (data->fatal_error)
		return FLAC__STREAM_DECODER_READ_STATUS_ABORT;

	/* use up lookahead first */
	if (data->lookahead_length) {
		n = min(data->lookahead_length, *bytes);
		memcpy(buffer, data->lookahead, n);
		buffer += n;
		data->lookahead += n;
		data->lookahead_length -= n;
	}

	/* get the rest from file */
	if (*bytes > n) {
		*bytes = n + fread(buffer, 1, *bytes-n, e->fin);
		if(ferror(e->fin))
			return FLAC__STREAM_DECODER_READ_STATUS_ABORT;
		else if(0 == *bytes)
			return FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
		else
			return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
	}
	else
		return FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
}

FLAC__StreamDecoderSeekStatus flac_decoder_seek_callback(const FLAC__StreamDecoder *decoder, FLAC__uint64 absolute_byte_offset, void *client_data)
{
	EncoderSession *e = (EncoderSession*)client_data;
	(void)decoder;

	if(fseeko(e->fin, (FLAC__off_t)absolute_byte_offset, SEEK_SET) < 0)
		return FLAC__STREAM_DECODER_SEEK_STATUS_ERROR;
	else
		return FLAC__STREAM_DECODER_SEEK_STATUS_OK;
}

FLAC__StreamDecoderTellStatus flac_decoder_tell_callback(const FLAC__StreamDecoder *decoder, FLAC__uint64 *absolute_byte_offset, void *client_data)
{
	EncoderSession *e = (EncoderSession*)client_data;
	FLAC__off_t pos;
	(void)decoder;

	if((pos = ftello(e->fin)) < 0)
		return FLAC__STREAM_DECODER_TELL_STATUS_ERROR;
	else {
		*absolute_byte_offset = (FLAC__uint64)pos;
		return FLAC__STREAM_DECODER_TELL_STATUS_OK;
	}
}

FLAC__StreamDecoderLengthStatus flac_decoder_length_callback(const FLAC__StreamDecoder *decoder, FLAC__uint64 *stream_length, void *client_data)
{
	const EncoderSession *e = (EncoderSession*)client_data;
    const FLACDecoderData *data = &e->fmt.flac.client_data;
	(void)decoder;

	if(data->filesize < 0)
		return FLAC__STREAM_DECODER_LENGTH_STATUS_ERROR;
	else {
		*stream_length = (FLAC__uint64)data->filesize;
		return FLAC__STREAM_DECODER_LENGTH_STATUS_OK;
	}
}

FLAC__bool flac_decoder_eof_callback(const FLAC__StreamDecoder *decoder, void *client_data)
{
	EncoderSession *e = (EncoderSession*)client_data;
	(void)decoder;

	return feof(e->fin)? true : false;
}

FLAC__StreamDecoderWriteStatus flac_decoder_write_callback(const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 * const buffer[], void *client_data)
{
	EncoderSession *e = (EncoderSession*)client_data;
    FLACDecoderData *data = &e->fmt.flac.client_data;
	FLAC__uint64 n = min(data->samples_left_to_process, frame->header.blocksize);
	(void)decoder;

	if(!EncoderSession_process(e, buffer, (unsigned)n)) {
		print_error_with_state(e, "ERROR during encoding");
		data->fatal_error = true;
		return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
	}

	data->samples_left_to_process -= n;
	return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
}

void flac_decoder_metadata_callback(const FLAC__StreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data)
{
	EncoderSession *e = (EncoderSession*)client_data;
    FLACDecoderData *data = &e->fmt.flac.client_data;
	(void)decoder;

	if (data->fatal_error)
		return;

	if (
		data->num_metadata_blocks == sizeof(data->metadata_blocks)/sizeof(data->metadata_blocks[0]) ||
		0 == (data->metadata_blocks[data->num_metadata_blocks] = FLAC__metadata_object_clone(metadata))
	)
		data->fatal_error = true;
	else
		data->num_metadata_blocks++;
}

void flac_decoder_error_callback(const FLAC__StreamDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data)
{
	EncoderSession *e = (EncoderSession*)client_data;
    FLACDecoderData *data = &e->fmt.flac.client_data;
	(void)decoder;

	stats_print_name(1, e->inbasefilename);
	flac__utils_printf(stderr, 1, "ERROR got %s while decoding FLAC input\n", FLAC__StreamDecoderErrorStatusString[status]);
	if(!e->continue_through_decode_errors)
		data->fatal_error = true;
}

FLAC__bool parse_cuesheet(FLAC__StreamMetadata **cuesheet, const char *cuesheet_filename, const char *inbasefilename, unsigned sample_rate, FLAC__bool is_cdda, FLAC__uint64 lead_out_offset, FLAC__bool treat_warnings_as_errors)
{
	FILE *f;
	unsigned last_line_read;
	const char *error_message;

	if(0 == cuesheet_filename)
		return true;

	if(lead_out_offset == 0) {
		flac__utils_printf(stderr, 1, "%s: ERROR cannot import cuesheet when the number of input samples to encode is unknown\n", inbasefilename);
		return false;
	}

	if(0 == (f = flac_fopen(cuesheet_filename, "r"))) {
		flac__utils_printf(stderr, 1, "%s: ERROR opening cuesheet \"%s\" for reading: %s\n", inbasefilename, cuesheet_filename, strerror(errno));
		return false;
	}

	*cuesheet = grabbag__cuesheet_parse(f, &error_message, &last_line_read, sample_rate, is_cdda, lead_out_offset);

	fclose(f);

	if(0 == *cuesheet) {
		flac__utils_printf(stderr, 1, "%s: ERROR parsing cuesheet \"%s\" on line %u: %s\n", inbasefilename, cuesheet_filename, last_line_read, error_message);
		return false;
	}

	if(!FLAC__format_cuesheet_is_legal(&(*cuesheet)->data.cue_sheet, /*check_cd_da_subset=*/false, &error_message)) {
		flac__utils_printf(stderr, 1, "%s: ERROR parsing cuesheet \"%s\": %s\n", inbasefilename, cuesheet_filename, error_message);
		return false;
	}

	/* if we're expecting CDDA, warn about non-compliance */
	if(is_cdda && !FLAC__format_cuesheet_is_legal(&(*cuesheet)->data.cue_sheet, /*check_cd_da_subset=*/true, &error_message)) {
		flac__utils_printf(stderr, 1, "%s: WARNING cuesheet \"%s\" is not audio CD compliant: %s\n", inbasefilename, cuesheet_filename, error_message);
		if(treat_warnings_as_errors)
			return false;
		(*cuesheet)->data.cue_sheet.is_cd = false;
	}

	return true;
}

static void print_stats(const EncoderSession *encoder_session)
{
	if(flac__utils_verbosity_ >= 2) {
		char ratiostr[16];

		FLAC__ASSERT(encoder_session->total_samples_to_encode > 0);

		if (encoder_session->compression_ratio > 0.0)
			flac_snprintf(ratiostr, sizeof(ratiostr), "%0.3f", encoder_session->compression_ratio);
		else
			flac_snprintf(ratiostr, sizeof(ratiostr), "N/A");

		if(encoder_session->samples_written == encoder_session->total_samples_to_encode) {
			stats_print_name(2, encoder_session->inbasefilename);
			stats_print_info(2, "%swrote %" PRIu64 " bytes, ratio=%s",
				encoder_session->verify? "Verify OK, " : "",
				encoder_session->bytes_written,
				ratiostr
			);
		}
		else {
			stats_print_name(2, encoder_session->inbasefilename);
			stats_print_info(2, "%u%% complete, ratio=%s", (unsigned)floor(encoder_session->progress * 100.0 + 0.5), ratiostr);
		}
	}
}

void print_error_with_init_status(const EncoderSession *e, const char *message, FLAC__StreamEncoderInitStatus init_status)
{
	const int ilen = strlen(e->inbasefilename) + 1;
	const char *state_string = "";

	flac__utils_printf(stderr, 1, "\n%s: %s\n", e->inbasefilename, message);

	flac__utils_printf(stderr, 1, "%*s init_status = %s\n", ilen, "", FLAC__StreamEncoderInitStatusString[init_status]);

	if(init_status == FLAC__STREAM_ENCODER_INIT_STATUS_ENCODER_ERROR) {
		state_string = FLAC__stream_encoder_get_resolved_state_string(e->encoder);

		flac__utils_printf(stderr, 1, "%*s state = %s\n", ilen, "", state_string);

		/* print out some more info for some errors: */
		if(0 == strcmp(state_string, FLAC__StreamEncoderStateString[FLAC__STREAM_ENCODER_CLIENT_ERROR])) {
			flac__utils_printf(stderr, 1,
				"\n"
				"An error occurred while writing; the most common cause is that the disk is full.\n"
			);
		}
		else if(0 == strcmp(state_string, FLAC__StreamEncoderStateString[FLAC__STREAM_ENCODER_IO_ERROR])) {
			flac__utils_printf(stderr, 1,
				"\n"
				"An error occurred opening the output file; it is likely that the output\n"
				"directory does not exist or is not writable, the output file already exists and\n"
				"is not writable, or the disk is full.\n"
			);
		}
	}
	else if(init_status == FLAC__STREAM_ENCODER_INIT_STATUS_NOT_STREAMABLE) {
		flac__utils_printf(stderr, 1,
			"\n"
			"The encoding parameters specified do not conform to the FLAC Subset and may not\n"
			"be streamable or playable in hardware devices.  If you really understand the\n"
			"consequences, you can add --lax to the command-line options to encode with\n"
			"these parameters anyway.  See http://xiph.org/flac/format.html#subset\n"
		);
	}
}

void print_error_with_state(const EncoderSession *e, const char *message)
{
	const int ilen = strlen(e->inbasefilename) + 1;
	const char *state_string;

	flac__utils_printf(stderr, 1, "\n%s: %s\n", e->inbasefilename, message);

	state_string = FLAC__stream_encoder_get_resolved_state_string(e->encoder);

	flac__utils_printf(stderr, 1, "%*s state = %s\n", ilen, "", state_string);

	/* print out some more info for some errors: */
	if(0 == strcmp(state_string, FLAC__StreamEncoderStateString[FLAC__STREAM_ENCODER_CLIENT_ERROR])) {
		flac__utils_printf(stderr, 1,
			"\n"
			"An error occurred while writing; the most common cause is that the disk is full.\n"
		);
	}
}

void print_verify_error(EncoderSession *e)
{
	FLAC__uint64 absolute_sample;
	unsigned frame_number;
	unsigned channel;
	unsigned sample;
	FLAC__int32 expected;
	FLAC__int32 got;

	FLAC__stream_encoder_get_verify_decoder_error_stats(e->encoder, &absolute_sample, &frame_number, &channel, &sample, &expected, &got);

	flac__utils_printf(stderr, 1, "%s: ERROR: mismatch in decoded data, verify FAILED!\n", e->inbasefilename);
	flac__utils_printf(stderr, 1, "       Absolute sample=%" PRIu64 ", frame=%u, channel=%u, sample=%u, expected %d, got %d\n", absolute_sample, frame_number, channel, sample, expected, got);
	flac__utils_printf(stderr, 1, "       In all known cases, verify errors are caused by hardware problems,\n");
	flac__utils_printf(stderr, 1, "       usually overclocking or bad RAM.  Delete %s\n", e->outfilename);
	flac__utils_printf(stderr, 1, "       and repeat the flac command exactly as before.  If it does not give a\n");
	flac__utils_printf(stderr, 1, "       verify error in the exact same place each time you try it, then there is\n");
	flac__utils_printf(stderr, 1, "       a problem with your hardware; please see the FAQ:\n");
	flac__utils_printf(stderr, 1, "           http://xiph.org/flac/faq.html#tools__hardware_prob\n");
	flac__utils_printf(stderr, 1, "       If it does fail in the exact same place every time, keep\n");
	flac__utils_printf(stderr, 1, "       %s and submit a bug report to:\n", e->outfilename);
	flac__utils_printf(stderr, 1, "           https://sourceforge.net/p/flac/bugs/\n");
	flac__utils_printf(stderr, 1, "       Make sure to upload the FLAC file and use the \"Monitor\" feature to\n");
	flac__utils_printf(stderr, 1, "       monitor the bug status.\n");
	flac__utils_printf(stderr, 1, "Verify FAILED!  Do not trust %s\n", e->outfilename);
}

FLAC__bool read_bytes(FILE *f, FLAC__byte *buf, size_t n, FLAC__bool eof_ok, const char *fn)
{
	size_t bytes_read = fread(buf, 1, n, f);

	if(bytes_read == 0) {
		if(!eof_ok) {
			flac__utils_printf(stderr, 1, "%s: ERROR: unexpected EOF\n", fn);
			return false;
		}
		else
			return true;
	}
	if(bytes_read < n) {
		flac__utils_printf(stderr, 1, "%s: ERROR: unexpected EOF\n", fn);
		return false;
	}
	return true;
}

FLAC__bool read_uint16(FILE *f, FLAC__bool big_endian, FLAC__uint16 *val, const char *fn)
{
	if(!read_bytes(f, (FLAC__byte*)val, 2, /*eof_ok=*/false, fn))
		return false;
	if(is_big_endian_host_ != big_endian) {
		FLAC__byte tmp, *b = (FLAC__byte*)val;
		tmp = b[1]; b[1] = b[0]; b[0] = tmp;
	}
	return true;
}

FLAC__bool read_uint32(FILE *f, FLAC__bool big_endian, FLAC__uint32 *val, const char *fn)
{
	if(!read_bytes(f, (FLAC__byte*)val, 4, /*eof_ok=*/false, fn))
		return false;
	if(is_big_endian_host_ != big_endian) {
		FLAC__byte tmp, *b = (FLAC__byte*)val;
		tmp = b[3]; b[3] = b[0]; b[0] = tmp;
		tmp = b[2]; b[2] = b[1]; b[1] = tmp;
	}
	return true;
}

FLAC__bool read_uint64(FILE *f, FLAC__bool big_endian, FLAC__uint64 *val, const char *fn)
{
	if(!read_bytes(f, (FLAC__byte*)val, 8, /*eof_ok=*/false, fn))
		return false;
	if(is_big_endian_host_ != big_endian) {
		FLAC__byte tmp, *b = (FLAC__byte*)val;
		tmp = b[7]; b[7] = b[0]; b[0] = tmp;
		tmp = b[6]; b[6] = b[1]; b[1] = tmp;
		tmp = b[5]; b[5] = b[2]; b[2] = tmp;
		tmp = b[4]; b[4] = b[3]; b[3] = tmp;
	}
	return true;
}

FLAC__bool read_sane_extended(FILE *f, FLAC__uint32 *val, const char *fn)
	/* Read an IEEE 754 80-bit (aka SANE) extended floating point value from 'f',
	 * convert it into an integral value and store in 'val'.  Return false if only
	 * between 1 and 9 bytes remain in 'f', if 0 bytes remain in 'f', or if the
	 * value is negative, between zero and one, or too large to be represented by
	 * 'val'; return true otherwise.
	 */
{
	unsigned int i;
	FLAC__byte buf[10];
	FLAC__uint64 p = 0;
	FLAC__int16 e;
	FLAC__int16 shift;

	if(!read_bytes(f, buf, sizeof(buf), /*eof_ok=*/false, fn))
		return false;
	e = ((FLAC__uint16)(buf[0])<<8 | (FLAC__uint16)(buf[1]))-0x3FFF;
	shift = 63-e;
	if((buf[0]>>7)==1U || e<0 || e>63) {
		flac__utils_printf(stderr, 1, "%s: ERROR: invalid floating-point value\n", fn);
		return false;
	}

	for(i = 0; i < 8; ++i)
		p |= (FLAC__uint64)(buf[i+2])<<(56U-i*8);
	*val = (FLAC__uint32)((p>>shift)+(p>>(shift-1) & 0x1));

	return true;
}

FLAC__bool fskip_ahead(FILE *f, FLAC__uint64 offset)
{
	static unsigned char dump[8192];
	struct flac_stat_s stb;

	if(flac_fstat(fileno(f), &stb) == 0 && (stb.st_mode & S_IFMT) == S_IFREG)
	{
		if(fseeko(f, offset, SEEK_CUR) == 0)
			return true;
	}
	while(offset > 0) {
		const long need = (long)min(offset, sizeof(dump));
		if((long)fread(dump, 1, need, f) < need)
			return false;
		offset -= need;
	}
	return true;
}

unsigned count_channel_mask_bits(FLAC__uint32 mask)
{
	unsigned count = 0;
	while(mask) {
		if(mask & 1)
			count++;
		mask >>= 1;
	}
	return count;
}

#if 0
FLAC__uint32 limit_channel_mask(FLAC__uint32 mask, unsigned channels)
{
	FLAC__uint32 x = 0x80000000;
	unsigned count = count_channel_mask_bits(mask);
	while(x && count > channels) {
		if(mask & x) {
			mask &= ~x;
			count--;
		}
		x >>= 1;
	}
	FLAC__ASSERT(count_channel_mask_bits(mask) == channels);
	return mask;
}
#endif
