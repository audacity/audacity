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
#include <math.h> /* for floor() */
#include <stdio.h> /* for FILE etc. */
#include <string.h> /* for strcmp(), strerror() */
#include "FLAC/all.h"
#include "share/grabbag.h"
#include "share/replaygain_synthesis.h"
#include "share/compat.h"
#include "decode.h"

typedef struct {
#if FLAC__HAS_OGG
	FLAC__bool is_ogg;
	FLAC__bool use_first_serial_number;
	long serial_number;
#endif

	FileFormat format;
	FLAC__bool treat_warnings_as_errors;
	FLAC__bool continue_through_decode_errors;
	FLAC__bool channel_map_none;

	struct {
		replaygain_synthesis_spec_t spec;
		FLAC__bool apply; /* 'spec.apply' is just a request; this 'apply' means we actually parsed the RG tags and are ready to go */
		double scale;
		DitherContext dither_context;
	} replaygain;

	FLAC__bool test_only;
	FLAC__bool analysis_mode;
	analysis_options aopts;
	utils__SkipUntilSpecification *skip_specification;
	utils__SkipUntilSpecification *until_specification; /* a canonicalized value of 0 mean end-of-stream (i.e. --until=-0) */
	utils__CueSpecification *cue_specification;

	const char *inbasefilename;
	const char *infilename;
	const char *outfilename;

	FLAC__uint64 samples_processed;
	unsigned frame_counter;
	FLAC__bool abort_flag;
	FLAC__bool aborting_due_to_until; /* true if we intentionally abort decoding prematurely because we hit the --until point */
	FLAC__bool aborting_due_to_unparseable; /* true if we abort decoding because we hit an unparseable frame */
	FLAC__bool error_callback_suppress_messages; /* turn on to prevent repeating messages from the error callback */

	FLAC__bool iff_headers_need_fixup;

	FLAC__bool is_big_endian;
	FLAC__bool is_unsigned_samples;
	FLAC__bool got_stream_info;
	FLAC__bool has_md5sum;
	FLAC__uint64 total_samples;
	unsigned bps;
	unsigned channels;
	unsigned sample_rate;
	FLAC__uint32 channel_mask;

	/* these are used only in analyze mode */
	FLAC__uint64 decode_position;

	FLAC__StreamDecoder *decoder;

	FILE *fout;

	foreign_metadata_t *foreign_metadata; /* NULL unless --keep-foreign-metadata requested */
	FLAC__off_t fm_offset1, fm_offset2, fm_offset3;
} DecoderSession;


static FLAC__bool is_big_endian_host_;


/*
 * local routines
 */
static FLAC__bool DecoderSession_construct(DecoderSession *d, FLAC__bool is_ogg, FLAC__bool use_first_serial_number, long serial_number, FileFormat format, FLAC__bool treat_warnings_as_errors, FLAC__bool continue_through_decode_errors, FLAC__bool channel_map_none, replaygain_synthesis_spec_t replaygain_synthesis_spec, FLAC__bool analysis_mode, analysis_options aopts, utils__SkipUntilSpecification *skip_specification, utils__SkipUntilSpecification *until_specification, utils__CueSpecification *cue_specification, foreign_metadata_t *foreign_metadata, const char *infilename, const char *outfilename);
static void DecoderSession_destroy(DecoderSession *d, FLAC__bool error_occurred);
static FLAC__bool DecoderSession_init_decoder(DecoderSession *d, const char *infilename);
static FLAC__bool DecoderSession_process(DecoderSession *d);
static int DecoderSession_finish_ok(DecoderSession *d);
static int DecoderSession_finish_error(DecoderSession *d);
static FLAC__bool canonicalize_until_specification(utils__SkipUntilSpecification *spec, const char *inbasefilename, unsigned sample_rate, FLAC__uint64 skip, FLAC__uint64 total_samples_in_input);
static FLAC__bool write_iff_headers(FILE *f, DecoderSession *decoder_session, FLAC__uint64 samples);
static FLAC__bool write_riff_wave_fmt_chunk_body(FILE *f, FLAC__bool is_waveformatextensible, unsigned bps, unsigned channels, unsigned sample_rate, FLAC__uint32 channel_mask);
static FLAC__bool write_aiff_form_comm_chunk(FILE *f, FLAC__uint64 samples, unsigned bps, unsigned channels, unsigned sample_rate);
static FLAC__bool write_little_endian_uint16(FILE *f, FLAC__uint16 val);
static FLAC__bool write_little_endian_uint32(FILE *f, FLAC__uint32 val);
static FLAC__bool write_little_endian_uint64(FILE *f, FLAC__uint64 val);
static FLAC__bool write_big_endian_uint16(FILE *f, FLAC__uint16 val);
static FLAC__bool write_big_endian_uint32(FILE *f, FLAC__uint32 val);
static FLAC__bool write_sane_extended(FILE *f, unsigned val);
static FLAC__bool fixup_iff_headers(DecoderSession *d);
static FLAC__StreamDecoderWriteStatus write_callback(const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 * const buffer[], void *client_data);
static void metadata_callback(const FLAC__StreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data);
static void error_callback(const FLAC__StreamDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data);
static void print_error_with_init_status(const DecoderSession *d, const char *message, FLAC__StreamDecoderInitStatus init_status);
static void print_error_with_state(const DecoderSession *d, const char *message);
static void print_stats(const DecoderSession *decoder_session);


/*
 * public routines
 */
int flac__decode_file(const char *infilename, const char *outfilename, FLAC__bool analysis_mode, analysis_options aopts, decode_options_t options)
{
	DecoderSession decoder_session;

	FLAC__ASSERT(
		options.format == FORMAT_WAVE ||
		options.format == FORMAT_WAVE64 ||
		options.format == FORMAT_RF64 ||
		options.format == FORMAT_AIFF ||
		options.format == FORMAT_AIFF_C ||
		options.format == FORMAT_RAW
	);

	if(options.format == FORMAT_RAW) {
		decoder_session.is_big_endian = options.format_options.raw.is_big_endian;
		decoder_session.is_unsigned_samples = options.format_options.raw.is_unsigned_samples;
	}

	if(!
		DecoderSession_construct(
			&decoder_session,
#if FLAC__HAS_OGG
			options.is_ogg,
			options.use_first_serial_number,
			options.serial_number,
#else
			/*is_ogg=*/false,
			/*use_first_serial_number=*/false,
			/*serial_number=*/0,
#endif
			options.format,
			options.treat_warnings_as_errors,
			options.continue_through_decode_errors,
			options.channel_map_none,
			options.replaygain_synthesis_spec,
			analysis_mode,
			aopts,
			&options.skip_specification,
			&options.until_specification,
			options.has_cue_specification? &options.cue_specification : 0,
			options.format == FORMAT_RAW? NULL : options.format_options.iff.foreign_metadata,
			infilename,
			outfilename
		)
	)
		return 1;

	stats_new_file();
	if(!DecoderSession_init_decoder(&decoder_session, infilename))
		return DecoderSession_finish_error(&decoder_session);

	if(!DecoderSession_process(&decoder_session))
		return DecoderSession_finish_error(&decoder_session);

	return DecoderSession_finish_ok(&decoder_session);
}

FLAC__bool DecoderSession_construct(DecoderSession *d, FLAC__bool is_ogg, FLAC__bool use_first_serial_number, long serial_number, FileFormat format, FLAC__bool treat_warnings_as_errors, FLAC__bool continue_through_decode_errors, FLAC__bool channel_map_none, replaygain_synthesis_spec_t replaygain_synthesis_spec, FLAC__bool analysis_mode, analysis_options aopts, utils__SkipUntilSpecification *skip_specification, utils__SkipUntilSpecification *until_specification, utils__CueSpecification *cue_specification, foreign_metadata_t *foreign_metadata, const char *infilename, const char *outfilename)
{
#if FLAC__HAS_OGG
	d->is_ogg = is_ogg;
	d->use_first_serial_number = use_first_serial_number;
	d->serial_number = serial_number;
#else
	(void)is_ogg;
	(void)use_first_serial_number;
	(void)serial_number;
#endif

	d->format = format;
	d->treat_warnings_as_errors = treat_warnings_as_errors;
	d->continue_through_decode_errors = continue_through_decode_errors;
	d->channel_map_none = channel_map_none;
	d->replaygain.spec = replaygain_synthesis_spec;
	d->replaygain.apply = false;
	d->replaygain.scale = 0.0;
	/* d->replaygain.dither_context gets initialized later once we know the sample resolution */
	d->test_only = (0 == outfilename);
	d->analysis_mode = analysis_mode;
	d->aopts = aopts;
	d->skip_specification = skip_specification;
	d->until_specification = until_specification;
	d->cue_specification = cue_specification;

	d->inbasefilename = grabbag__file_get_basename(infilename);
	d->infilename = infilename;
	d->outfilename = outfilename;

	d->samples_processed = 0;
	d->frame_counter = 0;
	d->abort_flag = false;
	d->aborting_due_to_until = false;
	d->aborting_due_to_unparseable = false;
	d->error_callback_suppress_messages = false;

	d->iff_headers_need_fixup = false;

	d->total_samples = 0;
	d->got_stream_info = false;
	d->has_md5sum = false;
	d->bps = 0;
	d->channels = 0;
	d->sample_rate = 0;
	d->channel_mask = 0;

	d->decode_position = 0;

	d->decoder = 0;

	d->fout = 0; /* initialized with an open file later if necessary */

	d->foreign_metadata = foreign_metadata;

	FLAC__ASSERT(!(d->test_only && d->analysis_mode));

	if(!d->test_only) {
		if(0 == strcmp(outfilename, "-")) {
			d->fout = grabbag__file_get_binary_stdout();
		}
		else {
			if(0 == (d->fout = flac_fopen(outfilename, "wb"))) {
				flac__utils_printf(stderr, 1, "%s: ERROR: can't open output file %s: %s\n", d->inbasefilename, outfilename, strerror(errno));
				DecoderSession_destroy(d, /*error_occurred=*/true);
				return false;
			}
		}
	}

	if(analysis_mode)
		flac__analyze_init(aopts);

	return true;
}

void DecoderSession_destroy(DecoderSession *d, FLAC__bool error_occurred)
{
	if(0 != d->fout && d->fout != stdout) {
#ifdef _WIN32
		if(!error_occurred) {
			FLAC__off_t written_size = ftello(d->fout);
			if(written_size > 0) {
				HANDLE fh = CreateFile_utf8(d->outfilename, GENERIC_READ|GENERIC_WRITE, FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
				if(fh != INVALID_HANDLE_VALUE) {
					if(GetFileType(fh) == FILE_TYPE_DISK) {
						LARGE_INTEGER size;
						size.QuadPart = written_size;
						if(SetFilePointerEx(fh, size, NULL, FILE_CURRENT)) /* correct the file size */
							SetEndOfFile(fh); 
					}
					CloseHandle(fh);
				}
			}
		}
#endif
		fclose(d->fout);
		if(error_occurred)
			flac_unlink(d->outfilename);
	}
}

FLAC__bool DecoderSession_init_decoder(DecoderSession *decoder_session, const char *infilename)
{
	FLAC__StreamDecoderInitStatus init_status;
	FLAC__uint32 test = 1;

	is_big_endian_host_ = (*((FLAC__byte*)(&test)))? false : true;

	if(!decoder_session->analysis_mode && !decoder_session->test_only && decoder_session->foreign_metadata) {
		const char *error;
		if(!flac__foreign_metadata_read_from_flac(decoder_session->foreign_metadata, infilename, &error)) {
			flac__utils_printf(stderr, 1, "%s: ERROR reading foreign metadata: %s\n", decoder_session->inbasefilename, error);
			return false;
		}
	}

	decoder_session->decoder = FLAC__stream_decoder_new();

	if(0 == decoder_session->decoder) {
		flac__utils_printf(stderr, 1, "%s: ERROR creating the decoder instance\n", decoder_session->inbasefilename);
		return false;
	}

	FLAC__stream_decoder_set_md5_checking(decoder_session->decoder, true);
	if (0 != decoder_session->cue_specification)
		FLAC__stream_decoder_set_metadata_respond(decoder_session->decoder, FLAC__METADATA_TYPE_CUESHEET);
	if (decoder_session->replaygain.spec.apply || !decoder_session->channel_map_none)
		FLAC__stream_decoder_set_metadata_respond(decoder_session->decoder, FLAC__METADATA_TYPE_VORBIS_COMMENT);

#if FLAC__HAS_OGG
	if(decoder_session->is_ogg) {
		if(!decoder_session->use_first_serial_number)
			FLAC__stream_decoder_set_ogg_serial_number(decoder_session->decoder, decoder_session->serial_number);
		init_status = FLAC__stream_decoder_init_ogg_file(decoder_session->decoder, strcmp(infilename, "-")? infilename : 0, write_callback, metadata_callback, error_callback, /*client_data=*/decoder_session);
	}
	else
#endif
	{
		init_status = FLAC__stream_decoder_init_file(decoder_session->decoder, strcmp(infilename, "-")? infilename : 0, write_callback, metadata_callback, error_callback, /*client_data=*/decoder_session);
	}

	if(init_status != FLAC__STREAM_DECODER_INIT_STATUS_OK) {
		print_error_with_init_status(decoder_session, "ERROR initializing decoder", init_status);
		return false;
	}

	return true;
}

FLAC__bool DecoderSession_process(DecoderSession *d)
{
	if(!FLAC__stream_decoder_process_until_end_of_metadata(d->decoder)) {
		flac__utils_printf(stderr, 2, "\n");
		print_error_with_state(d, "ERROR while decoding metadata");
		return false;
	}
	if(FLAC__stream_decoder_get_state(d->decoder) > FLAC__STREAM_DECODER_END_OF_STREAM) {
		flac__utils_printf(stderr, 2, "\n");
		print_error_with_state(d, "ERROR during metadata decoding");
		if(!d->continue_through_decode_errors)
			return false;
	}

	if(d->abort_flag)
		return false;

	/* set channel mapping */
	/* currently FLAC order matches SMPTE/WAVEFORMATEXTENSIBLE order, so no reordering is necessary; see encode.c */
	/* only the channel mask must be set if it was not already picked up from the WAVEFORMATEXTENSIBLE_CHANNEL_MASK tag */
	if(!d->channel_map_none && d->channel_mask == 0) {
		if(d->channels == 1) {
			d->channel_mask = 0x0004;
		}
		else if(d->channels == 2) {
			d->channel_mask = 0x0003;
		}
		else if(d->channels == 3) {
			d->channel_mask = 0x0007;
		}
		else if(d->channels == 4) {
			d->channel_mask = 0x0033;
		}
		else if(d->channels == 5) {
			d->channel_mask = 0x0607;
		}
		else if(d->channels == 6) {
			d->channel_mask = 0x060f;
		}
		else if(d->channels == 7) {
			d->channel_mask = 0x070f;
		}
		else if(d->channels == 8) {
			d->channel_mask = 0x063f;
		}
	}

#ifdef _WIN32
	if(!d->analysis_mode && !d->test_only && d->total_samples > 0 && d->fout != stdout) {
		HANDLE fh = CreateFile_utf8(d->outfilename, GENERIC_READ|GENERIC_WRITE, FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
		if(fh != INVALID_HANDLE_VALUE) {
			if (GetFileType(fh) == FILE_TYPE_DISK) {
				LARGE_INTEGER size;
				size.QuadPart = d->total_samples * d->channels * ((d->bps+7)/8);
				if(d->format != FORMAT_RAW) {
					size.QuadPart += 512;
					if(d->foreign_metadata) {
						size_t i;
						for(i = d->format==FORMAT_RF64?2:1; i < d->foreign_metadata->num_blocks; i++) {
							if(i != d->foreign_metadata->format_block && i != d->foreign_metadata->audio_block)
								size.QuadPart += d->foreign_metadata->blocks[i].size;
						}
					}
				}

				if(SetFilePointerEx(fh, size, NULL, FILE_CURRENT)) /* tell filesystem the expected filesize to eliminate fragmentation */
					SetEndOfFile(fh); 
			}
			CloseHandle(fh);
		}
	}
#endif

	/* write the WAVE/AIFF headers if necessary */
	if(!d->analysis_mode && !d->test_only && d->format != FORMAT_RAW) {
		if(!write_iff_headers(d->fout, d, d->total_samples)) {
			d->abort_flag = true;
			return false;
		}
	}

	if(d->skip_specification->value.samples > 0) {
		const FLAC__uint64 skip = (FLAC__uint64)d->skip_specification->value.samples;

		if(!FLAC__stream_decoder_seek_absolute(d->decoder, skip)) {
			print_error_with_state(d, "ERROR seeking while skipping bytes");
			return false;
		}
	}
	if(!FLAC__stream_decoder_process_until_end_of_stream(d->decoder) && !d->aborting_due_to_until) {
		flac__utils_printf(stderr, 2, "\n");
		print_error_with_state(d, "ERROR while decoding data");
		if(!d->continue_through_decode_errors)
			return false;
	}
	if(
		(d->abort_flag && !(d->aborting_due_to_until || d->continue_through_decode_errors)) ||
		(FLAC__stream_decoder_get_state(d->decoder) > FLAC__STREAM_DECODER_END_OF_STREAM && !d->aborting_due_to_until)
	) {
		flac__utils_printf(stderr, 2, "\n");
		print_error_with_state(d, "ERROR during decoding");
		return false;
	}

	/* write padding bytes for alignment if necessary */
	if(!d->analysis_mode && !d->test_only && d->format != FORMAT_RAW) {
		const FLAC__uint64 data_size = d->total_samples * d->channels * ((d->bps+7)/8);
		unsigned padding;
		if(d->format != FORMAT_WAVE64) {
			padding = (unsigned)(data_size & 1);
		}
		else {
			/* 8-byte alignment for Wave64 */
			padding = (8 - (unsigned)(data_size & 7)) & 7;
		}
		for( ; padding > 0; --padding) {
			if(flac__utils_fwrite("\000", 1, 1, d->fout) != 1) {
				print_error_with_state(
					d,
					d->format == FORMAT_WAVE?   "ERROR writing pad byte to WAVE data chunk" :
					d->format == FORMAT_WAVE64? "ERROR writing pad bytes to WAVE64 data chunk" :
					d->format == FORMAT_RF64?   "ERROR writing pad byte to RF64 data chunk" :
					"ERROR writing pad byte to AIFF SSND chunk"
				);
				return false;
			}
		}
	}

	return true;
}

int DecoderSession_finish_ok(DecoderSession *d)
{
	FLAC__bool ok = true, md5_failure = false;

	if(d->decoder) {
		md5_failure = !FLAC__stream_decoder_finish(d->decoder) && !d->aborting_due_to_until;
		print_stats(d);
		FLAC__stream_decoder_delete(d->decoder);
	}
	if(d->analysis_mode)
		flac__analyze_finish(d->aopts);
	if(md5_failure) {
		stats_print_name(1, d->inbasefilename);
		flac__utils_printf(stderr, 1, "ERROR, MD5 signature mismatch\n");
		ok = d->continue_through_decode_errors;
	}
	else {
		if(!d->got_stream_info) {
			stats_print_name(1, d->inbasefilename);
			flac__utils_printf(stderr, 1, "WARNING, cannot check MD5 signature since there was no STREAMINFO\n");
			ok = !d->treat_warnings_as_errors;
		}
		else if(!d->has_md5sum) {
			stats_print_name(1, d->inbasefilename);
			flac__utils_printf(stderr, 1, "WARNING, cannot check MD5 signature since it was unset in the STREAMINFO\n");
			ok = !d->treat_warnings_as_errors;
		}
		stats_print_name(2, d->inbasefilename);
		flac__utils_printf(stderr, 2, "%s         \n", d->test_only? "ok           ":d->analysis_mode?"done           ":"done");
	}
	DecoderSession_destroy(d, /*error_occurred=*/!ok);
	if(!d->analysis_mode && !d->test_only && d->format != FORMAT_RAW) {
		if(d->iff_headers_need_fixup || (!d->got_stream_info && strcmp(d->outfilename, "-"))) {
			if(!fixup_iff_headers(d))
				return 1;
		}
		if(d->foreign_metadata) {
			const char *error;
			if(!flac__foreign_metadata_write_to_iff(d->foreign_metadata, d->infilename, d->outfilename, d->fm_offset1, d->fm_offset2, d->fm_offset3, &error)) {
				flac__utils_printf(stderr, 1, "ERROR updating foreign metadata from %s to %s: %s\n", d->infilename, d->outfilename, error);
				return 1;
			}
		}
	}
	return ok? 0 : 1;
}

int DecoderSession_finish_error(DecoderSession *d)
{
	if(d->decoder) {
		(void)FLAC__stream_decoder_finish(d->decoder);
		FLAC__stream_decoder_delete(d->decoder);
	}
	if(d->analysis_mode)
		flac__analyze_finish(d->aopts);
	DecoderSession_destroy(d, /*error_occurred=*/true);
	return 1;
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
		flac__utils_printf(stderr, 1, "%s: ERROR, cannot use --until when FLAC metadata has total sample count of 0\n", inbasefilename);
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

FLAC__bool write_iff_headers(FILE *f, DecoderSession *decoder_session, FLAC__uint64 samples)
{
	const FileFormat format = decoder_session->format;
	const char *fmt_desc =
		format==FORMAT_WAVE? "WAVE" :
		format==FORMAT_WAVE64? "Wave64" :
		format==FORMAT_RF64? "RF64" :
		"AIFF";
	const FLAC__bool is_waveformatextensible =
		(format == FORMAT_WAVE || format == FORMAT_WAVE64 || format == FORMAT_RF64) &&
		(
			decoder_session->channel_mask == 2 ||
			decoder_session->channel_mask > 3 ||
			decoder_session->bps%8 ||
			decoder_session->channels > 2
		);
	const FLAC__uint64 data_size = samples * decoder_session->channels * ((decoder_session->bps+7)/8);
	const FLAC__uint64 aligned_data_size =
		format == FORMAT_WAVE64?
			(data_size+7) & (~(FLAC__uint64)7) :
			(data_size+1) & (~(FLAC__uint64)1);

	FLAC__uint64 iff_size;
	unsigned foreign_metadata_size = 0; /* size of all non-audio non-fmt/COMM foreign metadata chunks */
	foreign_metadata_t *fm = decoder_session->foreign_metadata;
	size_t i;

	FLAC__ASSERT(
		format == FORMAT_WAVE ||
		format == FORMAT_WAVE64 ||
		format == FORMAT_RF64 ||
		format == FORMAT_AIFF ||
		format == FORMAT_AIFF_C
	);

	if(samples == 0) {
		if(f == stdout) {
			flac__utils_printf(stderr, 1, "%s: WARNING, don't have accurate sample count available for %s header.\n", decoder_session->inbasefilename, fmt_desc);
			flac__utils_printf(stderr, 1, "             Generated %s file will have a data chunk size of 0.  Try\n", fmt_desc);
			flac__utils_printf(stderr, 1, "             decoding directly to a file instead.\n");
			if(decoder_session->treat_warnings_as_errors)
				return false;
		}
		else {
			decoder_session->iff_headers_need_fixup = true;
		}
	}

	if(fm) {
		FLAC__ASSERT(fm->format_block);
		FLAC__ASSERT(fm->audio_block);
		FLAC__ASSERT(fm->format_block < fm->audio_block);
		/* calc foreign metadata size; we always skip the first chunk, ds64 chunk, format chunk, and sound chunk since we write our own */
		for(i = format==FORMAT_RF64?2:1; i < fm->num_blocks; i++) {
			if(i != fm->format_block && i != fm->audio_block)
				foreign_metadata_size += fm->blocks[i].size;
		}
	}

	if(samples == 0)
		iff_size = 0;
	else if(format == FORMAT_WAVE || format == FORMAT_RF64)
		/* 4 for WAVE form bytes */
		/* +{36,0} for ds64 chunk */
		/* +8+{40,16} for fmt chunk header and body */
		/* +8 for data chunk header */
		iff_size = 4 + (format==FORMAT_RF64?36:0) + 8+(is_waveformatextensible?40:16) + 8 + foreign_metadata_size + aligned_data_size;
	else if(format == FORMAT_WAVE64)
		/* 16+8 for RIFF GUID and size field */
		/* +16 for WAVE GUID */
		/* +16+8+{40,16} for fmt chunk header (GUID and size field) and body */
		/* +16+8 for data chunk header (GUID and size field) */
		iff_size = 16+8 + 16 + 16+8+(is_waveformatextensible?40:16) + 16+8 + foreign_metadata_size + aligned_data_size;
	else /* AIFF */
		iff_size = 46 + foreign_metadata_size + aligned_data_size;

	if(format != FORMAT_WAVE64 && format != FORMAT_RF64 && iff_size >= 0xFFFFFFF4) {
		flac__utils_printf(stderr, 1, "%s: ERROR: stream is too big to fit in a single %s file\n", decoder_session->inbasefilename, fmt_desc);
		return false;
	}

	if(format == FORMAT_WAVE || format == FORMAT_WAVE64 || format == FORMAT_RF64) {
		/* RIFF header */
		switch(format) {
			case FORMAT_WAVE:
				if(flac__utils_fwrite("RIFF", 1, 4, f) != 4)
					return false;
				if(!write_little_endian_uint32(f, (FLAC__uint32)iff_size)) /* filesize-8 */
					return false;
				if(flac__utils_fwrite("WAVE", 1, 4, f) != 4)
					return false;
				break;
			case FORMAT_WAVE64:
				/* RIFF GUID 66666972-912E-11CF-A5D6-28DB04C10000 */
				if(flac__utils_fwrite("\x72\x69\x66\x66\x2E\x91\xCF\x11\xA5\xD6\x28\xDB\x04\xC1\x00\x00", 1, 16, f) != 16)
					return false;
				if(!write_little_endian_uint64(f, iff_size))
					return false;
				/* WAVE GUID 65766177-ACF3-11D3-8CD1-00C04F8EDB8A */
				if(flac__utils_fwrite("\x77\x61\x76\x65\xF3\xAC\xD3\x11\x8C\xD1\x00\xC0\x4F\x8E\xDB\x8A", 1, 16, f) != 16)
					return false;
				break;
			case FORMAT_RF64:
				if(flac__utils_fwrite("RF64", 1, 4, f) != 4)
					return false;
				if(!write_little_endian_uint32(f, 0xffffffff))
					return false;
				if(flac__utils_fwrite("WAVE", 1, 4, f) != 4)
					return false;
				break;
			default:
				return false;
		}

		/* ds64 chunk for RF64 */
		if(format == FORMAT_RF64) {
			if(flac__utils_fwrite("ds64", 1, 4, f) != 4)
				return false;

			if(!write_little_endian_uint32(f, 28)) /* chunk size */
				return false;

			if(!write_little_endian_uint64(f, iff_size))
				return false;

			if(!write_little_endian_uint64(f, data_size))
				return false;

			if(!write_little_endian_uint64(f, samples)) /*@@@@@@ correct? */
				return false;

			if(!write_little_endian_uint32(f, 0)) /* table size */
				return false;
		}

		decoder_session->fm_offset1 = ftello(f);

		if(fm) {
			/* seek forward to {allocate} or {skip over already-written chunks} before "fmt " */
			for(i = format==FORMAT_RF64?2:1; i < fm->format_block; i++) {
				if(fseeko(f, fm->blocks[i].size, SEEK_CUR) < 0) {
					flac__utils_printf(stderr, 1, "%s: ERROR: allocating/skipping foreign metadata before \"fmt \"\n", decoder_session->inbasefilename);
					return false;
				}
			}
		}

		if(format != FORMAT_WAVE64) {
			if(flac__utils_fwrite("fmt ", 1, 4, f) != 4)
				return false;
			if(!write_little_endian_uint32(f, is_waveformatextensible? 40 : 16)) /* chunk size */
				return false;
		}
		else { /* Wave64 */
			/* fmt GUID 20746D66-ACF3-11D3-8CD1-00C04F8EDB8A */
			if(flac__utils_fwrite("\x66\x6D\x74\x20\xF3\xAC\xD3\x11\x8C\xD1\x00\xC0\x4F\x8E\xDB\x8A", 1, 16, f) != 16)
				return false;
			/* chunk size (+16+8 for GUID and size fields) */
			if(!write_little_endian_uint64(f, 16+8+(is_waveformatextensible?40:16)))
				return false;
		}

		if(!write_riff_wave_fmt_chunk_body(f, is_waveformatextensible, decoder_session->bps, decoder_session->channels, decoder_session->sample_rate, decoder_session->channel_mask))
			return false;

		decoder_session->fm_offset2 = ftello(f);

		if(fm) {
			/* seek forward to {allocate} or {skip over already-written chunks} after "fmt " but before "data" */
			for(i = fm->format_block+1; i < fm->audio_block; i++) {
				if(fseeko(f, fm->blocks[i].size, SEEK_CUR) < 0) {
					flac__utils_printf(stderr, 1, "%s: ERROR: allocating/skipping foreign metadata after \"fmt \"\n", decoder_session->inbasefilename);
					return false;
				}
			}
		}

		if(format != FORMAT_WAVE64) {
			if(flac__utils_fwrite("data", 1, 4, f) != 4)
				return false;
			if(!write_little_endian_uint32(f, format==FORMAT_RF64? 0xffffffff : (FLAC__uint32)data_size))
				return false;
		}
		else { /* Wave64 */
			/* data GUID 61746164-ACF3-11D3-8CD1-00C04F8EDB8A */
			if(flac__utils_fwrite("\x64\x61\x74\x61\xF3\xAC\xD3\x11\x8C\xD1\x00\xC0\x4F\x8E\xDB\x8A", 1, 16, f) != 16)
				return false;
			/* +16+8 for GUID and size fields */
			if(!write_little_endian_uint64(f, 16+8 + data_size))
				return false;
		}

		decoder_session->fm_offset3 = ftello(f) + aligned_data_size;
	}
	else {
		if(flac__utils_fwrite("FORM", 1, 4, f) != 4)
			return false;

		if(!write_big_endian_uint32(f, (FLAC__uint32)iff_size)) /* filesize-8 */
			return false;

		if(flac__utils_fwrite("AIFF", 1, 4, f) != 4)
			return false;

		decoder_session->fm_offset1 = ftello(f);

		if(fm) {
			/* seek forward to {allocate} or {skip over already-written chunks} before "COMM" */
			for(i = 1; i < fm->format_block; i++) {
				if(fseeko(f, fm->blocks[i].size, SEEK_CUR) < 0) {
					flac__utils_printf(stderr, 1, "%s: ERROR: allocating/skipping foreign metadata before \"COMM\"\n", decoder_session->inbasefilename);
					return false;
				}
			}
		}

		if(!write_aiff_form_comm_chunk(f, samples, decoder_session->bps, decoder_session->channels, decoder_session->sample_rate))
			return false;

		decoder_session->fm_offset2 = ftello(f);

		if(fm) {
			/* seek forward to {allocate} or {skip over already-written chunks} after "COMM" but before "SSND" */
			for(i = fm->format_block+1; i < fm->audio_block; i++) {
				if(fseeko(f, fm->blocks[i].size, SEEK_CUR) < 0) {
					flac__utils_printf(stderr, 1, "%s: ERROR: allocating/skipping foreign metadata after \"COMM\"\n", decoder_session->inbasefilename);
					return false;
				}
			}
		}

		if(flac__utils_fwrite("SSND", 1, 4, f) != 4)
			return false;

		if(!write_big_endian_uint32(f, (FLAC__uint32)data_size + 8)) /* data size */
			return false;

		if(!write_big_endian_uint32(f, 0/*offset_size*/))
			return false;

		if(!write_big_endian_uint32(f, 0/*block_size*/))
			return false;

		decoder_session->fm_offset3 = ftello(f) + aligned_data_size;
	}

	return true;
}

FLAC__bool write_riff_wave_fmt_chunk_body(FILE *f, FLAC__bool is_waveformatextensible, unsigned bps, unsigned channels, unsigned sample_rate, FLAC__uint32 channel_mask)
{
	if(!write_little_endian_uint16(f, (FLAC__uint16)(is_waveformatextensible? 65534 : 1))) /* compression code */
		return false;

	if(!write_little_endian_uint16(f, (FLAC__uint16)channels))
		return false;

	if(!write_little_endian_uint32(f, sample_rate))
		return false;

	if(!write_little_endian_uint32(f, sample_rate * channels * ((bps+7) / 8)))
		return false;

	if(!write_little_endian_uint16(f, (FLAC__uint16)(channels * ((bps+7) / 8)))) /* block align */
		return false;

	if(!write_little_endian_uint16(f, (FLAC__uint16)(((bps+7)/8)*8))) /* bits per sample */
		return false;

	if(is_waveformatextensible) {
		if(!write_little_endian_uint16(f, (FLAC__uint16)22)) /* cbSize */
			return false;

		if(!write_little_endian_uint16(f, (FLAC__uint16)bps)) /* validBitsPerSample */
			return false;

		if(!write_little_endian_uint32(f, channel_mask))
			return false;

		/* GUID = {0x00000001, 0x0000, 0x0010, {0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71}} */
		if(flac__utils_fwrite("\x01\x00\x00\x00\x00\x00\x10\x00\x80\x00\x00\xaa\x00\x38\x9b\x71", 1, 16, f) != 16)
			return false;
	}

	return true;
}

FLAC__bool write_aiff_form_comm_chunk(FILE *f, FLAC__uint64 samples, unsigned bps, unsigned channels, unsigned sample_rate)
{
	FLAC__ASSERT(samples <= 0xffffffff);

	if(flac__utils_fwrite("COMM", 1, 4, f) != 4)
		return false;

	if(!write_big_endian_uint32(f, 18)) /* chunk size = 18 */
		return false;

	if(!write_big_endian_uint16(f, (FLAC__uint16)channels))
		return false;

	if(!write_big_endian_uint32(f, (FLAC__uint32)samples))
		return false;

	if(!write_big_endian_uint16(f, (FLAC__uint16)bps))
		return false;

	if(!write_sane_extended(f, sample_rate))
		return false;

	return true;
}

FLAC__bool write_little_endian_uint16(FILE *f, FLAC__uint16 val)
{
	FLAC__byte *b = (FLAC__byte*)(&val);
	if(is_big_endian_host_) {
		FLAC__byte tmp;
		tmp = b[1]; b[1] = b[0]; b[0] = tmp;
	}
	return flac__utils_fwrite(b, 1, 2, f) == 2;
}

FLAC__bool write_little_endian_uint32(FILE *f, FLAC__uint32 val)
{
	FLAC__byte *b = (FLAC__byte*)(&val);
	if(is_big_endian_host_) {
		FLAC__byte tmp;
		tmp = b[3]; b[3] = b[0]; b[0] = tmp;
		tmp = b[2]; b[2] = b[1]; b[1] = tmp;
	}
	return flac__utils_fwrite(b, 1, 4, f) == 4;
}

FLAC__bool write_little_endian_uint64(FILE *f, FLAC__uint64 val)
{
	FLAC__byte *b = (FLAC__byte*)(&val);
	if(is_big_endian_host_) {
		FLAC__byte tmp;
		tmp = b[7]; b[7] = b[0]; b[0] = tmp;
		tmp = b[6]; b[6] = b[1]; b[1] = tmp;
		tmp = b[5]; b[5] = b[2]; b[2] = tmp;
		tmp = b[4]; b[4] = b[3]; b[3] = tmp;
	}
	return flac__utils_fwrite(b, 1, 8, f) == 8;
}

FLAC__bool write_big_endian_uint16(FILE *f, FLAC__uint16 val)
{
	FLAC__byte *b = (FLAC__byte*)(&val);
	if(!is_big_endian_host_) {
		FLAC__byte tmp;
		tmp = b[1]; b[1] = b[0]; b[0] = tmp;
	}
	return flac__utils_fwrite(b, 1, 2, f) == 2;
}

FLAC__bool write_big_endian_uint32(FILE *f, FLAC__uint32 val)
{
	FLAC__byte *b = (FLAC__byte*)(&val);
	if(!is_big_endian_host_) {
		FLAC__byte tmp;
		tmp = b[3]; b[3] = b[0]; b[0] = tmp;
		tmp = b[2]; b[2] = b[1]; b[1] = tmp;
	}
	return flac__utils_fwrite(b, 1, 4, f) == 4;
}

FLAC__bool write_sane_extended(FILE *f, unsigned val)
	/* Write to 'f' a SANE extended representation of 'val'.  Return false if
	* the write succeeds; return true otherwise.
	*
	* SANE extended is an 80-bit IEEE-754 representation with sign bit, 15 bits
	* of exponent, and 64 bits of significand (mantissa).  Unlike most IEEE-754
	* representations, it does not imply a 1 above the MSB of the significand.
	*
	* Preconditions:
	*  val!=0U
	*/
{
	unsigned int shift, exponent;

	FLAC__ASSERT(val!=0U); /* handling 0 would require a special case */

	for(shift= 0U; (val>>(31-shift))==0U; ++shift)
		;
	val<<= shift;
	exponent= 63U-(shift+32U); /* add 32 for unused second word */

	if(!write_big_endian_uint16(f, (FLAC__uint16)(exponent+0x3FFF)))
		return false;
	if(!write_big_endian_uint32(f, val))
		return false;
	if(!write_big_endian_uint32(f, 0)) /* unused second word */
		return false;

	return true;
}

FLAC__bool fixup_iff_headers(DecoderSession *d)
{
	const char *fmt_desc =
		d->format==FORMAT_WAVE? "WAVE" :
		d->format==FORMAT_WAVE64? "Wave64" :
		d->format==FORMAT_RF64? "RF64" :
		"AIFF";
	FILE *f = flac_fopen(d->outfilename, "r+b"); /* stream is positioned at beginning of file */

	if(0 == f) {
		flac__utils_printf(stderr, 1, "ERROR, couldn't open file %s while fixing up %s chunk size: %s\n", d->outfilename, fmt_desc, strerror(errno));
		return false;
	}

	if(!write_iff_headers(f, d, d->samples_processed)) {
		fclose(f);
		return false;
	}

	fclose(f);
	return true;
}

FLAC__StreamDecoderWriteStatus write_callback(const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 * const buffer[], void *client_data)
{
	DecoderSession *decoder_session = (DecoderSession*)client_data;
	FILE *fout = decoder_session->fout;
	const unsigned bps = frame->header.bits_per_sample, channels = frame->header.channels;
	const unsigned shift = (decoder_session->format != FORMAT_RAW && (bps%8))? 8-(bps%8): 0;
	FLAC__bool is_big_endian = (
		decoder_session->format == FORMAT_AIFF || decoder_session->format == FORMAT_AIFF_C ? true : (
		decoder_session->format == FORMAT_WAVE || decoder_session->format == FORMAT_WAVE64 || decoder_session->format == FORMAT_RF64 ? false :
		decoder_session->is_big_endian
	));
	FLAC__bool is_unsigned_samples = (
		decoder_session->format == FORMAT_AIFF || decoder_session->format == FORMAT_AIFF_C ? false : (
		decoder_session->format == FORMAT_WAVE || decoder_session->format == FORMAT_WAVE64 || decoder_session->format == FORMAT_RF64 ? bps<=8 :
		decoder_session->is_unsigned_samples
	));
	unsigned wide_samples = frame->header.blocksize, wide_sample, sample, channel;
	unsigned frame_bytes = 0;

	static union
	{	/* The arrays defined within this union are all the same size. */
		FLAC__int8	 s8buffer	[FLAC__MAX_BLOCK_SIZE * FLAC__MAX_CHANNELS * sizeof(FLAC__int32)]; /* WATCHOUT: can be up to 2 megs */
		FLAC__uint8  u8buffer	[FLAC__MAX_BLOCK_SIZE * FLAC__MAX_CHANNELS * sizeof(FLAC__int32)];
		FLAC__int16  s16buffer	[FLAC__MAX_BLOCK_SIZE * FLAC__MAX_CHANNELS * sizeof(FLAC__int16)];
		FLAC__uint16 u16buffer	[FLAC__MAX_BLOCK_SIZE * FLAC__MAX_CHANNELS * sizeof(FLAC__int16)];
		FLAC__int32  s32buffer	[FLAC__MAX_BLOCK_SIZE * FLAC__MAX_CHANNELS];
		FLAC__uint32 u32buffer	[FLAC__MAX_BLOCK_SIZE * FLAC__MAX_CHANNELS];
	} ubuf;

	size_t bytes_to_write = 0;

	(void)decoder;

	if(decoder_session->abort_flag)
		return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;

	/* sanity-check the bits-per-sample */
	if(decoder_session->bps) {
		if(bps != decoder_session->bps) {
			if(decoder_session->got_stream_info)
				flac__utils_printf(stderr, 1, "%s: ERROR, bits-per-sample is %u in frame but %u in STREAMINFO\n", decoder_session->inbasefilename, bps, decoder_session->bps);
			else
				flac__utils_printf(stderr, 1, "%s: ERROR, bits-per-sample is %u in this frame but %u in previous frames\n", decoder_session->inbasefilename, bps, decoder_session->bps);
			if(!decoder_session->continue_through_decode_errors)
				return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
		}
	}
	else {
		/* must not have gotten STREAMINFO, save the bps from the frame header */
		FLAC__ASSERT(!decoder_session->got_stream_info);
		decoder_session->bps = bps;
	}

	/* sanity-check the #channels */
	if(decoder_session->channels) {
		if(channels != decoder_session->channels) {
			if(decoder_session->got_stream_info)
				flac__utils_printf(stderr, 1, "%s: ERROR, channels is %u in frame but %u in STREAMINFO\n", decoder_session->inbasefilename, channels, decoder_session->channels);
			else
				flac__utils_printf(stderr, 1, "%s: ERROR, channels is %u in this frame but %u in previous frames\n", decoder_session->inbasefilename, channels, decoder_session->channels);
			if(!decoder_session->continue_through_decode_errors)
				return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
		}
	}
	else {
		/* must not have gotten STREAMINFO, save the #channels from the frame header */
		FLAC__ASSERT(!decoder_session->got_stream_info);
		decoder_session->channels = channels;
	}

	/* sanity-check the sample rate */
	if(decoder_session->sample_rate) {
		if(frame->header.sample_rate != decoder_session->sample_rate) {
			if(decoder_session->got_stream_info)
				flac__utils_printf(stderr, 1, "%s: ERROR, sample rate is %u in frame but %u in STREAMINFO\n", decoder_session->inbasefilename, frame->header.sample_rate, decoder_session->sample_rate);
			else
				flac__utils_printf(stderr, 1, "%s: ERROR, sample rate is %u in this frame but %u in previous frames\n", decoder_session->inbasefilename, frame->header.sample_rate, decoder_session->sample_rate);
			if(!decoder_session->continue_through_decode_errors)
				return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
		}
	}
	else {
		/* must not have gotten STREAMINFO, save the sample rate from the frame header */
		FLAC__ASSERT(!decoder_session->got_stream_info);
		decoder_session->sample_rate = frame->header.sample_rate;
	}

	/*
	 * limit the number of samples to accept based on --until
	 */
	FLAC__ASSERT(!decoder_session->skip_specification->is_relative);
	/* if we never got the total_samples from the metadata, the skip and until specs would never have been canonicalized, so protect against that: */
	if(decoder_session->skip_specification->is_relative) {
		if(decoder_session->skip_specification->value.samples == 0) /* special case for when no --skip was given */
			decoder_session->skip_specification->is_relative = false; /* convert to our meaning of beginning-of-stream */
		else {
			flac__utils_printf(stderr, 1, "%s: ERROR, cannot use --skip because the total sample count was not found in the metadata\n", decoder_session->inbasefilename);
			return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
		}
	}
	if(decoder_session->until_specification->is_relative) {
		if(decoder_session->until_specification->value.samples == 0) /* special case for when no --until was given */
			decoder_session->until_specification->is_relative = false; /* convert to our meaning of end-of-stream */
		else {
			flac__utils_printf(stderr, 1, "%s: ERROR, cannot use --until because the total sample count was not found in the metadata\n", decoder_session->inbasefilename);
			return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
		}
	}
	FLAC__ASSERT(decoder_session->skip_specification->value.samples >= 0);
	FLAC__ASSERT(decoder_session->until_specification->value.samples >= 0);
	if(decoder_session->until_specification->value.samples > 0) {
		const FLAC__uint64 skip = (FLAC__uint64)decoder_session->skip_specification->value.samples;
		const FLAC__uint64 until = (FLAC__uint64)decoder_session->until_specification->value.samples;
		const FLAC__uint64 input_samples_passed = skip + decoder_session->samples_processed;
		FLAC__ASSERT(until >= input_samples_passed);
		if(input_samples_passed + wide_samples > until)
			wide_samples = (unsigned)(until - input_samples_passed);
		if (wide_samples == 0) {
			decoder_session->abort_flag = true;
			decoder_session->aborting_due_to_until = true;
			return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
		}
	}

	if(decoder_session->analysis_mode) {
		FLAC__uint64 dpos;
		FLAC__stream_decoder_get_decode_position(decoder_session->decoder, &dpos);
		frame_bytes = (unsigned)(dpos-decoder_session->decode_position);
		decoder_session->decode_position = dpos;
	}

	if(wide_samples > 0) {
		decoder_session->samples_processed += wide_samples;
		decoder_session->frame_counter++;

		if(!(decoder_session->frame_counter & 0x1ff))
			print_stats(decoder_session);

		if(decoder_session->analysis_mode) {
			flac__analyze_frame(frame, decoder_session->frame_counter-1, decoder_session->decode_position-frame_bytes, frame_bytes, decoder_session->aopts, fout);
		}
		else if(!decoder_session->test_only) {
			if(shift && !decoder_session->replaygain.apply) {
				for(wide_sample = 0; wide_sample < wide_samples; wide_sample++)
					for(channel = 0; channel < channels; channel++)
						((FLAC__int32**)buffer)[channel][wide_sample] <<= shift;/*@@@@@@un-const'ing the buffer is hacky but safe*/
			}
			if(decoder_session->replaygain.apply) {
				bytes_to_write = FLAC__replaygain_synthesis__apply_gain(
					ubuf.u8buffer,
					!is_big_endian,
					is_unsigned_samples,
					buffer,
					wide_samples,
					channels,
					bps, /* source_bps */
					bps+shift, /* target_bps */
					decoder_session->replaygain.scale,
					decoder_session->replaygain.spec.limiter == RGSS_LIMIT__HARD, /* hard_limit */
					decoder_session->replaygain.spec.noise_shaping != NOISE_SHAPING_NONE, /* do_dithering */
					&decoder_session->replaygain.dither_context
				);
			}
			/* first some special code for common cases */
			else if(is_big_endian == is_big_endian_host_ && !is_unsigned_samples && channels == 2 && bps+shift == 16) {
				FLAC__int16 *buf1_ = ubuf.s16buffer + 1;
				if(is_big_endian)
					memcpy(ubuf.s16buffer, ((FLAC__byte*)(buffer[0]))+2, sizeof(FLAC__int32) * wide_samples - 2);
				else
					memcpy(ubuf.s16buffer, buffer[0], sizeof(FLAC__int32) * wide_samples);
				for(sample = 0; sample < wide_samples; sample++, buf1_+=2)
					*buf1_ = (FLAC__int16)buffer[1][sample];
				bytes_to_write = 4 * sample;
			}
			else if(is_big_endian == is_big_endian_host_ && !is_unsigned_samples && channels == 1 && bps+shift == 16) {
				FLAC__int16 *buf1_ = ubuf.s16buffer;
				for(sample = 0; sample < wide_samples; sample++)
					*buf1_++ = (FLAC__int16)buffer[0][sample];
				bytes_to_write = 2 * sample;
			}
			/* generic code for the rest */
			else if(bps+shift == 16) {
				if(is_unsigned_samples) {
					if(channels == 2) {
						for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++) {
							ubuf.u16buffer[sample++] = (FLAC__uint16)(buffer[0][wide_sample] + 0x8000);
							ubuf.u16buffer[sample++] = (FLAC__uint16)(buffer[1][wide_sample] + 0x8000);
						}
					}
					else if(channels == 1) {
						for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
							ubuf.u16buffer[sample++] = (FLAC__uint16)(buffer[0][wide_sample] + 0x8000);
					}
					else { /* works for any 'channels' but above flavors are faster for 1 and 2 */
						for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
							for(channel = 0; channel < channels; channel++, sample++)
								ubuf.u16buffer[sample] = (FLAC__uint16)(buffer[channel][wide_sample] + 0x8000);
					}
				}
				else {
					if(channels == 2) {
						for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++) {
							ubuf.s16buffer[sample++] = (FLAC__int16)(buffer[0][wide_sample]);
							ubuf.s16buffer[sample++] = (FLAC__int16)(buffer[1][wide_sample]);
						}
					}
					else if(channels == 1) {
						for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
							ubuf.s16buffer[sample++] = (FLAC__int16)(buffer[0][wide_sample]);
					}
					else { /* works for any 'channels' but above flavors are faster for 1 and 2 */
						for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
							for(channel = 0; channel < channels; channel++, sample++)
								ubuf.s16buffer[sample] = (FLAC__int16)(buffer[channel][wide_sample]);
					}
				}
				if(is_big_endian != is_big_endian_host_) {
					unsigned char tmp;
					const unsigned bytes = sample * 2;
					unsigned b;
					for(b = 0; b < bytes; b += 2) {
						tmp = ubuf.u8buffer[b];
						ubuf.u8buffer[b] = ubuf.u8buffer[b+1];
						ubuf.u8buffer[b+1] = tmp;
					}
				}
				bytes_to_write = 2 * sample;
			}
			else if(bps+shift == 24) {
				if(is_unsigned_samples) {
					for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
						for(channel = 0; channel < channels; channel++, sample++)
							ubuf.u32buffer[sample] = buffer[channel][wide_sample] + 0x800000;
				}
				else {
					for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
						for(channel = 0; channel < channels; channel++, sample++)
							ubuf.s32buffer[sample] = buffer[channel][wide_sample];
				}
				if(is_big_endian != is_big_endian_host_) {
					unsigned char tmp;
					const unsigned bytes = sample * 4;
					unsigned b;
					for(b = 0; b < bytes; b += 4) {
						tmp = ubuf.u8buffer[b];
						ubuf.u8buffer[b] = ubuf.u8buffer[b+3];
						ubuf.u8buffer[b+3] = tmp;
						tmp = ubuf.u8buffer[b+1];
						ubuf.u8buffer[b+1] = ubuf.u8buffer[b+2];
						ubuf.u8buffer[b+2] = tmp;
					}
				}
				if(is_big_endian) {
					unsigned b, lbyte;
					const unsigned bytes = sample * 4;
					for(lbyte = b = 0; b < bytes; ) {
						b++;
						ubuf.u8buffer[lbyte++] = ubuf.u8buffer[b++];
						ubuf.u8buffer[lbyte++] = ubuf.u8buffer[b++];
						ubuf.u8buffer[lbyte++] = ubuf.u8buffer[b++];
					}
				}
				else {
					unsigned b, lbyte;
					const unsigned bytes = sample * 4;
					for(lbyte = b = 0; b < bytes; ) {
						ubuf.u8buffer[lbyte++] = ubuf.u8buffer[b++];
						ubuf.u8buffer[lbyte++] = ubuf.u8buffer[b++];
						ubuf.u8buffer[lbyte++] = ubuf.u8buffer[b++];
						b++;
					}
				}
				bytes_to_write = 3 * sample;
			}
			else if(bps+shift == 8) {
				if(is_unsigned_samples) {
					for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
						for(channel = 0; channel < channels; channel++, sample++)
							ubuf.u8buffer[sample] = (FLAC__uint8)(buffer[channel][wide_sample] + 0x80);
				}
				else {
					for(sample = wide_sample = 0; wide_sample < wide_samples; wide_sample++)
						for(channel = 0; channel < channels; channel++, sample++)
							ubuf.s8buffer[sample] = (FLAC__int8)(buffer[channel][wide_sample]);
				}
				bytes_to_write = sample;
			}
			else {
				FLAC__ASSERT(0);
				/* double protection */
				decoder_session->abort_flag = true;
				return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
			}
		}
	}
	if(bytes_to_write > 0) {
		if(flac__utils_fwrite(ubuf.u8buffer, 1, bytes_to_write, fout) != bytes_to_write) {
			/* if a pipe closed when writing to stdout, we let it go without an error message */
			if(errno == EPIPE && decoder_session->fout == stdout)
				decoder_session->aborting_due_to_until = true;
			decoder_session->abort_flag = true;
			return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
		}
	}
	return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
}

void metadata_callback(const FLAC__StreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data)
{
	DecoderSession *decoder_session = (DecoderSession*)client_data;

	if(decoder_session->analysis_mode)
		FLAC__stream_decoder_get_decode_position(decoder, &decoder_session->decode_position);

	if(metadata->type == FLAC__METADATA_TYPE_STREAMINFO) {
		FLAC__uint64 skip, until;
		decoder_session->got_stream_info = true;
		decoder_session->has_md5sum = memcmp(metadata->data.stream_info.md5sum, "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0", 16);
		decoder_session->bps = metadata->data.stream_info.bits_per_sample;
		decoder_session->channels = metadata->data.stream_info.channels;
		decoder_session->sample_rate = metadata->data.stream_info.sample_rate;

		flac__utils_canonicalize_skip_until_specification(decoder_session->skip_specification, decoder_session->sample_rate);
		FLAC__ASSERT(decoder_session->skip_specification->value.samples >= 0);
		skip = (FLAC__uint64)decoder_session->skip_specification->value.samples;

		/* remember, metadata->data.stream_info.total_samples can be 0, meaning 'unknown' */
		if(metadata->data.stream_info.total_samples > 0 && skip >= metadata->data.stream_info.total_samples) {
			flac__utils_printf(stderr, 1, "%s: ERROR trying to --skip more samples than in stream\n", decoder_session->inbasefilename);
			decoder_session->abort_flag = true;
			return;
		}
		else if(metadata->data.stream_info.total_samples == 0 && skip > 0) {
			flac__utils_printf(stderr, 1, "%s: ERROR, can't --skip when FLAC metadata has total sample count of 0\n", decoder_session->inbasefilename);
			decoder_session->abort_flag = true;
			return;
		}
		FLAC__ASSERT(skip == 0 || 0 == decoder_session->cue_specification);
		decoder_session->total_samples = metadata->data.stream_info.total_samples - skip;

		/* note that we use metadata->data.stream_info.total_samples instead of decoder_session->total_samples */
		if(!canonicalize_until_specification(decoder_session->until_specification, decoder_session->inbasefilename, decoder_session->sample_rate, skip, metadata->data.stream_info.total_samples)) {
			decoder_session->abort_flag = true;
			return;
		}
		FLAC__ASSERT(decoder_session->until_specification->value.samples >= 0);
		until = (FLAC__uint64)decoder_session->until_specification->value.samples;

		if(until > 0) {
			FLAC__ASSERT(decoder_session->total_samples != 0);
			FLAC__ASSERT(0 == decoder_session->cue_specification);
			decoder_session->total_samples -= (metadata->data.stream_info.total_samples - until);
		}

		if(decoder_session->bps < 4 || decoder_session->bps > 24) {
			flac__utils_printf(stderr, 1, "%s: ERROR: bits per sample is %u, must be 4-24\n", decoder_session->inbasefilename, decoder_session->bps);
			decoder_session->abort_flag = true;
			return;
		}
	}
	else if(metadata->type == FLAC__METADATA_TYPE_CUESHEET) {
		/* remember, at this point, decoder_session->total_samples can be 0, meaning 'unknown' */
		if(decoder_session->total_samples == 0) {
			flac__utils_printf(stderr, 1, "%s: ERROR can't use --cue when FLAC metadata has total sample count of 0\n", decoder_session->inbasefilename);
			decoder_session->abort_flag = true;
			return;
		}

		flac__utils_canonicalize_cue_specification(decoder_session->cue_specification, &metadata->data.cue_sheet, decoder_session->total_samples, decoder_session->skip_specification, decoder_session->until_specification);

		FLAC__ASSERT(!decoder_session->skip_specification->is_relative);
		FLAC__ASSERT(decoder_session->skip_specification->value_is_samples);

		FLAC__ASSERT(!decoder_session->until_specification->is_relative);
		FLAC__ASSERT(decoder_session->until_specification->value_is_samples);

		FLAC__ASSERT(decoder_session->skip_specification->value.samples >= 0);
		FLAC__ASSERT(decoder_session->until_specification->value.samples >= 0);
		FLAC__ASSERT((FLAC__uint64)decoder_session->until_specification->value.samples <= decoder_session->total_samples);
		FLAC__ASSERT(decoder_session->skip_specification->value.samples <= decoder_session->until_specification->value.samples);

		decoder_session->total_samples = decoder_session->until_specification->value.samples - decoder_session->skip_specification->value.samples;
	}
	else if(metadata->type == FLAC__METADATA_TYPE_VORBIS_COMMENT) {
		if (decoder_session->replaygain.spec.apply) {
			double reference, gain, peak;
			if (!(decoder_session->replaygain.apply = grabbag__replaygain_load_from_vorbiscomment(metadata, decoder_session->replaygain.spec.use_album_gain, /*strict=*/false, &reference, &gain, &peak))) {
				flac__utils_printf(stderr, 1, "%s: WARNING: can't get %s (or even %s) ReplayGain tags\n", decoder_session->inbasefilename, decoder_session->replaygain.spec.use_album_gain? "album":"track", decoder_session->replaygain.spec.use_album_gain? "track":"album");
				if(decoder_session->treat_warnings_as_errors) {
					decoder_session->abort_flag = true;
					return;
				}
			}
			else {
				const char *ls[] = { "no", "peak", "hard" };
				const char *ns[] = { "no", "low", "medium", "high" };
				decoder_session->replaygain.scale = grabbag__replaygain_compute_scale_factor(peak, gain, decoder_session->replaygain.spec.preamp, decoder_session->replaygain.spec.limiter == RGSS_LIMIT__PEAK);
				FLAC__ASSERT(decoder_session->bps > 0 && decoder_session->bps <= 32);
				FLAC__replaygain_synthesis__init_dither_context(&decoder_session->replaygain.dither_context, decoder_session->bps, decoder_session->replaygain.spec.noise_shaping);
				flac__utils_printf(stderr, 1, "%s: INFO: applying %s ReplayGain (gain=%0.2fdB+preamp=%0.1fdB, %s noise shaping, %s limiting) to output\n", decoder_session->inbasefilename, decoder_session->replaygain.spec.use_album_gain? "album":"track", gain, decoder_session->replaygain.spec.preamp, ns[decoder_session->replaygain.spec.noise_shaping], ls[decoder_session->replaygain.spec.limiter]);
				flac__utils_printf(stderr, 1, "%s: WARNING: applying ReplayGain is not lossless\n", decoder_session->inbasefilename);
				/* don't check if(decoder_session->treat_warnings_as_errors) because the user explicitly asked for it */
			}
		}
		(void)flac__utils_get_channel_mask_tag(metadata, &decoder_session->channel_mask);
	}
}

void error_callback(const FLAC__StreamDecoder *decoder, FLAC__StreamDecoderErrorStatus status, void *client_data)
{
	DecoderSession *decoder_session = (DecoderSession*)client_data;
	(void)decoder;
	if(!decoder_session->error_callback_suppress_messages)
		stats_print_name(1, decoder_session->inbasefilename);
		flac__utils_printf(stderr, 1, "*** Got error code %d:%s\n", status, FLAC__StreamDecoderErrorStatusString[status]);
	if(!decoder_session->continue_through_decode_errors) {
		/* if we got a sync error while looking for metadata, either it's not a FLAC file (more likely) or the file is corrupted */
		if(
			!decoder_session->error_callback_suppress_messages &&
			status == FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC &&
			FLAC__stream_decoder_get_state(decoder) == FLAC__STREAM_DECODER_SEARCH_FOR_METADATA
		) {
			flac__utils_printf(stderr, 1,
				"\n"
				"The input file is either not a FLAC file or is corrupted.  If you are\n"
				"convinced it is a FLAC file, you can rerun the same command and add the\n"
				"-F parameter to try and recover as much as possible from the file.\n"
			);
			decoder_session->error_callback_suppress_messages = true;
		}
		else if(status == FLAC__STREAM_DECODER_ERROR_STATUS_UNPARSEABLE_STREAM)
			decoder_session->aborting_due_to_unparseable = true;
		decoder_session->abort_flag = true;
	}
}

void print_error_with_init_status(const DecoderSession *d, const char *message, FLAC__StreamDecoderInitStatus init_status)
{
	const int ilen = strlen(d->inbasefilename) + 1;

	flac__utils_printf(stderr, 1, "\n%s: %s\n", d->inbasefilename, message);

	flac__utils_printf(stderr, 1, "%*s init status = %s\n", ilen, "", FLAC__StreamDecoderInitStatusString[init_status]);

	/* print out some more info for some errors: */
	if (init_status == FLAC__STREAM_DECODER_INIT_STATUS_ERROR_OPENING_FILE) {
		flac__utils_printf(stderr, 1,
			"\n"
			"An error occurred opening the input file; it is likely that it does not exist\n"
			"or is not readable.\n"
		);
	}
}

void print_error_with_state(const DecoderSession *d, const char *message)
{
	const int ilen = strlen(d->inbasefilename) + 1;

	flac__utils_printf(stderr, 1, "\n%s: %s\n", d->inbasefilename, message);
	flac__utils_printf(stderr, 1, "%*s state = %s\n", ilen, "", FLAC__stream_decoder_get_resolved_state_string(d->decoder));

	/* print out some more info for some errors: */
	if (d->aborting_due_to_unparseable) {
		flac__utils_printf(stderr, 1,
			"\n"
			"The FLAC stream may have been created by a more advanced encoder.  Try\n"
			"  metaflac --show-vendor-tag %s\n"
			"If the version number is greater than %s, this decoder is probably\n"
			"not able to decode the file.  If the version number is not, the file\n"
			"may be corrupted, or you may have found a bug.  In this case please\n"
			"submit a bug report to\n"
			"    https://sourceforge.net/p/flac/bugs/\n"
			"Make sure to use the \"Monitor\" feature to monitor the bug status.\n",
			d->inbasefilename, FLAC__VERSION_STRING
		);
	}
}

void print_stats(const DecoderSession *decoder_session)
{
	if(flac__utils_verbosity_ >= 2) {
		const double progress = (double)decoder_session->samples_processed / (double)decoder_session->total_samples * 100.0;

		if(decoder_session->total_samples > 0) {
			if ((unsigned)floor(progress + 0.5) == 100)
				return;

			stats_print_name(2, decoder_session->inbasefilename);
			stats_print_info(2, "%s%u%% complete",
				decoder_session->test_only? "testing, " : decoder_session->analysis_mode? "analyzing, " : "",
				(unsigned)floor(progress + 0.5)
			);
		}
		else {
			stats_print_name(2, decoder_session->inbasefilename);
			stats_print_info(2, "%s %" PRIu64 " samples",
				decoder_session->test_only? "tested" : decoder_session->analysis_mode? "analyzed" : "wrote",
				decoder_session->samples_processed
			);
		}
	}
}
