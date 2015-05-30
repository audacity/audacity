/* flacdiff - Displays where two FLAC streams differ
 * Copyright (C) 2007-2009  Josh Coalson
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

#include <stdio.h>
#include <string.h>
#include "FLAC++/decoder.h"
#include "share/compat.h"

#ifdef _MSC_VER
// warning C4800: 'int' : forcing to bool 'true' or 'false' (performance warning)
#pragma warning ( disable : 4800 )
#endif

class AutoFILE {
protected:
	::FILE *f_;
public:
	inline AutoFILE(const char *path, const char *mode): f_(::fopen(path, mode)) { }
	inline virtual ~AutoFILE() { if (f_) (void)::fclose(f_); }

	inline operator bool() const { return 0 != f_; }
	inline operator const ::FILE *() const { return f_; }
	inline operator ::FILE *() { return f_; }
private:
	AutoFILE();
	AutoFILE(const AutoFILE &);
	void operator=(const AutoFILE &);
};

class Decoder: public FLAC::Decoder::Stream {
public:
	Decoder(AutoFILE &f, FLAC__off_t tgt): tgtpos_((FLAC__uint64)tgt), curpos_(0), go_(true), err_(false), frame_(), f_(f) { memset(&frame_, 0, sizeof(::FLAC__Frame)); }
	FLAC__uint64 tgtpos_, curpos_;
	bool go_, err_;
	::FLAC__Frame frame_;
protected:
	AutoFILE &f_;
	// from FLAC::Decoder::Stream
	virtual ::FLAC__StreamDecoderReadStatus read_callback(FLAC__byte buffer[], size_t *bytes)
	{
		*bytes = fread(buffer, 1, *bytes, f_);
		if(ferror((FILE*)f_))
			return ::FLAC__STREAM_DECODER_READ_STATUS_ABORT;
		else if(*bytes == 0 && feof((FILE*)f_))
			return ::FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
		else
			return ::FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
	}

	virtual ::FLAC__StreamDecoderTellStatus tell_callback(FLAC__uint64 *absolute_byte_offset)
	{
		FLAC__off_t off = ftello(f_);
		if(off < 0)
			return ::FLAC__STREAM_DECODER_TELL_STATUS_ERROR;
		*absolute_byte_offset = off;
		return ::FLAC__STREAM_DECODER_TELL_STATUS_OK;
	}

	virtual bool eof_callback()
	{
		return (bool)feof((FILE*)f_);
	}

	virtual ::FLAC__StreamDecoderWriteStatus write_callback(const ::FLAC__Frame *frame, const FLAC__int32 * const /*buffer*/[])
	{
		FLAC__uint64 pos;
		if(!get_decode_position(&pos)) {
			go_ = false;
			err_ = true;
			return ::FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
		}
		if(pos > tgtpos_) {
			go_ = false;
			frame_ = *frame;
		}
		else
			curpos_ = pos;
		return ::FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
	}

	virtual void error_callback(::FLAC__StreamDecoderErrorStatus status)
	{
		fprintf(stderr, "got error %d:%s\n", status, ::FLAC__StreamDecoderErrorStatusString[status]);
		go_ = false;
		err_ = true;
	}
};

static bool show_diff(AutoFILE &f1, AutoFILE &f2, FLAC__off_t off)
{
	Decoder d1(f1, off), d2(f2, off);
	if(!d1) {
		fprintf(stderr, "ERROR: setting up decoder1, state=%s\n", d1.get_state().resolved_as_cstring(d1));
		return false;
	}
	if(!d2) {
		fprintf(stderr, "ERROR: setting up decoder2, state=%s\n", d2.get_state().resolved_as_cstring(d2));
		return false;
	}
	::FLAC__StreamDecoderInitStatus is;
	if((is = d1.init()) != FLAC__STREAM_DECODER_INIT_STATUS_OK) {
		fprintf(stderr, "ERROR: initializing decoder1, status=%s state=%s\n", FLAC__StreamDecoderInitStatusString[is], d1.get_state().resolved_as_cstring(d1));
		return false;
	}
	if((is = d2.init()) != FLAC__STREAM_DECODER_INIT_STATUS_OK) {
		fprintf(stderr, "ERROR: initializing decoder2, status=%s state=%s\n", FLAC__StreamDecoderInitStatusString[is], d2.get_state().resolved_as_cstring(d2));
		return false;
	}
	if(!d1.process_until_end_of_metadata()) {
		fprintf(stderr, "ERROR: skipping metadata in decoder1, state=%s\n", d1.get_state().resolved_as_cstring(d1));
		return false;
	}
	if(!d2.process_until_end_of_metadata()) {
		fprintf(stderr, "ERROR: skipping metadata in decoder2, state=%s\n", d2.get_state().resolved_as_cstring(d2));
		return false;
	}
	while(d1.go_ && d2.go_) {
		if(!d1.process_single()) {
			fprintf(stderr, "ERROR: decoding frame in decoder1, state=%s\n", d1.get_state().resolved_as_cstring(d1));
			return false;
		}
		if(!d2.process_single()) {
			fprintf(stderr, "ERROR: decoding frame in decoder2, state=%s\n", d2.get_state().resolved_as_cstring(d2));
			return false;
		}
	}
	if(d1.err_) {
		fprintf(stderr, "ERROR: got err_ in decoder1, state=%s\n", d1.get_state().resolved_as_cstring(d1));
		return false;
	}
	if(d2.err_) {
		fprintf(stderr, "ERROR: got err_ in decoder2, state=%s\n", d2.get_state().resolved_as_cstring(d2));
		return false;
	}
	if(d1.go_ != d2.go_) {
		fprintf(stderr, "ERROR: d1.go_(%s) != d2.go_(%s)\n", d1.go_?"true":"false", d2.go_?"true":"false");
		return false;
	}
	fprintf(stdout, "pos1 = %" PRIu64 "  blocksize=%u sample#%" PRIu64 " frame#%" PRIu64 "\n", d1.curpos_, d1.frame_.header.blocksize, d1.frame_.header.number.sample_number, d1.frame_.header.number.sample_number / d1.frame_.header.blocksize);
	fprintf(stdout, "pos2 = %" PRIu64 "  blocksize=%u sample#%" PRIu64 " frame#%" PRIu64 "\n", d2.curpos_, d2.frame_.header.blocksize, d2.frame_.header.number.sample_number, d2.frame_.header.number.sample_number / d2.frame_.header.blocksize);

	return true;
}

static FLAC__off_t get_diff_offset(AutoFILE &f1, AutoFILE &f2)
{
	FLAC__off_t off = 0;
	while(1) {
		if(feof((FILE*)f1) && feof((FILE*)f1)) {
			fprintf(stderr, "ERROR: files are identical\n");
			return -1;
		}
		if(feof((FILE*)f1)) {
			fprintf(stderr, "ERROR: file1 EOF\n");
			return -1;
		}
		if(feof((FILE*)f2)) {
			fprintf(stderr, "ERROR: file2 EOF\n");
			return -1;
		}
		if(fgetc(f1) != fgetc(f2))
			return off;
		off++;
	}
}

static bool run(const char *fn1, const char *fn2)
{
	FLAC__off_t off;
	AutoFILE f1(fn1, "rb"), f2(fn2, "rb");

	if(!f1) {
		flac_fprintf(stderr, "ERROR: opening %s for reading\n", fn1);
		return false;
	}
	if(!f2) {
		flac_fprintf(stderr, "ERROR: opening %s for reading\n", fn2);
		return false;
	}

	if((off = get_diff_offset(f1, f2)) < 0)
		return false;

	fprintf(stdout, "got diff offset = %" PRId64 "\n", off);

	return show_diff(f1, f2, off);
}

int main(int argc, char *argv[])
{
	const char *usage = "usage: flacdiff flacfile1 flacfile2\n";

#ifdef FLAC__STRINGS_IN_UTF8
	if (get_utf8_argv(&argc, &argv) != 0) {
		fprintf(stderr, "%ERROR: failed to convert command line parameters to UTF-8\n");
		return 1;
	}
#endif

	if(argc > 1 && 0 == strcmp(argv[1], "-h")) {
		printf(usage);
		return 0;
	}
	else if(argc != 3) {
		fprintf(stderr, usage);
		return 255;
	}

	return run(argv[1], argv[2])? 0 : 1;
}
