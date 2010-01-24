The files in this directory comprise ANSI-C language reference implementations
of the CCITT (International Telegraph and Telephone Consultative Committee)
G.711, G.721 and G.723 voice compressions.  They have been tested on Sun
SPARCstations and passed 82 out of 84 test vectors published by CCITT
(Dec. 20, 1988) for G.721 and G.723.  [The two remaining test vectors,
which the G.721 decoder implementation for u-law samples did not pass,
may be in error because they are identical to two other vectors for G.723_40.]

This source code is released by Sun Microsystems, Inc. to the public domain.
Please give your acknowledgement in product literature if this code is used
in your product implementation.

Sun Microsystems supports some CCITT audio formats in Solaris 2.0 system
software.  However, Sun's implementations have been optimized for higher
performance on SPARCstations.


The source files for CCITT conversion routines in this directory are:

	g72x.h		header file for g721.c, g723_24.c and g723_40.c
	g711.c		CCITT G.711 u-law and A-law compression
	g72x.c		common denominator of G.721 and G.723 ADPCM codes
	g721.c		CCITT G.721 32Kbps ADPCM coder (with g72x.c)
	g723_24.c	CCITT G.723 24Kbps ADPCM coder (with g72x.c)
	g723_40.c	CCITT G.723 40Kbps ADPCM coder (with g72x.c)


Simple conversions between u-law, A-law, and 16-bit linear PCM are invoked
as follows:

	unsigned char		ucode, acode;
	short			pcm_val;

	ucode = linear2ulaw(pcm_val);
	ucode = alaw2ulaw(acode);

	acode = linear2alaw(pcm_val);
	acode = ulaw2alaw(ucode);

	pcm_val = ulaw2linear(ucode);
	pcm_val = alaw2linear(acode);


The other CCITT compression routines are invoked as follows:

	#include "g72x.h"

	struct g72x_state	state;
	int			sample, code;

	g72x_init_state(&state);
	code = {g721,g723_24,g723_40}_encoder(sample, coding, &state);
	sample = {g721,g723_24,g723_40}_decoder(code, coding, &state);

where
	coding = AUDIO_ENCODING_ULAW	for 8-bit u-law samples
		 AUDIO_ENCODING_ALAW	for 8-bit A-law samples
		 AUDIO_ENCODING_LINEAR	for 16-bit linear PCM samples



This directory also includes the following sample programs:

	encode.c	CCITT ADPCM encoder
	decode.c	CCITT ADPCM decoder
	Makefile	makefile for the sample programs


The sample programs contain examples of how to call the various compression
routines and pack/unpack the bits.  The sample programs read byte streams from
stdin and write to stdout.  The input/output data is raw data (no file header
or other identifying information is embedded).  The sample programs are
invoked as follows:

	encode [-3|4|5] [-a|u|l] <infile >outfile
	decode [-3|4|5] [-a|u|l] <infile >outfile
where:
	-3	encode to (decode from) G.723 24kbps (3-bit) data
	-4	encode to (decode from) G.721 32kbps (4-bit) data [the default]
	-5	encode to (decode from) G.723 40kbps (5-bit) data
	-a	encode from (decode to) A-law data
	-u	encode from (decode to) u-law data [the default]
	-l	encode from (decode to) 16-bit linear data

Examples:
	# Read 16-bit linear and output G.721
	encode -4 -l <pcmfile >g721file

	# Read 40Kbps G.723 and output A-law
	decode -5 -a <g723file >alawfile

	# Compress and then decompress u-law data using 24Kbps G.723
	encode -3 <ulawin | deoced -3 >ulawout

