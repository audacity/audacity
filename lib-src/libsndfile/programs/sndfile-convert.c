/*
** Copyright (C) 1999-2019 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are
** met:
**
**     * Redistributions of source code must retain the above copyright
**       notice, this list of conditions and the following disclaimer.
**     * Redistributions in binary form must reproduce the above copyright
**       notice, this list of conditions and the following disclaimer in
**       the documentation and/or other materials provided with the
**       distribution.
**     * Neither the author nor the names of any contributors may be used
**       to endorse or promote products derived from this software without
**       specific prior written permission.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
** "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
** TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
** PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
** CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
** EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
** PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
** OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
** WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
** OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
** ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <sndfile.h>

#include "common.h"


typedef	struct
{	char	*infilename, *outfilename ;
	SF_INFO	infileinfo, outfileinfo ;
} OptionData ;

static void copy_metadata (SNDFILE *outfile, SNDFILE *infile, int channels) ;

static void
usage_exit (const char *progname)
{
	printf ("\nUsage : %s [options] [encoding] <input file> <output file>\n", progname) ;
	puts ("\n"
		"    where [option] may be:\n\n"
		"        -override-sample-rate=X  : force sample rate of input to X\n"
		"        -endian=little           : force output file to little endian data\n"
		"        -endian=big              : force output file to big endian data\n"
		"        -endian=cpu              : force output file same endian-ness as the CPU\n"
		"        -normalize               : normalize the data in the output file\n"
		) ;

	puts (
		"    where [encoding] may be one of the following:\n\n"
		"        -pcms8     : signed 8 bit pcm\n"
		"        -pcmu8     : unsigned 8 bit pcm\n"
		"        -pcm16     : 16 bit pcm\n"
		"        -pcm24     : 24 bit pcm\n"
		"        -pcm32     : 32 bit pcm\n"
		"        -float32   : 32 bit floating point\n"
		"        -float64   : 64 bit floating point\n"
		"        -ulaw      : ULAW\n"
		"        -alaw      : ALAW\n"
		"        -alac16    : 16 bit ALAC (CAF only)\n"
		"        -alac20    : 20 bit ALAC (CAF only)\n"
		"        -alac24    : 24 bit ALAC (CAF only)\n"
		"        -alac32    : 32 bit ALAC (CAF only)\n"
		"        -ima-adpcm : IMA ADPCM (WAV only)\n"
		"        -ms-adpcm  : MS ADPCM (WAV only)\n"
		"        -gsm610    : GSM6.10 (WAV only)\n"
		"        -dwvw12    : 12 bit DWVW (AIFF only)\n"
		"        -dwvw16    : 16 bit DWVW (AIFF only)\n"
		"        -dwvw24    : 24 bit DWVW (AIFF only)\n"
		"        -vorbis    : Vorbis (OGG only)\n"
		"        -opus      : Opus (OGG only)\n"
		) ;

	puts (
		"    If no encoding is specified, the program will try to use the encoding\n"
		"    of the input file in the output file. This will not always work as\n"
		"    most container formats (eg WAV, AIFF etc) only support a small subset\n"
		"    of codec formats (eg 16 bit PCM, a-law, Vorbis etc).\n"
		) ;

	puts (
		"    The format of the output file is determined by the file extension of the\n"
		"    output file name. The following extensions are currently understood:\n"
		) ;

	sfe_dump_format_map () ;

	puts ("") ;
	exit (1) ;
} /* usage_exit */

static void
report_format_error_exit (const char * argv0, SF_INFO * sfinfo)
{	int old_format = sfinfo->format ;
	int endian = sfinfo->format & SF_FORMAT_ENDMASK ;
	int channels = sfinfo->channels ;

	sfinfo->format = old_format & (SF_FORMAT_TYPEMASK | SF_FORMAT_SUBMASK) ;

	if (endian && sf_format_check (sfinfo))
	{	printf ("Error : output file format does not support %s endian-ness.\n", sfe_endian_name (endian)) ;
		exit (1) ;
		} ;

	sfinfo->channels = 1 ;
	if (sf_format_check (sfinfo))
	{	printf ("Error : output file format does not support %d channels.\n", channels) ;
		exit (1) ;
		} ;

	printf ("\n"
			"Error : output file format is invalid.\n"
			"The '%s' container does not support '%s' codec data.\n"
			"Run '%s --help' for clues.\n\n",
			sfe_container_name (sfinfo->format), sfe_codec_name (sfinfo->format), program_name (argv0)) ;
	exit (1) ;
} /* report_format_error_exit */

int
main (int argc, char * argv [])
{	const char	*progname, *infilename, *outfilename ;
	SNDFILE		*infile = NULL, *outfile = NULL ;
	SF_INFO		sfinfo ;
	int			k, outfilemajor, outfileminor = 0, infileminor ;
	int			override_sample_rate = 0 ; /* assume no sample rate override. */
	int			endian = SF_ENDIAN_FILE, normalize = SF_FALSE ;

	progname = program_name (argv [0]) ;

	if (argc < 3 || argc > 5)
		usage_exit (progname) ;

	infilename = argv [argc-2] ;
	outfilename = argv [argc-1] ;

	if (strcmp (infilename, outfilename) == 0)
	{	printf ("Error : Input and output filenames are the same.\n\n") ;
		usage_exit (progname) ;
		} ;

	if (strlen (infilename) > 1 && infilename [0] == '-')
	{	printf ("Error : Input filename (%s) looks like an option.\n\n", infilename) ;
		usage_exit (progname) ;
		} ;

	if (outfilename [0] == '-')
	{	printf ("Error : Output filename (%s) looks like an option.\n\n", outfilename) ;
		usage_exit (progname) ;
		} ;

	for (k = 1 ; k < argc - 2 ; k++)
	{	if (! strcmp (argv [k], "-pcms8"))
		{	outfileminor = SF_FORMAT_PCM_S8 ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-pcmu8"))
		{	outfileminor = SF_FORMAT_PCM_U8 ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-pcm16"))
		{	outfileminor = SF_FORMAT_PCM_16 ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-pcm24"))
		{	outfileminor = SF_FORMAT_PCM_24 ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-pcm32"))
		{	outfileminor = SF_FORMAT_PCM_32 ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-float32"))
		{	outfileminor = SF_FORMAT_FLOAT ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-float64"))
		{	outfileminor = SF_FORMAT_DOUBLE ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-ulaw"))
		{	outfileminor = SF_FORMAT_ULAW ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-alaw"))
		{	outfileminor = SF_FORMAT_ALAW ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-alac16"))
		{	outfileminor = SF_FORMAT_ALAC_16 ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-alac20"))
		{	outfileminor = SF_FORMAT_ALAC_20 ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-alac24"))
		{	outfileminor = SF_FORMAT_ALAC_24 ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-alac32"))
		{	outfileminor = SF_FORMAT_ALAC_32 ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-ima-adpcm"))
		{	outfileminor = SF_FORMAT_IMA_ADPCM ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-ms-adpcm"))
		{	outfileminor = SF_FORMAT_MS_ADPCM ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-gsm610"))
		{	outfileminor = SF_FORMAT_GSM610 ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-dwvw12"))
		{	outfileminor = SF_FORMAT_DWVW_12 ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-dwvw16"))
		{	outfileminor = SF_FORMAT_DWVW_16 ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-dwvw24"))
		{	outfileminor = SF_FORMAT_DWVW_24 ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-vorbis"))
		{	outfileminor = SF_FORMAT_VORBIS ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-opus"))
		{	outfileminor = SF_FORMAT_OPUS ;
			continue ;
			} ;

		if (strstr (argv [k], "-override-sample-rate=") == argv [k])
		{	const char *ptr ;

			ptr = argv [k] + strlen ("-override-sample-rate=") ;
			override_sample_rate = atoi (ptr) ;
			continue ;
			} ;

		if (! strcmp (argv [k], "-endian=little"))
		{	endian = SF_ENDIAN_LITTLE ;
			continue ;
			} ;

		if (! strcmp (argv [k], "-endian=big"))
		{	endian = SF_ENDIAN_BIG ;
			continue ;
			} ;

		if (! strcmp (argv [k], "-endian=cpu"))
		{	endian = SF_ENDIAN_CPU ;
			continue ;
			} ;

		if (! strcmp (argv [k], "-endian=file"))
		{	endian = SF_ENDIAN_FILE ;
			continue ;
			} ;

		if (! strcmp (argv [k], "-normalize"))
		{	normalize = SF_TRUE ;
			continue ;
			} ;

		printf ("Error : Not able to decode argunment '%s'.\n", argv [k]) ;
		exit (1) ;
		} ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	if ((infile = sf_open (infilename, SFM_READ, &sfinfo)) == NULL)
	{	printf ("Not able to open input file %s.\n", infilename) ;
		puts (sf_strerror (NULL)) ;
		return 1 ;
		} ;

	/* Update sample rate if forced to something else. */
	if (override_sample_rate)
		sfinfo.samplerate = override_sample_rate ;

	infileminor = sfinfo.format & SF_FORMAT_SUBMASK ;

	if ((sfinfo.format = sfe_file_type_of_ext (outfilename, sfinfo.format)) == 0)
	{	printf ("Error : Not able to determine output file type for %s.\n", outfilename) ;
		return 1 ;
		} ;

	outfilemajor = sfinfo.format & (SF_FORMAT_TYPEMASK | SF_FORMAT_ENDMASK) ;

	if (outfileminor == 0)
		outfileminor = sfinfo.format & SF_FORMAT_SUBMASK ;

	if (outfileminor != 0)
		sfinfo.format = outfilemajor | outfileminor ;
	else
		sfinfo.format = outfilemajor | (sfinfo.format & SF_FORMAT_SUBMASK) ;

	sfinfo.format |= endian ;

	if ((sfinfo.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_XI)
		switch (sfinfo.format & SF_FORMAT_SUBMASK)
		{	case SF_FORMAT_PCM_16 :
					sfinfo.format = outfilemajor | SF_FORMAT_DPCM_16 ;
					break ;

			case SF_FORMAT_PCM_S8 :
			case SF_FORMAT_PCM_U8 :
					sfinfo.format = outfilemajor | SF_FORMAT_DPCM_8 ;
					break ;
			} ;

	if (sf_format_check (&sfinfo) == 0)
	{	sf_close (infile) ;
		report_format_error_exit (argv [0], &sfinfo) ;
		} ;

	if ((sfinfo.format & SF_FORMAT_SUBMASK) == SF_FORMAT_GSM610 && sfinfo.samplerate != 8000)
	{	printf (
			"WARNING: GSM 6.10 data format only supports 8kHz sample rate. The converted\n"
			"ouput file will contain the input data converted to the GSM 6.10 data format\n"
			"but not re-sampled.\n"
			) ;
		} ;

	/* Open the output file. */
	if ((outfile = sf_open (outfilename, SFM_WRITE, &sfinfo)) == NULL)
	{	printf ("Not able to open output file %s : %s\n", outfilename, sf_strerror (NULL)) ;
		return 1 ;
		} ;

	/* Copy the metadata */
	copy_metadata (outfile, infile, sfinfo.channels) ;

	if (normalize
			|| (outfileminor == SF_FORMAT_DOUBLE) || (outfileminor == SF_FORMAT_FLOAT)
			|| (infileminor == SF_FORMAT_DOUBLE) || (infileminor == SF_FORMAT_FLOAT)
			|| (infileminor == SF_FORMAT_OPUS) || (outfileminor == SF_FORMAT_OPUS)
			|| (infileminor == SF_FORMAT_VORBIS) || (outfileminor == SF_FORMAT_VORBIS))
		sfe_copy_data_fp (outfile, infile, sfinfo.channels, normalize) ;
	else
		sfe_copy_data_int (outfile, infile, sfinfo.channels) ;

	sf_close (infile) ;
	sf_close (outfile) ;

	return 0 ;
} /* main */

static void
copy_metadata (SNDFILE *outfile, SNDFILE *infile, int channels)
{	SF_INSTRUMENT inst ;
	SF_CUES cues ;
	SF_BROADCAST_INFO_2K binfo ;
	const char *str ;
	int k, chanmap [256] ;

	for (k = SF_STR_FIRST ; k <= SF_STR_LAST ; k++)
	{	str = sf_get_string (infile, k) ;
		if (str != NULL)
			sf_set_string (outfile, k, str) ;
		} ;

	memset (&inst, 0, sizeof (inst)) ;
	memset (&cues, 0, sizeof (cues)) ;
	memset (&binfo, 0, sizeof (binfo)) ;

	if (channels < ARRAY_LEN (chanmap))
	{	size_t size = channels * sizeof (chanmap [0]) ;

		if (sf_command (infile, SFC_GET_CHANNEL_MAP_INFO, chanmap, size) == SF_TRUE)
			sf_command (outfile, SFC_SET_CHANNEL_MAP_INFO, chanmap, size) ;
		} ;

	if (sf_command (infile, SFC_GET_CUE, &cues, sizeof (cues)) == SF_TRUE)
		sf_command (outfile, SFC_SET_CUE, &cues, sizeof (cues)) ;

	if (sf_command (infile, SFC_GET_INSTRUMENT, &inst, sizeof (inst)) == SF_TRUE)
		sf_command (outfile, SFC_SET_INSTRUMENT, &inst, sizeof (inst)) ;

	if (sf_command (infile, SFC_GET_BROADCAST_INFO, &binfo, sizeof (binfo)) == SF_TRUE)
		sf_command (outfile, SFC_SET_BROADCAST_INFO, &binfo, sizeof (binfo)) ;

} /* copy_metadata */

