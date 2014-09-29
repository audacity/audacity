/*
** Copyright (C) 1999-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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
		) ;

	puts (
		"    where [encoding] may be one of the following:\n\n"
		"        -pcms8     : force the output to signed 8 bit pcm\n"
		"        -pcmu8     : force the output to unsigned 8 bit pcm\n"
		"        -pcm16     : force the output to 16 bit pcm\n"
		"        -pcm24     : force the output to 24 bit pcm\n"
		"        -pcm32     : force the output to 32 bit pcm\n"
		"        -float32   : force the output to 32 bit floating point"
		) ;
	puts (
		"        -ulaw      : force the output ULAW\n"
		"        -alaw      : force the output ALAW\n"
		"        -ima-adpcm : force the output to IMA ADPCM (WAV only)\n"
		"        -ms-adpcm  : force the output to MS ADPCM (WAV only)\n"
		"        -gsm610    : force the GSM6.10 (WAV only)\n"
		"        -dwvw12    : force the output to 12 bit DWVW (AIFF only)\n"
		"        -dwvw16    : force the output to 16 bit DWVW (AIFF only)\n"
		"        -dwvw24    : force the output to 24 bit DWVW (AIFF only)\n"
		"        -vorbis    : force the output to Vorbis (OGG only)\n"
		) ;

	puts (
		"    The format of the output file is determined by the file extension of the\n"
		"    output file name. The following extensions are currently understood:\n"
		) ;

	sfe_dump_format_map () ;

	puts ("") ;
	exit (0) ;
} /* usage_exit */

int
main (int argc, char * argv [])
{	const char	*progname, *infilename, *outfilename ;
	SNDFILE	 	*infile = NULL, *outfile = NULL ;
	SF_INFO	 	sfinfo ;
	int			k, outfilemajor, outfileminor = 0, infileminor ;
	int			override_sample_rate = 0 ; /* assume no sample rate override. */

	progname = program_name (argv [0]) ;

	if (argc < 3 || argc > 5)
	{	usage_exit (progname) ;
		return 1 ;
		} ;

	infilename = argv [argc-2] ;
	outfilename = argv [argc-1] ;

	if (strcmp (infilename, outfilename) == 0)
	{	printf ("Error : Input and output filenames are the same.\n\n") ;
		usage_exit (progname) ;
		return 1 ;
		} ;

	if (strlen (infilename) > 1 && infilename [0] == '-')
	{	printf ("Error : Input filename (%s) looks like an option.\n\n", infilename) ;
		usage_exit (progname) ;
		return 1 ;
		} ;

	if (outfilename [0] == '-')
	{	printf ("Error : Output filename (%s) looks like an option.\n\n", outfilename) ;
		usage_exit (progname) ;
		return 1 ;
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
		if (! strcmp (argv [k], "-ulaw"))
		{	outfileminor = SF_FORMAT_ULAW ;
			continue ;
			} ;
		if (! strcmp (argv [k], "-alaw"))
		{	outfileminor = SF_FORMAT_ALAW ;
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

		if (strstr (argv [k], "-override-sample-rate=") == argv [k])
		{	const char *ptr ;

			ptr = argv [k] + strlen ("-override-sample-rate=") ;
			override_sample_rate = atoi (ptr) ;
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
	{	printf ("Error : output file format is invalid (0x%08X).\n", sfinfo.format) ;
		return 1 ;
		} ;

	/* Open the output file. */
	if ((outfile = sf_open (outfilename, SFM_WRITE, &sfinfo)) == NULL)
	{	printf ("Not able to open output file %s : %s\n", outfilename, sf_strerror (NULL)) ;
		return 1 ;
		} ;

	/* Copy the metadata */
	copy_metadata (outfile, infile, sfinfo.channels) ;

	if ((outfileminor == SF_FORMAT_DOUBLE) || (outfileminor == SF_FORMAT_FLOAT)
			|| (infileminor == SF_FORMAT_DOUBLE) || (infileminor == SF_FORMAT_FLOAT)
			|| (infileminor == SF_FORMAT_VORBIS) || (outfileminor == SF_FORMAT_VORBIS))
		sfe_copy_data_fp (outfile, infile, sfinfo.channels) ;
	else
		sfe_copy_data_int (outfile, infile, sfinfo.channels) ;

	sf_close (infile) ;
	sf_close (outfile) ;

	return 0 ;
} /* main */

static void
copy_metadata (SNDFILE *outfile, SNDFILE *infile, int channels)
{	SF_INSTRUMENT inst ;
	SF_BROADCAST_INFO_2K binfo ;
	const char *str ;
	int k, err = 0, chanmap [256] ;

	for (k = SF_STR_FIRST ; k <= SF_STR_LAST ; k++)
	{	str = sf_get_string (infile, k) ;
		if (str != NULL)
			err = sf_set_string (outfile, k, str) ;
		} ;

	memset (&inst, 0, sizeof (inst)) ;
	memset (&binfo, 0, sizeof (binfo)) ;

	if (channels < ARRAY_LEN (chanmap))
	{	size_t size = channels * sizeof (chanmap [0]) ;

		if (sf_command (infile, SFC_GET_CHANNEL_MAP_INFO, chanmap, size) == SF_TRUE)
			sf_command (outfile, SFC_SET_CHANNEL_MAP_INFO, chanmap, size) ;
		} ;

	if (sf_command (infile, SFC_GET_INSTRUMENT, &inst, sizeof (inst)) == SF_TRUE)
		sf_command (outfile, SFC_SET_INSTRUMENT, &inst, sizeof (inst)) ;

	if (sf_command (infile, SFC_GET_BROADCAST_INFO, &binfo, sizeof (binfo)) == SF_TRUE)
		sf_command (outfile, SFC_SET_BROADCAST_INFO, &binfo, sizeof (binfo)) ;

} /* copy_metadata */

