/*
** Copyright (C) 1999-2015 Erik de Castro Lopo <erikd@mega-nerd.com>
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

/*
** A quick/rough hack to add SF_INSTRUMENT data to a file. It compiles, but
** no guarantees beyond that. Happy to receive patches to fix/improve it.
**
** Code for this was stolen from programs/sndfile-convert.c and related code.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <sndfile.h>

#define BUFFER_LEN		(1 << 14)


typedef	struct
{	char	*infilename, *outfilename ;
	SF_INFO	infileinfo, outfileinfo ;
} OptionData ;

const char * program_name (const char * argv0) ;
static void sfe_copy_data_int (SNDFILE *outfile, SNDFILE *infile, int channels) ;
static void add_instrument_data (SNDFILE *outfile, const SF_INFO * in_info) ;

static void
usage_exit (const char *progname)
{
	printf ("\nUsage : %s <input file> <output file>\n", progname) ;
	puts ("") ;
	exit (1) ;
} /* usage_exit */

int
main (int argc, char * argv [])
{	const char	*progname, *infilename, *outfilename ;
	SNDFILE		*infile = NULL, *outfile = NULL ;
	SF_INFO		in_sfinfo, out_sfinfo ;

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

	memset (&in_sfinfo, 0, sizeof (in_sfinfo)) ;

	if ((infile = sf_open (infilename, SFM_READ, &in_sfinfo)) == NULL)
	{	printf ("Not able to open input file %s.\n", infilename) ;
		puts (sf_strerror (NULL)) ;
		return 1 ;
		} ;

	memcpy (&out_sfinfo, &in_sfinfo, sizeof (out_sfinfo)) ;
	/* Open the output file. */
	if ((outfile = sf_open (outfilename, SFM_WRITE, &out_sfinfo)) == NULL)
	{	printf ("Not able to open output file %s : %s\n", outfilename, sf_strerror (NULL)) ;
		return 1 ;
		} ;

	/* Add the loop data */
	add_instrument_data (outfile, &in_sfinfo) ;

	/* Copy the audio data */
	sfe_copy_data_int (outfile, infile, in_sfinfo.channels) ;

	sf_close (infile) ;
	sf_close (outfile) ;

	return 0 ;
} /* main */

const char *
program_name (const char * argv0)
{	const char * tmp ;

	tmp = strrchr (argv0, '/') ;
	argv0 = tmp ? tmp + 1 : argv0 ;

	/* Remove leading libtool name mangling. */
	if (strstr (argv0, "lt-") == argv0)
		return argv0 + 3 ;

	return argv0 ;
} /* program_name */

static void
sfe_copy_data_int (SNDFILE *outfile, SNDFILE *infile, int channels)
{	static int	data [BUFFER_LEN] ;
	int		frames, readcount ;

	frames = BUFFER_LEN / channels ;
	readcount = frames ;

	while (readcount > 0)
	{	readcount = sf_readf_int (infile, data, frames) ;
		sf_writef_int (outfile, data, readcount) ;
		} ;

	return ;
} /* sfe_copy_data_int */

static void
add_instrument_data (SNDFILE *file, const SF_INFO *info)
{	SF_INSTRUMENT instr ;

	memset (&instr, 0, sizeof (instr)) ;

	instr.gain = 1 ;
	instr.basenote = 0 ;
	instr.detune = 0 ;
	instr.velocity_lo = 0 ;
	instr.velocity_hi = 0 ;
	instr.key_lo = 0 ;
	instr.key_hi = 0 ;
	instr.loop_count = 1 ;

	instr.loops [0].mode = SF_LOOP_FORWARD ;
	instr.loops [0].start = 0 ;
	instr.loops [0].end = info->frames ;
	instr.loops [0].count = 0 ;

	if (sf_command (file, SFC_SET_INSTRUMENT, &instr, sizeof (instr)) == SF_FALSE)
	{	printf ("\n\nLine %d : sf_command (SFC_SET_INSTRUMENT) failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;

	return ;
} /* add_instrument_data */

