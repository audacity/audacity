/*
** Copyright (C) 1999-2014 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<ctype.h>

#include	<sndfile.h>

#include	"common.h"

#define		BUFFER_LEN	(1 << 16)


static void concat_data_fp (SNDFILE *wfile, SNDFILE *rofile, int channels) ;
static void concat_data_int (SNDFILE *wfile, SNDFILE *rofile, int channels) ;

static void
usage_exit (const char *progname)
{
	printf ("\nUsage : %s <infile1> <infile2>  ... <outfile>\n\n", progname) ;
	puts (
		"    Create a new output file <outfile> containing the concatenated\n"
		"    audio data from froms <infile1> <infile2> ....\n"
		"\n"
		"    The joined file will be encoded in the same format as the data\n"
		"    in infile1, with all the data in subsequent files automatically\n"
		"    converted to the correct encoding.\n"
		"\n"
		"    The only restriction is that the two files must have the same\n"
		"    number of channels.\n"
		) ;

	exit (1) ;
} /* usage_exit */

int
main (int argc, char *argv [])
{	const char	*progname, *outfilename ;
	SNDFILE		*outfile, **infiles ;
	SF_INFO		sfinfo_out, sfinfo_in ;
	void 		(*func) (SNDFILE*, SNDFILE*, int) ;
	int			k ;

	progname = program_name (argv [0]) ;

	if (argc < 4)
		usage_exit (progname) ;

	argv ++ ;
	argc -- ;

	argc -- ;
	outfilename = argv [argc] ;

	if ((infiles = calloc (argc, sizeof (SNDFILE*))) == NULL)
	{	printf ("\nError : Malloc failed.\n\n") ;
		exit (1) ;
		} ;

	memset (&sfinfo_in, 0, sizeof (sfinfo_in)) ;

	if ((infiles [0] = sf_open (argv [0], SFM_READ, &sfinfo_in)) == NULL)
	{	printf ("\nError : failed to open file '%s'.\n\n", argv [0]) ;
		exit (1) ;
		} ;

	sfinfo_out = sfinfo_in ;

	for (k = 1 ; k < argc ; k++)
	{	if ((infiles [k] = sf_open (argv [k], SFM_READ, &sfinfo_in)) == NULL)
		{	printf ("\nError : failed to open file '%s'.\n\n", argv [k]) ;
			exit (1) ;
			} ;

		if (sfinfo_in.channels != sfinfo_out.channels)
		{	printf ("\nError : File '%s' has %d channels (should have %d).\n\n", argv [k], sfinfo_in.channels, sfinfo_out.channels) ;
			exit (1) ;
			} ;
		} ;

	if ((outfile = sf_open (outfilename, SFM_WRITE, &sfinfo_out)) == NULL)
	{	printf ("\nError : Not able to open input file %s.\n", outfilename) ;
		puts (sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	if ((sfinfo_out.format & SF_FORMAT_SUBMASK) == SF_FORMAT_DOUBLE ||
			(sfinfo_out.format & SF_FORMAT_SUBMASK) == SF_FORMAT_FLOAT)
		func = concat_data_fp ;
	else
		func = concat_data_int ;

	for (k = 0 ; k < argc ; k++)
	{	func (outfile, infiles [k], sfinfo_out.channels) ;
		sf_close (infiles [k]) ;
		} ;

	sf_close (outfile) ;
	free (infiles) ;

	return 0 ;
} /* main */

static void
concat_data_fp (SNDFILE *wfile, SNDFILE *rofile, int channels)
{	static double	data [BUFFER_LEN] ;
	int		frames, readcount ;

	frames = BUFFER_LEN / channels ;
	readcount = frames ;

	sf_seek (wfile, 0, SEEK_END) ;

	while (readcount > 0)
	{	readcount = sf_readf_double (rofile, data, frames) ;
		sf_writef_double (wfile, data, readcount) ;
		} ;

	return ;
} /* concat_data_fp */

static void
concat_data_int (SNDFILE *wfile, SNDFILE *rofile, int channels)
{	static int	data [BUFFER_LEN] ;
	int		frames, readcount ;

	frames = BUFFER_LEN / channels ;
	readcount = frames ;

	sf_seek (wfile, 0, SEEK_END) ;

	while (readcount > 0)
	{	readcount = sf_readf_int (rofile, data, frames) ;
		sf_writef_int (wfile, data, readcount) ;
		} ;

	return ;
} /* concat_data_int */

