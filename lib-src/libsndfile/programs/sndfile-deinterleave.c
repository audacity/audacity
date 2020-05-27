/*
** Copyright (C) 2009-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <sndfile.h>

#include "common.h"

#define	BUFFER_LEN	4096
#define	MAX_CHANNELS	16


typedef struct
{	SNDFILE * infile ;
	SNDFILE * outfile [MAX_CHANNELS] ;

	union
	{	double	d [MAX_CHANNELS * BUFFER_LEN] ;
		int		i [MAX_CHANNELS * BUFFER_LEN] ;
	} din ;

	union
	{	double	d [BUFFER_LEN] ;
		int		i [BUFFER_LEN] ;
	} dout ;

	int channels ;
} STATE ;

static void usage_exit (void) ;

static void deinterleave_int (STATE * state) ;
static void deinterleave_double (STATE * state) ;

int
main (int argc, char **argv)
{	STATE state ;
	SF_INFO sfinfo ;
	char pathname [512], ext [32], *cptr ;
	int ch, double_split ;

	if (argc != 2)
	{	if (argc != 1)
			puts ("\nError : need a single input file.\n") ;
		usage_exit () ;
		} ;

	memset (&state, 0, sizeof (state)) ;
	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	if ((state.infile = sf_open (argv [1], SFM_READ, &sfinfo)) == NULL)
	{	printf ("\nError : Not able to open input file '%s'\n%s\n", argv [1], sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	if (sfinfo.channels < 2)
	{	printf ("\nError : Input file '%s' only has one channel.\n", argv [1]) ;
		exit (1) ;
		} ;

	if (sfinfo.channels > MAX_CHANNELS)
	{	printf ("\nError : Input file '%s' has too many (%d) channels. Limit is %d.\n",
			argv [1], sfinfo.channels, MAX_CHANNELS) ;
		exit (1) ;
		} ;


	state.channels = sfinfo.channels ;
	sfinfo.channels = 1 ;

	if (snprintf (pathname, sizeof (pathname), "%s", argv [1]) > (int) sizeof (pathname))
	{	printf ("\nError : Length of provided filename '%s' exceeds MAX_PATH (%d).\n", argv [1], (int) sizeof (pathname)) ;
		exit (1) ;
		} ;

	if ((cptr = strrchr (pathname, '.')) == NULL)
		ext [0] = 0 ;
	else
	{	snprintf (ext, sizeof (ext), "%s", cptr) ;
		cptr [0] = 0 ;
		} ;

	printf ("Input file : %s\n", pathname) ;
	puts ("Output files :") ;

	for (ch = 0 ; ch < state.channels ; ch++)
	{	char filename [520] ;
		size_t count ;

		count = snprintf (filename, sizeof (filename), "%s_%02d%s", pathname, ch, ext) ;

		if (count >= sizeof (filename))
		{	printf ("File name truncated to %s\n", filename) ;
			} ;

		if ((state.outfile [ch] = sf_open (filename, SFM_WRITE, &sfinfo)) == NULL)
		{	printf ("Not able to open output file '%s'\n%s\n", filename, sf_strerror (NULL)) ;
			exit (1) ;
			} ;

		printf ("    %s\n", filename) ;
		} ;

	switch (sfinfo.format & SF_FORMAT_SUBMASK)
	{	case SF_FORMAT_FLOAT :
		case SF_FORMAT_DOUBLE :
		case SF_FORMAT_VORBIS :
			double_split = 1 ;
			break ;

		default :
			double_split = 0 ;
			break ;
		} ;

	if (double_split)
		deinterleave_double (&state) ;
	else
		deinterleave_int (&state) ;

	sf_close (state.infile) ;
	for (ch = 0 ; ch < MAX_CHANNELS ; ch++)
		if (state.outfile [ch] != NULL)
			sf_close (state.outfile [ch]) ;

	return 0 ;
} /* main */

/*------------------------------------------------------------------------------
*/

static void
usage_exit (void)
{	puts ("\nUsage : sndfile-deinterleave <filename>\n") ;
	puts (
		"Split a mutli-channel file into a set of mono files.\n"
		"\n"
		"If the input file is named 'a.wav', the output files will be named\n"
		"a_00.wav, a_01.wav and so on.\n"
		) ;
	printf ("Using %s.\n\n", sf_version_string ()) ;
	exit (1) ;
} /* usage_exit */

static void
deinterleave_int (STATE * state)
{	int read_len ;
	int ch, k ;

	do
	{	read_len = sf_readf_int (state->infile, state->din.i, BUFFER_LEN) ;

		for (ch = 0 ; ch < state->channels ; ch ++)
		{	for (k = 0 ; k < read_len ; k++)
				state->dout.i [k] = state->din.i [k * state->channels + ch] ;
			sf_write_int (state->outfile [ch], state->dout.i, read_len) ;
			} ;
		}
	while (read_len > 0) ;

} /* deinterleave_int */

static void
deinterleave_double (STATE * state)
{	int read_len ;
	int ch, k ;

	do
	{	read_len = sf_readf_double (state->infile, state->din.d, BUFFER_LEN) ;

		for (ch = 0 ; ch < state->channels ; ch ++)
		{	for (k = 0 ; k < read_len ; k++)
				state->dout.d [k] = state->din.d [k * state->channels + ch] ;
			sf_write_double (state->outfile [ch], state->dout.d, read_len) ;
			} ;
		}
	while (read_len > 0) ;

} /* deinterleave_double */
