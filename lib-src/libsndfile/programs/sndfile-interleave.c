/*
** Copyright (C) 2009-2015 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#define	MAX_INPUTS	16


typedef struct
{	SNDFILE * infile [MAX_INPUTS] ;
	SNDFILE * outfile ;

	union
	{	double	d [BUFFER_LEN] ;
		int		i [BUFFER_LEN] ;
	} din ;

	union

	{	double	d [MAX_INPUTS * BUFFER_LEN] ;
		int		i [MAX_INPUTS * BUFFER_LEN] ;
	} dout ;

	int channels ;
} STATE ;


static void usage_exit (void) ;
static void interleave_int (STATE * state) ;
static void interleave_double (STATE * state) ;


int
main (int argc, char **argv)
{	STATE state ;
	SF_INFO sfinfo ;
	int k, double_merge = 0 ;

	if (argc < 5)
	{	if (argc > 1)
			puts ("\nError : need at least 2 input files.") ;
		usage_exit () ;
		} ;

	if (strcmp (argv [argc - 2], "-o") != 0)
	{	puts ("\nError : second last command line parameter should be '-o'.\n") ;
		usage_exit () ;
		} ;

	if (argc - 3 > MAX_INPUTS)
	{	printf ("\nError : Cannot handle more than %d input channels.\n\n", MAX_INPUTS) ;
		exit (1) ;
		} ;

	memset (&state, 0, sizeof (state)) ;
	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	for (k = 1 ; k < argc - 2 ; k++)
	{
		if ((state.infile [k - 1] = sf_open (argv [k], SFM_READ, &sfinfo)) == NULL)
		{	printf ("\nError : Not able to open input file '%s'\n%s\n", argv [k], sf_strerror (NULL)) ;
			exit (1) ;
			} ;

		if (sfinfo.channels != 1)
		{	printf ("\bError : Input file '%s' should be mono (has %d channels).\n", argv [k], sfinfo.channels) ;
			exit (1) ;
			} ;

		switch (sfinfo.format & SF_FORMAT_SUBMASK)
		{	case SF_FORMAT_FLOAT :
			case SF_FORMAT_DOUBLE :
			case SF_FORMAT_VORBIS :
				double_merge = 1 ;
				break ;

			default :
				break ;
			} ;

		state.channels ++ ;
		} ;

	sfinfo.channels = state.channels ;
	sfinfo.format = sfe_file_type_of_ext (argv [argc - 1], sfinfo.format) ;

	if ((state.outfile = sf_open (argv [argc - 1], SFM_WRITE, &sfinfo)) == NULL)
	{	printf ("Not able to open output file '%s'\n%s\n", argv [argc - 1], sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	if (double_merge)
		interleave_double (&state) ;
	else
		interleave_int (&state) ;

	for (k = 0 ; k < MAX_INPUTS ; k++)
		if (state.infile [k] != NULL)
			sf_close (state.infile [k]) ;
	sf_close (state.outfile) ;

	return 0 ;
} /* main */

/*------------------------------------------------------------------------------
*/


static void
usage_exit (void)
{	puts ("\nUsage : sndfile-interleave <input 1> <input 2> ... -o <output file>\n") ;
	puts ("Merge two or more mono files into a single multi-channel file.\n") ;
	printf ("Using %s.\n\n", sf_version_string ()) ;
	exit (1) ;
} /* usage_exit */


static void
interleave_int (STATE * state)
{	int max_read_len, read_len ;
	int ch, k ;

	do
	{	max_read_len = 0 ;

		for (ch = 0 ; ch < state->channels ; ch ++)
		{	read_len = sf_read_int (state->infile [ch], state->din.i, BUFFER_LEN) ;
			if (read_len < BUFFER_LEN)
				memset (state->din.i + read_len, 0, sizeof (state->din.i [0]) * (BUFFER_LEN - read_len)) ;

			for (k = 0 ; k < BUFFER_LEN ; k++)
				state->dout.i [k * state->channels + ch] = state->din.i [k] ;

			max_read_len = MAX (max_read_len, read_len) ;
			} ;

		sf_writef_int (state->outfile, state->dout.i, max_read_len) ;
		}
	while (max_read_len > 0) ;

} /* interleave_int */


static void
interleave_double (STATE * state)
{	int max_read_len, read_len ;
	int ch, k ;

	do
	{	max_read_len = 0 ;

		for (ch = 0 ; ch < state->channels ; ch ++)
		{	read_len = sf_read_double (state->infile [ch], state->din.d, BUFFER_LEN) ;
			if (read_len < BUFFER_LEN)
				memset (state->din.d + read_len, 0, sizeof (state->din.d [0]) * (BUFFER_LEN - read_len)) ;

			for (k = 0 ; k < BUFFER_LEN ; k++)
				state->dout.d [k * state->channels + ch] = state->din.d [k] ;

			max_read_len = MAX (max_read_len, read_len) ;
			} ;

		sf_writef_double (state->outfile, state->dout.d, max_read_len) ;
		}
	while (max_read_len > 0) ;

} /* interleave_double */
