/*
** Copyright (C) 2008-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2008 Conrad Parker <conrad@metadecks.org>
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

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include <sndfile.h>

#include "common.h"

/* Length of comparison data buffers in units of items */
#define BUFLEN 65536

static const char * progname = NULL ;
static char * filename1 = NULL, * filename2 = NULL ;

static int
comparison_error (const char * what, sf_count_t frame_offset)
{	char buffer [128] = "" ;

	if (frame_offset >= 0)
		snprintf (buffer, sizeof (buffer), " (at frame offset %" PRId64 ")", frame_offset) ;

	printf ("%s: %s of files %s and %s differ%s.\n", progname, what, filename1, filename2, buffer) ;
	return 1 ;
} /* comparison_error */

static int
compare (void)
{
	double buf1 [BUFLEN], buf2 [BUFLEN] ;
	SF_INFO sfinfo1, sfinfo2 ;
	SNDFILE * sf1 = NULL, * sf2 = NULL ;
	sf_count_t items, i, nread1, nread2, offset = 0 ;
	int retval = 0 ;

	memset (&sfinfo1, 0, sizeof (SF_INFO)) ;
	sf1 = sf_open (filename1, SFM_READ, &sfinfo1) ;
	if (sf1 == NULL)
	{	printf ("Error opening %s.\n", filename1) ;
		retval = 1 ;
		goto out ;
		} ;

	memset (&sfinfo2, 0, sizeof (SF_INFO)) ;
	sf2 = sf_open (filename2, SFM_READ, &sfinfo2) ;
	if (sf2 == NULL)
	{	printf ("Error opening %s.\n", filename2) ;
		retval = 1 ;
		goto out ;
		} ;

	if (sfinfo1.samplerate != sfinfo2.samplerate)
	{	retval = comparison_error ("Samplerates", -1) ;
		goto out ;
		} ;

	if (sfinfo1.channels != sfinfo2.channels)
	{	retval = comparison_error ("Number of channels", -1) ;
		goto out ;
		} ;

	/* Calculate the framecount that will fit in our data buffers */
	items = BUFLEN / sfinfo1.channels ;

	while ( (nread1 = sf_readf_double (sf1, buf1, items)) > 0)
	{	nread2 = sf_readf_double (sf2, buf2, nread1) ;
		if (nread2 != nread1)
		{	retval = comparison_error ("PCM data lengths", -1) ;
			goto out ;
			} ;
		for (i = 0 ; i < nread1 * sfinfo1.channels ; i++)
		{	if (buf1 [i] != buf2 [i])
			{	retval = comparison_error ("PCM data", offset + i / sfinfo1.channels) ;
				goto out ;
				} ;
			} ;
		offset += nread1 ;
		} ;

	if ( (nread2 = sf_readf_double (sf2, buf2, nread1)) != 0)
	{	retval = comparison_error ("PCM data lengths", -1) ;
		goto out ;
		} ;

out :
	sf_close (sf1) ;
	sf_close (sf2) ;

	return retval ;
} /* compare */

static void
print_version (void)
{	char buffer [256] ;

	sf_command (NULL, SFC_GET_LIB_VERSION, buffer, sizeof (buffer)) ;
	printf ("\nVersion : %s\n\n", buffer) ;
} /* print_version */

static void
usage_exit (void)
{
	print_version () ;

	printf ("Usage : %s <filename> <filename>\n", progname) ;
	printf ("	Compare the PCM data of two sound files.\n\n") ;
	exit (0) ;
} /* usage_exit */

int
main (int argc, char *argv [])
{
	progname = program_name (argv [0]) ;

	if (argc != 3)
	{	usage_exit () ;
		return 1 ;
		} ;

	filename1 = argv [argc - 2] ;
	filename2 = argv [argc - 1] ;

	if (strcmp (filename1, filename2) == 0)
	{	printf ("Error : Input filenames are the same.\n\n") ;
		usage_exit () ;
		return 1 ;
		} ;

	return compare () ;
} /* main */
