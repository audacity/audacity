/*
** Copyright (C) 2008-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2008-2010 George Blood Audio
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

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#include <sndfile.h>

#include "common.h"

#define	BUFFER_LEN		(1 << 16)

static void usage_exit (const char *progname, int exit_code) ;
static void process_args (SNDFILE * file, const SF_BROADCAST_INFO_2K * binfo, int argc, char * argv []) ;

int
main (int argc, char *argv [])
{	SNDFILE *file ;
	SF_INFO sfinfo ;
	SF_BROADCAST_INFO_2K binfo ;
	const char *progname ;
	const char * filename = NULL ;
	int	start ;

	/* Store the program name. */
	progname = program_name (argv [0]) ;

	/* Check if we've been asked for help. */
	if (argc <= 2 || strcmp (argv [1], "--help") == 0 || strcmp (argv [1], "-h") == 0)
		usage_exit (progname, 0) ;

	if (argv [argc - 1][0] != '-')
	{	filename = argv [argc - 1] ;
		start = 1 ;
		}
	else if (argv [1][0] != '-')
	{	filename = argv [1] ;
		start = 2 ;
		}
	else
	{	printf ("Error : Either the first or the last command line parameter should be a filename.\n\n") ;
		exit (1) ;
		} ;

	/* Get the time in case we need it later. */
	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	if ((file = sf_open (filename, SFM_READ, &sfinfo)) == NULL)
	{	printf ("Error : Open of file '%s' failed : %s\n\n", filename, sf_strerror (file)) ;
		exit (1) ;
		} ;

	memset (&binfo, 0, sizeof (binfo)) ;
	if (sf_command (file, SFC_GET_BROADCAST_INFO, &binfo, sizeof (binfo)) == 0)
		memset (&binfo, 0, sizeof (binfo)) ;

	process_args (file, &binfo, argc - 2, argv + start) ;

	sf_close (file) ;
	return 0 ;
} /* main */

/*==============================================================================
**	Print version and usage.
*/

static void
usage_exit (const char *progname, int exit_code)
{	printf ("\nUsage :\n  %s [options] <file>\n\nOptions:\n", progname) ;

	puts (
		"    --bext-description    Print the 'bext' description.\n"
		"    --bext-originator     Print the 'bext; originator info.\n"
		"    --bext-orig-ref       Print the 'bext' origination reference.\n"
		"    --bext-umid           Print the 'bext' UMID.\n"
		"    --bext-orig-date      Print the 'bext' origination date.\n"
		"    --bext-orig-time      Print the 'bext' origination time.\n"
		"    --bext-coding-hist    Print the 'bext' coding history.\n"
		) ;

	puts (
		"    --str-title           Print the title metadata.\n"
		"    --str-copyright       Print the copyright metadata.\n"
		"    --str-artist          Print the artist metadata.\n"
		"    --str-comment         Print the comment metadata.\n"
		"    --str-date            Print the creation date metadata.\n"
		"    --str-album           Print the album metadata.\n"
		"    --str-license         Print the license metadata.\n"
		) ;

	printf ("Using %s.\n\n", sf_version_string ()) ;
	exit (exit_code) ;
} /* usage_exit */

static void
process_args (SNDFILE * file, const SF_BROADCAST_INFO_2K * binfo, int argc, char * argv [])
{	const char * str ;
	int k, do_all = 0 ;

#define HANDLE_BEXT_ARG(cmd,name,field) \
		if (do_all || strcmp (argv [k], cmd) == 0) \
		{	printf ("%-20s : %.*s\n", name, (int) sizeof (binfo->field), binfo->field) ; \
			if (! do_all) \
				continue ; \
			} ;

#define HANDLE_STR_ARG(cmd,name,id) \
		if (do_all || strcmp (argv [k], cmd) == 0) \
		{	str = sf_get_string (file, id) ; \
			printf ("%-20s : %s\n", name, str ? str : "") ; \
			if (! do_all) continue ; \
			} ;

	for (k = 0 ; k < argc ; k++)
	{	if (do_all || strcmp (argv [k], "--all") == 0)
			do_all = 1 ;

		HANDLE_BEXT_ARG ("--bext-description", "Description", description) ;
		HANDLE_BEXT_ARG ("--bext-originator", "Originator", originator) ;
		HANDLE_BEXT_ARG ("--bext-orig-ref", "Origination ref", originator_reference) ;
		HANDLE_BEXT_ARG ("--bext-umid", "UMID", umid) ;
		HANDLE_BEXT_ARG ("--bext-orig-date", "Origination date", origination_date) ;
		HANDLE_BEXT_ARG ("--bext-orig-time", "Origination time", origination_time) ;
		HANDLE_BEXT_ARG ("--bext-coding-hist", "Coding history", coding_history) ;

		HANDLE_STR_ARG ("--str-title", "Name", SF_STR_TITLE) ;
		HANDLE_STR_ARG ("--str-copyright", "Copyright", SF_STR_COPYRIGHT) ;
		HANDLE_STR_ARG ("--str-artist", "Artist", SF_STR_ARTIST) ;
		HANDLE_STR_ARG ("--str-comment", "Comment", SF_STR_COMMENT) ;
		HANDLE_STR_ARG ("--str-date", "Create date", SF_STR_DATE) ;
		HANDLE_STR_ARG ("--str-album", "Album", SF_STR_ALBUM) ;
		HANDLE_STR_ARG ("--str-license", "License", SF_STR_LICENSE) ;

		if (! do_all)
		{	printf ("Error : Don't know what to do with command line arg '%s'.\n\n", argv [k]) ;
			exit (1) ;
			} ;
		break ;
		} ;

	return ;
} /* process_args */
