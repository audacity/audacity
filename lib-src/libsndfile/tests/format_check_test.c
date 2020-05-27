/*
** Copyright (C) 2011-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#else
#include "sf_unistd.h"
#endif

#include "sndfile.h"
#include "utils.h"

static void format_error_test (void) ;
static void format_combo_test (void) ;

int
main (void)
{
	format_error_test () ;
	format_combo_test () ;

	return 0 ;
} /* main */

/*==============================================================================
*/

static void
format_error_test (void)
{	const char *filename = "format-error.wav" ;
	SNDFILE *file ;
	SF_INFO info ;

	print_test_name (__func__, NULL) ;

	memset (&info, 0, sizeof (info)) ;
	info.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16 ;
	info.channels = 1 ;
	info.samplerate = 44100 ;

	info.format = SF_FORMAT_WAV ;
	file = sf_open (filename, SFM_WRITE, &info) ;
	exit_if_true (file != NULL, "\n\nLine %d : Format should not be valid.\n\n", __LINE__) ;
	exit_if_true (
		strstr (sf_strerror (NULL), "minor format") == NULL,
		"\n\nLine %d : Error string should reference bad 'minor format'.\n\n", __LINE__
		) ;

	info.format = SF_FORMAT_PCM_16 ;
	file = sf_open (filename, SFM_WRITE, &info) ;
	exit_if_true (file != NULL, "\n\nLine %d : Format should not be valid.\n\n", __LINE__) ;
	exit_if_true (
		strstr (sf_strerror (NULL), "major format") == NULL,
		"\n\nLine %d : Error string should reference bad 'major format'.\n\n", __LINE__
		) ;

	unlink (filename) ;
	puts ("ok") ;
} /* format_error_test */

static void
format_combo_test (void)
{	int container_max, codec_max, cont, codec ;

	print_test_name (__func__, NULL) ;

	sf_command (NULL, SFC_GET_FORMAT_MAJOR_COUNT, &container_max, sizeof (container_max)) ;
	sf_command (NULL, SFC_GET_FORMAT_SUBTYPE_COUNT, &codec_max, sizeof (codec_max)) ;

	for (cont = 0 ; cont < container_max + 10 ; cont ++)
	{	SF_FORMAT_INFO major_fmt_info ;

		memset (&major_fmt_info, 0, sizeof (major_fmt_info)) ;
		major_fmt_info.format = cont ;
		(void) sf_command (NULL, SFC_GET_FORMAT_MAJOR, &major_fmt_info, sizeof (major_fmt_info)) ;

		for (codec = 0 ; codec < codec_max + 10 ; codec ++)
		{	SF_FORMAT_INFO subtype_fmt_info ;
			SNDFILE * sndfile ;
			SF_INFO info ;
			char filename [128] ;
			int subtype_is_valid, check_is_valid ;

			memset (&info, 0, sizeof (info)) ;
			memset (&subtype_fmt_info, 0, sizeof (subtype_fmt_info)) ;
			subtype_fmt_info.format = codec ;
			subtype_is_valid = sf_command (NULL, SFC_GET_FORMAT_SUBTYPE, &subtype_fmt_info, sizeof (subtype_fmt_info)) == 0 ;

			/* Opus only works with a fixed set of sample rates. */
			if (subtype_fmt_info.format == SF_FORMAT_OPUS)
				sf_info_setup (&info, major_fmt_info.format | subtype_fmt_info.format, 24000, 1) ;
			else
				sf_info_setup (&info, major_fmt_info.format | subtype_fmt_info.format, 22050, 1) ;

			check_is_valid = sf_format_check (&info) ;

			exit_if_true (
				NOT (subtype_is_valid) && check_is_valid,
				"\n\nLine %d : Subtype is not valid but checks ok.\n",
				__LINE__
				) ;

			snprintf (filename, sizeof (filename), "format-check.%s", major_fmt_info.extension) ;

			sndfile = sf_open (filename, SFM_WRITE, &info) ;

			sf_close (sndfile) ;
			unlink (filename) ;

			if (major_fmt_info.extension != NULL && strcmp (major_fmt_info.extension, "sd2") == 0)
			{	snprintf (filename, sizeof (filename), "._format-check.%s", major_fmt_info.extension) ;
				unlink (filename) ;
				} ;

			exit_if_true (
				sndfile && NOT (check_is_valid),
				"\n\nError : Format was not valid but file opened correctly.\n"
				"    Container : %s\n"
				"    Codec     : %s\n\n",
				major_fmt_info.name, subtype_fmt_info.name
				) ;

			exit_if_true (
				NOT (sndfile) && check_is_valid,
				"\n\nError : Format was valid but file failed to open.\n"
				"    Container : %s\n"
				"    Codec     : %s\n\n",
				major_fmt_info.name, subtype_fmt_info.name
				) ;
			} ;
		} ;

	puts ("ok") ;
} /* format_combo_test */

