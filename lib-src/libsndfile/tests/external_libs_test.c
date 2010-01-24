/*
** Copyright (C) 2008-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software ; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation ; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY ; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program ; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <sndfile.h>

#include "utils.h"

static void major_format_test (void) ;
static void subtype_format_test (void) ;
static void simple_format_test (void) ;

int
main (void)
{
	major_format_test () ;
	subtype_format_test () ;
	simple_format_test () ;

	return 0 ;
} /* main */

static void
major_format_test (void)
{	SF_FORMAT_INFO	info ;
	int have_ogg = 0, have_flac = 0 ;
	int m, major_count ;

	print_test_name (__func__, NULL) ;

	sf_command (NULL, SFC_GET_FORMAT_MAJOR_COUNT, &major_count, sizeof (int)) ;

	for (m = 0 ; m < major_count ; m++)
	{	info.format = m ;
		sf_command (NULL, SFC_GET_FORMAT_MAJOR, &info, sizeof (info)) ;

		have_flac = info.format == SF_FORMAT_FLAC ? 1 : have_flac ;
		have_ogg = info.format == SF_FORMAT_OGG ? 1 : have_ogg ;
		} ;

	if (HAVE_EXTERNAL_LIBS)
	{	exit_if_true (have_flac == 0, "\n\nLine %d : FLAC should be available.\n\n", __LINE__) ;
		exit_if_true (have_ogg == 0, "\n\nLine %d : Ogg/Vorbis should be available.\n\n", __LINE__) ;
		}
	else
	{	exit_if_true (have_flac, "\n\nLine %d : FLAC should not be available.\n\n", __LINE__) ;
		exit_if_true (have_ogg, "\n\nLine %d : Ogg/Vorbis should not be available.\n\n", __LINE__) ;
		} ;

	puts ("ok") ;
} /* major_format_test */

static void
subtype_format_test (void)
{	SF_FORMAT_INFO	info ;
	int have_vorbis = 0 ;
	int s, subtype_count ;

	print_test_name (__func__, NULL) ;

	sf_command (NULL, SFC_GET_FORMAT_SUBTYPE_COUNT, &subtype_count, sizeof (int)) ;

	for (s = 0 ; s < subtype_count ; s++)
	{	info.format = s ;
		sf_command (NULL, SFC_GET_FORMAT_SUBTYPE, &info, sizeof (info)) ;

		have_vorbis = info.format == SF_FORMAT_VORBIS ? 1 : have_vorbis ;
		} ;

	if (HAVE_EXTERNAL_LIBS)
		exit_if_true (have_vorbis == 0, "\n\nLine %d : Ogg/Vorbis should be available.\n\n", __LINE__) ;
	else
		exit_if_true (have_vorbis, "\n\nLine %d : Ogg/Vorbis should not be available.\n\n", __LINE__) ;

	puts ("ok") ;
} /* subtype_format_test */

static void
simple_format_test (void)
{	SF_FORMAT_INFO	info ;
	int have_flac = 0, have_ogg = 0, have_vorbis = 0 ;
	int s, simple_count ;

	print_test_name (__func__, NULL) ;

	sf_command (NULL, SFC_GET_SIMPLE_FORMAT_COUNT, &simple_count, sizeof (int)) ;

	for (s = 0 ; s < simple_count ; s++)
	{	info.format = s ;
		sf_command (NULL, SFC_GET_SIMPLE_FORMAT, &info, sizeof (info)) ;

		switch (info.format & SF_FORMAT_TYPEMASK)
		{	case SF_FORMAT_FLAC :
				have_flac = 1 ;
				break ;

			case SF_FORMAT_OGG :
				have_ogg = 1 ;
				break ;

			default :
				break ;
			} ;

		switch (info.format & SF_FORMAT_SUBMASK)
		{	case SF_FORMAT_VORBIS :
				have_vorbis = 1 ;
				break ;

			default :
				break ;
			} ;

		} ;

	if (HAVE_EXTERNAL_LIBS)
	{	exit_if_true (have_flac == 0, "\n\nLine %d : FLAC should be available.\n\n", __LINE__) ;
		exit_if_true (have_ogg == 0, "\n\nLine %d : Ogg/Vorbis should be available.\n\n", __LINE__) ;
		exit_if_true (have_vorbis == 0, "\n\nLine %d : Ogg/Vorbis should be available.\n\n", __LINE__) ;
		}
	else
	{	exit_if_true (have_flac, "\n\nLine %d : FLAC should not be available.\n\n", __LINE__) ;
		exit_if_true (have_ogg, "\n\nLine %d : Ogg/Vorbis should not be available.\n\n", __LINE__) ;
		exit_if_true (have_vorbis, "\n\nLine %d : Ogg/Vorbis should not be available.\n\n", __LINE__) ;
		} ;

	puts ("ok") ;
} /* simple_format_test */
