/*
** Copyright (C) 2005-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#if HAVE_LOCALE_H
#include <locale.h>
#endif

#if OS_IS_WIN32
#include <windows.h>
#define ENABLE_SNDFILE_WINDOWS_PROTOTYPES 1
#endif

#include "sndfile.h"
#include "utils.h"

static void utf8_test (void) ;
static void wchar_test (void) ;

int
main (void)
{
	utf8_test () ;
	wchar_test () ;

	return 0 ;
} /* main */

/*==============================================================================
*/

static void
wchar_test (void)
{
#if OS_IS_WIN32
	SNDFILE * file ;
	SF_INFO info ;
	LPCWSTR filename = L"test.wav" ;

	print_test_name (__func__, "test.wav") ;

	info.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16 ;
	info.channels = 1 ;
	info.samplerate = 44100 ;

	file = sf_wchar_open (filename, SFM_WRITE, &info) ;
	exit_if_true (file == NULL, "\n\nLine %d : sf_wchar_open failed : %s\n\n", __LINE__, sf_strerror (NULL)) ;
	sf_close (file) ;

	/*	This should check that the file did in fact get created with a
	**	wchar_t * filename.
	*/
	exit_if_true (
		GetFileAttributesW (filename) == INVALID_FILE_ATTRIBUTES,
		"\n\nLine %d : GetFileAttributes failed.\n\n", __LINE__
		) ;

	/* Use this because the file was created with CreateFileW. */
	DeleteFileW (filename) ;

	puts ("ok") ;
#endif
} /* wchar_test */

/*==============================================================================
*/

typedef struct
{	const char *locale ;
	int utf8 ;
	const char *filename ;
	int	width ;
} LOCALE_DATA ;

static void locale_test (const LOCALE_DATA * locdata) ;

static void
utf8_test (void)
{	LOCALE_DATA ldata [] =
	{	{	"de_DE", 1, "F\303\274\303\237e.au", 7 },
		{	"en_AU", 1, "kangaroo.au", 11 },
		{	"POSIX", 0, "posix.au", 8 },
		{	"pt_PT", 1, "concei\303\247\303\243o.au", 12 },

#if OS_IS_WIN32 == 0
		{	"ja_JP", 1, "\343\201\212\343\201\257\343\202\210\343\201\206\343\201\224\343\201\226\343\201\204\343\201\276\343\201\231.au", 21 },
#endif

		{	"vi_VN", 1, "qu\341\273\221c ng\341\273\257.au", 11 },
		{	NULL, 0, NULL, 0 }
		} ;
	int k ;

	for (k = 0 ; ldata [k].locale != NULL ; k++)
		locale_test (ldata + k) ;
} /* utf8_test */


static void
locale_test (const LOCALE_DATA * ldata)
{
#if (HAVE_LOCALE_H == 0 || HAVE_SETLOCALE == 0)
	locname = filename = NULL ;
	width = 0 ;
	return ;
#else
	const short wdata [] = { 1, 2, 3, 4, 5, 6, 7, 8 } ;
	short rdata [ARRAY_LEN (wdata)] ;
	const char *old_locale ;
	char utf8_locname [32] ;
	SNDFILE *file ;
	SF_INFO sfinfo ;

	snprintf (utf8_locname, sizeof (utf8_locname), "%s%s", ldata->locale, ldata->utf8 ? ".UTF-8" : "") ;

	/* Change the locale saving the old one. */
	if ((old_locale = setlocale (LC_CTYPE, utf8_locname)) == NULL)
		return ;

	printf ("    locale_test           %-8s : %s %*c ", ldata->locale, ldata->filename, 24 - ldata->width, ' ') ;
	fflush (stdout) ;

	sfinfo.format = SF_FORMAT_AU | SF_FORMAT_PCM_16 ;
	sfinfo.channels = 1 ;
	sfinfo.samplerate = 44100 ;

	file = test_open_file_or_die (ldata->filename, SFM_WRITE, &sfinfo, 0, __LINE__) ;
	test_write_short_or_die (file, 0, wdata, ARRAY_LEN (wdata), __LINE__) ;
	sf_close (file) ;

	file = test_open_file_or_die (ldata->filename, SFM_READ, &sfinfo, 0, __LINE__) ;
	test_read_short_or_die (file, 0, rdata, ARRAY_LEN (rdata), __LINE__) ;
	sf_close (file) ;

	unlink (ldata->filename) ;

	/* Restore old locale. */
	setlocale (LC_CTYPE, old_locale) ;

	puts ("ok") ;
#endif
} /* locale_test */

