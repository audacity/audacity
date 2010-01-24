/*
** Copyright (C) 2003-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <math.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include	<sndfile.h>

#include	"utils.h"

#define	BUFFER_LEN			(1 << 10)
#define LOG_BUFFER_SIZE		1024

static void	string_start_test (const char *filename, int typemajor) ;
static void	string_start_end_test (const char *filename, int typemajor) ;
static void	string_multi_set_test (const char *filename, int typemajor) ;
static void	string_rdwr_test (const char *filename, int typemajor) ;
static void	string_short_rdwr_test (const char *filename, int typemajor) ;

static int str_count (const char * haystack, const char * needle) ;

int
main (int argc, char *argv [])
{	int		do_all = 0 ;
	int		test_count = 0 ;

	if (argc != 2)
	{	printf ("Usage : %s <test>\n", argv [0]) ;
		printf ("    Where <test> is one of the following:\n") ;
		printf ("           wav  - test adding strings to WAV files\n") ;
		printf ("           aiff - test adding strings to AIFF files\n") ;
		printf ("           flac - test adding strings to FLAC files\n") ;
		printf ("           ogg  - test adding strings to OGG files\n") ;
		printf ("           all  - perform all tests\n") ;
		exit (1) ;
		} ;

	do_all =! strcmp (argv [1], "all") ;

	if (do_all || ! strcmp (argv [1], "wav"))
	{	string_start_end_test ("strings.wav", SF_FORMAT_WAV) ;
		string_multi_set_test ("multi.wav", SF_FORMAT_WAV) ;
		string_rdwr_test ("rdwr.wav", SF_FORMAT_WAV) ;
		string_short_rdwr_test ("short_rdwr.wav", SF_FORMAT_WAV) ;

		string_start_end_test ("strings.wavex", SF_FORMAT_WAVEX) ;
		string_multi_set_test ("multi.wavex", SF_FORMAT_WAVEX) ;
		string_rdwr_test ("rdwr.wavex", SF_FORMAT_WAVEX) ;
		string_short_rdwr_test ("short_rdwr.wavex", SF_FORMAT_WAVEX) ;

		string_start_end_test ("strings.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV) ;
		string_multi_set_test ("multi.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV) ;
		string_rdwr_test ("rdwr.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV) ;
		string_short_rdwr_test ("short_rdwr.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "aiff"))
	{	string_start_end_test ("strings.aiff", SF_FORMAT_AIFF) ;
		/*
		string_multi_set_test ("multi.aiff", SF_FORMAT_AIFF) ;
		string_rdwr_test ("rdwr.aiff", SF_FORMAT_AIFF) ;
		string_short_rdwr_test ("short_rdwr.aiff", SF_FORMAT_AIFF) ;
		*/
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "flac"))
	{	if (HAVE_EXTERNAL_LIBS)
			string_start_test ("strings.flac", SF_FORMAT_FLAC) ;
		else
			puts ("    No FLAC tests because FLAC support was not compiled in.") ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "ogg"))
	{	if (HAVE_EXTERNAL_LIBS)
			string_start_test ("vorbis.oga", SF_FORMAT_OGG) ;
		else
			puts ("    No Ogg/Vorbis tests because Ogg/Vorbis support was not compiled in.") ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "rf64"))
	{	puts ("\n\n     **** String test not working yet for RF64 format. ****\n") ;
		/*
		string_start_end_test ("strings.rf64", SF_FORMAT_RF64) ;
		string_multi_set_test ("multi.rf64", SF_FORMAT_RF64) ;
		string_rdwr_test ("rdwr.rf64", SF_FORMAT_RF64) ;
		string_short_rdwr_test ("short_rdwr.rf64", SF_FORMAT_RF64) ;
		*/
		test_count++ ;
		} ;

	if (test_count == 0)
	{	printf ("Mono : ************************************\n") ;
		printf ("Mono : *  No '%s' test defined.\n", argv [1]) ;
		printf ("Mono : ************************************\n") ;
		return 1 ;
		} ;

	return 0 ;
} /* main */


/*============================================================================================
**	Here are the test functions.
*/

static const char
	software	[]	= "software (libsndfile-X.Y.Z)",
	artist		[]	= "The Artist",
	copyright	[]	= "Copyright (c) 2001 Artist",
	comment		[]	= "Comment goes here!!!",
	date		[]	= "2001/01/27",
	album		[]	= "The Album",
	license		[]	= "The license",
	title		[]	= "This is the title",
	long_title	[]	= "This is a very long and very boring title for this file",
	long_artist	[]	= "The artist who kept on changing its name" ;


static	short	data_out [BUFFER_LEN] ;

static void
string_start_end_test (const char *filename, int typemajor)
{	const char	*cptr ;
	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int			frames, errors = 0 ;

	typemajor = typemajor ;

	print_test_name ("string_start_end_test", filename) ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.channels		= 1 ;
	sfinfo.frames		= 0 ;
	sfinfo.format		= typemajor | SF_FORMAT_PCM_16 ;

	frames = BUFFER_LEN / sfinfo.channels ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;

	/* Write stuff at start of file. */
	sf_set_string (file, SF_STR_TITLE, filename) ;
	sf_set_string (file, SF_STR_SOFTWARE, software) ;
	sf_set_string (file, SF_STR_ARTIST, artist) ;

	/* Write data to file. */
	test_write_short_or_die (file, 0, data_out, BUFFER_LEN, __LINE__) ;
	test_seek_or_die (file, 0, SEEK_SET, 0, sfinfo.channels, __LINE__) ;

	/* Write more stuff at end of file. */
	sf_set_string (file, SF_STR_COPYRIGHT, copyright) ;
	sf_set_string (file, SF_STR_COMMENT, comment) ;
	sf_set_string (file, SF_STR_DATE, date) ;
	sf_set_string (file, SF_STR_ALBUM, album) ;
	sf_set_string (file, SF_STR_LICENSE, license) ;

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	check_log_buffer_or_die (file, __LINE__) ;

	if (sfinfo.frames != BUFFER_LEN)
	{	printf ("***** Bad frame count %d (should be %d)\n\n", (int) sfinfo.frames, BUFFER_LEN) ;
		errors ++ ;
		} ;

	cptr = sf_get_string (file, SF_STR_TITLE) ;
	if (cptr == NULL || strcmp (filename, cptr) != 0)
	{	if (errors++ == 0)
			puts ("\n") ;
		printf ("    Bad filename  : %s\n", cptr) ;
		} ;

	cptr = sf_get_string (file, SF_STR_COPYRIGHT) ;
	if (cptr == NULL || strcmp (copyright, cptr) != 0)
	{	if (errors++ == 0)
			puts ("\n") ;
		printf ("    Bad copyright : %s\n", cptr) ;
		} ;

	cptr = sf_get_string (file, SF_STR_SOFTWARE) ;
	if (cptr == NULL || strstr (cptr, software) != cptr)
	{	if (errors++ == 0)
			puts ("\n") ;
		printf ("    Bad software  : %s\n", cptr) ;
		} ;

	if (str_count (cptr, "libsndfile") != 1)
	{	if (errors++ == 0)
			puts ("\n") ;
		printf ("    Bad software  : %s\n", cptr) ;
		} ;

	cptr = sf_get_string (file, SF_STR_ARTIST) ;
	if (cptr == NULL || strcmp (artist, cptr) != 0)
	{	if (errors++ == 0)
			puts ("\n") ;
		printf ("    Bad artist    : %s\n", cptr) ;
		} ;

	cptr = sf_get_string (file, SF_STR_COMMENT) ;
	if (cptr == NULL || strcmp (comment, cptr) != 0)
	{	if (errors++ == 0)
			puts ("\n") ;
		printf ("    Bad comment   : %s\n", cptr) ;
		} ;

	if (typemajor != SF_FORMAT_AIFF)
	{	cptr = sf_get_string (file, SF_STR_DATE) ;
		if (cptr == NULL || strcmp (date, cptr) != 0)
		{	if (errors++ == 0)
				puts ("\n") ;
			printf ("    Bad date      : %s\n", cptr) ;
			} ;
		} ;

	switch (typemajor)
	{	case SF_FORMAT_AIFF :
		case SF_FORMAT_WAV :
		case SF_FORMAT_WAVEX :
		case SF_ENDIAN_BIG | SF_FORMAT_WAV :
			break ;

		default :
			cptr = sf_get_string (file, SF_STR_ALBUM) ;
			if (cptr == NULL || strcmp (album, cptr) != 0)
			{	if (errors++ == 0)
					puts ("\n") ;
				printf ("    Bad album   : %s\n", cptr) ;
				} ;

			cptr = sf_get_string (file, SF_STR_LICENSE) ;
			if (cptr == NULL || strcmp (license, cptr) != 0)
			{	if (errors++ == 0)
					puts ("\n") ;
				printf ("    Bad license : %s\n", cptr) ;
				} ;

			break ;
		} ;

	if (errors > 0)
	{	printf ("\n*** Error count : %d ***\n\n", errors) ;
		dump_log_buffer (file) ;
		exit (1) ;
		} ;

	sf_close (file) ;
	unlink (filename) ;

	puts ("ok") ;
} /* string_start_end_test */

static void
string_start_test (const char *filename, int typemajor)
{	const char	*cptr ;
	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int			frames, errors = 0 ;

	print_test_name ("string_start_test", filename) ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.channels		= 1 ;
	sfinfo.frames		= 0 ;

	switch (typemajor)
	{	case SF_FORMAT_OGG :
			sfinfo.format = typemajor | SF_FORMAT_VORBIS ;
			break ;

		default :
			sfinfo.format = typemajor | SF_FORMAT_PCM_16 ;
			break ;
		} ;

	frames = BUFFER_LEN / sfinfo.channels ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;

	/* Write stuff at start of file. */
	sf_set_string (file, SF_STR_TITLE, filename) ;
	sf_set_string (file, SF_STR_SOFTWARE, software) ;
	sf_set_string (file, SF_STR_ARTIST, artist) ;
	sf_set_string (file, SF_STR_COPYRIGHT, copyright) ;
	sf_set_string (file, SF_STR_COMMENT, comment) ;
	sf_set_string (file, SF_STR_DATE, date) ;
	sf_set_string (file, SF_STR_ALBUM, album) ;
	sf_set_string (file, SF_STR_LICENSE, license) ;

	/* Write data to file. */
	test_write_short_or_die (file, 0, data_out, BUFFER_LEN, __LINE__) ;

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	check_log_buffer_or_die (file, __LINE__) ;

	if (sfinfo.frames != BUFFER_LEN)
	{	printf ("***** Bad frame count %d (should be %d)\n\n", (int) sfinfo.frames, BUFFER_LEN) ;
		errors ++ ;
		} ;

	cptr = sf_get_string (file, SF_STR_TITLE) ;
	if (cptr == NULL || strcmp (filename, cptr) != 0)
	{	if (errors++ == 0)
			puts ("\n") ;
		printf ("    Bad filename  : %s\n", cptr) ;
		} ;

	cptr = sf_get_string (file, SF_STR_COPYRIGHT) ;
	if (cptr == NULL || strcmp (copyright, cptr) != 0)
	{	if (errors++ == 0)
			puts ("\n") ;
		printf ("    Bad copyright : %s\n", cptr) ;
		} ;

	cptr = sf_get_string (file, SF_STR_SOFTWARE) ;
	if (cptr == NULL || strstr (cptr, software) != cptr)
	{	if (errors++ == 0)
			puts ("\n") ;
		printf ("    Bad software  : %s\n", cptr) ;
		} ;

	if (str_count (cptr, "libsndfile") != 1)
	{	if (errors++ == 0)
			puts ("\n") ;
		printf ("    Bad software  : %s\n", cptr) ;
		} ;

	cptr = sf_get_string (file, SF_STR_ARTIST) ;
	if (cptr == NULL || strcmp (artist, cptr) != 0)
	{	if (errors++ == 0)
			puts ("\n") ;
		printf ("    Bad artist    : %s\n", cptr) ;
		} ;

	cptr = sf_get_string (file, SF_STR_COMMENT) ;
	if (cptr == NULL || strcmp (comment, cptr) != 0)
	{	if (errors++ == 0)
			puts ("\n") ;
		printf ("    Bad comment   : %s\n", cptr) ;
		} ;

	if (typemajor != SF_FORMAT_AIFF)
	{	cptr = sf_get_string (file, SF_STR_DATE) ;
		if (cptr == NULL || strcmp (date, cptr) != 0)
		{	if (errors++ == 0)
				puts ("\n") ;
			printf ("    Bad date      : %s\n", cptr) ;
			} ;
		} ;

	if (typemajor != SF_FORMAT_WAV && typemajor != SF_FORMAT_AIFF)
	{	cptr = sf_get_string (file, SF_STR_ALBUM) ;
		if (cptr == NULL || strcmp (album, cptr) != 0)
		{	if (errors++ == 0)
				puts ("\n") ;
			printf ("    Bad album     : %s\n", cptr) ;
			} ;
		} ;

	if (typemajor != SF_FORMAT_WAV && typemajor != SF_FORMAT_AIFF)
	{	cptr = sf_get_string (file, SF_STR_LICENSE) ;
		if (cptr == NULL || strcmp (license, cptr) != 0)
		{	if (errors++ == 0)
				puts ("\n") ;
			printf ("    Bad license   : %s\n", cptr) ;
			} ;
		} ;

	if (errors > 0)
	{	printf ("\n*** Error count : %d ***\n\n", errors) ;
		dump_log_buffer (file) ;
		exit (1) ;
		} ;

	sf_close (file) ;
	unlink (filename) ;

	puts ("ok") ;
} /* string_start_test */

static void
string_multi_set_test (const char *filename, int typemajor)
{	static const char
		new_software	[]	= "new software (libsndfile-X.Y.Z)",
		new_copyright	[]	= "Copyright (c) 2001 New Artist",
		new_artist		[]	= "The New Artist",
		new_title		[]	= "This is the new title" ;

	static char buffer [2048] ;
	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	int			count ;

	print_test_name (__func__, filename) ;

	sfinfo.format		= typemajor | SF_FORMAT_PCM_16 ;
	sfinfo.samplerate	= 44100 ;
	sfinfo.channels		= 1 ;
	sfinfo.frames		= 0 ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;

	/* Write stuff at start of file. */
	sf_set_string (file, SF_STR_TITLE, title) ;
	sf_set_string (file, SF_STR_SOFTWARE, software) ;
	sf_set_string (file, SF_STR_ARTIST, artist) ;

	/* Write data to file. */
	test_write_short_or_die (file, 0, data_out, BUFFER_LEN, __LINE__) ;

	/* Write it all again. */

	sf_set_string (file, SF_STR_TITLE, new_title) ;
	sf_set_string (file, SF_STR_SOFTWARE, new_software) ;
	sf_set_string (file, SF_STR_ARTIST, new_artist) ;

	sf_set_string (file, SF_STR_COPYRIGHT, copyright) ;
	sf_set_string (file, SF_STR_COMMENT, comment) ;
	sf_set_string (file, SF_STR_DATE, date) ;
	sf_set_string (file, SF_STR_ALBUM, album) ;
	sf_set_string (file, SF_STR_LICENSE, license) ;
	sf_set_string (file, SF_STR_COPYRIGHT, new_copyright) ;
	sf_set_string (file, SF_STR_COMMENT, comment) ;
	sf_set_string (file, SF_STR_DATE, date) ;
	sf_set_string (file, SF_STR_ALBUM, album) ;
	sf_set_string (file, SF_STR_LICENSE, license) ;

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	sf_command	(file, SFC_GET_LOG_INFO, buffer, sizeof (buffer)) ;
	sf_close (file) ;

	count = str_count (buffer, new_title) ;
	exit_if_true (count < 1, "\n\nLine %d : Could not find new_title in :\n%s\n", __LINE__, buffer) ;
	exit_if_true (count > 1, "\n\nLine %d : new_title appears %d times in :\n\n%s\n", __LINE__, count, buffer) ;

	count = str_count (buffer, software) ;
	exit_if_true (count < 1, "\n\nLine %d : Could not find new_software in :\n%s\n", __LINE__, buffer) ;
	exit_if_true (count > 1, "\n\nLine %d : new_software appears %d times in :\n\n%s\n", __LINE__, count, buffer) ;

	count = str_count (buffer, new_artist) ;
	exit_if_true (count < 1, "\n\nLine %d : Could not find new_artist in :\n%s\n", __LINE__, buffer) ;
	exit_if_true (count > 1, "\n\nLine %d : new_artist appears %d times in :\n\n%s\n", __LINE__, count, buffer) ;

	count = str_count (buffer, new_copyright) ;
	exit_if_true (count < 1, "\n\nLine %d : Could not find new_copyright in :\n%s\n", __LINE__, buffer) ;
	exit_if_true (count > 1, "\n\nLine %d : new_copyright appears %d times in :\n\n%s\n", __LINE__, count, buffer) ;

	unlink (filename) ;

	puts ("ok") ;
} /* string_multi_set_test */

static void
string_rdwr_test (const char *filename, int typemajor)
{	SNDFILE *file ;
	SF_INFO sfinfo ;
	sf_count_t frames ;
	const char * str ;

	print_test_name (__func__, filename) ;
	create_short_sndfile (filename, typemajor | SF_FORMAT_PCM_16, 2) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_FALSE, __LINE__) ;
	frames = sfinfo.frames ;
	sf_set_string (file, SF_STR_TITLE, title) ;
	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;
	exit_if_true (frames != sfinfo.frames, "\n\nLine %d : Frame count %lld should be %lld.\n", __LINE__, sfinfo.frames, frames) ;
	str = sf_get_string (file, SF_STR_TITLE) ;
	exit_if_true (str == NULL, "\n\nLine %d : SF_STR_TITLE string is NULL.\n", __LINE__) ;
	exit_if_true (strcmp (str, title) != 0, "\n\nLine %d : SF_STR_TITLE doesn't match what was written.\n", __LINE__) ;
	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_FALSE, __LINE__) ;
	frames = sfinfo.frames ;
	sf_set_string (file, SF_STR_TITLE, title) ;
	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_FALSE, __LINE__) ;
	str = sf_get_string (file, SF_STR_TITLE) ;
	exit_if_true (str == NULL, "\n\nLine %d : SF_STR_TITLE string is NULL.\n", __LINE__) ;
	sf_set_string (file, SF_STR_ARTIST, artist) ;
	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	str = sf_get_string (file, SF_STR_ARTIST) ;
	exit_if_true (str == NULL, "\n\nLine %d : SF_STR_ARTIST string is NULL.\n", __LINE__) ;
	exit_if_true (strcmp (str, artist) != 0, "\n\nLine %d : SF_STR_ARTIST doesn't match what was written.\n", __LINE__) ;

	str = sf_get_string (file, SF_STR_TITLE) ;
	exit_if_true (str == NULL, "\n\nLine %d : SF_STR_TITLE string is NULL.\n", __LINE__) ;
	exit_if_true (strcmp (str, title) != 0, "\n\nLine %d : SF_STR_TITLE doesn't match what was written.\n", __LINE__) ;

	exit_if_true (frames != sfinfo.frames, "\n\nLine %d : Frame count %lld should be %lld.\n", __LINE__, sfinfo.frames, frames) ;

	sf_close (file) ;
	unlink (filename) ;

	puts ("ok") ;
} /* string_rdwr_test */

static void
string_short_rdwr_test (const char *filename, int typemajor)
{	SNDFILE *file ;
	SF_INFO sfinfo ;
	sf_count_t frames = BUFFER_LEN ;
	const char * str ;

	print_test_name (__func__, filename) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	sfinfo.format		= typemajor | SF_FORMAT_PCM_16 ;
	sfinfo.samplerate	= 44100 ;
	sfinfo.channels		= 1 ;
	sfinfo.frames		= 0 ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_FALSE, __LINE__) ;

	/* Write data to file. */
	test_write_short_or_die (file, 0, data_out, BUFFER_LEN, __LINE__) ;

	sf_set_string (file, SF_STR_TITLE, long_title) ;
	sf_set_string (file, SF_STR_ARTIST, long_artist) ;
	sf_close (file) ;

	/* Open the file RDWR. */
	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_FALSE, __LINE__) ;
	exit_if_true (frames != sfinfo.frames, "\n\nLine %d : Frame count %lld should be %lld.\n", __LINE__, sfinfo.frames, frames) ;
	str = sf_get_string (file, SF_STR_TITLE) ;
	exit_if_true (str == NULL, "\n\nLine %d : SF_STR_TITLE string is NULL.\n", __LINE__) ;
	exit_if_true (strcmp (str, long_title) != 0, "\n\nLine %d : SF_STR_TITLE doesn't match what was written.\n", __LINE__) ;
	str = sf_get_string (file, SF_STR_ARTIST) ;
	exit_if_true (str == NULL, "\n\nLine %d : SF_STR_TITLE string is NULL.\n", __LINE__) ;
	exit_if_true (strcmp (str, long_artist) != 0, "\n\nLine %d : SF_STR_ARTIST doesn't match what was written.\n", __LINE__) ;

	/* Change title and artist. */
	sf_set_string (file, SF_STR_TITLE, title) ;
	sf_set_string (file, SF_STR_ARTIST, artist) ;

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_FALSE, __LINE__) ;

	check_log_buffer_or_die (file, __LINE__) ;

	str = sf_get_string (file, SF_STR_TITLE) ;
	exit_if_true (str == NULL, "\n\nLine %d : SF_STR_TITLE string is NULL.\n", __LINE__) ;
	exit_if_true (strcmp (str, title) != 0, "\n\nLine %d : SF_STR_TITLE doesn't match what was written.\n", __LINE__) ;

	str = sf_get_string (file, SF_STR_ARTIST) ;
	exit_if_true (str == NULL, "\n\nLine %d : SF_STR_ARTIST string is NULL.\n", __LINE__) ;
	exit_if_true (strcmp (str, artist) != 0, "\n\nLine %d : SF_STR_ARTIST doesn't match what was written.\n", __LINE__) ;

	sf_close (file) ;
	unlink (filename) ;

	puts ("ok") ;
} /* string_short_rdwr_test */

static int
str_count (const char * haystack, const char * needle)
{	int count = 0 ;

	while ((haystack = strstr (haystack, needle)) != NULL)
	{	count ++ ;
		haystack ++ ;
		} ;

	return count ;
} /* str_count */

