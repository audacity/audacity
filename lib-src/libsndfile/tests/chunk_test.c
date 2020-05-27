/*
** Copyright (C) 2003-2016 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#else
#include "sf_unistd.h"
#endif

#include	<sndfile.h>

#include	"sfendian.h"
#include	"utils.h"

#define	BUFFER_LEN			(1 << 10)
#define LOG_BUFFER_SIZE		1024

static void	chunk_test (const char *filename, int format) ;
static void wav_subchunk_test (size_t chunk_size) ;
static void	large_free_test (const char *filename, int format, size_t chunk_size) ;

int
main (int argc, char *argv [])
{	int		do_all = 0 ;
	int		test_count = 0, k ;

	if (argc != 2)
	{	printf ("Usage : %s <test>\n", argv [0]) ;
		printf ("    Where <test> is one of the following:\n") ;
		printf ("           wav  - test adding chunks to WAV files\n") ;
		printf ("           aiff - test adding chunks to AIFF files\n") ;
		printf ("           caf  - test adding chunks to CAF files\n") ;
		printf ("           rf64 - test adding chunks to RF64 files\n") ;
		printf ("           all  - perform all tests\n") ;
		exit (1) ;
		} ;

	do_all = ! strcmp (argv [1], "all") ;

	if (do_all || ! strcmp (argv [1], "wav"))
	{	chunk_test ("chunks_pcm16.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_16) ;
		chunk_test ("chunks_pcm16.rifx", SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_PCM_16) ;
		chunk_test ("chunks_pcm16.wavex", SF_FORMAT_WAVEX | SF_FORMAT_PCM_16) ;

		for (k = 100 ; k < 10000 ; k *= 4)
			wav_subchunk_test (k) ;

		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "aiff"))
	{	chunk_test ("chunks_pcm16.aiff", SF_FORMAT_AIFF | SF_FORMAT_PCM_16) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "caf"))
	{	chunk_test ("chunks_pcm16.caf", SF_FORMAT_CAF | SF_FORMAT_PCM_16) ;
		chunk_test ("chunks_alac.caf", SF_FORMAT_CAF | SF_FORMAT_ALAC_16) ;
		large_free_test ("large_free.caf", SF_FORMAT_CAF | SF_FORMAT_PCM_16, 100) ;
		large_free_test ("large_free.caf", SF_FORMAT_CAF | SF_FORMAT_PCM_16, 20000) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "rf64"))
	{	chunk_test ("chunks_pcm16.rf64", SF_FORMAT_RF64 | SF_FORMAT_PCM_16) ;
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

static void
chunk_test_helper (const char *filename, int format, const char * testdata)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	SF_CHUNK_INFO	chunk_info ;
	SF_CHUNK_ITERATOR * iterator ;
	uint32_t		length_before ;
	int				err, allow_fd ;

	switch (format & SF_FORMAT_SUBMASK)
	{	case SF_FORMAT_ALAC_16 :
			allow_fd = SF_FALSE ;
			break ;
		default :
			allow_fd = SF_TRUE ;
			break ;
		} ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.channels		= 1 ;
	sfinfo.frames		= 0 ;
	sfinfo.format		= format ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, allow_fd, __LINE__) ;

	/* Set up the chunk to write. */
	memset (&chunk_info, 0, sizeof (chunk_info)) ;
	snprintf (chunk_info.id, sizeof (chunk_info.id), "Test") ;
	chunk_info.id_size = 4 ;
	chunk_info.data = strdup (testdata) ;
	chunk_info.datalen = strlen (chunk_info.data) ;

	length_before = chunk_info.datalen ;

	err = sf_set_chunk (file, &chunk_info) ;
	exit_if_true (
		err != SF_ERR_NO_ERROR,
		"\n\nLine %d : sf_set_chunk returned for testdata '%s' : %s\n\n", __LINE__, testdata, sf_error_number (err)
		) ;

	memset (chunk_info.data, 0, chunk_info.datalen) ;
	free (chunk_info.data) ;

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, allow_fd, __LINE__) ;

	memset (&chunk_info, 0, sizeof (chunk_info)) ;
	snprintf (chunk_info.id, sizeof (chunk_info.id), "Test") ;
	chunk_info.id_size = 4 ;

	iterator = sf_get_chunk_iterator (file, &chunk_info) ;
	err = sf_get_chunk_size (iterator, &chunk_info) ;
	exit_if_true (
		err != SF_ERR_NO_ERROR,
		"\n\nLine %d : sf_get_chunk_size returned for testdata '%s' : %s\n\n", __LINE__, testdata, sf_error_number (err)
		) ;

	exit_if_true (
		length_before > chunk_info.datalen || chunk_info.datalen - length_before > 4,
		"\n\nLine %d : testdata '%s' : Bad chunk length %u (previous length %u)\n\n", __LINE__, testdata, chunk_info.datalen, length_before
		) ;

	chunk_info.data = malloc (chunk_info.datalen) ;
	err = sf_get_chunk_data (iterator, &chunk_info) ;
	exit_if_true (
		err != SF_ERR_NO_ERROR,
		"\n\nLine %d : sf_get_chunk_size returned for testdata '%s' : %s\n\n", __LINE__, testdata, sf_error_number (err)
		) ;

	exit_if_true (
		memcmp (testdata, chunk_info.data, length_before),
		"\n\nLine %d : Data compare failed.\n    %s\n    %s\n\n", __LINE__, testdata, (char*) chunk_info.data
		) ;

	free (chunk_info.data) ;

	sf_close (file) ;
	unlink (filename) ;
} /* chunk_test_helper */

static void
multichunk_test_helper (const char *filename, int format, const char * testdata [], size_t testdata_len)
{	SNDFILE			*file ;
	SF_INFO			sfinfo ;
	SF_CHUNK_INFO	chunk_info ;
	SF_CHUNK_ITERATOR * iterator ;
	uint32_t		length_before [16] ;
	int				err, allow_fd ;
	size_t			i ;


	exit_if_true (
		ARRAY_LEN (length_before) < testdata_len,
		"\n\nLine %d : Bad array length.\n\n", __LINE__
		) ;


	sfinfo.samplerate	= 44100 ;
	sfinfo.channels		= 1 ;
	sfinfo.frames		= 0 ;
	sfinfo.format		= format ;

	switch (format & SF_FORMAT_SUBMASK)
	{	case SF_FORMAT_ALAC_16 :
			allow_fd = SF_FALSE ;
			break ;
		default :
			allow_fd = SF_TRUE ;
			break ;
		} ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, allow_fd, __LINE__) ;

	/* Set up the chunk to write. */
	for (i = 0 ; i < testdata_len ; i++)
	{	memset (&chunk_info, 0, sizeof (chunk_info)) ;
		snprintf (chunk_info.id, sizeof (chunk_info.id), "Test") ;
		chunk_info.id_size = 4 ;

		chunk_info.data = strdup (testdata [i]) ;
		chunk_info.datalen = strlen (chunk_info.data) ;

		length_before [i] = chunk_info.datalen ;

		err = sf_set_chunk (file, &chunk_info) ;
		exit_if_true (
			err != SF_ERR_NO_ERROR,
			"\n\nLine %d : sf_set_chunk returned for testdata[%d] '%s' : %s\n\n", __LINE__, (int) i, testdata [i], sf_error_number (err)
			) ;

		memset (chunk_info.data, 0, chunk_info.datalen) ;
		free (chunk_info.data) ;
	}

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, allow_fd, __LINE__) ;

	memset (&chunk_info, 0, sizeof (chunk_info)) ;
	snprintf (chunk_info.id, sizeof (chunk_info.id), "Test") ;
	chunk_info.id_size = 4 ;

	iterator = sf_get_chunk_iterator (file, &chunk_info) ;

	i = 0 ;
	while (iterator)
	{	memset (&chunk_info, 0, sizeof (chunk_info)) ;
		err = sf_get_chunk_size (iterator, &chunk_info) ;
		exit_if_true (
			i > testdata_len,
			"\n\nLine %d : iterated to chunk #%d, but only %d chunks have been written\n\n", __LINE__, (int) i, (int) testdata_len
			) ;

		exit_if_true (
			err != SF_ERR_NO_ERROR,
			"\n\nLine %d : sf_get_chunk_size returned for testdata[%d] '%s' : %s\n\n", __LINE__, (int) i, testdata [i], sf_error_number (err)
			) ;

		exit_if_true (
			length_before [i] > chunk_info.datalen || chunk_info.datalen - length_before [i] > 4,
			"\n\nLine %d : testdata[%d] '%s' : Bad chunk length %u (previous length %u)\n\n", __LINE__, (int) i, testdata [i], chunk_info.datalen, length_before [i]
			) ;

		chunk_info.data = malloc (chunk_info.datalen) ;
		err = sf_get_chunk_data (iterator, &chunk_info) ;
		exit_if_true (
			err != SF_ERR_NO_ERROR,
			"\n\nLine %d : sf_get_chunk_size returned for testdata[%d] '%s' : %s\n\n", __LINE__, (int) i, testdata [i], sf_error_number (err)
			) ;

		exit_if_true (
			4 != chunk_info.id_size,
			"\n\nLine %d : testdata[%d] : Bad ID length %u (previous length %u)\n\n", __LINE__, (int) i, chunk_info.id_size, 4
			) ;
		exit_if_true (
			memcmp ("Test", chunk_info.id, 4),
			"\n\nLine %d : ID compare failed at %d.\n    %s\n    %s\n\n", __LINE__, (int) i, "Test", (char*) chunk_info.id
			) ;

		exit_if_true (
			memcmp (testdata [i], chunk_info.data, length_before [i]),
			"\n\nLine %d : Data compare failed at %d.\n    %s\n    %s\n\n", __LINE__, (int) i, testdata [i], (char*) chunk_info.data
			) ;

		free (chunk_info.data) ;
		iterator = sf_next_chunk_iterator (iterator) ;
		i++ ;
	}

	sf_close (file) ;
	unlink (filename) ;
} /* multichunk_test_helper */


static void
chunk_test (const char *filename, int format)
{	const char*		testdata [] =
	{	"There can be only one.", "", "A", "AB", "ABC", "ABCD", "ABCDE" } ;
	uint32_t k ;

	print_test_name (__func__, filename) ;

	for (k = 0 ; k < ARRAY_LEN (testdata) ; k++)
		chunk_test_helper (filename, format, testdata [k]) ;

	multichunk_test_helper (filename, format, testdata, ARRAY_LEN (testdata)) ;

	puts ("ok") ;
} /* chunk_test */


static void
wav_subchunk_test (size_t chunk_size)
{	SNDFILE 		* file ;
	SF_INFO			sfinfo ;
	SF_CHUNK_INFO	chunk_info ;
	char filename [256] ;
	char chunk_data [10240] ;
	short audio [16] ;
	int	err, value ;

	snprintf (filename, sizeof (filename), "subchunk_%04d.wav", (int) chunk_size) ;
	print_test_name (__func__, filename) ;

	exit_if_true (sizeof (chunk_data) < chunk_size, "\n\nLine %d : sizeof (data) < chunk_size\n\n", __LINE__) ;

	memset (chunk_data, 53, sizeof (chunk_data)) ;
	chunk_data [chunk_size] = 0 ;

	/* Fill in the chunk data. */
	value = MAKE_MARKER ('a', 'd', 't', 'l') ;
	memcpy (chunk_data, &value, sizeof (value)) ;
	value = MAKE_MARKER ('n', 'o', 't', 'e') ;
	memcpy (chunk_data + 4, &value, sizeof (value)) ;
	value = H2LE_32 (chunk_size - 12) ;
	memcpy (chunk_data + 8, &value, sizeof (value)) ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.channels		= 1 ;
	sfinfo.frames		= 0 ;
	sfinfo.format		= SF_FORMAT_WAV | SF_FORMAT_PCM_16 ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;

	/* Set up the chunk to write. */
	memset (&chunk_info, 0, sizeof (chunk_info)) ;
	snprintf (chunk_info.id, sizeof (chunk_info.id), "LIST") ;
	chunk_info.id_size = 4 ;
	chunk_info.data = chunk_data ;
	chunk_info.datalen = chunk_size ;

	err = sf_set_chunk (file, &chunk_info) ;
	exit_if_true (
		err != SF_ERR_NO_ERROR,
		"\n\nLine %d : sf_set_chunk returned for testdata : %s\n\n", __LINE__, sf_error_number (err)
		) ;

	memset (chunk_info.data, 0, chunk_info.datalen) ;

	/* Add some audio data. */
	memset (audio, 0, sizeof (audio)) ;
	sf_write_short (file, audio, ARRAY_LEN (audio)) ;

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	exit_if_true (
		sfinfo.frames != ARRAY_LEN (audio),
		"\n\nLine %d : Incorrect sample count (%d should be %d)\n", __LINE__, (int) sfinfo.frames, (int) ARRAY_LEN (audio)
		) ;

	if (chunk_size < 512)
		check_log_buffer_or_die (file, __LINE__) ;

	sf_close (file) ;

	unlink (filename) ;
	puts ("ok") ;
} /* wav_subchunk_test */

static void
large_free_test (const char *filename, int format, size_t chunk_size)
{	SNDFILE 		* file ;
	SF_INFO			sfinfo ;
	SF_CHUNK_INFO	chunk_info ;
	char chunk_data [20002] ;
	short audio [16] ;
	int	err ;

	print_test_name (__func__, filename) ;

	exit_if_true (sizeof (chunk_data) <= chunk_size, "\n\nLine %d : sizeof (data) < chunk_size\n\n", __LINE__) ;

	memset (chunk_data, 53, sizeof (chunk_data)) ;
	chunk_data [chunk_size] = 0 ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.channels		= 1 ;
	sfinfo.frames		= 0 ;
	sfinfo.format		= format ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;

	/* Set up the chunk to write. */
	memset (&chunk_info, 0, sizeof (chunk_info)) ;
	snprintf (chunk_info.id, sizeof (chunk_info.id), "free") ;
	chunk_info.id_size = 4 ;
	chunk_info.data = chunk_data ;
	chunk_info.datalen = chunk_size ;

	err = sf_set_chunk (file, &chunk_info) ;
	exit_if_true (
		err != SF_ERR_NO_ERROR,
		"\n\nLine %d : sf_set_chunk returned for testdata : %s\n\n", __LINE__, sf_error_number (err)
		) ;

	memset (chunk_info.data, 0, chunk_info.datalen) ;

	/* Add some audio data. */
	memset (audio, 0, sizeof (audio)) ;
	sf_write_short (file, audio, ARRAY_LEN (audio)) ;

	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	exit_if_true (
		sfinfo.frames != ARRAY_LEN (audio),
		"\n\nLine %d : Incorrect sample count (%d should be %d)\n", __LINE__, (int) sfinfo.frames, (int) ARRAY_LEN (audio)
		) ;

	if (chunk_size < 512)
		check_log_buffer_or_die (file, __LINE__) ;

	sf_close (file) ;

	unlink (filename) ;
	puts ("ok") ;
} /* large_free_test */
