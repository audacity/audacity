/*
** Copyright (C) 1999-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#if (HAVE_DECL_S_IRGRP == 0)
#include <sf_unistd.h>
#endif

#include <fcntl.h>
#include <math.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>

#include <sndfile.h>

#include "utils.h"

#define	DATA_LENGTH 		(512)

static void write_file_at_end (int fd, int filetype, int channels, int file_num) ;

static void multi_file_test (const char *filename, int *formats, int format_count) ;

static short data [DATA_LENGTH] ;

static int 	wav_formats [] =
{	SF_FORMAT_WAV | SF_FORMAT_PCM_16,
	SF_FORMAT_WAV | SF_FORMAT_PCM_24,
	SF_FORMAT_WAV | SF_FORMAT_ULAW,
	SF_FORMAT_WAV | SF_FORMAT_ALAW,
	/* Lite remove start */
	SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM,
	SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM,
	/* Lite remove end */
	/*-SF_FORMAT_WAV | SF_FORMAT_GSM610 Doesn't work yet. -*/
} ;

static int 	aiff_formats [] =
{	SF_FORMAT_AIFF | SF_FORMAT_PCM_16,
	SF_FORMAT_AIFF | SF_FORMAT_PCM_24,
	SF_FORMAT_AIFF | SF_FORMAT_ULAW,
	SF_FORMAT_AIFF | SF_FORMAT_ALAW
} ;

static int 	au_formats [] =
{	SF_FORMAT_AU | SF_FORMAT_PCM_16,
	SF_FORMAT_AU | SF_FORMAT_PCM_24,
	SF_FORMAT_AU | SF_FORMAT_ULAW,
	SF_FORMAT_AU | SF_FORMAT_ALAW
} ;

static int verbose = SF_FALSE ;

int
main (int argc, char **argv)
{	int		do_all = 0 ;
	int		test_count = 0 ;

	if (argc == 3 && strcmp (argv [2], "-v") == 0)
	{	verbose = SF_TRUE ;
		argc -- ;
		} ;

	if (argc != 2)
	{	printf ("Usage : %s <test>\n", argv [0]) ;
		printf ("    Where <test> is one of the following:\n") ;
		printf ("           wav   - test WAV file functions (little endian)\n") ;
		printf ("           aiff  - test AIFF file functions (big endian)\n") ;
		printf ("           au    - test AU file functions\n") ;
#if 0
		printf ("           svx   - test 8SVX/16SV file functions\n") ;
		printf ("           nist  - test NIST Sphere file functions\n") ;
		printf ("           ircam - test IRCAM file functions\n") ;
		printf ("           voc   - Create Voice file functions\n") ;
		printf ("           w64   - Sonic Foundry's W64 file functions\n") ;
#endif
		printf ("           all   - perform all tests\n") ;
		exit (1) ;
		} ;

	do_all = !strcmp (argv [1], "all") ;

	if (do_all || ! strcmp (argv [1], "wav"))
	{	multi_file_test	("multi_wav.dat", wav_formats, ARRAY_LEN (wav_formats)) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "aiff"))
	{	multi_file_test	("multi_aiff.dat", aiff_formats, ARRAY_LEN (aiff_formats)) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "au"))
	{	multi_file_test	("multi_au.dat", au_formats, ARRAY_LEN (au_formats)) ;
		test_count++ ;
		} ;

	return 0 ;
} /* main */

/*======================================================================================
*/

static void
multi_file_test (const char *filename, int *formats, int format_count)
{	SNDFILE				*sndfile ;
	SF_INFO				sfinfo ;
	SF_EMBED_FILE_INFO	embed_info ;
	sf_count_t			filelen ;
	int					fd, k, file_count = 0, open_perm ;

	print_test_name ("multi_file_test", filename) ;

	unlink (filename) ;

	open_perm = OS_IS_WIN32 ? 0 : S_IRUSR | S_IWUSR | S_IRGRP ;

	if ((fd = open (filename, O_RDWR | O_CREAT, open_perm)) < 0)
	{	printf ("\n\nLine %d: open failed : %s\n", __LINE__, strerror (errno)) ;
		exit (1) ;
		} ;

	k = write (fd, "1234", 4) ;

	for (k = 0 ; k < format_count ; k++)
		write_file_at_end (fd, formats [k], 2, k) ;

	filelen = file_length_fd (fd) ;

	embed_info.offset = 4 ;
	embed_info.length = 0 ;


	for (file_count = 0 ; embed_info.offset + embed_info.length < filelen ; )
	{
		file_count ++ ;

		if (verbose)
		{	puts ("\n------------------------------------") ;
			printf ("This offset : %ld\n", SF_COUNT_TO_LONG (embed_info.offset + embed_info.length)) ;
			} ;

		if (lseek (fd, embed_info.offset + embed_info.length, SEEK_SET) < 0)
		{	printf ("\n\nLine %d: lseek failed : %s\n", __LINE__, strerror (errno)) ;
			exit (1) ;
			} ;

		memset (&sfinfo, 0, sizeof (sfinfo)) ;
		if ((sndfile = sf_open_fd (fd, SFM_READ, &sfinfo, SF_FALSE)) == NULL)
		{	printf ("\n\nLine %d: sf_open_fd failed\n", __LINE__) ;
			printf ("Embedded file number : %d   offset : %ld\n", file_count, SF_COUNT_TO_LONG (embed_info.offset)) ;
			puts (sf_strerror (sndfile)) ;
			dump_log_buffer (sndfile) ;
			exit (1) ;
			} ;

		sf_command (sndfile, SFC_GET_EMBED_FILE_INFO, &embed_info, sizeof (embed_info)) ;

		sf_close (sndfile) ;

		if (verbose)
			printf ("\nNext offset : %ld\nNext length : %ld\n", SF_COUNT_TO_LONG (embed_info.offset), SF_COUNT_TO_LONG (embed_info.length)) ;
		} ;

	if (file_count != format_count)
	{	printf ("\n\nLine %d: file count (%d) not equal to %d.\n\n", __LINE__, file_count, format_count) ;
		printf ("Embedded file number : %d\n", file_count) ;
		exit (1) ;
		} ;

	close (fd) ;
	unlink (filename) ;
	printf ("ok\n") ;

	return ;
} /* multi_file_test */

/*======================================================================================
*/

static void
write_file_at_end (int fd, int filetype, int channels, int file_num)
{	SNDFILE *sndfile ;
	SF_INFO sfinfo ;

	int	frames, k ;

	lseek (fd, 0, SEEK_END) ;

	for (k = 0 ; k < DATA_LENGTH ; k++)
		data [k] = k ;

	frames = DATA_LENGTH / channels ;

	sfinfo.format = filetype ;
	sfinfo.channels = channels ;
	sfinfo.samplerate = 44100 ;

	if ((sndfile = sf_open_fd (fd, SFM_WRITE, &sfinfo, SF_FALSE)) == NULL)
	{	printf ("\n\nLine %d: sf_open_fd failed\n", __LINE__) ;
		printf ("Embedded file number : %d\n", file_num) ;
		puts (sf_strerror (sndfile)) ;
		dump_log_buffer (sndfile) ;
		exit (1) ;
		} ;

	if (sf_writef_short (sndfile, data, frames) != frames)
	{	printf ("\n\nLine %d: short write\n", __LINE__) ;
		printf ("Embedded file number : %d\n", file_num) ;
		exit (1) ;
		} ;

	sf_close (sndfile) ;
} /* write_file_at_end */

