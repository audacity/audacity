/*
** Copyright (C) 2010-2014 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	"sfconfig.h"

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<inttypes.h>
#include	<ctype.h>
#include	<math.h>
#include	<errno.h>
#if HAVE_UNISTD_H
#include	<unistd.h>
#else
#include	"sf_unistd.h"
#endif
#include	<fcntl.h>
#include	<sys/stat.h>
#include	<sys/types.h>

#include	<sndfile.h>

#include	"common.h"

#define	BUFFER_LEN		(1 << 16)

#define	NOT(x)			(! (x))


static void usage_exit (const char *progname) ;
static void salvage_file (const char * broken_wav, const char * fixed_w64) ;

int
main (int argc, char *argv [])
{
	if (argc != 3)
		usage_exit (program_name (argv [0])) ;

	salvage_file (argv [1], argv [2]) ;

	return 0 ;
} /* main */

/*==============================================================================
*/

static void lseek_or_die (int fd, off_t offset, int whence) ;
static sf_count_t get_file_length (int fd, const char * name) ;
static sf_count_t find_data_offset (int fd, int format) ;
static void copy_data (int fd, SNDFILE * sndfile, int readsize) ;


static void
usage_exit (const char *progname)
{	printf ("Usage :\n\n  %s <broken wav file> <fixed w64 file>\n\n", progname) ;
	puts ("Salvages the audio data from WAV files which are more than 4G in length.\n") ;
	printf ("Using %s.\n\n", sf_version_string ()) ;
	exit (1) ;
} /* usage_exit */

static void
salvage_file (const char * broken_wav, const char * fixed_w64)
{	SNDFILE * sndfile ;
	SF_INFO sfinfo ;
	sf_count_t broken_len, data_offset ;
	int fd, read_size ;

	if (strcmp (broken_wav, fixed_w64) == 0)
	{	printf ("Error : Input and output files must be different.\n\n") ;
		exit (1) ;
		} ;

	if ((fd = open (broken_wav, O_RDONLY)) < 0)
	{	printf ("Error : Not able to open file '%s' : %s\n", broken_wav, strerror (errno)) ;
		exit (1) ;
		} ;

	broken_len = get_file_length (fd, broken_wav) ;
	if (broken_len <= 0xffffffff)
		printf ("File is not greater than 4Gig but salvaging anyway.\n") ;

	/* Grab the format info from the broken file. */
	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	if ((sndfile = sf_open (broken_wav, SFM_READ, &sfinfo)) == NULL)
	{	printf ("sf_open ('%s') failed : %s\n", broken_wav, sf_strerror (NULL)) ;
		exit (1) ;
		} ;
	sf_close (sndfile) ;

	data_offset = find_data_offset (fd, sfinfo.format & SF_FORMAT_TYPEMASK) ;

	printf ("Offset to audio data : %" PRId64 "\n", data_offset) ;

	switch (sfinfo.format & SF_FORMAT_TYPEMASK)
	{	case SF_FORMAT_WAV :
		case SF_FORMAT_WAVEX :
			sfinfo.format = SF_FORMAT_W64 | (sfinfo.format & SF_FORMAT_SUBMASK) ;
			break ;

		default :
			printf ("Don't currently support this file type.\n") ;
			exit (1) ;
		} ;

	switch (sfinfo.format & SF_FORMAT_SUBMASK)
	{	case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_S8 :
				read_size = 1 ;
				break ;

		case SF_FORMAT_PCM_16 :
				read_size = 2 ;
				break ;

		case SF_FORMAT_PCM_24 :
				read_size = 3 ;
				break ;

		case SF_FORMAT_PCM_32 :
		case SF_FORMAT_FLOAT :
				read_size = 4 ;
				break ;

		case SF_FORMAT_DOUBLE :
				read_size = 8 ;
				break ;

		default :
			printf ("Sorry, don't currently support this file encoding type.\n") ;
			exit (1) ;
		} ;

	read_size *= sfinfo.channels ;

	if ((sndfile = sf_open (fixed_w64, SFM_WRITE, &sfinfo)) == NULL)
	{	printf ("sf_open ('%s') failed : %s\n", fixed_w64, sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	lseek_or_die (fd, data_offset, SEEK_SET) ;

	copy_data (fd, sndfile, read_size) ;

	sf_close (sndfile) ;

	puts ("Done!") ;
} /* salvage_file */

/*------------------------------------------------------------------------------
*/

static void
lseek_or_die (int fd, off_t offset, int whence)
{
	if (lseek (fd, offset, whence) < 0)
	{	printf ("lseek failed : %s\n", strerror (errno)) ;
		exit (1) ;
		} ;

	return ;
} /* lseek_or_die */


static sf_count_t
get_file_length (int fd, const char * name)
{	struct stat sbuf ;

	if (sizeof (sbuf.st_size) != 8)
	{	puts ("Error : sizeof (sbuf.st_size) != 8. Was program compiled with\n"
				"        64 bit file offsets?\n") ;
		exit (1) ;
		} ;

	if (fstat (fd, &sbuf) != 0)
	{	printf ("Error : fstat ('%s') failed : %s\n", name, strerror (errno)) ;
		exit (1) ;
		} ;

	return sbuf.st_size ;
} /* get_file_length */

static sf_count_t
find_data_offset (int fd, int format)
{	char buffer [8192], *cptr ;
	const char * target = "XXXX" ;
	sf_count_t offset = -1, extra ;
	int rlen, slen ;

	switch (format)
	{	case SF_FORMAT_WAV :
		case SF_FORMAT_WAVEX :
			target = "data" ;
			extra = 8 ;
			break ;

		case SF_FORMAT_AIFF :
			target = "SSND" ;
			extra = 16 ;
			break ;

		default :
			puts ("Error : Sorry, don't handle this input file format.\n") ;
			exit (1) ;
		} ;

	slen = strlen (target) ;

	lseek_or_die (fd, 0, SEEK_SET) ;

	printf ("Searching for '%s' maker.\n", target) ;

	if ((rlen = read (fd, buffer, sizeof (buffer))) < 0)
	{	printf ("Error : failed read : %s\n", strerror (errno)) ;
		exit (1) ;
		} ;

	cptr = memchr (buffer, target [0], rlen - slen) ;
	if (cptr && memcmp (cptr, target, slen) == 0)
		offset = cptr - buffer ;
	else
	{	printf ("Error : Could not find data offset.\n") ;
		exit (1) ;
		} ;

	return offset + extra ;
} /* find_data_offset */

static void
copy_data (int fd, SNDFILE * sndfile, int readsize)
{	static char * buffer ;
	sf_count_t readlen, count ;
	int bufferlen, done = 0 ;

	bufferlen = readsize * 1024 ;
	buffer = malloc (bufferlen) ;

	while (NOT (done) && (readlen = read (fd, buffer, bufferlen)) >= 0)
	{	if (readlen < bufferlen)
		{	readlen -= readlen % readsize ;
			done = 1 ;
			} ;

		if ((count = sf_write_raw (sndfile, buffer, readlen)) != readlen)
		{	printf ("Error : sf_write_raw returned %" PRId64 " : %s\n", count, sf_strerror (sndfile)) ;
			return ;
			} ;
		} ;

	free (buffer) ;

	return ;
} /* copy_data */

