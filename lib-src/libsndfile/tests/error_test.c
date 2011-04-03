/*
** Copyright (C) 1999-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#endif

#if OS_IS_WIN32
#include <windows.h>
#endif

#include <sndfile.h>

#include "utils.h"

#define	BUFFER_SIZE		(1<<15)
#define	SHORT_BUFFER	(256)

static void
error_number_test (void)
{	const char 	*noerror, *errstr ;
	int			k ;

	print_test_name ("error_number_test", "") ;

	noerror = sf_error_number (0) ;

	for (k = 1 ; k < 300 ; k++)
	{	errstr = sf_error_number (k) ;

		/* Test for termination condition. */
		if (errstr == noerror)
			break ;

		/* Test for error. */
		if (strstr (errstr, "This is a bug in libsndfile."))
		{	printf ("\n\nError : error number %d : %s\n\n\n", k, errstr) ;
			exit (1) ;
			} ;
		} ;


	puts ("ok") ;
	return ;
} /* error_number_test */

static void
error_value_test (void)
{	static unsigned char aiff_data [0x1b0] =
	{	'F' , 'O' , 'R' , 'M' ,
		0x00, 0x00, 0x01, 0xA8, /* FORM length */

		'A' , 'I' , 'F' , 'C' ,
		} ;

	const char *filename = "error.aiff" ;
	SNDFILE *file ;
	SF_INFO sfinfo ;
	int error_num ;

	print_test_name ("error_value_test", filename) ;

	dump_data_to_file (filename, aiff_data, sizeof (aiff_data)) ;

	file = sf_open (filename, SFM_READ, &sfinfo) ;
	if (file != NULL)
	{	printf ("\n\nLine %d : Should not have been able to open this file.\n\n", __LINE__) ;
		exit (1) ;
		} ;

	if ((error_num = sf_error (NULL)) <= 1 || error_num > 300)
	{	printf ("\n\nLine %d : Should not have had an error number of %d.\n\n", __LINE__, error_num) ;
		exit (1) ;
		} ;

	remove (filename) ;
	puts ("ok") ;
	return ;
} /* error_value_test */

static void
no_file_test (const char * filename)
{	SNDFILE		*sndfile ;
	SF_INFO		sfinfo ;

	print_test_name (__func__, filename) ;

	unlink (filename) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	sndfile = sf_open (filename, SFM_READ, &sfinfo) ;

	exit_if_true (sndfile != NULL, "\n\nLine %d : should not have received a valid SNDFILE* pointer.\n", __LINE__) ;

	unlink (filename) ;
	puts ("ok") ;
} /* no_file_test */

static void
zero_length_test (const char *filename)
{	SNDFILE		*sndfile ;
	SF_INFO		sfinfo ;
	FILE		*file ;

	print_test_name (__func__, filename) ;

	/* Create a zero length file. */
	file = fopen (filename, "w") ;
	exit_if_true (file == NULL, "\n\nLine %d : fopen ('%s') failed.\n", __LINE__, filename) ;
	fclose (file) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	sndfile = sf_open (filename, SFM_READ, &sfinfo) ;

	exit_if_true (sndfile != NULL, "\n\nLine %d : should not have received a valid SNDFILE* pointer.\n", __LINE__) ;

	exit_if_true (0 && sf_error (NULL) != SF_ERR_UNRECOGNISED_FORMAT,
		"\n\nLine %3d : Error : %s\n       should be : %s\n", __LINE__,
		sf_strerror (NULL), sf_error_number (SF_ERR_UNRECOGNISED_FORMAT)) ;

	unlink (filename) ;
	puts ("ok") ;
} /* zero_length_test */

static void
bad_wav_test (const char * filename)
{	SNDFILE		*sndfile ;
	SF_INFO		sfinfo ;

	FILE		*file ;
	const char	data [] = "RIFF    WAVEfmt            " ;

	print_test_name (__func__, filename) ;

	if ((file = fopen (filename, "w")) == NULL)
	{	printf ("\n\nLine %d : fopen returned NULL.\n", __LINE__) ;
		exit (1) ;
		} ;

	exit_if_true (fwrite (data, sizeof (data), 1, file) != 1, "\n\nLine %d : fwrite failed.\n", __LINE__) ;
	fclose (file) ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	sndfile = sf_open (filename, SFM_READ, &sfinfo) ;

	if (sndfile)
	{	printf ("\n\nLine %d : should not have received a valid SNDFILE* pointer.\n", __LINE__) ;
		exit (1) ;
		} ;

	unlink (filename) ;
	puts ("ok") ;
} /* bad_wav_test */

static void
error_close_test (void)
{	static short buffer [SHORT_BUFFER] ;
	const char	*filename = "error_close.wav" ;
	SNDFILE		*sndfile ;
	SF_INFO		sfinfo ;
	FILE		*file ;

	print_test_name (__func__, filename) ;

	/* Open a FILE* from which we will extract a file descriptor. */
	if ((file = fopen (filename, "w")) == NULL)
	{	printf ("\n\nLine %d : fopen returned NULL.\n", __LINE__) ;
		exit (1) ;
		} ;

	/* Set parameters for writing the file. */
	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	sfinfo.channels = 1 ;
	sfinfo.samplerate = 44100 ;
	sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16 ;

	sndfile = sf_open_fd (fileno (file), SFM_WRITE, &sfinfo, SF_TRUE) ;
	if (sndfile == NULL)
	{	printf ("\n\nLine %d : sf_open_fd failed : %s\n", __LINE__, sf_strerror (NULL)) ;
		exit (1) ;
		} ;

	test_write_short_or_die (sndfile, 0, buffer, ARRAY_LEN (buffer), __LINE__) ;

	/* Now close the fd associated with file before calling sf_close. */
	fclose (file) ;

	if (sf_close (sndfile) == 0)
	{
#if OS_IS_WIN32
		OSVERSIONINFOEX osvi ;

		memset (&osvi, 0, sizeof (OSVERSIONINFOEX)) ;
		osvi.dwOSVersionInfoSize = sizeof (OSVERSIONINFOEX) ;

		if (GetVersionEx ((OSVERSIONINFO *) &osvi))
		{	printf ("\n\nLine %d : sf_close should not have returned zero.\n", __LINE__) ;
			printf ("\nHowever, this is a known bug in version %d.%d of windows so we'll ignore it.\n\n",
					(int) osvi.dwMajorVersion, (int) osvi.dwMinorVersion) ;
			} ;
#else
		printf ("\n\nLine %d : sf_close should not have returned zero.\n", __LINE__) ;
		exit (1) ;
#endif
		} ;

	unlink (filename) ;
	puts ("ok") ;
} /* error_close_test */

int
main (void)
{
	error_number_test () ;
	error_value_test () ;

	error_close_test () ;

	no_file_test ("no_file.wav") ;
	zero_length_test ("zero_length.wav") ;
	bad_wav_test ("bad_wav.wav") ;

	return 0 ;
} /* main */

