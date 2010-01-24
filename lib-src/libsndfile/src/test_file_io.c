/*
** Copyright (C) 2002-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <string.h>
#include <errno.h>

#include "common.h"

#include "test_main.h"

static void make_data (int *data, int len, int seed) ;

static void file_open_test (const char *filename) ;
static void file_read_write_test (const char *filename) ;
static void file_truncate_test (const char *filename) ;

static void test_open_or_die (SF_PRIVATE *psf, int linenum) ;
static void test_close_or_die (SF_PRIVATE *psf, int linenum) ;

static void test_write_or_die	(SF_PRIVATE *psf, void *data, sf_count_t bytes, sf_count_t items, sf_count_t new_position, int linenum) ;
static void test_read_or_die	(SF_PRIVATE *psf, void *data, sf_count_t bytes, sf_count_t items, sf_count_t new_position, int linenum) ;
static void test_equal_or_die	(int *array1, int *array2, int len, int linenum) ;
static void test_seek_or_die (SF_PRIVATE *psf, sf_count_t offset, int whence, sf_count_t new_position, int linenum) ;



/*==============================================================================
** Actual test functions.
*/

static void
file_open_test (const char *filename)
{	SF_PRIVATE sf_data, *psf ;
	int		error ;

	print_test_name ("Testing file open") ;

	memset (&sf_data, 0, sizeof (sf_data)) ;
	psf = &sf_data ;

	/* Ensure that the file doesn't already exist. */
	if (unlink (filename) != 0 && errno != ENOENT)
	{	printf ("\n\nLine %d: unlink failed (%d) : %s\n\n", __LINE__, errno, strerror (errno)) ;
		exit (1) ;
		} ;

	strncpy (psf->filename, filename, sizeof (psf->filename)) ;

	/* Test that open for read fails if the file doesn't exist. */
	error = psf_fopen (psf, psf->filename, SFM_READ) ;
	if (error == 0)
	{	printf ("\n\nLine %d: psf_fopen() should have failed.\n\n", __LINE__) ;
		exit (1) ;
		} ;

	/* Reset error to zero. */
	psf->error = SFE_NO_ERROR ;

	/* Test file open in write mode. */
	psf->mode = SFM_WRITE ;
	test_open_or_die (psf, __LINE__) ;

	test_close_or_die (psf, __LINE__) ;

	unlink (psf->filename) ;

	/* Test file open in read/write mode for a non-existant file. */
	psf->mode = SFM_RDWR ;
	test_open_or_die (psf, __LINE__) ;

	test_close_or_die (psf, __LINE__) ;

	/* Test file open in read/write mode for an existing file. */
	psf->mode = SFM_RDWR ;
	test_open_or_die (psf, __LINE__) ;

	test_close_or_die (psf, __LINE__) ;

	unlink (psf->filename) ;
	puts ("ok") ;
} /* file_open_test */

static void
file_read_write_test (const char *filename)
{	static int data_out	[512] ;
	static int data_in	[512] ;

	SF_PRIVATE sf_data, *psf ;
	sf_count_t retval ;

	/*
	** Open a new file and write two blocks of data to the file. After each
	** write, test that psf_get_filelen() returns the new length.
	*/

	print_test_name ("Testing file write") ;

	memset (&sf_data, 0, sizeof (sf_data)) ;
	psf = &sf_data ;
	strncpy (psf->filename, filename, sizeof (psf->filename)) ;

	/* Test file open in write mode. */
	psf->mode = SFM_WRITE ;
	test_open_or_die (psf, __LINE__) ;

	make_data (data_out, ARRAY_LEN (data_out), 1) ;
	test_write_or_die (psf, data_out, sizeof (data_out [0]), ARRAY_LEN (data_out), sizeof (data_out), __LINE__) ;

	if ((retval = psf_get_filelen (psf)) != sizeof (data_out))
	{	printf ("\n\nLine %d: file length after write is not correct (%ld should be %d).\n\n", __LINE__, (long) retval, (int) sizeof (data_out)) ;
		if (retval == 0)
			printf ("An fsync() may be necessary before fstat() in psf_get_filelen().\n\n") ;
		exit (1) ;
		} ;

	make_data (data_out, ARRAY_LEN (data_out), 2) ;
	test_write_or_die (psf, data_out, ARRAY_LEN (data_out), sizeof (data_out [0]), 2 * sizeof (data_out), __LINE__) ;

	if ((retval = psf_get_filelen (psf)) != 2 * sizeof (data_out))
	{	printf ("\n\nLine %d: file length after write is not correct. (%ld should be %d)\n\n", __LINE__, (long) retval, 2 * ((int) sizeof (data_out))) ;
		exit (1) ;
		} ;

	test_close_or_die (psf, __LINE__) ;
	puts ("ok") ;

	/*
	** Now open the file in read mode, check the file length and check
	** that the data is correct.
	*/

	print_test_name ("Testing file read") ;

	/* Test file open in write mode. */
	psf->mode = SFM_READ ;
	test_open_or_die (psf, __LINE__) ;

	make_data (data_out, ARRAY_LEN (data_out), 1) ;
	test_read_or_die (psf, data_in, 1, sizeof (data_in), sizeof (data_in), __LINE__) ;
	test_equal_or_die	(data_out, data_in, ARRAY_LEN (data_out), __LINE__) ;

	make_data (data_out, ARRAY_LEN (data_out), 2) ;
	test_read_or_die (psf, data_in, sizeof (data_in [0]), ARRAY_LEN (data_in), 2 * sizeof (data_in), __LINE__) ;
	test_equal_or_die	(data_out, data_in, ARRAY_LEN (data_out), __LINE__) ;

	test_close_or_die (psf, __LINE__) ;

	puts ("ok") ;

	/*
	** Open the file in read/write mode, seek around a bit and then seek to
	** the end of the file and write another block of data (3rd block). Then
	** go back and check that all three blocks are correct.
	*/

	print_test_name ("Testing file seek") ;

	/* Test file open in read/write mode. */
	psf->mode = SFM_RDWR ;
	test_open_or_die (psf, __LINE__) ;

	test_seek_or_die (psf, 0, SEEK_SET, 0, __LINE__) ;
	test_seek_or_die (psf, 0, SEEK_END, 2 * SIGNED_SIZEOF (data_out), __LINE__) ;
	test_seek_or_die (psf, -1 * SIGNED_SIZEOF (data_out), SEEK_CUR, (sf_count_t) sizeof (data_out), __LINE__) ;

	test_seek_or_die (psf, SIGNED_SIZEOF (data_out), SEEK_CUR, 2 * SIGNED_SIZEOF (data_out), __LINE__) ;
	make_data (data_out, ARRAY_LEN (data_out), 3) ;
	test_write_or_die (psf, data_out, sizeof (data_out [0]), ARRAY_LEN (data_out), 3 * sizeof (data_out), __LINE__) ;

	test_seek_or_die (psf, 0, SEEK_SET, 0, __LINE__) ;
	make_data (data_out, ARRAY_LEN (data_out), 1) ;
	test_read_or_die (psf, data_in, 1, sizeof (data_in), sizeof (data_in), __LINE__) ;
	test_equal_or_die	(data_out, data_in, ARRAY_LEN (data_out), __LINE__) ;

	test_seek_or_die (psf, 2 * SIGNED_SIZEOF (data_out), SEEK_SET, 2 * SIGNED_SIZEOF (data_out), __LINE__) ;
	make_data (data_out, ARRAY_LEN (data_out), 3) ;
	test_read_or_die (psf, data_in, 1, sizeof (data_in), 3 * sizeof (data_in), __LINE__) ;
	test_equal_or_die	(data_out, data_in, ARRAY_LEN (data_out), __LINE__) ;

	test_seek_or_die (psf, SIGNED_SIZEOF (data_out), SEEK_SET, SIGNED_SIZEOF (data_out), __LINE__) ;
	make_data (data_out, ARRAY_LEN (data_out), 2) ;
	test_read_or_die (psf, data_in, 1, sizeof (data_in), 2 * sizeof (data_in), __LINE__) ;
	test_equal_or_die	(data_out, data_in, ARRAY_LEN (data_out), __LINE__) ;

	test_close_or_die (psf, __LINE__) ;
	puts ("ok") ;

	/*
	** Now test operations with a non-zero psf->fileoffset field. This field
	** sets an artificial file start positions so that a seek to the start of
	** the file will actually be a seek to the value given by psf->fileoffset.
	*/

	print_test_name ("Testing file offset") ;

	/* Test file open in read/write mode. */
	psf->mode = SFM_RDWR ;
	psf->fileoffset = sizeof (data_out [0]) * ARRAY_LEN (data_out) ;
	test_open_or_die (psf, __LINE__) ;

	if ((retval = psf_get_filelen (psf)) != 3 * sizeof (data_out))
	{	printf ("\n\nLine %d: file length after write is not correct. (%ld should be %d)\n\n", __LINE__, (long) retval, 3 * ((int) sizeof (data_out))) ;
		exit (1) ;
		} ;

	test_seek_or_die (psf, SIGNED_SIZEOF (data_out), SEEK_SET, SIGNED_SIZEOF (data_out), __LINE__) ;
	make_data (data_out, ARRAY_LEN (data_out), 5) ;
	test_write_or_die (psf, data_out, sizeof (data_out [0]), ARRAY_LEN (data_out), 2 * sizeof (data_out), __LINE__) ;
	test_close_or_die (psf, __LINE__) ;

	/* final test with psf->fileoffset == 0. */

	psf->mode = SFM_RDWR ;
	psf->fileoffset = 0 ;
	test_open_or_die (psf, __LINE__) ;

	if ((retval = psf_get_filelen (psf)) != 3 * sizeof (data_out))
	{	printf ("\n\nLine %d: file length after write is not correct. (%ld should be %d)\n\n", __LINE__, (long) retval, 3 * ((int) sizeof (data_out))) ;
		exit (1) ;
		} ;

	make_data (data_out, ARRAY_LEN (data_out), 1) ;
	test_read_or_die (psf, data_in, 1, sizeof (data_in), sizeof (data_in), __LINE__) ;
	test_equal_or_die	(data_out, data_in, ARRAY_LEN (data_out), __LINE__) ;

	make_data (data_out, ARRAY_LEN (data_out), 2) ;
	test_read_or_die (psf, data_in, 1, sizeof (data_in), 2 * sizeof (data_in), __LINE__) ;
	test_equal_or_die	(data_out, data_in, ARRAY_LEN (data_out), __LINE__) ;

	make_data (data_out, ARRAY_LEN (data_out), 5) ;
	test_read_or_die (psf, data_in, 1, sizeof (data_in), 3 * sizeof (data_in), __LINE__) ;
	test_equal_or_die	(data_out, data_in, ARRAY_LEN (data_out), __LINE__) ;

	test_close_or_die (psf, __LINE__) ;

	puts ("ok") ;
} /* file_read_write_test */

static void
file_truncate_test (const char *filename)
{	SF_PRIVATE sf_data, *psf ;
	unsigned char buffer [256] ;
	int k ;

	/*
	** Open a new file and write two blocks of data to the file. After each
	** write, test that psf_get_filelen() returns the new length.
	*/

	print_test_name ("Testing file truncate") ;

	memset (&sf_data, 0, sizeof (sf_data)) ;
	memset (buffer, 0xEE, sizeof (buffer)) ;

	psf = &sf_data ;
	strncpy (psf->filename, filename, sizeof (psf->filename)) ;

	/*
	** Open the file write mode, write 0xEE data and then extend the file
	** using truncate (the extended data should be 0x00).
	*/
	psf->mode = SFM_WRITE ;
	test_open_or_die (psf, __LINE__) ;
	test_write_or_die (psf, buffer, sizeof (buffer) / 2, 1, sizeof (buffer) / 2, __LINE__) ;
	psf_ftruncate (psf, sizeof (buffer)) ;
	test_close_or_die (psf, __LINE__) ;

	/* Open the file in read mode and check the data. */
	psf->mode = SFM_READ ;
	test_open_or_die (psf, __LINE__) ;
	test_read_or_die (psf, buffer, sizeof (buffer), 1, sizeof (buffer), __LINE__) ;
	test_close_or_die (psf, __LINE__) ;

	for (k = 0 ; k < SIGNED_SIZEOF (buffer) / 2 ; k++)
		if (buffer [k] != 0xEE)
		{	printf ("\n\nLine %d : buffer [%d] = %d (should be 0xEE)\n\n", __LINE__, k, buffer [k]) ;
			exit (1) ;
			} ;

	for (k = SIGNED_SIZEOF (buffer) / 2 ; k < SIGNED_SIZEOF (buffer) ; k++)
		if (buffer [k] != 0)
		{	printf ("\n\nLine %d : buffer [%d] = %d (should be 0)\n\n", __LINE__, k, buffer [k]) ;
			exit (1) ;
			} ;

	/* Open the file in read/write and shorten the file using truncate. */
	psf->mode = SFM_RDWR ;
	test_open_or_die (psf, __LINE__) ;
	psf_ftruncate (psf, sizeof (buffer) / 4) ;
	test_close_or_die (psf, __LINE__) ;

	/* Check the file length. */
	psf->mode = SFM_READ ;
	test_open_or_die (psf, __LINE__) ;
	test_seek_or_die (psf, 0, SEEK_END, SIGNED_SIZEOF (buffer) / 4, __LINE__) ;
	test_close_or_die (psf, __LINE__) ;

	puts ("ok") ;
} /* file_truncate_test */

/*==============================================================================
** Testing helper functions.
*/

static void
test_open_or_die (SF_PRIVATE *psf, int linenum)
{	int		error ;

	/* Test that open for read fails if the file doesn't exist. */
	error = psf_fopen (psf, psf->filename, psf->mode) ;
	if (error)
	{	printf ("\n\nLine %d: psf_fopen() failed : %s\n\n", linenum, strerror (errno)) ;
		exit (1) ;
		} ;

} /* test_open_or_die */

static void
test_close_or_die (SF_PRIVATE *psf, int linenum)
{
	psf_fclose (psf) ;
	if (psf_file_valid (psf))
	{	printf ("\n\nLine %d: psf->filedes should not be valid.\n\n", linenum) ;
		exit (1) ;
		} ;

} /* test_close_or_die */

static void
test_write_or_die (SF_PRIVATE *psf, void *data, sf_count_t bytes, sf_count_t items, sf_count_t new_position, int linenum)
{	sf_count_t	retval ;

	retval = psf_fwrite (data, bytes, items, psf) ;
	if (retval != items)
	{	printf ("\n\nLine %d: psf_write() returned %ld (should be %ld)\n\n", linenum, (long) retval, (long) items) ;
		exit (1) ;
		} ;

	if ((retval = psf_ftell (psf)) != new_position)
	{	printf ("\n\nLine %d: file length after write is not correct. (%ld should be %ld)\n\n", linenum, (long) retval, (long) new_position) ;
		exit (1) ;
		} ;

	return ;
} /* test_write_or_die */

static void
test_read_or_die (SF_PRIVATE *psf, void *data, sf_count_t bytes, sf_count_t items, sf_count_t new_position, int linenum)
{	sf_count_t	retval ;

	retval = psf_fread (data, bytes, items, psf) ;
	if (retval != items)
	{	printf ("\n\nLine %d: psf_write() returned %ld (should be %ld)\n\n", linenum, (long) retval, (long) items) ;
		exit (1) ;
		} ;

	if ((retval = psf_ftell (psf)) != new_position)
	{	printf ("\n\nLine %d: file length after write is not correct. (%ld should be %ld)\n\n", linenum, (long) retval, (long) new_position) ;
		exit (1) ;
		} ;

	return ;
} /* test_write_or_die */

static void
test_seek_or_die (SF_PRIVATE *psf, sf_count_t offset, int whence, sf_count_t new_position, int linenum)
{	sf_count_t retval ;

	retval = psf_fseek (psf, offset, whence) ;

	if (retval != new_position)
	{	printf ("\n\nLine %d: psf_fseek() failed. New position is %ld (should be %ld).\n\n",
			linenum, (long) retval, (long) new_position) ;
		exit (1) ;
		} ;

} /* test_seek_or_die */

static void
test_equal_or_die	(int *array1, int *array2, int len, int linenum)
{	int k ;

	for (k = 0 ; k < len ; k++)
		if (array1 [k] != array2 [k])
			printf ("\n\nLine %d: error at index %d (%d != %d).\n\n",
				linenum, k, array1 [k], array2 [k]) ;

	return ;
} /* test_equal_or_die */

static void
make_data (int *data, int len, int seed)
{	int k ;

	srand (seed * 3333333 + 14756123) ;

	for (k = 0 ; k < len ; k++)
		data [k] = rand () ;

} /* make_data */

void
test_file_io (void)
{	const char *filename = "file_io.dat" ;

	file_open_test	(filename) ;
	file_read_write_test	(filename) ;
	file_truncate_test (filename) ;

	unlink (filename) ;
} /* main */

