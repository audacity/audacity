/*
** Copyright (C) 2001-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include "sndfile.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#if (HAVE_DECL_S_IRGRP == 0)
#include <sf_unistd.h>
#endif

#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#define SIGNED_SIZEOF(x)	((int) sizeof (x))

/* EMX is OS/2. */
#if defined (__CYGWIN__) || defined (__EMX__)

	#define		LSEEK	lseek
	#define		FSTAT	fstat

	typedef		struct stat			STATBUF ;
	typedef		off_t				INT64 ;

	static char dir_cmd [] = "ls -l" ;

#elif (defined (WIN32) || defined (_WIN32))

	#define		LSEEK	_lseeki64
	#define		FSTAT	_fstati64

	typedef		struct _stati64		STATBUF ;
	typedef		__int64				INT64 ;

	static char dir_cmd [] = "dir" ;

#else

	#define		LSEEK	lseek
	#define		FSTAT	fstat

	typedef		struct stat		STATBUF ;
	typedef		sf_count_t		INT64 ;

	#define		O_BINARY	0
	static char dir_cmd [] = "ls -l" ;

#endif

static void show_fstat_error (void) ;
static void show_lseek_error (void) ;
static void show_stat_fstat_error (void) ;
static void write_to_closed_file (void) ;

int
main (void)
{
	puts ("\n\n\n\n"
		"This program shows up errors in the Win32 implementation of\n"
		"a couple of POSIX API functions on some versions of windoze.\n"
		"It can also be compiled on Linux (which works correctly) and\n"
		"other OSes just to provide a sanity check.\n"
		) ;

	show_fstat_error () ;
	show_lseek_error () ;
	show_stat_fstat_error () ;
	write_to_closed_file () ;

	puts ("\n\n") ;

	return 0 ;
} /* main */

static void
show_fstat_error (void)
{	static const char *filename = "fstat.dat" ;
	static char data [256] ;

	STATBUF 	statbuf ;
	int fd, mode, flags ;

	if (sizeof (statbuf.st_size) != sizeof (INT64))
	{	printf ("\n\nLine %d: Error, sizeof (statbuf.st_size) != 8.\n\n", __LINE__) ;
		return ;
		} ;

	puts ("\n64 bit fstat() test.\n--------------------") ;

	printf ("0) Create a file, write %d bytes and close it.\n", SIGNED_SIZEOF (data)) ;
	mode = O_WRONLY | O_CREAT | O_TRUNC | O_BINARY ;
	flags = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH ;
	if ((fd = open (filename, mode, flags)) < 0)
	{	printf ("\n\nLine %d: open() failed : %s\n\n", __LINE__, strerror (errno)) ;
		return ;
		} ;
	assert (write (fd, data, sizeof (data)) > 0) ;
	close (fd) ;

	printf ("1) Re-open file in read/write mode and write another %d bytes at the end.\n", SIGNED_SIZEOF (data)) ;
	mode = O_RDWR | O_BINARY ;
	flags = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH ;
	if ((fd = open (filename, mode, flags)) < 0)
	{	printf ("\n\nLine %d: open() failed : %s\n\n", __LINE__, strerror (errno)) ;
		return ;
		} ;
	LSEEK (fd, 0, SEEK_END) ;
	assert (write (fd, data, sizeof (data)) > 0) ;

	printf ("2) Now use system (\"%s %s\") to show the file length.\n\n", dir_cmd, filename) ;

	/* Would use snprintf, but thats not really available on windows. */
	memset (data, 0, sizeof (data)) ;
	strncpy (data, dir_cmd, sizeof (data) - 1) ;
	strncat (data, " ", sizeof (data) - 1 - strlen (data)) ;
	strncat (data, filename, sizeof (data) - 1 - strlen (data)) ;

	assert (system (data) >= 0) ;
	puts ("") ;

	printf ("3) Now use fstat() to get the file length.\n") ;
	if (FSTAT (fd, &statbuf) != 0)
	{	printf ("\n\nLine %d: fstat() returned error : %s\n", __LINE__, strerror (errno)) ;
		return ;
		} ;

	printf ("4) According to fstat(), the file length is %ld, ", (long) statbuf.st_size) ;

	close (fd) ;

	if (statbuf.st_size != 2 * sizeof (data))
		printf ("but thats just plain ***WRONG***.\n\n") ;
	else
	{	printf ("which is correct.\n\n") ;
		unlink (filename) ;
		} ;

} /* show_fstat_error */

static void
show_lseek_error (void)
{	static const char *filename = "fstat.dat" ;
	static char data [256] ;

	INT64	retval ;
	int fd, mode, flags ;

	puts ("\n64 bit lseek() test.\n--------------------") ;

	printf ("0) Create a file, write %d bytes and close it.\n", SIGNED_SIZEOF (data)) ;
	mode = O_WRONLY | O_CREAT | O_TRUNC | O_BINARY ;
	flags = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH ;
	if ((fd = open (filename, mode, flags)) < 0)
	{	printf ("\n\nLine %d: open() failed : %s\n\n", __LINE__, strerror (errno)) ;
		return ;
		} ;
	assert (write (fd, data, sizeof (data)) > 0) ;
	close (fd) ;

	printf ("1) Re-open file in read/write mode and write another %d bytes at the end.\n", SIGNED_SIZEOF (data)) ;
	mode = O_RDWR | O_BINARY ;
	flags = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH ;
	if ((fd = open (filename, mode, flags)) < 0)
	{	printf ("\n\nLine %d: open() failed : %s\n\n", __LINE__, strerror (errno)) ;
		return ;
		} ;

	LSEEK (fd, 0, SEEK_END) ;
	assert (write (fd, data, sizeof (data)) > 0) ;

	printf ("2) Now use system (\"%s %s\") to show the file length.\n\n", dir_cmd, filename) ;

	/* Would use snprintf, but thats not really available on windows. */
	memset (data, 0, sizeof (data)) ;
	strncpy (data, dir_cmd, sizeof (data) - 1) ;
	strncat (data, " ", sizeof (data) - 1 - strlen (data)) ;
	strncat (data, filename, sizeof (data) - 1 - strlen (data)) ;

	assert (system (data) >= 0) ;
	puts ("") ;

	printf ("3) Now use lseek() to go to the end of the file.\n") ;
	retval = LSEEK (fd, 0, SEEK_END) ;

	printf ("4) We are now at position %ld, ", (long) retval) ;

	close (fd) ;

	if (retval != 2 * sizeof (data))
		printf ("but thats just plain ***WRONG***.\n\n") ;
	else
	{	printf ("which is correct.\n\n") ;
		unlink (filename) ;
		} ;

} /* show_lseek_error */

static void
show_stat_fstat_error (void)
{	static const char *filename = "stat_fstat.dat" ;
	static char data [256] ;

	int fd, mode, flags ;
	int stat_size, fstat_size ;
	struct stat buf ;

	/* Known to fail on WinXP. */
	puts ("\nstat/fstat test.\n----------------") ;

	printf ("0) Create a file and write %d bytes.\n", SIGNED_SIZEOF (data)) ;

	mode = O_WRONLY | O_CREAT | O_TRUNC | O_BINARY ;
	flags = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH ;
	if ((fd = open (filename, mode, flags)) < 0)
	{	printf ("\n\nLine %d: open() failed : %s\n\n", __LINE__, strerror (errno)) ;
		return ;
		} ;

	assert (write (fd, data, sizeof (data)) > 0) ;

	printf ("1) Now call stat and fstat on the file and retreive the file lengths.\n") ;

	if (stat (filename, &buf) != 0)
	{	printf ("\n\nLine %d: stat() failed : %s\n\n", __LINE__, strerror (errno)) ;
		goto error_exit ;
		} ;
	stat_size = buf.st_size ;

	if (fstat (fd, &buf) != 0)
	{	printf ("\n\nLine %d: fstat() failed : %s\n\n", __LINE__, strerror (errno)) ;
		goto error_exit ;
		} ;
	fstat_size = buf.st_size ;

	printf ("2) Size returned by stat and fstat is %d and %d, ", stat_size, fstat_size) ;


	if (stat_size == 0 || stat_size != fstat_size)
		printf ("but thats just plain ***WRONG***.\n\n") ;
	else
		printf ("which is correct.\n\n") ;

error_exit :

	close (fd) ;
	unlink (filename) ;

	return ;
} /* show_stat_fstat_error */


static void
write_to_closed_file (void)
{	const char * filename = "closed_write_test.txt" ;
	struct stat buf ;
	FILE * file ;
	int		fd ;

	puts ("\nWrite to closed file test.\n--------------------------") ;

	printf ("0) First we open file for write using fopen().\n") ;
	if ((file = fopen (filename, "w")) == NULL)
	{	printf ("\n\nLine %d: fopen() failed : %s\n\n", __LINE__, strerror (errno)) ;
		return ;
		} ;

	printf ("1) Now we grab the file descriptor fileno().\n") ;
	fd = fileno (file) ;

	printf ("2) Write some text via the file descriptor.\n") ;
	assert (write (fd, "a\n", 2) > 0) ;

	printf ("3) Now we close the file using fclose().\n") ;
	fclose (file) ;

	stat (filename, &buf) ;
	printf ("   File size is %d bytes.\n", (int) buf.st_size) ;

	printf ("4) Now write more data to the file descriptor which should fail.\n") ;
	if (write (fd, "b\n", 2) < 0)
		printf ("5) Good, write returned an error code as it should have.\n") ;
	else
	{	printf ("5) Attempting to write to a closed file should have failed but didn't! *** WRONG ***\n") ;

		stat (filename, &buf) ;
		printf ("   File size is %d bytes.\n", (int) buf.st_size) ;
		} ;

	unlink (filename) ;

	return ;
} /* write_to_closed_file */
