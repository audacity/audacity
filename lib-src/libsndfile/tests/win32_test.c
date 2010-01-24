/*
** Copyright (C) 2001-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#if defined (__CYGWIN__)

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

	puts ("\n\n") ;

	return 0 ;
} /* main */

static void
show_fstat_error (void)
{	static const char *filename = "fstat.dat" ;
	static char data [256] ;

	STATBUF 	statbuf ;
	int fd, mode, flags, ignored ;

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
	ignored = write (fd, data, sizeof (data)) ;
	close (fd) ;

	printf ("1) Re-open file in read/write mode and write another %d bytes at the end.\n", SIGNED_SIZEOF (data)) ;
	mode = O_RDWR | O_BINARY ;
	flags = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH ;
	if ((fd = open (filename, mode, flags)) < 0)
	{	printf ("\n\nLine %d: open() failed : %s\n\n", __LINE__, strerror (errno)) ;
		return ;
		} ;
	LSEEK (fd, 0, SEEK_END) ;
	ignored = write (fd, data, sizeof (data)) ;

	printf ("2) Now use system (\"%s %s\") to show the file length.\n\n", dir_cmd, filename) ;
	sprintf (data, "%s %s", dir_cmd, filename) ;
	ignored = system (data) ;
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
	int fd, mode, flags, ignored ;

	puts ("\n64 bit lseek() test.\n--------------------") ;

	printf ("0) Create a file, write %d bytes and close it.\n", SIGNED_SIZEOF (data)) ;
	mode = O_WRONLY | O_CREAT | O_TRUNC | O_BINARY ;
	flags = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH ;
	if ((fd = open (filename, mode, flags)) < 0)
	{	printf ("\n\nLine %d: open() failed : %s\n\n", __LINE__, strerror (errno)) ;
		return ;
		} ;
	ignored = write (fd, data, sizeof (data)) ;
	close (fd) ;

	printf ("1) Re-open file in read/write mode and write another %d bytes at the end.\n", SIGNED_SIZEOF (data)) ;
	mode = O_RDWR | O_BINARY ;
	flags = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH ;
	if ((fd = open (filename, mode, flags)) < 0)
	{	printf ("\n\nLine %d: open() failed : %s\n\n", __LINE__, strerror (errno)) ;
		return ;
		} ;

	LSEEK (fd, 0, SEEK_END) ;
	ignored = write (fd, data, sizeof (data)) ;

	printf ("2) Now use system (\"%s %s\") to show the file length.\n\n", dir_cmd, filename) ;
	sprintf (data, "%s %s", dir_cmd, filename) ;
	ignored = system (data) ;
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
	int stat_size, fstat_size, ignored ;
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

	ignored = write (fd, data, sizeof (data)) ;

	printf ("1) Now call stat and fstat on the file and retreive the file lengths.\n") ;

	if (stat (filename, &buf) != 0)
	{	printf ("\n\nLine %d: stat() failed : %s\n\n", __LINE__, strerror (errno)) ;
		return ;
		} ;
	stat_size = buf.st_size ;

	if (fstat (fd, &buf) != 0)
	{	printf ("\n\nLine %d: fstat() failed : %s\n\n", __LINE__, strerror (errno)) ;
		return ;
		} ;
	fstat_size = buf.st_size ;

	printf ("3) Size returned by stat and fstat is %d and %d, ", stat_size, fstat_size) ;

	if (stat_size == 0 || stat_size != fstat_size)
		printf ("but thats just plain ***WRONG***.\n\n") ;
	else
	{	printf ("which is correct.\n\n") ;
		unlink (filename) ;
		} ;

	return ;
} /* show_stat_fstat_error */


