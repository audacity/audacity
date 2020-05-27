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

/*==========================================================================
** This is a test program which tests reading from stdin and writing to
** stdout.
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

#include <sys/types.h>
#include <sys/stat.h>

#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include "utils.h"

/* EMX is OS/2. */
#if (OS_IS_WIN32) || defined (__EMX__)

int
main (void)
{
	puts ("    stdio_test : this test doesn't work on win32.") ;
	return 0 ;
} /* main */

#else

#ifndef WIFEXITED
#define WIFEXITED(s) (((s) & 0xff) == 0)
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(s) (((s) & 0xff00) >> 8)
#endif


static size_t	file_length (const char *filename) ;
static void		stdio_test (const char *filetype) ;

static const char *filetypes [] =
{	"raw", "wav", "aiff", "au", "paf", "svx", "nist", "ircam",
	"voc", "w64", "mat4", "mat5", "pvf",
	NULL
} ;

int
main (void)
{	int k ;

	for (k = 0 ; filetypes [k] ; k++)
		stdio_test (filetypes [k]) ;

	return 0 ;
} /* main */


static void
stdio_test (const char *filetype)
{	static char buffer [256] ;

	int file_size, retval ;

	print_test_name ("stdio_test", filetype) ;

	snprintf (buffer, sizeof (buffer), "./tests/stdout_test %s > stdio.%s", filetype, filetype) ;
	if ((retval = system (buffer)))
	{	retval = WIFEXITED (retval) ? WEXITSTATUS (retval) : 1 ;
		printf ("%s : %s", buffer, (strerror (retval))) ;
		exit (1) ;
		} ;

	snprintf (buffer, sizeof (buffer), "stdio.%s", filetype) ;
	if ((file_size = file_length (buffer)) < PIPE_TEST_LEN)
	{	printf ("\n    Error : test file '%s' too small (%d).\n\n", buffer, file_size) ;
		exit (1) ;
		} ;

	snprintf (buffer, sizeof (buffer), "./tests/stdin_test %s < stdio.%s", filetype, filetype) ;
	if ((retval = system (buffer)))
	{	retval = WIFEXITED (retval) ? WEXITSTATUS (retval) : 1 ;
		printf ("%s : %s", buffer, (strerror (retval))) ;
		exit (1) ;
		} ;

	snprintf (buffer, sizeof (buffer), "rm stdio.%s", filetype) ;
	if ((retval = system (buffer)))
	{	retval = WIFEXITED (retval) ? WEXITSTATUS (retval) : 1 ;
		printf ("%s : %s", buffer, (strerror (retval))) ;
		exit (1) ;
		} ;

	puts ("ok") ;

	return ;
} /* stdio_test */




static size_t
file_length (const char *filename)
{	struct stat buf ;

	if (stat (filename, &buf))
	{	perror (filename) ;
		exit (1) ;
		} ;

	return buf.st_size ;
} /* file_length */

#endif

