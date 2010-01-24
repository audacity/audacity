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

/*==========================================================================
** This is a test program which tests reading from and writing to pipes.
*/

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if (OS_IS_WIN32)

int
main (void)
{
	puts ("    pipe_test  : this test doesn't work on win32.") ;
	return 0 ;
} /* main */

#else

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <sndfile.h>

#include "utils.h"

typedef struct
{	int			format ;
	const char	*ext ;
} FILETYPE ;

static int		file_exists (const char *filename) ;
static void		useek_pipe_rw_test (int filetype, const char *ext) ;
static void		pipe_read_test (int filetype, const char *ext) ;
static void		pipe_write_test (const char *ext) ;
static void		pipe_test_others (FILETYPE*, FILETYPE*) ;

static FILETYPE read_write_types [] =
{	{	SF_FORMAT_RAW	, "raw"		},
	{	SF_FORMAT_AU	, "au"		},
	/* Lite remove start */
	{	SF_FORMAT_PAF	, "paf"		},
	{	SF_FORMAT_IRCAM	, "ircam"	},
	{	SF_FORMAT_PVF	, "pvf"	},
	/* Lite remove end */
	{	0				, NULL		}
} ;

static FILETYPE read_only_types [] =
{	{	SF_FORMAT_RAW	, "raw"		},
	{	SF_FORMAT_AU	, "au"		},
	{	SF_FORMAT_AIFF	, "aiff"	},
	{	SF_FORMAT_WAV	, "wav"		},
	{	SF_FORMAT_W64	, "w64"		},
	/* Lite remove start */
	{	SF_FORMAT_PAF	, "paf"		},
	{	SF_FORMAT_NIST	, "nist"	},
	{	SF_FORMAT_IRCAM	, "ircam"	},
	{	SF_FORMAT_MAT4	, "mat4"	},
	{	SF_FORMAT_MAT5	, "mat5"	},
	{	SF_FORMAT_SVX	, "svx"		},
	{	SF_FORMAT_PVF	, "pvf"		},
	/* Lite remove end */
	{	0				, NULL		}
} ;

int
main (void)
{	int k ;

	if (file_exists ("libsndfile.spec.in"))
		exit_if_true (chdir ("tests") != 0, "\n    Error : chdir ('tests') failed.\n") ;

	for (k = 0 ; read_only_types [k].format ; k++)
		pipe_read_test (read_only_types [k].format, read_only_types [k].ext) ;

	for (k = 0 ; read_write_types [k].format ; k++)
		pipe_write_test (read_write_types [k].ext) ;

	for (k = 0 ; read_write_types [k].format ; k++)
		useek_pipe_rw_test (read_write_types [k].format, read_write_types [k].ext) ;

	if (0)
		pipe_test_others (read_write_types, read_only_types) ;

	return 0 ;
} /* main */

/*==============================================================================
*/

static void
pipe_read_test (int filetype, const char *ext)
{	static short data [PIPE_TEST_LEN] ;
	static char buffer [256] ;
	static char filename [256] ;

	SNDFILE	*outfile ;
	SF_INFO sfinfo ;
	int k, retval ;

	snprintf (filename, sizeof (filename), "pipe_in.%s", ext) ;
	print_test_name ("pipe_read_test", filename) ;

	sfinfo.format = filetype | SF_FORMAT_PCM_16 ;
	sfinfo.channels = 1 ;
	sfinfo.samplerate = 44100 ;

	for (k = 0 ; k < PIPE_TEST_LEN ; k++)
		data [k] = PIPE_INDEX (k) ;

	outfile = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	test_writef_short_or_die (outfile, 0, data, PIPE_TEST_LEN, __LINE__) ;
	sf_close (outfile) ;

	snprintf (buffer, sizeof (buffer), "cat %s | ./stdin_test %s ", filename, ext) ;
	if ((retval = system (buffer)) != 0)
	{	retval = WEXITSTATUS (retval) ;
		printf ("\n\n    Line %d : pipe test returned error for file type \"%s\".\n\n", __LINE__, ext) ;
		exit (retval) ;
		} ;

	unlink (filename) ;
	puts ("ok") ;

	return ;
} /* pipe_read_test */

static void
pipe_write_test (const char *ext)
{	static char buffer [256] ;

	int retval ;

	print_test_name ("pipe_write_test", ext) ;

	snprintf (buffer, sizeof (buffer), "./stdout_test %s | ./stdin_test %s ", ext, ext) ;
	if ((retval = system (buffer)))
	{	retval = WEXITSTATUS (retval) ;
		printf ("\n\n     Line %d : pipe test returned error file type \"%s\".\n\n", __LINE__, ext) ;
		exit (retval) ;
		} ;

	puts ("ok") ;

	return ;
} /* pipe_write_test */

/*==============================================================================
*/


static void
useek_pipe_rw_short (const char * ext, SF_INFO * psfinfo_write, SF_INFO * psfinfo_read)
{	static short buffer [PIPE_TEST_LEN] ;
	static short data [PIPE_TEST_LEN] ;
	SNDFILE *outfile ;
	SNDFILE *infile_piped ;

	int k, status = 0 ;
	int pipefd [2] ;
	pid_t pida ;

	for (k = 0 ; k < PIPE_TEST_LEN ; k++)
		data [k] = PIPE_INDEX (k) ;

	/*
	** Create the pipe.
	*/
	exit_if_true (pipe (pipefd) != 0, "\n\n%s %d : pipe failed : %s\n", __func__, __LINE__, strerror (errno)) ;

	/*
	** Attach the write end of the pipe to be written to.
	*/
	if ((outfile = sf_open_fd (pipefd [1], SFM_WRITE, psfinfo_write, SF_TRUE)) == NULL)
	{	printf ("\n\n%s %d : unable to create unseekable pipe for write type \"%s\".\n", __func__, __LINE__, ext) ;
		printf ("\t%s\n\n", sf_strerror (outfile)) ;
		exit (1) ;
		} ;

	if (sf_error (outfile) != SF_ERR_NO_ERROR)
	{	printf ("\n\n%s %d : unable to open unseekable pipe for write type \"%s\".\n\n", __func__, __LINE__, ext) ;
		exit (1) ;
		} ;

	/*
	** Attach the read end of the pipe to be read from.
	*/
	if ((infile_piped = sf_open_fd (pipefd [0], SFM_READ, psfinfo_read, SF_TRUE)) == NULL)
	{	printf ("\n\n%s %d : unable to create unseekable pipe for read type. \"%s\".\n\n", __func__, __LINE__, ext) ;
		exit (1) ;
		} ;

	if (sf_error (infile_piped) != SF_ERR_NO_ERROR)
	{	printf ("\n\n%s %d : unable to open unseekable pipe for read type \"%s\".\n\n", __func__, __LINE__, ext) ;
		exit (1) ;
		} ;

	/* Fork a child process that will write directly into the pipe. */
	if ((pida = fork ()) == 0) /* child process */
	{	test_writef_short_or_die (outfile, 0, data, PIPE_TEST_LEN, __LINE__) ;
		exit (0) ;
		} ;

	/* In the parent process, read from the pipe and compare what is read
	** to what is written, if they match everything went as planned.
	*/
	test_readf_short_or_die (infile_piped, 0, buffer, PIPE_TEST_LEN, __LINE__) ;
	if (memcmp (buffer, data, sizeof (buffer)) != 0)
	{	printf ("\n\n%s %d : unseekable pipe test failed for file type \"%s\".\n\n", __func__, __LINE__, ext) ;
		exit (1) ;
		} ;

	/* Wait for the child process to return. */
	waitpid (pida, &status, 0) ;
	status = WEXITSTATUS (status) ;
	sf_close (outfile) ;
	sf_close (infile_piped) ;

	if (status != 0)
	{	printf ("\n\n%s %d : status of child process is %d for file type %s.\n\n", __func__, __LINE__, status, ext) ;
		exit (1) ;
		} ;

	return ;
} /* useek_pipe_rw_short */


static void
useek_pipe_rw_float (const char * ext, SF_INFO * psfinfo_write, SF_INFO * psfinfo_read)
{	static float buffer [PIPE_TEST_LEN] ;
	static float data [PIPE_TEST_LEN] ;
	SNDFILE *outfile ;
	SNDFILE *infile_piped ;

	int k, status = 0 ;
	int pipefd [2] ;
	pid_t pida ;

	for (k = 0 ; k < PIPE_TEST_LEN ; k++)
		data [k] = PIPE_INDEX (k) ;

	/*
	** Create the pipe.
	*/
	exit_if_true (pipe (pipefd) != 0, "\n\n%s %d : pipe failed : %s\n", __func__, __LINE__, strerror (errno)) ;

	/*
	** Attach the write end of the pipe to be written to.
	*/
	if ((outfile = sf_open_fd (pipefd [1], SFM_WRITE, psfinfo_write, SF_TRUE)) == NULL)
	{	printf ("\n\n%s %d : unable to create unseekable pipe for write type \"%s\".\n", __func__, __LINE__, ext) ;
		printf ("\t%s\n\n", sf_strerror (outfile)) ;
		exit (1) ;
		} ;

	if (sf_error (outfile) != SF_ERR_NO_ERROR)
	{	printf ("\n\n%s %d : unable to open unseekable pipe for write type \"%s\".\n\n", __func__, __LINE__, ext) ;
		exit (1) ;
		} ;

	/*
	** Attach the read end of the pipe to be read from.
	*/
	if ((infile_piped = sf_open_fd (pipefd [0], SFM_READ, psfinfo_read, SF_TRUE)) == NULL)
	{	printf ("\n\n%s %d : unable to create unseekable pipe for read type. \"%s\".\n\n", __func__, __LINE__, ext) ;
		exit (1) ;
		} ;

	if (sf_error (infile_piped) != SF_ERR_NO_ERROR)
	{	printf ("\n\n%s %d : unable to open unseekable pipe for read type \"%s\".\n\n", __func__, __LINE__, ext) ;
		exit (1) ;
		} ;

	/* Fork a child process that will write directly into the pipe. */
	if ((pida = fork ()) == 0) /* child process */
	{	test_writef_float_or_die (outfile, 0, data, PIPE_TEST_LEN, __LINE__) ;
		exit (0) ;
		} ;

	/* In the parent process, read from the pipe and compare what is read
	** to what is written, if they match everything went as planned.
	*/
	test_readf_float_or_die (infile_piped, 0, buffer, PIPE_TEST_LEN, __LINE__) ;
	if (memcmp (buffer, data, sizeof (buffer)) != 0)
	{	printf ("\n\n%s %d : unseekable pipe test failed for file type \"%s\".\n\n", __func__, __LINE__, ext) ;
		exit (1) ;
		} ;

	/* Wait for the child process to return. */
	waitpid (pida, &status, 0) ;
	status = WEXITSTATUS (status) ;
	sf_close (outfile) ;
	sf_close (infile_piped) ;

	if (status != 0)
	{	printf ("\n\n%s %d : status of child process is %d for file type %s.\n\n", __func__, __LINE__, status, ext) ;
		exit (1) ;
		} ;

	return ;
} /* useek_pipe_rw_float */


static void
useek_pipe_rw_double (const char * ext, SF_INFO * psfinfo_write, SF_INFO * psfinfo_read)
{	static double buffer [PIPE_TEST_LEN] ;
	static double data [PIPE_TEST_LEN] ;
	SNDFILE *outfile ;
	SNDFILE *infile_piped ;

	int k, status = 0 ;
	int pipefd [2] ;
	pid_t pida ;

	for (k = 0 ; k < PIPE_TEST_LEN ; k++)
		data [k] = PIPE_INDEX (k) ;

	/*
	** Create the pipe.
	*/
	exit_if_true (pipe (pipefd) != 0, "\n\n%s %d : pipe failed : %s\n", __func__, __LINE__, strerror (errno)) ;

	/*
	** Attach the write end of the pipe to be written to.
	*/
	if ((outfile = sf_open_fd (pipefd [1], SFM_WRITE, psfinfo_write, SF_TRUE)) == NULL)
	{	printf ("\n\n%s %d : unable to create unseekable pipe for write type \"%s\".\n", __func__, __LINE__, ext) ;
		printf ("\t%s\n\n", sf_strerror (outfile)) ;
		exit (1) ;
		} ;

	if (sf_error (outfile) != SF_ERR_NO_ERROR)
	{	printf ("\n\n%s %d : unable to open unseekable pipe for write type \"%s\".\n\n", __func__, __LINE__, ext) ;
		exit (1) ;
		} ;

	/*
	** Attach the read end of the pipe to be read from.
	*/
	if ((infile_piped = sf_open_fd (pipefd [0], SFM_READ, psfinfo_read, SF_TRUE)) == NULL)
	{	printf ("\n\n%s %d : unable to create unseekable pipe for read type. \"%s\".\n\n", __func__, __LINE__, ext) ;
		exit (1) ;
		} ;

	if (sf_error (infile_piped) != SF_ERR_NO_ERROR)
	{	printf ("\n\n%s %d : unable to open unseekable pipe for read type \"%s\".\n\n", __func__, __LINE__, ext) ;
		exit (1) ;
		} ;

	/* Fork a child process that will write directly into the pipe. */
	if ((pida = fork ()) == 0) /* child process */
	{	test_writef_double_or_die (outfile, 0, data, PIPE_TEST_LEN, __LINE__) ;
		exit (0) ;
		} ;

	/* In the parent process, read from the pipe and compare what is read
	** to what is written, if they match everything went as planned.
	*/
	test_readf_double_or_die (infile_piped, 0, buffer, PIPE_TEST_LEN, __LINE__) ;
	if (memcmp (buffer, data, sizeof (buffer)) != 0)
	{	printf ("\n\n%s %d : unseekable pipe test failed for file type \"%s\".\n\n", __func__, __LINE__, ext) ;
		exit (1) ;
		} ;

	/* Wait for the child process to return. */
	waitpid (pida, &status, 0) ;
	status = WEXITSTATUS (status) ;
	sf_close (outfile) ;
	sf_close (infile_piped) ;

	if (status != 0)
	{	printf ("\n\n%s %d : status of child process is %d for file type %s.\n\n", __func__, __LINE__, status, ext) ;
		exit (1) ;
		} ;

	return ;
} /* useek_pipe_rw_double */




static void
useek_pipe_rw_test (int filetype, const char *ext)
{	SF_INFO sfinfo_write ;
	SF_INFO sfinfo_read ;

	print_test_name ("useek_pipe_rw_test", ext) ;

	/*
	** Setup the INFO structures for the filetype we will be
	** working with.
	*/
	sfinfo_write.format = filetype | SF_FORMAT_PCM_16 ;
	sfinfo_write.channels = 1 ;
	sfinfo_write.samplerate = 44100 ;


	sfinfo_read.format = 0 ;
	if (filetype == SF_FORMAT_RAW)
	{	sfinfo_read.format = filetype | SF_FORMAT_PCM_16 ;
		sfinfo_read.channels = 1 ;
		sfinfo_read.samplerate = 44100 ;
		} ;

	useek_pipe_rw_short (ext, &sfinfo_write, &sfinfo_read) ;

	sfinfo_read.format = sfinfo_write.format = filetype | SF_FORMAT_FLOAT ;
	if (sf_format_check (&sfinfo_read) != 0)
		useek_pipe_rw_float (ext, &sfinfo_write, &sfinfo_read) ;

	sfinfo_read.format = sfinfo_write.format = filetype | SF_FORMAT_DOUBLE ;
	if (sf_format_check (&sfinfo_read) != 0)
		useek_pipe_rw_double (ext, &sfinfo_write, &sfinfo_read) ;

	puts ("ok") ;
	return ;
} /* useek_pipe_rw_test */



static void
pipe_test_others (FILETYPE* list1, FILETYPE* list2)
{	SF_FORMAT_INFO	info ;
	int		k, m, major_count, in_list ;

	print_test_name ("pipe_test_others", "") ;

	sf_command (NULL, SFC_GET_FORMAT_MAJOR_COUNT, &major_count, sizeof (int)) ;

	for (k = 0 ; k < major_count ; k++)
	{	info.format = k ;

		sf_command (NULL, SFC_GET_FORMAT_MAJOR, &info, sizeof (info)) ;

		in_list = SF_FALSE ;
		for (m = 0 ; list1 [m].format ; m++)
			if (info.format == list1 [m].format)
				in_list = SF_TRUE ;

		for (m = 0 ; list2 [m].format ; m++)
			if (info.format == list2 [m].format)
				in_list = SF_TRUE ;

		if (in_list)
			continue ;

		printf ("%s  %x\n", info.name, info.format) ;

		if (1)
		{	static short data [PIPE_TEST_LEN] ;
			static char buffer [256] ;
			static const char *filename = "pipe_in.dat" ;

			SNDFILE	*outfile ;
			SF_INFO sfinfo ;
			int retval ;

			sfinfo.format = info.format | SF_FORMAT_PCM_16 ;
			sfinfo.channels = 1 ;
			sfinfo.samplerate = 44100 ;

			outfile = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
			test_writef_short_or_die (outfile, 0, data, PIPE_TEST_LEN, __LINE__) ;
			sf_close (outfile) ;

			snprintf (buffer, sizeof (buffer), "cat %s | ./stdin_test %s %d ", filename, info.extension, PIPE_TEST_LEN) ;
			if ((retval = system (buffer)) == 0)
			{	retval = WEXITSTATUS (retval) ;
				printf ("\n\n     Line %d : pipe test should have returned error file type \"%s\" but didn't.\n\n", __LINE__, info.name) ;
				exit (1) ;
				} ;

			unlink (filename) ;
			} ;
		} ;


	puts ("ok") ;

	return ;
} /* pipe_test_others */


/*==============================================================================
*/

static int
file_exists (const char *filename)
{	struct stat buf ;

	if (stat (filename, &buf))
		return 0 ;

	return 1 ;
} /* file_exists */

#endif

