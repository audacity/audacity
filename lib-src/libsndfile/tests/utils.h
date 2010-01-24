/*
** Copyright (C) 2002-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

/*
**	Utility functions to make writing the test suite easier.
**
**	The .c and .h files were generated automagically with Autogen from
**	the files utils.def and utils.tpl.
*/



#ifdef __cplusplus
extern "C" {
#endif	/* __cplusplus */

#include <stdint.h>
#include <stdarg.h>

#define SF_COUNT_TO_LONG(x)	((long) (x))
#define	ARRAY_LEN(x)		((int) (sizeof (x)) / (sizeof ((x) [0])))
#define SIGNED_SIZEOF(x)	((int64_t) (sizeof (x)))

#define	PIPE_INDEX(x)	((x) + 500)
#define	PIPE_TEST_LEN	12345

#if (defined (WIN32) || defined (_WIN32) || defined (__OS2__))
#define	snprintf	_snprintf
#endif

void gen_windowed_sine_float (float *data, int len, double maximum) ;
void gen_windowed_sine_double (double *data, int len, double maximum) ;


void	create_short_sndfile (const char *filename, int format, int channels) ;

void	check_file_hash_or_die	(const char *filename, uint64_t target_hash, int line_num) ;

void	print_test_name (const char *test, const char *filename) ;

void	dump_data_to_file (const char *filename, const void *data, unsigned int datalen) ;

void	write_mono_file (const char * filename, int format, int srate, float * output, int len) ;

static inline void
exit_if_true (int test, const char *format, ...)
{	if (test)
	{	va_list	argptr ;
		va_start (argptr, format) ;
		vprintf (format, argptr) ;
		va_end (argptr) ;
		exit (1) ;
		} ;
} /* exit_if_true */

/*
**	Functions for saving two vectors of data in an ascii text file which
**	can then be loaded into GNU octave for comparison.
*/

int	oct_save_short	(const short *a, const short *b, int len) ;
int	oct_save_int	(const int *a, const int *b, int len) ;
int	oct_save_float	(const float *a, const float *b, int len) ;
int	oct_save_double	(const double *a, const double *b, int len) ;


void	delete_file (int format, const char *filename) ;

void	count_open_files (void) ;
void	increment_open_file_count (void) ;
void	check_open_file_count_or_die (int lineno) ;

#ifdef SNDFILE_H

void 	dump_log_buffer (SNDFILE *file) ;
void 	check_log_buffer_or_die (SNDFILE *file, int line_num) ;
int 	string_in_log_buffer (SNDFILE *file, const char *s) ;
void	hexdump_file (const char * filename, sf_count_t offset, sf_count_t length) ;


SNDFILE *test_open_file_or_die
			(const char *filename, int mode, SF_INFO *sfinfo, int allow_fd, int line_num) ;

void 	test_read_write_position_or_die
			(SNDFILE *file, int line_num, int pass, sf_count_t read_pos, sf_count_t write_pos) ;

void	test_seek_or_die
			(SNDFILE *file, sf_count_t offset, int whence, sf_count_t new_pos, int channels, int line_num) ;


void 	test_read_short_or_die
			(SNDFILE *file, int pass, short *test, sf_count_t items, int line_num) ;
void 	test_read_int_or_die
			(SNDFILE *file, int pass, int *test, sf_count_t items, int line_num) ;
void 	test_read_float_or_die
			(SNDFILE *file, int pass, float *test, sf_count_t items, int line_num) ;
void 	test_read_double_or_die
			(SNDFILE *file, int pass, double *test, sf_count_t items, int line_num) ;

void 	test_readf_short_or_die
			(SNDFILE *file, int pass, short *test, sf_count_t frames, int line_num) ;
void 	test_readf_int_or_die
			(SNDFILE *file, int pass, int *test, sf_count_t frames, int line_num) ;
void 	test_readf_float_or_die
			(SNDFILE *file, int pass, float *test, sf_count_t frames, int line_num) ;
void 	test_readf_double_or_die
			(SNDFILE *file, int pass, double *test, sf_count_t frames, int line_num) ;



void 	test_write_short_or_die
			(SNDFILE *file, int pass, const short *test, sf_count_t items, int line_num) ;
void 	test_write_int_or_die
			(SNDFILE *file, int pass, const int *test, sf_count_t items, int line_num) ;
void 	test_write_float_or_die
			(SNDFILE *file, int pass, const float *test, sf_count_t items, int line_num) ;
void 	test_write_double_or_die
			(SNDFILE *file, int pass, const double *test, sf_count_t items, int line_num) ;

void 	test_writef_short_or_die
			(SNDFILE *file, int pass, const short *test, sf_count_t frames, int line_num) ;
void 	test_writef_int_or_die
			(SNDFILE *file, int pass, const int *test, sf_count_t frames, int line_num) ;
void 	test_writef_float_or_die
			(SNDFILE *file, int pass, const float *test, sf_count_t frames, int line_num) ;
void 	test_writef_double_or_die
			(SNDFILE *file, int pass, const double *test, sf_count_t frames, int line_num) ;


void compare_short_or_die (const short *left, const short *right, unsigned count, int line_num) ;
void compare_int_or_die (const int *left, const int *right, unsigned count, int line_num) ;
void compare_float_or_die (const float *left, const float *right, unsigned count, int line_num) ;
void compare_double_or_die (const double *left, const double *right, unsigned count, int line_num) ;



void	gen_lowpass_noise_float (float *data, int len) ;

sf_count_t		file_length (const char * fname) ;
sf_count_t		file_length_fd (int fd) ;

#endif

#ifdef __cplusplus
}		/* extern "C" */
#endif	/* __cplusplus */



