/*
** Copyright (C) 1999-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<inttypes.h>
#include	<ctype.h>
#include	<math.h>

#include	<sndfile.h>

#include "common.h"

#define	BUFFER_LEN		(1 << 16)

#if (defined (WIN32) || defined (_WIN32))
#include <windows.h>
#define	snprintf	_snprintf
#endif

static void print_version (void) ;
static void print_usage (const char *progname) ;

static void info_dump (const char *filename) ;
static int	instrument_dump (const char *filename) ;
static int	broadcast_dump (const char *filename) ;
static void total_dump (void) ;

static double total_seconds = 0.0 ;

int
main (int argc, char *argv [])
{	int	k ;

	print_version () ;

	if (argc < 2 || strcmp (argv [1], "--help") == 0 || strcmp (argv [1], "-h") == 0)
	{	char *progname ;

		progname = strrchr (argv [0], '/') ;
		progname = progname ? progname + 1 : argv [0] ;

		print_usage (progname) ;
		return 1 ;
		} ;

	if (strcmp (argv [1], "-i") == 0)
	{	int error = 0 ;

		for (k = 2 ; k < argc ; k++)
			error += instrument_dump (argv [k]) ;
		return error ;
		} ;

	if (strcmp (argv [1], "-b") == 0)
	{	int error = 0 ;

		for (k = 2 ; k < argc ; k++)
			error += broadcast_dump (argv [k]) ;
		return error ;
		} ;

	for (k = 1 ; k < argc ; k++)
		info_dump (argv [k]) ;

	if (argc > 2)
		total_dump () ;

	return 0 ;
} /* main */

/*==============================================================================
**	Print version and usage.
*/

static double	data [BUFFER_LEN] ;

static void
print_version (void)
{	char buffer [256] ;

	sf_command (NULL, SFC_GET_LIB_VERSION, buffer, sizeof (buffer)) ;
	printf ("\nVersion : %s\n\n", buffer) ;
} /* print_version */


static void
print_usage (const char *progname)
{	printf ("Usage :\n  %s <file> ...\n", progname) ;
	printf ("    Prints out information about one or more sound files.\n\n") ;
	printf ("  %s -i <file>\n", progname) ;
	printf ("    Prints out the instrument data for the given file.\n\n") ;
	printf ("  %s -b <file>\n", progname) ;
	printf ("    Prints out the broadcast WAV info for the given file.\n\n") ;
#if (defined (_WIN32) || defined (WIN32))
		printf ("This is a Unix style command line application which\n"
				"should be run in a MSDOS box or Command Shell window.\n\n") ;
		printf ("Sleeping for 5 seconds before exiting.\n\n") ;
		fflush (stdout) ;

		/* This is the officially blessed by microsoft way but I can't get
		** it to link.
		**     Sleep (15) ;
		** Instead, use this:
		*/
		Sleep (5 * 1000) ;
#endif
} /* print_usage */

/*==============================================================================
**	Dumping of sndfile info.
*/

static double	data [BUFFER_LEN] ;

static double
get_signal_max (SNDFILE *file)
{	double	max ;

	sf_command (file, SFC_CALC_SIGNAL_MAX, &max, sizeof (max)) ;

	return max ;
} /* get_signal_max */

static double
calc_decibels (SF_INFO * sfinfo, double max)
{	double decibels ;

	switch (sfinfo->format & SF_FORMAT_SUBMASK)
	{	case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_PCM_S8 :
			decibels = max / 0x80 ;
			break ;

		case SF_FORMAT_PCM_16 :
			decibels = max / 0x8000 ;
			break ;

		case SF_FORMAT_PCM_24 :
			decibels = max / 0x800000 ;
			break ;

		case SF_FORMAT_PCM_32 :
			decibels = max / 0x80000000 ;
			break ;

		case SF_FORMAT_FLOAT :
		case SF_FORMAT_DOUBLE :
			decibels = max / 1.0 ;
			break ;

		default :
			decibels = max / 0x8000 ;
			break ;
		} ;

	return 20.0 * log10 (decibels) ;
} /* calc_decibels */

static const char *
format_duration_str (double seconds)
{	static char str [128] ;
	int hrs, min ;
	double sec ;

	memset (str, 0, sizeof (str)) ;

	hrs = (int) (seconds / 3600.0) ;
	min = (int) ((seconds - (hrs * 3600.0)) / 60.0) ;
	sec = seconds - (hrs * 3600.0) - (min * 60.0) ;

	snprintf (str, sizeof (str) - 1, "%02d:%02d:%06.3f", hrs, min, sec) ;

	return str ;
} /* format_duration_str */

static const char *
generate_duration_str (SF_INFO *sfinfo)
{
	double seconds ;

	if (sfinfo->samplerate < 1)
		return NULL ;

	if (sfinfo->frames / sfinfo->samplerate > 0x7FFFFFFF)
		return "unknown" ;

	seconds = (1.0 * sfinfo->frames) / sfinfo->samplerate ;

	/* Accumulate the total of all known file durations */
	total_seconds += seconds ;

	return format_duration_str (seconds) ;
} /* generate_duration_str */

static void
info_dump (const char *filename)
{	static	char	strbuffer [BUFFER_LEN] ;
	SNDFILE	 	*file ;
	SF_INFO	 	sfinfo ;
	double		signal_max, decibels ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	if ((file = sf_open (filename, SFM_READ, &sfinfo)) == NULL)
	{	printf ("Error : Not able to open input file %s.\n", filename) ;
		fflush (stdout) ;
		memset (data, 0, sizeof (data)) ;
		sf_command (file, SFC_GET_LOG_INFO, strbuffer, BUFFER_LEN) ;
		puts (strbuffer) ;
		puts (sf_strerror (NULL)) ;
		return ;
		} ;

	printf ("========================================\n") ;
	sf_command (file, SFC_GET_LOG_INFO, strbuffer, BUFFER_LEN) ;
	puts (strbuffer) ;
	printf ("----------------------------------------\n") ;

	if (file == NULL)
	{	printf ("Error : Not able to open input file %s.\n", filename) ;
		fflush (stdout) ;
		memset (data, 0, sizeof (data)) ;
		puts (sf_strerror (NULL)) ;
		}
	else
	{	printf ("Sample Rate : %d\n", sfinfo.samplerate) ;
		printf ("Frames      : %" PRId64 "\n", sfinfo.frames) ;
		printf ("Channels    : %d\n", sfinfo.channels) ;
		printf ("Format      : 0x%08X\n", sfinfo.format) ;
		printf ("Sections    : %d\n", sfinfo.sections) ;
		printf ("Seekable    : %s\n", (sfinfo.seekable ? "TRUE" : "FALSE")) ;
		printf ("Duration    : %s\n", generate_duration_str (&sfinfo)) ;

		if (sfinfo.frames < 100 * 1024 * 1024)
		{	/* Do not use sf_signal_max because it doesn't work for non-seekable files . */
			signal_max = get_signal_max (file) ;
			decibels = calc_decibels (&sfinfo, signal_max) ;
			printf ("Signal Max  : %g (%4.2f dB)\n", signal_max, decibels) ;
			} ;
		putchar ('\n') ;
		} ;

	sf_close (file) ;

} /* info_dump */

/*==============================================================================
**	Dumping of SF_INSTRUMENT data.
*/

static const char *
str_of_type (int mode)
{	switch (mode)
	{	case SF_LOOP_NONE : return "none" ;
		case SF_LOOP_FORWARD : return "fwd " ;
		case SF_LOOP_BACKWARD : return "back" ;
		case SF_LOOP_ALTERNATING : return "alt " ;
		default : break ;
		} ;

	return "????" ;
} /* str_of_mode */

static int
instrument_dump (const char *filename)
{	SNDFILE	 *file ;
	SF_INFO	 sfinfo ;
	SF_INSTRUMENT inst ;
	int got_inst, k ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	if ((file = sf_open (filename, SFM_READ, &sfinfo)) == NULL)
	{	printf ("Error : Not able to open input file %s.\n", filename) ;
		fflush (stdout) ;
		memset (data, 0, sizeof (data)) ;
		puts (sf_strerror (NULL)) ;
		return 1 ;
		} ;

	got_inst = sf_command (file, SFC_GET_INSTRUMENT, &inst, sizeof (inst)) ;
	sf_close (file) ;

	if (got_inst == SF_FALSE)
	{	printf ("Error : File '%s' does not contain instrument data.\n\n", filename) ;
		return 1 ;
		} ;

	printf ("Instrument : %s\n\n", filename) ;
	printf ("  Gain        : %d\n", inst.gain) ;
	printf ("  Base note   : %d\n", inst.basenote) ;
	printf ("  Velocity    : %d - %d\n", (int) inst.velocity_lo, (int) inst.velocity_hi) ;
	printf ("  Key         : %d - %d\n", (int) inst.key_lo, (int) inst.key_hi) ;
	printf ("  Loop points : %d\n", inst.loop_count) ;

	for (k = 0 ; k < inst.loop_count ; k++)
		printf ("  %-2d    Mode : %s    Start : %6d   End : %6d   Count : %6d\n", k, str_of_type (inst.loops [k].mode), inst.loops [k].start, inst.loops [k].end, inst.loops [k].count) ;

	putchar ('\n') ;
	return 0 ;
} /* instrument_dump */

static int
broadcast_dump (const char *filename)
{	SNDFILE	 *file ;
	SF_INFO	 sfinfo ;
	SF_BROADCAST_INFO_2K bext ;
	double time_ref_sec ;
	int got_bext ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	if ((file = sf_open (filename, SFM_READ, &sfinfo)) == NULL)
	{	printf ("Error : Not able to open input file %s.\n", filename) ;
		fflush (stdout) ;
		memset (data, 0, sizeof (data)) ;
		puts (sf_strerror (NULL)) ;
		return 1 ;
		} ;

	memset (&bext, 0, sizeof (SF_BROADCAST_INFO_2K)) ;

	got_bext = sf_command (file, SFC_GET_BROADCAST_INFO, &bext, sizeof (bext)) ;
	sf_close (file) ;

	if (got_bext == SF_FALSE)
	{	printf ("Error : File '%s' does not contain broadcast information.\n\n", filename) ;
		return 1 ;
		} ;

	/*
	**	From : http://www.ebu.ch/en/technical/publications/userguides/bwf_user_guide.php
	**
	**	Time Reference:
	**		This field is a count from midnight in samples to the first sample
	**		of the audio sequence.
	*/

	time_ref_sec = ((pow (2.0, 32) * bext.time_reference_high) + (1.0 * bext.time_reference_low)) / sfinfo.samplerate ;

	printf ("Description      : %.*s\n", (int) sizeof (bext.description), bext.description) ;
	printf ("Originator       : %.*s\n", (int) sizeof (bext.originator), bext.originator) ;
	printf ("Origination ref  : %.*s\n", (int) sizeof (bext.originator_reference), bext.originator_reference) ;
	printf ("Origination date : %.*s\n", (int) sizeof (bext.origination_date), bext.origination_date) ;
	printf ("Origination time : %.*s\n", (int) sizeof (bext.origination_time), bext.origination_time) ;

	if (bext.time_reference_high == 0 && bext.time_reference_low == 0)
		printf ("Time ref         : 0\n") ;
	else
		printf ("Time ref         : 0x%x%08x (%.6f seconds)\n", bext.time_reference_high, bext.time_reference_low, time_ref_sec) ;

	printf ("BWF version      : %d\n", bext.version) ;
	printf ("UMID             : %.*s\n", (int) sizeof (bext.umid), bext.umid) ;
	printf ("Coding history   : %.*s\n", bext.coding_history_size, bext.coding_history) ;

	return 0 ;
} /* broadcast_dump */

static void
total_dump (void)
{	printf ("========================================\n") ;
	printf ("Total Duration : %s\n", format_duration_str (total_seconds)) ;
} /* total_dump */
