[+ AutoGen5 template c +]
/*
** Copyright (C) 1999-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include <math.h>
#include <inttypes.h>


#if HAVE_UNISTD_H
#include <unistd.h>
#else
#include "sf_unistd.h"
#endif

#include <sndfile.h>

#include "utils.h"
#include "generate.h"

#define	SAMPLE_RATE			11025
#define	DATA_LENGTH			(1 << 12)

#define	SILLY_WRITE_COUNT	(234)

[+ FOR data_type
+]static void	pcm_test_[+ (get "type_name") +] (const char *str, int format, int long_file_ok) ;
[+ ENDFOR data_type
+]
static void empty_file_test (const char *filename, int format) ;

typedef union
{	double d [DATA_LENGTH] ;
	float f [DATA_LENGTH] ;
	int i [DATA_LENGTH] ;
	short s [DATA_LENGTH] ;
	char c [DATA_LENGTH] ;
} BUFFER ;

static	BUFFER	orig_data ;
static	BUFFER	test_data ;

int
main (int argc, char **argv)
{	int		do_all = 0 ;
	int		test_count = 0 ;

	count_open_files () ;

	if (argc != 2)
	{	printf ("Usage : %s <test>\n", argv [0]) ;
		printf ("    Where <test> is one of the following:\n") ;
		printf ("           wav   - test WAV file functions (little endian)\n") ;
		printf ("           aiff  - test AIFF file functions (big endian)\n") ;
		printf ("           au    - test AU file functions\n") ;
		printf ("           avr   - test AVR file functions\n") ;
		printf ("           caf   - test CAF file functions\n") ;
		printf ("           raw   - test RAW header-less PCM file functions\n") ;
		printf ("           paf   - test PAF file functions\n") ;
		printf ("           svx   - test 8SVX/16SV file functions\n") ;
		printf ("           nist  - test NIST Sphere file functions\n") ;
		printf ("           ircam - test IRCAM file functions\n") ;
		printf ("           voc   - Create Voice file functions\n") ;
		printf ("           w64   - Sonic Foundry's W64 file functions\n") ;
		printf ("           flac  - test FLAC file functions\n") ;
		printf ("           mpc2k - test MPC 2000 file functions\n") ;
		printf ("           rf64  - test RF64 file functions\n") ;
		printf ("           all   - perform all tests\n") ;
		exit (1) ;
		} ;

	do_all = !strcmp (argv [1], "all") ;

	if (do_all || ! strcmp (argv [1], "wav"))
	{	pcm_test_char	("char.wav"		, SF_FORMAT_WAV | SF_FORMAT_PCM_U8, SF_FALSE) ;
		pcm_test_short	("short.wav"	, SF_FORMAT_WAV | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit.wav"	, SF_FORMAT_WAV | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int.wav"		, SF_FORMAT_WAV | SF_FORMAT_PCM_32, SF_FALSE) ;

		pcm_test_char	("char.rifx"	, SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_PCM_U8, SF_FALSE) ;
		pcm_test_short	("short.rifx"	, SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit.rifx"	, SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int.rifx"		, SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_PCM_32, SF_FALSE) ;

		pcm_test_24bit	("24bit.wavex"	, SF_FORMAT_WAVEX | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int.wavex"	, SF_FORMAT_WAVEX | SF_FORMAT_PCM_32, SF_FALSE) ;

		/* Lite remove start */
		pcm_test_float	("float.wav"	, SF_FORMAT_WAV | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_double	("double.wav"	, SF_FORMAT_WAV | SF_FORMAT_DOUBLE, SF_FALSE) ;

		pcm_test_float	("float.rifx"	, SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_double	("double.rifx"	, SF_ENDIAN_BIG | SF_FORMAT_WAV | SF_FORMAT_DOUBLE, SF_FALSE) ;

		pcm_test_float	("float.wavex"	, SF_FORMAT_WAVEX | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_double	("double.wavex"	, SF_FORMAT_WAVEX | SF_FORMAT_DOUBLE, SF_FALSE) ;
		/* Lite remove end */

		empty_file_test ("empty_char.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_U8) ;
		empty_file_test ("empty_short.wav", SF_FORMAT_WAV | SF_FORMAT_PCM_16) ;
		empty_file_test ("empty_float.wav", SF_FORMAT_WAV | SF_FORMAT_FLOAT) ;

		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "aiff"))
	{	pcm_test_char	("char_u8.aiff"	, SF_FORMAT_AIFF | SF_FORMAT_PCM_U8, SF_FALSE) ;
		pcm_test_char	("char_s8.aiff"	, SF_FORMAT_AIFF | SF_FORMAT_PCM_S8, SF_FALSE) ;
		pcm_test_short	("short.aiff"	, SF_FORMAT_AIFF | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit.aiff"	, SF_FORMAT_AIFF | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int.aiff"		, SF_FORMAT_AIFF | SF_FORMAT_PCM_32, SF_FALSE) ;

		pcm_test_short	("short_sowt.aifc"	, SF_ENDIAN_LITTLE | SF_FORMAT_AIFF | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit_sowt.aifc"	, SF_ENDIAN_LITTLE | SF_FORMAT_AIFF | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int_sowt.aifc"	, SF_ENDIAN_LITTLE | SF_FORMAT_AIFF | SF_FORMAT_PCM_32, SF_FALSE) ;

		pcm_test_short	("short_twos.aifc"	, SF_ENDIAN_BIG | SF_FORMAT_AIFF | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit_twos.aifc"	, SF_ENDIAN_BIG | SF_FORMAT_AIFF | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int_twos.aifc"	, SF_ENDIAN_BIG | SF_FORMAT_AIFF | SF_FORMAT_PCM_32, SF_FALSE) ;

		/* Lite remove start */
		pcm_test_short	("dwvw16.aifc", SF_FORMAT_AIFF | SF_FORMAT_DWVW_16, SF_TRUE) ;
		pcm_test_24bit	("dwvw24.aifc", SF_FORMAT_AIFF | SF_FORMAT_DWVW_24, SF_TRUE) ;

		pcm_test_float	("float.aifc"	, SF_FORMAT_AIFF | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_double	("double.aifc"	, SF_FORMAT_AIFF | SF_FORMAT_DOUBLE, SF_FALSE) ;
		/* Lite remove end */

		empty_file_test ("empty_char.aiff", SF_FORMAT_AIFF | SF_FORMAT_PCM_U8) ;
		empty_file_test ("empty_short.aiff", SF_FORMAT_AIFF | SF_FORMAT_PCM_16) ;
		empty_file_test ("empty_float.aiff", SF_FORMAT_AIFF | SF_FORMAT_FLOAT) ;

		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "au"))
	{	pcm_test_char	("char.au"	, SF_FORMAT_AU | SF_FORMAT_PCM_S8, SF_FALSE) ;
		pcm_test_short	("short.au"	, SF_FORMAT_AU | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit.au"	, SF_FORMAT_AU | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int.au"	, SF_FORMAT_AU | SF_FORMAT_PCM_32, SF_FALSE) ;
		/* Lite remove start */
		pcm_test_float	("float.au"	, SF_FORMAT_AU | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_double	("double.au", SF_FORMAT_AU | SF_FORMAT_DOUBLE, SF_FALSE) ;
		/* Lite remove end */

		pcm_test_char	("char_le.au"	, SF_ENDIAN_LITTLE | SF_FORMAT_AU | SF_FORMAT_PCM_S8, SF_FALSE) ;
		pcm_test_short	("short_le.au"	, SF_ENDIAN_LITTLE | SF_FORMAT_AU | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit_le.au"	, SF_ENDIAN_LITTLE | SF_FORMAT_AU | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int_le.au"	, SF_ENDIAN_LITTLE | SF_FORMAT_AU | SF_FORMAT_PCM_32, SF_FALSE) ;
		/* Lite remove start */
		pcm_test_float	("float_le.au"	, SF_ENDIAN_LITTLE | SF_FORMAT_AU | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_double	("double_le.au"	, SF_ENDIAN_LITTLE | SF_FORMAT_AU | SF_FORMAT_DOUBLE, SF_FALSE) ;
		/* Lite remove end */
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "caf"))
	{	pcm_test_char	("char.caf"		, SF_FORMAT_CAF | SF_FORMAT_PCM_S8, SF_FALSE) ;
		pcm_test_short	("short.caf"	, SF_FORMAT_CAF | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit.caf"	, SF_FORMAT_CAF | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int.caf"		, SF_FORMAT_CAF | SF_FORMAT_PCM_32, SF_FALSE) ;
		/* Lite remove start */
		pcm_test_float	("float.caf"	, SF_FORMAT_CAF | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_double	("double.caf"	, SF_FORMAT_CAF | SF_FORMAT_DOUBLE, SF_FALSE) ;
		/* Lite remove end */

		pcm_test_short	("short_le.caf"	, SF_ENDIAN_LITTLE | SF_FORMAT_CAF | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit_le.caf"	, SF_ENDIAN_LITTLE | SF_FORMAT_CAF | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int_le.caf"	, SF_ENDIAN_LITTLE | SF_FORMAT_CAF | SF_FORMAT_PCM_32, SF_FALSE) ;
		/* Lite remove start */
		pcm_test_float	("float_le.caf"	, SF_ENDIAN_LITTLE | SF_FORMAT_CAF | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_double	("double_le.caf", SF_ENDIAN_LITTLE | SF_FORMAT_CAF | SF_FORMAT_DOUBLE, SF_FALSE) ;

		pcm_test_short	("alac16.caf"	, SF_FORMAT_CAF | SF_FORMAT_ALAC_16, SF_FALSE) ;
		pcm_test_20bit	("alac20.caf"	, SF_FORMAT_CAF | SF_FORMAT_ALAC_20, SF_FALSE) ;
		pcm_test_24bit	("alac24.caf"	, SF_FORMAT_CAF | SF_FORMAT_ALAC_24, SF_FALSE) ;
		pcm_test_int	("alac32.caf"	, SF_FORMAT_CAF | SF_FORMAT_ALAC_32, SF_FALSE) ;

		/* Lite remove end */
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "raw"))
	{	pcm_test_char	("char_s8.raw"	, SF_FORMAT_RAW | SF_FORMAT_PCM_S8, SF_FALSE) ;
		pcm_test_char	("char_u8.raw"	, SF_FORMAT_RAW | SF_FORMAT_PCM_U8, SF_FALSE) ;

		pcm_test_short	("short_le.raw"	, SF_ENDIAN_LITTLE	| SF_FORMAT_RAW | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_short	("short_be.raw"	, SF_ENDIAN_BIG		| SF_FORMAT_RAW | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit_le.raw"	, SF_ENDIAN_LITTLE	| SF_FORMAT_RAW | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_24bit	("24bit_be.raw"	, SF_ENDIAN_BIG		| SF_FORMAT_RAW | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int_le.raw"	, SF_ENDIAN_LITTLE	| SF_FORMAT_RAW | SF_FORMAT_PCM_32, SF_FALSE) ;
		pcm_test_int	("int_be.raw"	, SF_ENDIAN_BIG		| SF_FORMAT_RAW | SF_FORMAT_PCM_32, SF_FALSE) ;

		/* Lite remove start */
		pcm_test_float	("float_le.raw"	, SF_ENDIAN_LITTLE	| SF_FORMAT_RAW | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_float	("float_be.raw"	, SF_ENDIAN_BIG		| SF_FORMAT_RAW | SF_FORMAT_FLOAT , SF_FALSE) ;

		pcm_test_double	("double_le.raw", SF_ENDIAN_LITTLE	| SF_FORMAT_RAW | SF_FORMAT_DOUBLE, SF_FALSE) ;
		pcm_test_double	("double_be.raw", SF_ENDIAN_BIG		| SF_FORMAT_RAW | SF_FORMAT_DOUBLE, SF_FALSE) ;
		/* Lite remove end */
		test_count++ ;
		} ;

	/* Lite remove start */
	if (do_all || ! strcmp (argv [1], "paf"))
	{	pcm_test_char	("char_le.paf", SF_ENDIAN_LITTLE	| SF_FORMAT_PAF | SF_FORMAT_PCM_S8, SF_FALSE) ;
		pcm_test_char	("char_be.paf", SF_ENDIAN_BIG		| SF_FORMAT_PAF | SF_FORMAT_PCM_S8, SF_FALSE) ;
		pcm_test_short	("short_le.paf", SF_ENDIAN_LITTLE	| SF_FORMAT_PAF | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_short	("short_be.paf", SF_ENDIAN_BIG		| SF_FORMAT_PAF | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit_le.paf", SF_ENDIAN_LITTLE	| SF_FORMAT_PAF | SF_FORMAT_PCM_24, SF_TRUE) ;
		pcm_test_24bit	("24bit_be.paf", SF_ENDIAN_BIG		| SF_FORMAT_PAF | SF_FORMAT_PCM_24, SF_TRUE) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "svx"))
	{	pcm_test_char	("char.svx" , SF_FORMAT_SVX | SF_FORMAT_PCM_S8, SF_FALSE) ;
		pcm_test_short	("short.svx", SF_FORMAT_SVX | SF_FORMAT_PCM_16, SF_FALSE) ;

		empty_file_test ("empty_char.svx", SF_FORMAT_SVX | SF_FORMAT_PCM_S8) ;
		empty_file_test ("empty_short.svx", SF_FORMAT_SVX | SF_FORMAT_PCM_16) ;

		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "nist"))
	{	pcm_test_short	("short_le.nist", SF_ENDIAN_LITTLE	| SF_FORMAT_NIST | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_short	("short_be.nist", SF_ENDIAN_BIG		| SF_FORMAT_NIST | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit_le.nist", SF_ENDIAN_LITTLE	| SF_FORMAT_NIST | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_24bit	("24bit_be.nist", SF_ENDIAN_BIG		| SF_FORMAT_NIST | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int_le.nist"	, SF_ENDIAN_LITTLE	| SF_FORMAT_NIST | SF_FORMAT_PCM_32, SF_FALSE) ;
		pcm_test_int 	("int_be.nist"	, SF_ENDIAN_BIG		| SF_FORMAT_NIST | SF_FORMAT_PCM_32, SF_FALSE) ;

		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "ircam"))
	{	pcm_test_short	("short_be.ircam"	, SF_ENDIAN_BIG	| SF_FORMAT_IRCAM | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_short	("short_le.ircam"	, SF_ENDIAN_LITTLE	| SF_FORMAT_IRCAM | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_int	("int_be.ircam"		, SF_ENDIAN_BIG	| SF_FORMAT_IRCAM | SF_FORMAT_PCM_32, SF_FALSE) ;
		pcm_test_int 	("int_le.ircam"		, SF_ENDIAN_LITTLE	| SF_FORMAT_IRCAM | SF_FORMAT_PCM_32, SF_FALSE) ;
		pcm_test_float	("float_be.ircam"	, SF_ENDIAN_BIG	| SF_FORMAT_IRCAM | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_float	("float_le.ircam"	, SF_ENDIAN_LITTLE	| SF_FORMAT_IRCAM | SF_FORMAT_FLOAT , SF_FALSE) ;

		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "voc"))
	{	pcm_test_char 	("char.voc" , SF_FORMAT_VOC | SF_FORMAT_PCM_U8, SF_FALSE) ;
		pcm_test_short	("short.voc", SF_FORMAT_VOC | SF_FORMAT_PCM_16, SF_FALSE) ;

		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "mat4"))
	{	pcm_test_short	("short_be.mat4"	, SF_ENDIAN_BIG	| SF_FORMAT_MAT4 | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_short	("short_le.mat4"	, SF_ENDIAN_LITTLE	| SF_FORMAT_MAT4 | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_int	("int_be.mat4"		, SF_ENDIAN_BIG	| SF_FORMAT_MAT4 | SF_FORMAT_PCM_32, SF_FALSE) ;
		pcm_test_int 	("int_le.mat4"		, SF_ENDIAN_LITTLE	| SF_FORMAT_MAT4 | SF_FORMAT_PCM_32, SF_FALSE) ;
		pcm_test_float	("float_be.mat4"	, SF_ENDIAN_BIG	| SF_FORMAT_MAT4 | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_float	("float_le.mat4"	, SF_ENDIAN_LITTLE	| SF_FORMAT_MAT4 | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_double	("double_be.mat4"	, SF_ENDIAN_BIG	| SF_FORMAT_MAT4 | SF_FORMAT_DOUBLE, SF_FALSE) ;
		pcm_test_double	("double_le.mat4"	, SF_ENDIAN_LITTLE	| SF_FORMAT_MAT4 | SF_FORMAT_DOUBLE, SF_FALSE) ;

		empty_file_test ("empty_short.mat4", SF_FORMAT_MAT4 | SF_FORMAT_PCM_16) ;
		empty_file_test ("empty_float.mat4", SF_FORMAT_MAT4 | SF_FORMAT_FLOAT) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "mat5"))
	{	pcm_test_char 	("char_be.mat5"		, SF_ENDIAN_BIG	| SF_FORMAT_MAT5 | SF_FORMAT_PCM_U8, SF_FALSE) ;
		pcm_test_char 	("char_le.mat5"		, SF_ENDIAN_LITTLE	| SF_FORMAT_MAT5 | SF_FORMAT_PCM_U8, SF_FALSE) ;
		pcm_test_short	("short_be.mat5"	, SF_ENDIAN_BIG	| SF_FORMAT_MAT5 | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_short	("short_le.mat5"	, SF_ENDIAN_LITTLE	| SF_FORMAT_MAT5 | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_int	("int_be.mat5"		, SF_ENDIAN_BIG	| SF_FORMAT_MAT5 | SF_FORMAT_PCM_32, SF_FALSE) ;
		pcm_test_int 	("int_le.mat5"		, SF_ENDIAN_LITTLE	| SF_FORMAT_MAT5 | SF_FORMAT_PCM_32, SF_FALSE) ;
		pcm_test_float	("float_be.mat5"	, SF_ENDIAN_BIG	| SF_FORMAT_MAT5 | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_float	("float_le.mat5"	, SF_ENDIAN_LITTLE	| SF_FORMAT_MAT5 | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_double	("double_be.mat5"	, SF_ENDIAN_BIG	| SF_FORMAT_MAT5 | SF_FORMAT_DOUBLE, SF_FALSE) ;
		pcm_test_double	("double_le.mat5"	, SF_ENDIAN_LITTLE	| SF_FORMAT_MAT5 | SF_FORMAT_DOUBLE, SF_FALSE) ;

		increment_open_file_count () ;

		empty_file_test ("empty_char.mat5", SF_FORMAT_MAT5 | SF_FORMAT_PCM_U8) ;
		empty_file_test ("empty_short.mat5", SF_FORMAT_MAT5 | SF_FORMAT_PCM_16) ;
		empty_file_test ("empty_float.mat5", SF_FORMAT_MAT5 | SF_FORMAT_FLOAT) ;

		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "pvf"))
	{	pcm_test_char 	("char.pvf"	, SF_FORMAT_PVF | SF_FORMAT_PCM_S8, SF_FALSE) ;
		pcm_test_short	("short.pvf", SF_FORMAT_PVF | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_int	("int.pvf"	, SF_FORMAT_PVF | SF_FORMAT_PCM_32, SF_FALSE) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "htk"))
	{	pcm_test_short	("short.htk", SF_FORMAT_HTK | SF_FORMAT_PCM_16, SF_FALSE) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "mpc2k"))
	{	pcm_test_short	("short.mpc", SF_FORMAT_MPC2K | SF_FORMAT_PCM_16, SF_FALSE) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "avr"))
	{	pcm_test_char 	("char_u8.avr"	, SF_FORMAT_AVR | SF_FORMAT_PCM_U8, SF_FALSE) ;
		pcm_test_char 	("char_s8.avr"	, SF_FORMAT_AVR | SF_FORMAT_PCM_S8, SF_FALSE) ;
		pcm_test_short	("short.avr"	, SF_FORMAT_AVR | SF_FORMAT_PCM_16, SF_FALSE) ;
		test_count++ ;
		} ;
	/* Lite remove end */

	if (do_all || ! strcmp (argv [1], "w64"))
	{	pcm_test_char	("char.w64"		, SF_FORMAT_W64 | SF_FORMAT_PCM_U8, SF_FALSE) ;
		pcm_test_short	("short.w64"	, SF_FORMAT_W64 | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit.w64"	, SF_FORMAT_W64 | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int.w64"		, SF_FORMAT_W64 | SF_FORMAT_PCM_32, SF_FALSE) ;
		/* Lite remove start */
		pcm_test_float	("float.w64"	, SF_FORMAT_W64 | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_double	("double.w64"	, SF_FORMAT_W64 | SF_FORMAT_DOUBLE, SF_FALSE) ;
		/* Lite remove end */

		empty_file_test ("empty_char.w64", SF_FORMAT_W64 | SF_FORMAT_PCM_U8) ;
		empty_file_test ("empty_short.w64", SF_FORMAT_W64 | SF_FORMAT_PCM_16) ;
		empty_file_test ("empty_float.w64", SF_FORMAT_W64 | SF_FORMAT_FLOAT) ;

		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "sds"))
	{	pcm_test_char	("char.sds"		, SF_FORMAT_SDS | SF_FORMAT_PCM_S8, SF_FALSE) ;
		pcm_test_short	("short.sds"	, SF_FORMAT_SDS | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit.sds"	, SF_FORMAT_SDS | SF_FORMAT_PCM_24, SF_FALSE) ;

		empty_file_test ("empty_char.sds", SF_FORMAT_SDS | SF_FORMAT_PCM_S8) ;
		empty_file_test ("empty_short.sds", SF_FORMAT_SDS | SF_FORMAT_PCM_16) ;

		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "sd2"))
	{	pcm_test_char	("char.sd2"		, SF_FORMAT_SD2 | SF_FORMAT_PCM_S8, SF_TRUE) ;
		pcm_test_short	("short.sd2"	, SF_FORMAT_SD2 | SF_FORMAT_PCM_16, SF_TRUE) ;
		pcm_test_24bit	("24bit.sd2"	, SF_FORMAT_SD2 | SF_FORMAT_PCM_24, SF_TRUE) ;
		pcm_test_int	("32bit.sd2"	, SF_FORMAT_SD2 | SF_FORMAT_PCM_32, SF_TRUE) ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "flac"))
	{	if (HAVE_EXTERNAL_XIPH_LIBS)
		{	pcm_test_char	("char.flac"	, SF_FORMAT_FLAC | SF_FORMAT_PCM_S8, SF_TRUE) ;
			pcm_test_short	("short.flac"	, SF_FORMAT_FLAC | SF_FORMAT_PCM_16, SF_TRUE) ;
			pcm_test_24bit	("24bit.flac"	, SF_FORMAT_FLAC | SF_FORMAT_PCM_24, SF_TRUE) ;
			}
		else
			puts ("    No FLAC tests because FLAC support was not compiled in.") ;
		test_count++ ;
		} ;

	if (do_all || ! strcmp (argv [1], "rf64"))
	{	pcm_test_char	("char.rf64"	, SF_FORMAT_RF64 | SF_FORMAT_PCM_U8, SF_FALSE) ;
		pcm_test_short	("short.rf64"	, SF_FORMAT_RF64 | SF_FORMAT_PCM_16, SF_FALSE) ;
		pcm_test_24bit	("24bit.rf64"	, SF_FORMAT_RF64 | SF_FORMAT_PCM_24, SF_FALSE) ;
		pcm_test_int	("int.rf64"		, SF_FORMAT_RF64 | SF_FORMAT_PCM_32, SF_FALSE) ;

		/* Lite remove start */
		pcm_test_float	("float.rf64"	, SF_FORMAT_RF64 | SF_FORMAT_FLOAT , SF_FALSE) ;
		pcm_test_double	("double.rf64"	, SF_FORMAT_RF64 | SF_FORMAT_DOUBLE, SF_FALSE) ;
		empty_file_test ("empty_char.rf64", SF_FORMAT_RF64 | SF_FORMAT_PCM_U8) ;
		empty_file_test ("empty_short.rf64", SF_FORMAT_RF64 | SF_FORMAT_PCM_16) ;
		empty_file_test ("empty_float.rf64", SF_FORMAT_RF64 | SF_FORMAT_FLOAT) ;
		/* Lite remove end */

		test_count++ ;
		} ;

	if (test_count == 0)
	{	printf ("Mono : ************************************\n") ;
		printf ("Mono : *  No '%s' test defined.\n", argv [1]) ;
		printf ("Mono : ************************************\n") ;
		return 1 ;
		} ;

	/* Only open file descriptors should be stdin, stdout and stderr. */
	check_open_file_count_or_die (__LINE__) ;

	return 0 ;
} /* main */

/*============================================================================================
**	Helper functions and macros.
*/

static void	create_short_file (const char *filename) ;

#define	CHAR_ERROR(x, y)		(abs ((x) - (y)) > 255)
#define	INT_ERROR(x, y)			(((x) - (y)) != 0)
#define	BIT_20_ERROR(x, y)		(abs ((x) - (y)) > 4095)
#define	TRIBYTE_ERROR(x, y)		(abs ((x) - (y)) > 255)
#define	FLOAT_ERROR(x, y)		(fabs ((x) - (y)) > 1e-5)

#define CONVERT_DATA(k, len, new, orig)					\
			{	for ((k) = 0 ; (k) < (len) ; (k) ++)	\
					(new) [k] = (orig) [k] ;			\
				}

[+ FOR data_type
+]
/*======================================================================================
*/

static void mono_[+ (get "type_name") +]_test (const char *filename, int format, int long_file_ok, int allow_fd) ;
static void stereo_[+ (get "type_name") +]_test (const char *filename, int format, int long_file_ok, int allow_fd) ;
static void mono_rdwr_[+ (get "type_name") +]_test (const char *filename, int format, int long_file_ok, int allow_fd) ;
static void new_rdwr_[+ (get "type_name") +]_test (const char *filename, int format, int allow_fd) ;
static void multi_seek_test (const char * filename, int format) ;
static void write_seek_extend_test (const char * filename, int format) ;

static void
pcm_test_[+ (get "type_name") +] (const char *filename, int format, int long_file_ok)
{	SF_INFO		sfinfo ;
	[+ (get "data_type") +]		*orig ;
	int			k, allow_fd ;

	/* Sd2 files cannot be opened from an existing file descriptor. */
	allow_fd = ((format & SF_FORMAT_TYPEMASK) == SF_FORMAT_SD2) ? SF_FALSE : SF_TRUE ;

	print_test_name ("pcm_test_[+ (get "type_name") +]", filename) ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.frames		= SILLY_WRITE_COUNT ; /* Wrong length. Library should correct this on sf_close. */
	sfinfo.channels		= 1 ;
	sfinfo.format		= format ;

	test_sf_format_or_die (&sfinfo, __LINE__) ;

	gen_windowed_sine_double (orig_data.d, DATA_LENGTH, [+ (get "max_val") +]) ;

	orig = orig_data.[+ (get "data_field") +] ;

	/* Make this a macro so gdb steps over it in one go. */
	CONVERT_DATA (k, DATA_LENGTH, orig, orig_data.d) ;

	/* Some test broken out here. */

	mono_[+ (get "type_name") +]_test (filename, format, long_file_ok, allow_fd) ;

	/* Sub format DWVW does not allow seeking. */
	if ((format & SF_FORMAT_SUBMASK) == SF_FORMAT_DWVW_16 ||
			(format & SF_FORMAT_SUBMASK) == SF_FORMAT_DWVW_24)
	{	unlink (filename) ;
		printf ("no seek : ok\n") ;
		return ;
		} ;

	if ((format & SF_FORMAT_TYPEMASK) != SF_FORMAT_FLAC
		&& (format & SF_FORMAT_SUBMASK) != SF_FORMAT_ALAC_16
		&& (format & SF_FORMAT_SUBMASK) != SF_FORMAT_ALAC_20
		&& (format & SF_FORMAT_SUBMASK) != SF_FORMAT_ALAC_24
		&& (format & SF_FORMAT_SUBMASK) != SF_FORMAT_ALAC_32
		)
		mono_rdwr_[+ (get "type_name") +]_test (filename, format, long_file_ok, allow_fd) ;

	/* If the format doesn't support stereo we're done. */
	sfinfo.channels = 2 ;
	if (sf_format_check (&sfinfo) == 0)
	{	unlink (filename) ;
		puts ("no stereo : ok") ;
		return ;
		} ;

	stereo_[+ (get "type_name") +]_test (filename, format, long_file_ok, allow_fd) ;

	/* New read/write test. Not sure if this is needed yet. */

	if ((format & SF_FORMAT_TYPEMASK) != SF_FORMAT_PAF
			&& (format & SF_FORMAT_TYPEMASK) != SF_FORMAT_VOC
			&& (format & SF_FORMAT_TYPEMASK) != SF_FORMAT_FLAC
			&& (format & SF_FORMAT_SUBMASK) != SF_FORMAT_ALAC_16
			&& (format & SF_FORMAT_SUBMASK) != SF_FORMAT_ALAC_20
			&& (format & SF_FORMAT_SUBMASK) != SF_FORMAT_ALAC_24
			&& (format & SF_FORMAT_SUBMASK) != SF_FORMAT_ALAC_32
			)
		new_rdwr_[+ (get "type_name") +]_test (filename, format, allow_fd) ;

	delete_file (format, filename) ;

	puts ("ok") ;
	return ;
} /* pcm_test_[+ (get "type_name") +] */

static void
mono_[+ (get "type_name") +]_test (const char *filename, int format, int long_file_ok, int allow_fd)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	[+ (get "data_type") +]		*orig, *test ;
	sf_count_t	count ;
	int			k, items, total ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.frames		= SILLY_WRITE_COUNT ; /* Wrong length. Library should correct this on sf_close. */
	sfinfo.channels		= 1 ;
	sfinfo.format		= format ;

	orig = orig_data.[+ (get "data_field") +] ;
	test = test_data.[+ (get "data_field") +] ;

	items = DATA_LENGTH ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, allow_fd, __LINE__) ;

	if (sfinfo.frames || sfinfo.sections || sfinfo.seekable)
	{	printf ("\n\nLine %d : Weird SF_INFO fields.\n", __LINE__) ;
		exit (1) ;
		} ;

	sf_set_string (file, SF_STR_ARTIST, "Your name here") ;

	test_write_[+ (get "data_type") +]_or_die (file, 0, orig, items, __LINE__) ;
	sf_write_sync (file) ;
	test_write_[+ (get "data_type") +]_or_die (file, 0, orig, items, __LINE__) ;
	sf_write_sync (file) ;

	/* Add non-audio data after the audio. */
	sf_set_string (file, SF_STR_COPYRIGHT, "Copyright (c) 2003") ;

	sf_close (file) ;

	memset (test, 0, items * sizeof ([+ (get "data_type") +])) ;

	if ((format & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW)
		memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, allow_fd, __LINE__) ;

	if (sfinfo.format != format)
	{	printf ("\n\nLine %d : Mono : Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, format, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < 2 * items)
	{	printf ("\n\nLine %d : Mono : Incorrect number of frames in file (too short). (%" PRId64 " should be %d)\n", __LINE__, sfinfo.frames, items) ;
		exit (1) ;
		} ;

	if (! long_file_ok && sfinfo.frames > 2 * items)
	{	printf ("\n\nLine %d : Mono : Incorrect number of frames in file (too long). (%" PRId64 " should be %d)\n", __LINE__, sfinfo.frames, items) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != 1)
	{	printf ("\n\nLine %d : Mono : Incorrect number of channels in file.\n", __LINE__) ;
		exit (1) ;
		} ;

	if (sfinfo.seekable != 1)
	{	printf ("\n\nLine %d : File should be seekable.\n", __LINE__) ;
		exit (1) ;
		} ;

	check_log_buffer_or_die (file, __LINE__) ;

	test_read_[+ (get "data_type") +]_or_die (file, 0, test, items, __LINE__) ;
	for (k = 0 ; k < items ; k++)
		if ([+ (get "error_func") +] (orig [k], test [k]))
		{	printf ("\n\nLine %d: Mono : Incorrect sample A (#%d : [+ (get "format_char") +] => [+ (get "format_char") +]).\n", __LINE__, k, orig [k], test [k]) ;
			oct_save_[+ (get "data_type") +] (orig, test, items) ;
			exit (1) ;
			} ;

	/* Test multiple short reads. */
	test_seek_or_die (file, 0, SEEK_SET, 0, sfinfo.channels, __LINE__) ;

	total = 0 ;
	for (k = 1 ; k <= 32 ; k++)
	{	int ik ;

		test_read_[+ (get "data_type") +]_or_die (file, 0, test + total, k, __LINE__) ;
		total += k ;

		for (ik = 0 ; ik < total ; ik++)
			if ([+ (get "error_func") +] (orig [ik], test [ik]))
			{	printf ("\n\nLine %d : Mono : Incorrect sample A (#%d : [+ (get "format_char") +] => [+ (get "format_char") +]).\n", __LINE__, ik, orig [ik], test [ik]) ;
				exit (1) ;
				} ;
		} ;

	/* Seek to start of file. */
	test_seek_or_die (file, 0, SEEK_SET, 0, sfinfo.channels, __LINE__) ;

	test_read_[+ (get "data_type") +]_or_die (file, 0, test, 4, __LINE__) ;
	for (k = 0 ; k < 4 ; k++)
		if ([+ (get "error_func") +] (orig [k], test [k]))
		{	printf ("\n\nLine %d : Mono : Incorrect sample A (#%d : [+ (get "format_char") +] => [+ (get "format_char") +]).\n", __LINE__, k, orig [k], test [k]) ;
			exit (1) ;
			} ;

	/* For some codecs we can't go past here. */
	if ((format & SF_FORMAT_SUBMASK) == SF_FORMAT_DWVW_16 ||
			(format & SF_FORMAT_SUBMASK) == SF_FORMAT_DWVW_24)
	{	sf_close (file) ;
		unlink (filename) ;
		printf ("no seek : ") ;
		return ;
		} ;

	/* Seek to offset from start of file. */
	test_seek_or_die (file, items + 10, SEEK_SET, items + 10, sfinfo.channels, __LINE__) ;

	test_read_[+ (get "data_type") +]_or_die (file, 0, test + 10, 4, __LINE__) ;
	for (k = 10 ; k < 14 ; k++)
		if ([+ (get "error_func") +] (orig [k], test [k]))
		{	printf ("\n\nLine %d : Mono : Incorrect sample A (#%d : [+ (get "format_char") +] => [+ (get "format_char") +]).\n", __LINE__, k, test [k], orig [k]) ;
			exit (1) ;
			} ;

	/* Seek to offset from current position. */
	test_seek_or_die (file, 6, SEEK_CUR, items + 20, sfinfo.channels, __LINE__) ;

	test_read_[+ (get "data_type") +]_or_die (file, 0, test + 20, 4, __LINE__) ;
	for (k = 20 ; k < 24 ; k++)
		if ([+ (get "error_func") +] (orig [k], test [k]))
		{	printf ("\n\nLine %d : Mono : Incorrect sample A (#%d : [+ (get "format_char") +] => [+ (get "format_char") +]).\n", __LINE__, k, test [k], orig [k]) ;
			exit (1) ;
			} ;

	/* Seek to offset from end of file. */
	test_seek_or_die (file, -1 * (sfinfo.frames - 10), SEEK_END, 10, sfinfo.channels, __LINE__) ;

	test_read_[+ (get "data_type") +]_or_die (file, 0, test + 10, 4, __LINE__) ;
	for (k = 10 ; k < 14 ; k++)
		if ([+ (get "error_func") +] (orig [k], test [k]))
		{	printf ("\n\nLine %d : Mono : Incorrect sample D (#%d : [+ (get "format_char") +] => [+ (get "format_char") +]).\n", __LINE__, k, test [k], orig [k]) ;
			exit (1) ;
			} ;

	/* Check read past end of file followed by sf_seek (sndfile, 0, SEEK_CUR). */
	test_seek_or_die (file, 0, SEEK_SET, 0, sfinfo.channels, __LINE__) ;

	count = 0 ;
	while (count < sfinfo.frames)
		count += sf_read_[+ (get "data_type") +] (file, test, 311) ;

	/* Check that no error has occurred. */
	if (sf_error (file))
	{	printf ("\n\nLine %d : Mono : error where there shouldn't have been one.\n", __LINE__) ;
		puts (sf_strerror (file)) ;
		exit (1) ;
		} ;

	/* Check that we haven't read beyond EOF. */
	if (count > sfinfo.frames)
	{	printf ("\n\nLines %d : read past end of file (%" PRId64 " should be %" PRId64 ")\n", __LINE__, count, sfinfo.frames) ;
		exit (1) ;
		} ;

	test_seek_or_die (file, 0, SEEK_CUR, sfinfo.frames, sfinfo.channels, __LINE__) ;

	sf_close (file) ;

	multi_seek_test (filename, format) ;
	write_seek_extend_test (filename, format) ;

} /* mono_[+ (get "type_name") +]_test */

static void
stereo_[+ (get "type_name") +]_test (const char *filename, int format, int long_file_ok, int allow_fd)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	[+ (get "data_type") +]		*orig, *test ;
	int			k, items, frames ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.frames		= SILLY_WRITE_COUNT ; /* Wrong length. Library should correct this on sf_close. */
	sfinfo.channels		= 2 ;
	sfinfo.format		= format ;

	gen_windowed_sine_double (orig_data.d, DATA_LENGTH, [+ (get "max_val") +]) ;

	orig = orig_data.[+ (get "data_field") +] ;
	test = test_data.[+ (get "data_field") +] ;

	/* Make this a macro so gdb steps over it in one go. */
	CONVERT_DATA (k, DATA_LENGTH, orig, orig_data.d) ;

	items = DATA_LENGTH ;
	frames = items / sfinfo.channels ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, allow_fd, __LINE__) ;

	sf_set_string (file, SF_STR_ARTIST, "Your name here") ;

	test_writef_[+ (get "data_type") +]_or_die (file, 0, orig, frames, __LINE__) ;

	sf_set_string (file, SF_STR_COPYRIGHT, "Copyright (c) 2003") ;

	sf_close (file) ;

	memset (test, 0, items * sizeof ([+ (get "data_type") +])) ;

	if ((format & SF_FORMAT_TYPEMASK) != SF_FORMAT_RAW)
		memset (&sfinfo, 0, sizeof (sfinfo)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, allow_fd, __LINE__) ;

	if (sfinfo.format != format)
	{	printf ("\n\nLine %d : Stereo : Returned format incorrect (0x%08X => 0x%08X).\n",
				__LINE__, format, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < frames)
	{	printf ("\n\nLine %d : Stereo : Incorrect number of frames in file (too short). (%" PRId64 " should be %d)\n",
				__LINE__, sfinfo.frames, frames) ;
		exit (1) ;
		} ;

	if (! long_file_ok && sfinfo.frames > frames)
	{	printf ("\n\nLine %d : Stereo : Incorrect number of frames in file (too long). (%" PRId64 " should be %d)\n",
				__LINE__, sfinfo.frames, frames) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != 2)
	{	printf ("\n\nLine %d : Stereo : Incorrect number of channels in file.\n", __LINE__) ;
		exit (1) ;
		} ;

	check_log_buffer_or_die (file, __LINE__) ;

	test_readf_[+ (get "data_type") +]_or_die (file, 0, test, frames, __LINE__) ;
	for (k = 0 ; k < items ; k++)
		if ([+ (get "error_func") +] (test [k], orig [k]))
		{	printf ("\n\nLine %d : Stereo : Incorrect sample (#%d : [+ (get "format_char") +] => [+ (get "format_char") +]).\n", __LINE__, k, orig [k], test [k]) ;
			exit (1) ;
			} ;

	/* Seek to start of file. */
	test_seek_or_die (file, 0, SEEK_SET, 0, sfinfo.channels, __LINE__) ;

	test_readf_[+ (get "data_type") +]_or_die (file, 0, test, 2, __LINE__) ;
	for (k = 0 ; k < 4 ; k++)
		if ([+ (get "error_func") +] (test [k], orig [k]))
		{	printf ("\n\nLine %d : Stereo : Incorrect sample (#%d : [+ (get "format_char") +] => [+ (get "format_char") +]).\n", __LINE__, k, orig [k], test [k]) ;
			exit (1) ;
			} ;

	/* Seek to offset from start of file. */
	test_seek_or_die (file, 10, SEEK_SET, 10, sfinfo.channels, __LINE__) ;

	/* Check for errors here. */
	if (sf_error (file))
	{	printf ("Line %d: Should NOT return an error.\n", __LINE__) ;
		puts (sf_strerror (file)) ;
		exit (1) ;
		} ;

	if (sf_read_[+ (get "data_type") +] (file, test, 1) > 0)
	{	printf ("Line %d: Should return 0.\n", __LINE__) ;
		exit (1) ;
		} ;

	if (! sf_error (file))
	{	printf ("Line %d: Should return an error.\n", __LINE__) ;
		exit (1) ;
		} ;
	/*-----------------------*/

	test_readf_[+ (get "data_type") +]_or_die (file, 0, test + 10, 2, __LINE__) ;
	for (k = 20 ; k < 24 ; k++)
		if ([+ (get "error_func") +] (test [k], orig [k]))
		{	printf ("\n\nLine %d : Stereo : Incorrect sample (#%d : [+ (get "format_char") +] => [+ (get "format_char") +]).\n", __LINE__, k, orig [k], test [k]) ;
			exit (1) ;
			} ;

	/* Seek to offset from current position. */
	test_seek_or_die (file, 8, SEEK_CUR, 20, sfinfo.channels, __LINE__) ;

	test_readf_[+ (get "data_type") +]_or_die (file, 0, test + 20, 2, __LINE__) ;
	for (k = 40 ; k < 44 ; k++)
		if ([+ (get "error_func") +] (test [k], orig [k]))
		{	printf ("\n\nLine %d : Stereo : Incorrect sample (#%d : [+ (get "format_char") +] => [+ (get "format_char") +]).\n", __LINE__, k, orig [k], test [k]) ;
			exit (1) ;
			} ;

	/* Seek to offset from end of file. */
	test_seek_or_die (file, -1 * (sfinfo.frames - 10), SEEK_END, 10, sfinfo.channels, __LINE__) ;

	test_readf_[+ (get "data_type") +]_or_die (file, 0, test + 20, 2, __LINE__) ;
	for (k = 20 ; k < 24 ; k++)
		if ([+ (get "error_func") +] (test [k], orig [k]))
		{	printf ("\n\nLine %d : Stereo : Incorrect sample (#%d : [+ (get "format_char") +] => [+ (get "format_char") +]).\n", __LINE__, k, orig [k], test [k]) ;
			exit (1) ;
			} ;

	sf_close (file) ;
} /* stereo_[+ (get "type_name") +]_test */

static void
mono_rdwr_[+ (get "type_name") +]_test (const char *filename, int format, int long_file_ok, int allow_fd)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	[+ (get "data_type") +]		*orig, *test ;
	int			k, pass ;

	switch (format & SF_FORMAT_SUBMASK)
	{	case SF_FORMAT_ALAC_16 :
		case SF_FORMAT_ALAC_20 :
		case SF_FORMAT_ALAC_24 :
		case SF_FORMAT_ALAC_32 :
			allow_fd = 0 ;
			break ;

		default :
			break ;
		} ;

	orig = orig_data.[+ (get "data_field") +] ;
	test = test_data.[+ (get "data_field") +] ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= DATA_LENGTH ;
	sfinfo.channels		= 1 ;
	sfinfo.format		= format ;

	if ((format & SF_FORMAT_TYPEMASK) == SF_FORMAT_RAW
		|| (format & SF_FORMAT_TYPEMASK) == SF_FORMAT_AU
		|| (format & SF_FORMAT_TYPEMASK) == SF_FORMAT_SD2)
		unlink (filename) ;
	else
	{	/* Create a short file. */
		create_short_file (filename) ;

		/* Opening a already existing short file (ie invalid header) RDWR is disallowed.
		** If this returns a valif pointer sf_open() screwed up.
		*/
		if ((file = sf_open (filename, SFM_RDWR, &sfinfo)))
		{	printf ("\n\nLine %d: sf_open should (SFM_RDWR) have failed but didn't.\n", __LINE__) ;
			exit (1) ;
			} ;

		/* Truncate the file to zero bytes. */
		if (truncate_file_to_zero (filename) < 0)
		{	printf ("\n\nLine %d: truncate_file_to_zero (%s) failed", __LINE__, filename) ;
			perror (NULL) ;
			exit (1) ;
			} ;
		} ;

	/* Opening a zero length file RDWR is allowed, but the SF_INFO struct must contain
	** all the usual data required when opening the file in WRITE mode.
	*/
	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= DATA_LENGTH ;
	sfinfo.channels		= 1 ;
	sfinfo.format		= format ;

	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, allow_fd, __LINE__) ;

	/* Do 3 writes followed by reads. After each, check the data and the current
	** read and write offsets.
	*/
	for (pass = 1 ; pass <= 3 ; pass ++)
	{	orig [20] = pass * 2 ;

		/* Write some data. */
		test_write_[+ (get "data_type") +]_or_die (file, pass, orig, DATA_LENGTH, __LINE__) ;

		test_read_write_position_or_die (file, __LINE__, pass, (pass - 1) * DATA_LENGTH, pass * DATA_LENGTH) ;

		/* Read what we just wrote. */
		test_read_[+ (get "data_type") +]_or_die (file, 0, test, DATA_LENGTH, __LINE__) ;

		/* Check the data. */
		for (k = 0 ; k < DATA_LENGTH ; k++)
			if ([+ (get "error_func") +] (orig [k], test [k]))
			{	printf ("\n\nLine %d (pass %d) A : Error at sample %d ([+ (get "format_char") +] => [+ (get "format_char") +]).\n", __LINE__, pass, k, orig [k], test [k]) ;
				oct_save_[+ (get "data_type") +] (orig, test, DATA_LENGTH) ;
				exit (1) ;
				} ;

		test_read_write_position_or_die (file, __LINE__, pass, pass * DATA_LENGTH, pass * DATA_LENGTH) ;
		} ; /* for (pass ...) */

	sf_close (file) ;

	/* Open the file again to check the data. */
	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, allow_fd, __LINE__) ;

	if (sfinfo.format != format)
	{	printf ("\n\nLine %d : Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, format, sfinfo.format) ;
		exit (1) ;
		} ;

	if (sfinfo.frames < 3 * DATA_LENGTH)
	{	printf ("\n\nLine %d : Not enough frames in file. (%" PRId64 " < %d)\n", __LINE__, sfinfo.frames, 3 * DATA_LENGTH) ;
		exit (1) ;
		}

	if (! long_file_ok && sfinfo.frames != 3 * DATA_LENGTH)
	{	printf ("\n\nLine %d : Incorrect number of frames in file. (%" PRId64 " should be %d)\n", __LINE__, sfinfo.frames, 3 * DATA_LENGTH) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != 1)
	{	printf ("\n\nLine %d : Incorrect number of channels in file.\n", __LINE__) ;
		exit (1) ;
		} ;

	if (! long_file_ok)
		test_read_write_position_or_die (file, __LINE__, 0, 0, 3 * DATA_LENGTH) ;
	else
		test_seek_or_die (file, 3 * DATA_LENGTH, SFM_WRITE | SEEK_SET, 3 * DATA_LENGTH, sfinfo.channels, __LINE__) ;

	for (pass = 1 ; pass <= 3 ; pass ++)
	{	orig [20] = pass * 2 ;

		test_read_write_position_or_die (file, __LINE__, pass, (pass - 1) * DATA_LENGTH, 3 * DATA_LENGTH) ;

		/* Read what we just wrote. */
		test_read_[+ (get "data_type") +]_or_die (file, pass, test, DATA_LENGTH, __LINE__) ;

		/* Check the data. */
		for (k = 0 ; k < DATA_LENGTH ; k++)
			if ([+ (get "error_func") +] (orig [k], test [k]))
			{	printf ("\n\nLine %d (pass %d) B : Error at sample %d ([+ (get "format_char") +] => [+ (get "format_char") +]).\n", __LINE__, pass, k, orig [k], test [k]) ;
				oct_save_[+ (get "data_type") +] (orig, test, DATA_LENGTH) ;
				exit (1) ;
				} ;

		} ; /* for (pass ...) */

	sf_close (file) ;
} /* mono_rdwr_[+ (get "data_type") +]_test */

static void
new_rdwr_[+ (get "type_name") +]_test (const char *filename, int format, int allow_fd)
{	SNDFILE *wfile, *rwfile ;
	SF_INFO	sfinfo ;
	[+ (get "data_type") +]		*orig, *test ;
	int		items, frames ;

	orig = orig_data.[+ (get "data_field") +] ;
	test = test_data.[+ (get "data_field") +] ;

	sfinfo.samplerate	= 44100 ;
	sfinfo.frames		= SILLY_WRITE_COUNT ; /* Wrong length. Library should correct this on sf_close. */
	sfinfo.channels		= 2 ;
	sfinfo.format		= format ;

	items = DATA_LENGTH ;
	frames = items / sfinfo.channels ;

	wfile = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, allow_fd, __LINE__) ;
	sf_command (wfile, SFC_SET_UPDATE_HEADER_AUTO, NULL, SF_TRUE) ;
	test_writef_[+ (get "data_type") +]_or_die (wfile, 1, orig, frames, __LINE__) ;
	sf_write_sync (wfile) ;
	test_writef_[+ (get "data_type") +]_or_die (wfile, 2, orig, frames, __LINE__) ;
	sf_write_sync (wfile) ;

	rwfile = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, allow_fd, __LINE__) ;
	if (sfinfo.frames != 2 * frames)
	{	printf ("\n\nLine %d : incorrect number of frames in file (%" PRId64 " should be %d)\n\n", __LINE__, sfinfo.frames, 2 * frames) ;
		exit (1) ;
		} ;

	test_writef_[+ (get "data_type") +]_or_die (wfile, 3, orig, frames, __LINE__) ;

	test_readf_[+ (get "data_type") +]_or_die (rwfile, 1, test, frames, __LINE__) ;
	test_readf_[+ (get "data_type") +]_or_die (rwfile, 2, test, frames, __LINE__) ;

	sf_close (wfile) ;
	sf_close (rwfile) ;
} /* new_rdwr_[+ (get "type_name") +]_test */

[+ ENDFOR data_type +]

/*----------------------------------------------------------------------------------------
*/

static void
empty_file_test (const char *filename, int format)
{	SNDFILE		*file ;
	SF_INFO	info ;
	int allow_fd ;

	/* Sd2 files cannot be opened from an existing file descriptor. */
	allow_fd = ((format & SF_FORMAT_TYPEMASK) == SF_FORMAT_SD2) ? SF_FALSE : SF_TRUE ;

	print_test_name ("empty_file_test", filename) ;

	unlink (filename) ;

	info.samplerate = 48000 ;
	info.channels = 2 ;
	info.format = format ;
	info.frames = 0 ;

	if (sf_format_check (&info) == SF_FALSE)
	{	info.channels = 1 ;
		if (sf_format_check (&info) == SF_FALSE)
		{	puts ("invalid file format") ;
			return ;
			} ;
		} ;

	/* Create an empty file. */
	file = test_open_file_or_die (filename, SFM_WRITE, &info, allow_fd, __LINE__) ;
	sf_close (file) ;

	/* Open for read and check the length. */
	file = test_open_file_or_die (filename, SFM_READ, &info, allow_fd, __LINE__) ;

	if (info.frames != 0)
	{	printf ("\n\nError : frame count (%" PRId64 ") should be zero.\n", info.frames) ;
			exit (1) ;
			} ;

	sf_close (file) ;

	/* Open for read/write and check the length. */
	file = test_open_file_or_die (filename, SFM_RDWR, &info, allow_fd, __LINE__) ;

	if (info.frames != 0)
	{	printf ("\n\nError : frame count (%" PRId64 ") should be zero.\n", info.frames) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	/* Open for read and check the length. */
	file = test_open_file_or_die (filename, SFM_READ, &info, allow_fd, __LINE__) ;

	if (info.frames != 0)
	{	printf ("\n\nError : frame count (%" PRId64 ") should be zero.\n", info.frames) ;
		exit (1) ;
		} ;

	sf_close (file) ;

	check_open_file_count_or_die (__LINE__) ;

	unlink (filename) ;
	puts ("ok") ;

	return ;
} /* empty_file_test */


/*----------------------------------------------------------------------------------------
*/

static void
create_short_file (const char *filename)
{	FILE *file ;

	if (! (file = fopen (filename, "w")))
	{	printf ("create_short_file : fopen (%s, \"w\") failed.", filename) ;
		fflush (stdout) ;
		perror (NULL) ;
		exit (1) ;
		} ;

	fprintf (file, "This is the file data.\n") ;

	fclose (file) ;
} /* create_short_file */


static void
multi_seek_test (const char * filename, int format)
{	SNDFILE * file ;
	SF_INFO info ;
	sf_count_t pos ;
	int k ;

	/* This test doesn't work on the following. */
	switch (format & SF_FORMAT_TYPEMASK)
	{	case SF_FORMAT_RAW :
			return ;

		default :
			break ;
		} ;

	memset (&info, 0, sizeof (info)) ;

	generate_file (filename, format, 88200) ;

	file = test_open_file_or_die (filename, SFM_READ, &info, SF_FALSE, __LINE__) ;

	for (k = 0 ; k < 10 ; k++)
	{	pos = info.frames / (k + 2) ;
		test_seek_or_die (file, pos, SEEK_SET, pos, info.channels, __LINE__) ;
		} ;

	sf_close (file) ;
} /* multi_seek_test */

static void
write_seek_extend_test (const char * filename, int format)
{	SNDFILE * file ;
	SF_INFO info ;
	short	*orig, *test ;
	unsigned items, k ;

	/* This test doesn't work on the following container formats. */
	switch (format & SF_FORMAT_TYPEMASK)
	{	case SF_FORMAT_FLAC :
		case SF_FORMAT_HTK :
		case SF_FORMAT_PAF :
		case SF_FORMAT_SDS :
		case SF_FORMAT_SVX :
			return ;

		default :
			break ;
		} ;

	/* This test doesn't work on the following codec formats. */
	switch (format & SF_FORMAT_SUBMASK)
	{	case SF_FORMAT_ALAC_16 :
		case SF_FORMAT_ALAC_20 :
		case SF_FORMAT_ALAC_24 :
		case SF_FORMAT_ALAC_32 :
			return ;

		default :
			break ;
		} ;

	memset (&info, 0, sizeof (info)) ;

	info.samplerate = 48000 ;
	info.channels = 1 ;
	info.format = format ;

	items = 512 ;
	exit_if_true (items > ARRAY_LEN (orig_data.s), "Line %d : Bad assumption.\n", __LINE__) ;

	orig = orig_data.s ;
	test = test_data.s ;

	for (k = 0 ; k < ARRAY_LEN (orig_data.s) ; k++)
		orig [k] = 0x3fff ;

	file = test_open_file_or_die (filename, SFM_WRITE, &info, SF_FALSE, __LINE__) ;
	test_write_short_or_die (file, 0, orig, items, __LINE__) ;

	/* Extend the file using a seek. */
	test_seek_or_die (file, 2 * items, SEEK_SET, 2 * items, info.channels, __LINE__) ;

	test_writef_short_or_die (file, 0, orig, items, __LINE__) ;
	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &info, SF_FALSE, __LINE__) ;
	test_read_short_or_die (file, 0, test, 3 * items, __LINE__) ;
	sf_close (file) ;

	if (info.frames < 3 * items)
	{	printf ("\n\nLine %d : Incorrect number of frames in file (too short). (%" PRId64 " should be %d)\n", __LINE__, info.frames, 3 * items) ;
		exit (1) ;
		} ;

	/* Can't do these formats due to scaling. */
	switch (format & SF_FORMAT_SUBMASK)
	{	case SF_FORMAT_PCM_S8 :
		case SF_FORMAT_PCM_U8 :
			return ;
		default :
			break ;
		} ;

	for (k = 0 ; k < items ; k++)
	{	exit_if_true (test [k] != 0x3fff, "Line %d : test [%d] == %d, should be 0x3fff.\n", __LINE__, k, test [k]) ;
		exit_if_true (test [items + k] != 0, "Line %d : test [%d] == %d, should be 0.\n", __LINE__, items + k, test [items + k]) ;
		exit_if_true (test [2 * items + k] != 0x3fff, "Line %d : test [%d] == %d, should be 0x3fff.\n", __LINE__, 2 * items + k, test [2 * items + k]) ;
		} ;

	return ;
} /* write_seek_extend_test */


