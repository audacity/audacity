[+ AutoGen5 template c +]
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
#include <math.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <sndfile.h>

#include "dft_cmp.h"
#include "utils.h"

#define	SAMPLE_RATE			16000

static void	float_scaled_test	(const char *filename, int allow_exit, int replace_float, int filetype, double target_snr) ;
static void	double_scaled_test	(const char *filename, int allow_exit, int replace_float, int filetype, double target_snr) ;

[+ FOR float_type +][+ FOR int_type +][+ FOR endian_type
+]static void [+ (get "float_name") +]_[+ (get "int_name") +]_[+ (get "end_name") +]_test (const char * filename) ;
[+ ENDFOR endian_type +][+ ENDFOR int_type +][+ ENDFOR float_type
+]

static	double	double_data [DFT_DATA_LENGTH] ;
static	double	double_test [DFT_DATA_LENGTH] ;

static float	float_data [DFT_DATA_LENGTH] ;
static float	float_test [DFT_DATA_LENGTH] ;

static double	double_data [DFT_DATA_LENGTH] ;
static short	short_data [DFT_DATA_LENGTH] ;
static int		int_data [DFT_DATA_LENGTH] ;

int
main (int argc, char *argv [])
{	int allow_exit = 1 ;

	if (argc == 2 && ! strstr (argv [1], "no-exit"))
		allow_exit = 0 ;

#if ((HAVE_LRINTF == 0) && (HAVE_LRINT_REPLACEMENT == 0))
	puts ("*** Cannot run this test on this platform because it lacks lrintf().") ;
	exit (0) ;
#endif

	/* Float tests. */
	float_scaled_test	("float.raw", allow_exit, SF_FALSE, SF_ENDIAN_LITTLE | SF_FORMAT_RAW | SF_FORMAT_FLOAT, -163.0) ;

	/* Test both signed and unsigned 8 bit files. */
	float_scaled_test	("pcm_s8.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_PCM_S8, -39.0) ;
	float_scaled_test	("pcm_u8.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_PCM_U8, -39.0) ;

	float_scaled_test	("pcm_16.raw", allow_exit, SF_FALSE, SF_ENDIAN_BIG | SF_FORMAT_RAW | SF_FORMAT_PCM_16, -87.0) ;
	float_scaled_test	("pcm_24.raw", allow_exit, SF_FALSE, SF_ENDIAN_LITTLE | SF_FORMAT_RAW | SF_FORMAT_PCM_24, -138.0) ;
	float_scaled_test	("pcm_32.raw", allow_exit, SF_FALSE, SF_ENDIAN_BIG | SF_FORMAT_RAW | SF_FORMAT_PCM_32, -163.0) ;

	float_scaled_test	("ulaw.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_ULAW, -50.0) ;
	float_scaled_test	("alaw.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_ALAW, -49.0) ;

	float_scaled_test	("ima_adpcm.wav", allow_exit, SF_FALSE, SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, -47.0) ;
	float_scaled_test	("ms_adpcm.wav" , allow_exit, SF_FALSE, SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, -40.0) ;
	float_scaled_test	("gsm610.raw"	, allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_GSM610, -33.0) ;

	float_scaled_test	("g721_32.au", allow_exit, SF_FALSE, SF_FORMAT_AU | SF_FORMAT_G721_32, -34.0) ;
	float_scaled_test	("g723_24.au", allow_exit, SF_FALSE, SF_FORMAT_AU | SF_FORMAT_G723_24, -34.0) ;
	float_scaled_test	("g723_40.au", allow_exit, SF_FALSE, SF_FORMAT_AU | SF_FORMAT_G723_40, -40.0) ;

	/*	PAF files do not use the same encoding method for 24 bit PCM data as other file
	**	formats so we need to explicitly test it here.
	*/
	float_scaled_test	("le_paf_24.paf", allow_exit, SF_FALSE, SF_ENDIAN_LITTLE | SF_FORMAT_PAF | SF_FORMAT_PCM_24, -149.0) ;
	float_scaled_test	("be_paf_24.paf", allow_exit, SF_FALSE, SF_ENDIAN_BIG | SF_FORMAT_PAF | SF_FORMAT_PCM_24, -149.0) ;

	float_scaled_test	("dwvw_12.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_DWVW_12, -64.0) ;
	float_scaled_test	("dwvw_16.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_DWVW_16, -92.0) ;
	float_scaled_test	("dwvw_24.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_DWVW_24, -151.0) ;

	float_scaled_test	("adpcm.vox", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM, -40.0) ;

	float_scaled_test	("dpcm_16.xi", allow_exit, SF_FALSE, SF_FORMAT_XI | SF_FORMAT_DPCM_16, -90.0) ;
	float_scaled_test	("dpcm_8.xi" , allow_exit, SF_FALSE, SF_FORMAT_XI | SF_FORMAT_DPCM_8 , -41.0) ;

	float_scaled_test	("pcm_s8.sds", allow_exit, SF_FALSE, SF_FORMAT_SDS | SF_FORMAT_PCM_S8, -90.0) ;
	float_scaled_test	("pcm_16.sds", allow_exit, SF_FALSE, SF_FORMAT_SDS | SF_FORMAT_PCM_16, -140.0) ;
	float_scaled_test	("pcm_24.sds", allow_exit, SF_FALSE, SF_FORMAT_SDS | SF_FORMAT_PCM_24, -170.0) ;

#if HAVE_EXTERNAL_LIBS
	float_scaled_test	("flac_8.flac", allow_exit, SF_FALSE, SF_FORMAT_FLAC | SF_FORMAT_PCM_S8, -39.0) ;
	float_scaled_test	("flac_16.flac", allow_exit, SF_FALSE, SF_FORMAT_FLAC | SF_FORMAT_PCM_16, -87.0) ;
	float_scaled_test	("flac_24.flac", allow_exit, SF_FALSE, SF_FORMAT_FLAC | SF_FORMAT_PCM_24, -138.0) ;

	float_scaled_test	("vorbis.oga", allow_exit, SF_FALSE, SF_FORMAT_OGG | SF_FORMAT_VORBIS, -31.0) ;
#endif

	float_scaled_test	("replace_float.raw", allow_exit, SF_TRUE, SF_ENDIAN_LITTLE | SF_FORMAT_RAW | SF_FORMAT_FLOAT, -163.0) ;

	/*==============================================================================
	** Double tests.
	*/

	double_scaled_test	("double.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_DOUBLE, -300.0) ;

	/* Test both signed (AIFF) and unsigned (WAV) 8 bit files. */
	double_scaled_test	("pcm_s8.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_PCM_S8, -39.0) ;
	double_scaled_test	("pcm_u8.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_PCM_U8, -39.0) ;

	double_scaled_test	("pcm_16.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_PCM_16, -87.0) ;
	double_scaled_test	("pcm_24.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_PCM_24, -135.0) ;
	double_scaled_test	("pcm_32.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_PCM_32, -184.0) ;

	double_scaled_test	("ulaw.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_ULAW, -50.0) ;
	double_scaled_test	("alaw.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_ALAW, -49.0) ;

	double_scaled_test	("ima_adpcm.wav", allow_exit, SF_FALSE, SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM, -47.0) ;
	double_scaled_test	("ms_adpcm.wav"	, allow_exit, SF_FALSE, SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM, -40.0) ;
	double_scaled_test	("gsm610.raw"	, allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_GSM610, -33.0) ;

	double_scaled_test	("g721_32.au", allow_exit, SF_FALSE, SF_FORMAT_AU | SF_FORMAT_G721_32, -34.0) ;
	double_scaled_test	("g723_24.au", allow_exit, SF_FALSE, SF_FORMAT_AU | SF_FORMAT_G723_24, -34.0) ;
	double_scaled_test	("g723_40.au", allow_exit, SF_FALSE, SF_FORMAT_AU | SF_FORMAT_G723_40, -40.0) ;

	/*	24 bit PCM PAF files tested here. */
	double_scaled_test	("be_paf_24.paf", allow_exit, SF_FALSE, SF_ENDIAN_BIG | SF_FORMAT_PAF | SF_FORMAT_PCM_24, -151.0) ;
	double_scaled_test	("le_paf_24.paf", allow_exit, SF_FALSE, SF_ENDIAN_LITTLE | SF_FORMAT_PAF | SF_FORMAT_PCM_24, -151.0) ;

	double_scaled_test	("dwvw_12.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_DWVW_12, -64.0) ;
	double_scaled_test	("dwvw_16.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_DWVW_16, -92.0) ;
	double_scaled_test	("dwvw_24.raw", allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_DWVW_24, -151.0) ;

	double_scaled_test	("adpcm.vox" , allow_exit, SF_FALSE, SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM, -40.0) ;

	double_scaled_test	("dpcm_16.xi", allow_exit, SF_FALSE, SF_FORMAT_XI | SF_FORMAT_DPCM_16, -90.0) ;
	double_scaled_test	("dpcm_8.xi" , allow_exit, SF_FALSE, SF_FORMAT_XI | SF_FORMAT_DPCM_8 , -42.0) ;

	double_scaled_test	("pcm_s8.sds", allow_exit, SF_FALSE, SF_FORMAT_SDS | SF_FORMAT_PCM_S8, -90.0) ;
	double_scaled_test	("pcm_16.sds", allow_exit, SF_FALSE, SF_FORMAT_SDS | SF_FORMAT_PCM_16, -140.0) ;
	double_scaled_test	("pcm_24.sds", allow_exit, SF_FALSE, SF_FORMAT_SDS | SF_FORMAT_PCM_24, -180.0) ;

#if HAVE_EXTERNAL_LIBS
	double_scaled_test	("flac_8.flac", allow_exit, SF_FALSE, SF_FORMAT_FLAC | SF_FORMAT_PCM_S8, -39.0) ;
	double_scaled_test	("flac_16.flac", allow_exit, SF_FALSE, SF_FORMAT_FLAC | SF_FORMAT_PCM_16, -87.0) ;
	double_scaled_test	("flac_24.flac", allow_exit, SF_FALSE, SF_FORMAT_FLAC | SF_FORMAT_PCM_24, -138.0) ;

	double_scaled_test	("vorbis.oga", allow_exit, SF_FALSE, SF_FORMAT_OGG | SF_FORMAT_VORBIS, -29.0) ;
#endif

	double_scaled_test	("replace_double.raw", allow_exit, SF_TRUE, SF_FORMAT_RAW | SF_FORMAT_DOUBLE, -300.0) ;

	putchar ('\n') ;
	/* Float int tests. */
[+ FOR float_type +][+ FOR int_type +][+ FOR endian_type
+]	[+ (get "float_name") +]_[+ (get "int_name") +]_[+ (get "end_name") +]_test ("[+ (get "float_name") +]_[+ (get "int_name") +]_[+ (get "end_name") +].au") ;
[+ ENDFOR endian_type +][+ ENDFOR int_type +][+ ENDFOR float_type
+]

	return 0 ;
} /* main */

/*============================================================================================
 *	Here are the test functions.
 */

static void
float_scaled_test (const char *filename, int allow_exit, int replace_float, int filetype, double target_snr)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	double		snr ;

	print_test_name ("float_scaled_test", filename) ;

	gen_windowed_sine_float (float_data, DFT_DATA_LENGTH, 1.0) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= DFT_DATA_LENGTH ;
	sfinfo.channels		= 1 ;
	sfinfo.format		= filetype ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	sf_command (file, SFC_TEST_IEEE_FLOAT_REPLACE, NULL, replace_float) ;

	test_write_float_or_die (file, 0, float_data, DFT_DATA_LENGTH, __LINE__) ;

	sf_close (file) ;

	memset (float_test, 0, sizeof (float_test)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	sf_command (file, SFC_TEST_IEEE_FLOAT_REPLACE, NULL, replace_float) ;

	exit_if_true (sfinfo.format != filetype, "\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
	exit_if_true (sfinfo.frames < DFT_DATA_LENGTH, "\n\nLine %d: Incorrect number of frames in file (too short). (%ld should be %d)\n", __LINE__, SF_COUNT_TO_LONG (sfinfo.frames), DFT_DATA_LENGTH) ;
	exit_if_true (sfinfo.channels != 1, "\n\nLine %d: Incorrect number of channels in file.\n", __LINE__) ;

	check_log_buffer_or_die (file, __LINE__) ;

	test_read_float_or_die (file, 0, float_test, DFT_DATA_LENGTH, __LINE__) ;

	sf_close (file) ;

	snr = dft_cmp_float (__LINE__, float_data, float_test, DFT_DATA_LENGTH, target_snr, allow_exit) ;

	exit_if_true (snr > target_snr, "% 6.1fdB SNR\n\n    Error : should be better than % 6.1fdB\n\n", snr, target_snr) ;

	printf ("% 6.1fdB SNR ... ok\n", snr) ;

	unlink (filename) ;

	return ;
} /* float_scaled_test */

static void
double_scaled_test (const char *filename, int allow_exit, int replace_float, int filetype, double target_snr)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	double		snr ;

	print_test_name ("double_scaled_test", filename) ;

	gen_windowed_sine_double (double_data, DFT_DATA_LENGTH, 0.95) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= DFT_DATA_LENGTH ;
	sfinfo.channels		= 1 ;
	sfinfo.format		= filetype ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	sf_command (file, SFC_TEST_IEEE_FLOAT_REPLACE, NULL, replace_float) ;

	test_write_double_or_die (file, 0, double_data, DFT_DATA_LENGTH, __LINE__) ;

	sf_close (file) ;

	memset (double_test, 0, sizeof (double_test)) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;
	sf_command (file, SFC_TEST_IEEE_FLOAT_REPLACE, NULL, replace_float) ;

	exit_if_true (sfinfo.format != filetype, "\n\nLine %d: Returned format incorrect (0x%08X => 0x%08X).\n", __LINE__, filetype, sfinfo.format) ;
	exit_if_true (sfinfo.frames < DFT_DATA_LENGTH, "\n\nLine %d: Incorrect number of frames in file (too short). (%ld should be %d)\n", __LINE__, SF_COUNT_TO_LONG (sfinfo.frames), DFT_DATA_LENGTH) ;
	exit_if_true (sfinfo.channels != 1, "\n\nLine %d: Incorrect number of channels in file.\n", __LINE__) ;

	check_log_buffer_or_die (file, __LINE__) ;

	test_read_double_or_die (file, 0, double_test, DFT_DATA_LENGTH, __LINE__) ;

	sf_close (file) ;

	snr = dft_cmp_double (__LINE__, double_data, double_test, DFT_DATA_LENGTH, target_snr, allow_exit) ;

	exit_if_true (snr > target_snr, "% 6.1fdB SNR\n\n    Error : should be better than % 6.1fdB\n\n", snr, target_snr) ;

	printf ("% 6.1fdB SNR ... ok\n", snr) ;

	unlink (filename) ;

	return ;
} /* double_scaled_test */

/*==============================================================================
*/

[+ FOR float_type +][+ FOR int_type +][+ FOR endian_type
+]
static void
[+ (get "float_name") +]_[+ (get "int_name") +]_[+ (get "end_name") +]_test (const char * filename)
{	SNDFILE		*file ;
	SF_INFO		sfinfo ;
	unsigned	k, max ;

	print_test_name ("[+ (get "float_name") +]_[+ (get "int_name") +]_[+ (get "end_name") +]_test", filename) ;

	gen_windowed_sine_[+ (get "float_name") +] ([+ (get "float_name") +]_data, ARRAY_LEN ([+ (get "float_name") +]_data), 0.98) ;

	sfinfo.samplerate	= SAMPLE_RATE ;
	sfinfo.frames		= ARRAY_LEN ([+ (get "int_name") +]_data) ;
	sfinfo.channels		= 1 ;
	sfinfo.format		= [+ (get "end_type") +] | SF_FORMAT_AU | [+ (get "minor_type") +] ;

	file = test_open_file_or_die (filename, SFM_WRITE, &sfinfo, SF_TRUE, __LINE__) ;
	test_write_[+ (get "float_name") +]_or_die (file, 0, [+ (get "float_name") +]_data, ARRAY_LEN ([+ (get "float_name") +]_data), __LINE__) ;
	sf_close (file) ;

	file = test_open_file_or_die (filename, SFM_READ, &sfinfo, SF_TRUE, __LINE__) ;

	if (sfinfo.frames != ARRAY_LEN ([+ (get "float_name") +]_data))
	{	printf ("\n\nLine %d: Incorrect number of frames in file (too short). (%ld should be %d)\n", __LINE__, SF_COUNT_TO_LONG (sfinfo.frames), DFT_DATA_LENGTH) ;
		exit (1) ;
		} ;

	if (sfinfo.channels != 1)
	{	printf ("\n\nLine %d: Incorrect number of channels in file.\n", __LINE__) ;
		exit (1) ;
		} ;

	sf_command (file, SFC_SET_SCALE_FLOAT_INT_READ, NULL, SF_TRUE) ;

	test_read_[+ (get "int_name") +]_or_die (file, 0, [+ (get "int_name") +]_data, ARRAY_LEN ([+ (get "int_name") +]_data), __LINE__) ;
	sf_close (file) ;

	max = 0 ;
	for (k = 0 ; k < ARRAY_LEN ([+ (get "int_name") +]_data) ; k++)
		if ((unsigned) abs ([+ (get "int_name") +]_data [k]) > max)
			max = abs ([+ (get "int_name") +]_data [k]) ;

	if (1.0 * abs (max - [+ (get "int_max") +]) / [+ (get "int_max") +] > 0.01)
	{	printf ("\n\nLine %d: Bad maximum (%d should be %d).\n\n", __LINE__, max, [+ (get "int_max") +]) ;
		exit (1) ;
		} ;

	unlink (filename) ;
	puts ("ok") ;
} /* [+ (get "float_name") +]_[+ (get "int_name") +]_[+ (get "end_name") +]_test */
[+ ENDFOR endian_type +][+ ENDFOR int_type +][+ ENDFOR float_type +]

