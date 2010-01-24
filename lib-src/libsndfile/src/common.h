/*
** Copyright (C) 1999-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#ifndef SNDFILE_COMMON_H
#define SNDFILE_COMMON_H

#include "sfconfig.h"

#include <stdlib.h>

#if HAVE_STDINT_H
#include <stdint.h>
#endif

#ifndef SNDFILE_H
#include "sndfile.h"
#elif HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#ifdef __cplusplus
#error "This code is not designed to be compiled with a C++ compiler."
#endif

#if (SIZEOF_LONG == 8)
#	define	SF_PLATFORM_S64(x)		x##l
#elif COMPILER_IS_GCC
#	define	SF_PLATFORM_S64(x)		x##ll
#elif OS_IS_WIN32
#	define	SF_PLATFORM_S64(x)		x##I64
#else
#	error "Don't know how to define a 64 bit integer constant."
#endif



/*
** Inspiration : http://sourcefrog.net/weblog/software/languages/C/unused.html
*/
#ifdef UNUSED
#elif defined (__GNUC__)
#	define UNUSED(x) UNUSED_ ## x __attribute__ ((unused))
#elif defined (__LCLINT__)
#	define UNUSED(x) /*@unused@*/ x
#else
#	define UNUSED(x) x
#endif

#ifdef __GNUC__
#	define WARN_UNUSED	__attribute__ ((warn_unused_result))
#else
#	define WARN_UNUSED
#endif

#define	SF_BUFFER_LEN			(8192*2)
#define	SF_FILENAME_LEN			(512)
#define SF_SYSERR_LEN			(256)
#define SF_MAX_STRINGS			(32)
#define SF_STR_BUFFER_LEN		(8192)
#define	SF_HEADER_LEN			(4100 + SF_STR_BUFFER_LEN)

#define	PSF_SEEK_ERROR			((sf_count_t) -1)


#define	BITWIDTH2BYTES(x)	(((x) + 7) / 8)

/*	For some reason sizeof returns an unsigned  value which causes
**	a warning when that value is added or subtracted from a signed
**	value. Use SIGNED_SIZEOF instead.
*/
#define		SIGNED_SIZEOF(x)	((int) sizeof (x))

#define		ARRAY_LEN(x)	((int) (sizeof (x) / sizeof ((x) [0])))

#if (COMPILER_IS_GCC == 1)
#define		SF_MAX(x,y)		({ \
								typeof (x) sf_max_x1 = (x) ; \
								typeof (y) sf_max_y1 = (y) ; \
								(void) (&sf_max_x1 == &sf_max_y1) ; \
								sf_max_x1 > sf_max_y1 ? sf_max_x1 : sf_max_y1 ; })

#define		SF_MIN(x,y)		({ \
								typeof (x) sf_min_x2 = (x) ; \
								typeof (y) sf_min_y2 = (y) ; \
								(void) (&sf_min_x2 == &sf_min_y2) ; \
								sf_min_x2 < sf_min_y2 ? sf_min_x2 : sf_min_y2 ; })
#else
#define		SF_MAX(a,b)		((a) > (b) ? (a) : (b))
#define		SF_MIN(a,b)		((a) < (b) ? (a) : (b))
#endif

/*
*	Macros for spliting the format file of SF_INFI into contrainer type,
**	codec type and endian-ness.
*/
#define SF_CONTAINER(x)		((x) & SF_FORMAT_TYPEMASK)
#define SF_CODEC(x)			((x) & SF_FORMAT_SUBMASK)
#define SF_ENDIAN(x)		((x) & SF_FORMAT_ENDMASK)


enum
{	/* PEAK chunk location. */
	SF_PEAK_START		= 42,
	SF_PEAK_END			= 43,

	/* PEAK chunk location. */
	SF_SCALE_MAX		= 52,
	SF_SCALE_MIN		= 53,

	/* str_flags values. */
	SF_STR_ALLOW_START	= 0x0100,
	SF_STR_ALLOW_END	= 0x0200,

	/* Location of strings. */
	SF_STR_LOCATE_START	= 0x0400,
	SF_STR_LOCATE_END	= 0x0800,

	SFD_TYPEMASK		= 0x0FFFFFFF
} ;

#define		SFM_MASK 	(SFM_READ | SFM_WRITE | SFM_RDWR)
#define		SFM_UNMASK 	(~SFM_MASK)

/*---------------------------------------------------------------------------------------
** Formats that may be supported at some time in the future.
** When support is finalised, these values move to src/sndfile.h.
*/

enum
{	/* Work in progress. */

	/* Formats supported read only. */
	SF_FORMAT_TXW			= 0x4030000,		/* Yamaha TX16 sampler file */
	SF_FORMAT_DWD			= 0x4040000,		/* DiamondWare Digirized */

	/* Following are detected but not supported. */
	SF_FORMAT_REX			= 0x40A0000,		/* Propellorheads Rex/Rcy */
	SF_FORMAT_REX2			= 0x40D0000,		/* Propellorheads Rex2 */
	SF_FORMAT_KRZ			= 0x40E0000,		/* Kurzweil sampler file */
	SF_FORMAT_WMA			= 0x4100000,		/* Windows Media Audio. */
	SF_FORMAT_SHN			= 0x4110000,		/* Shorten. */

	/* Unsupported encodings. */
	SF_FORMAT_SVX_FIB		= 0x1020, 		/* SVX Fibonacci Delta encoding. */
	SF_FORMAT_SVX_EXP		= 0x1021, 		/* SVX Exponential Delta encoding. */

	SF_FORMAT_PCM_N			= 0x1030
} ;

/*---------------------------------------------------------------------------------------
**	PEAK_CHUNK - This chunk type is common to both AIFF and WAVE files although their
**	endian encodings are different.
*/

typedef struct
{	double		value ;		/* signed value of peak */
	sf_count_t	position ;	/* the sample frame for the peak */
} PEAK_POS ;

typedef struct
{	/* libsndfile internal : write a PEAK chunk at the start or end of the file? */
	int				peak_loc ;

	/* WAV/AIFF */
	unsigned int	version ;	/* version of the PEAK chunk */
	unsigned int	timestamp ;	/* secs since 1/1/1970  */

	/* CAF */
	unsigned int	edit_number ;

#if HAVE_FLEXIBLE_ARRAY
	/* the per channel peak info */
	PEAK_POS		peaks [] ;
#else
	/*
	** This is not ISO compliant C. It works on some compilers which
	** don't support the ISO standard flexible struct array which is
	** used above. If your compiler doesn't like this I suggest you find
	** youself a 1999 ISO C standards compilant compiler. GCC-3.X is
	** highly recommended.
	*/
	PEAK_POS		peaks [0] ;
#endif
} PEAK_INFO ;

static inline PEAK_INFO *
peak_info_calloc (int channels)
{	return calloc (1, sizeof (PEAK_INFO) + channels * sizeof (PEAK_POS)) ;
} /* peak_info_calloc */

typedef struct
{	int		type ;
	int		flags ;
	char 	*str ;
} STR_DATA ;

static inline size_t
make_size_t (int x)
{	return (size_t) x ;
} /* size_t_of_int */

/*=======================================================================================
**	SF_PRIVATE stuct - a pointer to this struct is passed back to the caller of the
**	sf_open_XXXX functions. The caller however has no knowledge of the struct's
**	contents.
*/


typedef struct
{	int size ;
	SF_BROADCAST_INFO binfo ;
} SF_BROADCAST_VAR ;

typedef struct sf_private_tag
{
	/* Canary in a coal mine. */
	char canary [64] ;

	/* Force the compiler to double align the start of buffer. */
	union
	{	double			dbuf	[SF_BUFFER_LEN / sizeof (double)] ;
#if (defined (SIZEOF_INT64_T) && (SIZEOF_INT64_T == 8))
		int64_t			lbuf	[SF_BUFFER_LEN / sizeof (int64_t)] ;
#else
		long			lbuf	[SF_BUFFER_LEN / sizeof (double)] ;
#endif
		float			fbuf	[SF_BUFFER_LEN / sizeof (float)] ;
		int				ibuf	[SF_BUFFER_LEN / sizeof (int)] ;
		short			sbuf	[SF_BUFFER_LEN / sizeof (short)] ;
		char			cbuf	[SF_BUFFER_LEN / sizeof (char)] ;
		signed char		scbuf	[SF_BUFFER_LEN / sizeof (signed char)] ;
		unsigned char	ucbuf	[SF_BUFFER_LEN / sizeof (signed char)] ;
		} u ;

	char			filepath	[SF_FILENAME_LEN] ;
	char			rsrcpath	[SF_FILENAME_LEN] ;
	char			directory	[SF_FILENAME_LEN] ;
	char			filename	[SF_FILENAME_LEN / 4] ;

	char			syserr		[SF_SYSERR_LEN] ;

	/* logbuffer and logindex should only be changed within the logging functions
	** of common.c
	*/
	char			logbuffer	[SF_BUFFER_LEN] ;
	unsigned char	header		[SF_HEADER_LEN] ; /* Must be unsigned */
	int				rwf_endian ;	/* Header endian-ness flag. */

	/* Storage and housekeeping data for adding/reading strings from
	** sound files.
	*/
	STR_DATA		strings [SF_MAX_STRINGS] ;
	char			str_storage [SF_STR_BUFFER_LEN] ;
	char			*str_end ;
	int				str_flags ;

	/* Guard value. If this changes the buffers above have overflowed. */
	int				Magick ;

	unsigned		unique_id ;

	/* Index variables for maintaining logbuffer and header above. */
	int				logindex ;
	int				headindex, headend ;
	int				has_text ;
	int				do_not_close_descriptor ;

#if USE_WINDOWS_API
	/*
	**	These fields can only be used in src/file_io.c.
	**	They are basically the same as a windows file HANDLE.
	*/
	void 			*hfile, *hrsrc, *hsaved ;
#else
	/* These fields can only be used in src/file_io.c. */
	int 			filedes, rsrcdes, savedes ;
#endif

	int				error ;

	int				mode ;			/* Open mode : SFM_READ, SFM_WRITE or SFM_RDWR. */
	int				endian ;		/* File endianness : SF_ENDIAN_LITTLE or SF_ENDIAN_BIG. */
	int				data_endswap ;	/* Need to endswap data? */

	/*
	** Maximum float value for calculating the multiplier for
	** float/double to short/int conversions.
	*/
	int				float_int_mult ;
	float			float_max ;

	int				scale_int_float ;

	/* Vairables for handling pipes. */
	int				is_pipe ;		/* True if file is a pipe. */
	sf_count_t		pipeoffset ;	/* Number of bytes read from a pipe. */

	/* True if clipping must be performed on float->int conversions. */
	int				add_clipping ;

	SF_INFO			sf ;

	int				have_written ;	/* Has a single write been done to the file? */
	PEAK_INFO		*peak_info ;

	/* Loop Info */
	SF_LOOP_INFO	*loop_info ;
	SF_INSTRUMENT	*instrument ;

	/* Broadcast (EBU) Info */
	SF_BROADCAST_VAR *broadcast_var ;

	/* Channel map data (if present) : an array of ints. */
	int				*channel_map ;

	sf_count_t		filelength ;	/* Overall length of (embedded) file. */
	sf_count_t		fileoffset ;	/* Offset in number of bytes from beginning of file. */

	sf_count_t		rsrclength ;	/* Length of the resource fork (if it exists). */

	sf_count_t		dataoffset ;	/* Offset in number of bytes from beginning of file. */
	sf_count_t		datalength ;	/* Length in bytes of the audio data. */
	sf_count_t		dataend ;		/* Offset to file tailer. */

	int				blockwidth ;	/* Size in bytes of one set of interleaved samples. */
	int				bytewidth ;		/* Size in bytes of one sample (one channel). */

	void			*dither ;
	void			*interleave ;

	int				last_op ;		/* Last operation; either SFM_READ or SFM_WRITE */
	sf_count_t		read_current ;
	sf_count_t		write_current ;

	void			*container_data ;	/*	This is a pointer to dynamically allocated file
										**	container format specific data.
										*/

	void			*codec_data ;		/*	This is a pointer to dynamically allocated file
										**	codec format specific data.
										*/

	SF_DITHER_INFO	write_dither ;
	SF_DITHER_INFO	read_dither ;

	int				norm_double ;
	int				norm_float ;

	int				auto_header ;

	int				ieee_replace ;

	/* A set of file specific function pointers */
	sf_count_t		(*read_short)	(struct sf_private_tag*, short *ptr, sf_count_t len) ;
	sf_count_t		(*read_int)		(struct sf_private_tag*, int *ptr, sf_count_t len) ;
	sf_count_t		(*read_float)	(struct sf_private_tag*, float *ptr, sf_count_t len) ;
	sf_count_t		(*read_double)	(struct sf_private_tag*, double *ptr, sf_count_t len) ;

	sf_count_t		(*write_short)	(struct sf_private_tag*, const short *ptr, sf_count_t len) ;
	sf_count_t		(*write_int)	(struct sf_private_tag*, const int *ptr, sf_count_t len) ;
	sf_count_t		(*write_float)	(struct sf_private_tag*, const float *ptr, sf_count_t len) ;
	sf_count_t		(*write_double)	(struct sf_private_tag*, const double *ptr, sf_count_t len) ;

	sf_count_t		(*seek) 		(struct sf_private_tag*, int mode, sf_count_t samples_from_start) ;
	int				(*write_header)	(struct sf_private_tag*, int calc_length) ;
	int				(*command)		(struct sf_private_tag*, int command, void *data, int datasize) ;

	/*
	**	Separate close functions for the codec and the container.
	**	The codec close function is always called first.
	*/
	int				(*codec_close)		(struct sf_private_tag*) ;
	int				(*container_close)	(struct sf_private_tag*) ;

	char			*format_desc ;

	/* Virtual I/O functions. */
	int					virtual_io ;
	SF_VIRTUAL_IO		vio ;
	void				*vio_user_data ;
} SF_PRIVATE ;



enum
{	SFE_NO_ERROR				= SF_ERR_NO_ERROR,
	SFE_BAD_OPEN_FORMAT			= SF_ERR_UNRECOGNISED_FORMAT,
	SFE_SYSTEM					= SF_ERR_SYSTEM,
	SFE_MALFORMED_FILE			= SF_ERR_MALFORMED_FILE,
	SFE_UNSUPPORTED_ENCODING	= SF_ERR_UNSUPPORTED_ENCODING,

	SFE_ZERO_MAJOR_FORMAT,
	SFE_ZERO_MINOR_FORMAT,
	SFE_BAD_FILE,
	SFE_BAD_FILE_READ,
	SFE_OPEN_FAILED,
	SFE_BAD_SNDFILE_PTR,
	SFE_BAD_SF_INFO_PTR,
	SFE_BAD_SF_INCOMPLETE,
	SFE_BAD_FILE_PTR,
	SFE_BAD_INT_PTR,
	SFE_BAD_STAT_SIZE,
	SFE_MALLOC_FAILED,
	SFE_UNIMPLEMENTED,
	SFE_BAD_READ_ALIGN,
	SFE_BAD_WRITE_ALIGN,
	SFE_UNKNOWN_FORMAT,
	SFE_NOT_READMODE,
	SFE_NOT_WRITEMODE,
	SFE_BAD_MODE_RW,
	SFE_BAD_SF_INFO,
	SFE_BAD_OFFSET,
	SFE_NO_EMBED_SUPPORT,
	SFE_NO_EMBEDDED_RDWR,
	SFE_NO_PIPE_WRITE,

	SFE_INTERNAL,
	SFE_BAD_COMMAND_PARAM,
	SFE_BAD_ENDIAN,
	SFE_CHANNEL_COUNT_ZERO,
	SFE_CHANNEL_COUNT,

	SFE_BAD_VIRTUAL_IO,

	SFE_INTERLEAVE_MODE,
	SFE_INTERLEAVE_SEEK,
	SFE_INTERLEAVE_READ,

	SFE_BAD_SEEK,
	SFE_NOT_SEEKABLE,
	SFE_AMBIGUOUS_SEEK,
	SFE_WRONG_SEEK,
	SFE_SEEK_FAILED,

	SFE_BAD_OPEN_MODE,
	SFE_OPEN_PIPE_RDWR,
	SFE_RDWR_POSITION,
	SFE_RDWR_BAD_HEADER,
	SFE_CMD_HAS_DATA,
	SFE_BAD_BROADCAST_INFO_SIZE,

	SFE_STR_NO_SUPPORT,
	SFE_STR_NOT_WRITE,
	SFE_STR_MAX_DATA,
	SFE_STR_MAX_COUNT,
	SFE_STR_BAD_TYPE,
	SFE_STR_NO_ADD_END,
	SFE_STR_BAD_STRING,
	SFE_STR_WEIRD,

	SFE_WAV_NO_RIFF,
	SFE_WAV_NO_WAVE,
	SFE_WAV_NO_FMT,
	SFE_WAV_BAD_FMT,
	SFE_WAV_FMT_SHORT,
	SFE_WAV_BAD_FACT,
	SFE_WAV_BAD_PEAK,
	SFE_WAV_PEAK_B4_FMT,
	SFE_WAV_BAD_FORMAT,
	SFE_WAV_BAD_BLOCKALIGN,
	SFE_WAV_NO_DATA,
	SFE_WAV_BAD_LIST,
	SFE_WAV_ADPCM_NOT4BIT,
	SFE_WAV_ADPCM_CHANNELS,
	SFE_WAV_GSM610_FORMAT,
	SFE_WAV_UNKNOWN_CHUNK,
	SFE_WAV_WVPK_DATA,

	SFE_AIFF_NO_FORM,
	SFE_AIFF_AIFF_NO_FORM,
	SFE_AIFF_COMM_NO_FORM,
	SFE_AIFF_SSND_NO_COMM,
	SFE_AIFF_UNKNOWN_CHUNK,
	SFE_AIFF_COMM_CHUNK_SIZE,
	SFE_AIFF_BAD_COMM_CHUNK,
	SFE_AIFF_PEAK_B4_COMM,
	SFE_AIFF_BAD_PEAK,
	SFE_AIFF_NO_SSND,
	SFE_AIFF_NO_DATA,
	SFE_AIFF_RW_SSND_NOT_LAST,

	SFE_AU_UNKNOWN_FORMAT,
	SFE_AU_NO_DOTSND,
	SFE_AU_EMBED_BAD_LEN,

	SFE_RAW_READ_BAD_SPEC,
	SFE_RAW_BAD_BITWIDTH,
	SFE_RAW_BAD_FORMAT,

	SFE_PAF_NO_MARKER,
	SFE_PAF_VERSION,
	SFE_PAF_UNKNOWN_FORMAT,
	SFE_PAF_SHORT_HEADER,

	SFE_SVX_NO_FORM,
	SFE_SVX_NO_BODY,
	SFE_SVX_NO_DATA,
	SFE_SVX_BAD_COMP,
	SFE_SVX_BAD_NAME_LENGTH,

	SFE_NIST_BAD_HEADER,
	SFE_NIST_CRLF_CONVERISON,
	SFE_NIST_BAD_ENCODING,

	SFE_VOC_NO_CREATIVE,
	SFE_VOC_BAD_FORMAT,
	SFE_VOC_BAD_VERSION,
	SFE_VOC_BAD_MARKER,
	SFE_VOC_BAD_SECTIONS,
	SFE_VOC_MULTI_SAMPLERATE,
	SFE_VOC_MULTI_SECTION,
	SFE_VOC_MULTI_PARAM,
	SFE_VOC_SECTION_COUNT,
	SFE_VOC_NO_PIPE,

	SFE_IRCAM_NO_MARKER,
	SFE_IRCAM_BAD_CHANNELS,
	SFE_IRCAM_UNKNOWN_FORMAT,

	SFE_W64_64_BIT,
	SFE_W64_NO_RIFF,
	SFE_W64_NO_WAVE,
	SFE_W64_NO_DATA,
	SFE_W64_ADPCM_NOT4BIT,
	SFE_W64_ADPCM_CHANNELS,
	SFE_W64_GSM610_FORMAT,

	SFE_MAT4_BAD_NAME,
	SFE_MAT4_NO_SAMPLERATE,

	SFE_MAT5_BAD_ENDIAN,
	SFE_MAT5_NO_BLOCK,
	SFE_MAT5_SAMPLE_RATE,

	SFE_PVF_NO_PVF1,
	SFE_PVF_BAD_HEADER,
	SFE_PVF_BAD_BITWIDTH,

	SFE_DWVW_BAD_BITWIDTH,
	SFE_G72X_NOT_MONO,

	SFE_XI_BAD_HEADER,
	SFE_XI_EXCESS_SAMPLES,
	SFE_XI_NO_PIPE,

	SFE_HTK_NO_PIPE,

	SFE_SDS_NOT_SDS,
	SFE_SDS_BAD_BIT_WIDTH,

	SFE_SD2_FD_DISALLOWED,
	SFE_SD2_BAD_DATA_OFFSET,
	SFE_SD2_BAD_MAP_OFFSET,
	SFE_SD2_BAD_DATA_LENGTH,
	SFE_SD2_BAD_MAP_LENGTH,
	SFE_SD2_BAD_RSRC,
	SFE_SD2_BAD_SAMPLE_SIZE,

	SFE_FLAC_BAD_HEADER,
	SFE_FLAC_NEW_DECODER,
	SFE_FLAC_INIT_DECODER,
	SFE_FLAC_LOST_SYNC,
	SFE_FLAC_BAD_SAMPLE_RATE,
	SFE_FLAC_UNKOWN_ERROR,

	SFE_WVE_NOT_WVE,
	SFE_WVE_NO_PIPE,

	SFE_VORBIS_ENCODER_BUG,

	SFE_RF64_NOT_RF64,

	SFE_MAX_ERROR			/* This must be last in list. */
} ;

int subformat_to_bytewidth (int format) ;
int s_bitwidth_to_subformat (int bits) ;
int u_bitwidth_to_subformat (int bits) ;

/*  Functions for reading and writing floats and doubles on processors
**	with non-IEEE floats/doubles.
*/
float	float32_be_read		(unsigned char *cptr) ;
float	float32_le_read		(unsigned char *cptr) ;
void	float32_be_write	(float in, unsigned char *out) ;
void	float32_le_write	(float in, unsigned char *out) ;

double	double64_be_read	(unsigned char *cptr) ;
double	double64_le_read	(unsigned char *cptr) ;
void	double64_be_write	(double in, unsigned char *out) ;
void	double64_le_write	(double in, unsigned char *out) ;

/* Functions for writing to the internal logging buffer. */

void	psf_log_printf		(SF_PRIVATE *psf, const char *format, ...) ;
void	psf_log_SF_INFO 	(SF_PRIVATE *psf) ;

int32_t	psf_rand_int32 (void) ;

/* Functions used when writing file headers. */

int		psf_binheader_writef	(SF_PRIVATE *psf, const char *format, ...) ;
void	psf_asciiheader_printf	(SF_PRIVATE *psf, const char *format, ...) ;

/* Functions used when reading file headers. */

int		psf_binheader_readf	(SF_PRIVATE *psf, char const *format, ...) ;

/* Functions used in the write function for updating the peak chunk. */

void	peak_update_short	(SF_PRIVATE *psf, short *ptr, size_t items) ;
void	peak_update_int		(SF_PRIVATE *psf, int *ptr, size_t items) ;
void	peak_update_double	(SF_PRIVATE *psf, double *ptr, size_t items) ;

/* Functions defined in command.c. */

int		psf_get_format_simple_count	(void) ;
int		psf_get_format_simple		(SF_FORMAT_INFO *data) ;

int		psf_get_format_info			(SF_FORMAT_INFO *data) ;

int		psf_get_format_major_count	(void) ;
int		psf_get_format_major		(SF_FORMAT_INFO *data) ;

int		psf_get_format_subtype_count	(void) ;
int		psf_get_format_subtype		(SF_FORMAT_INFO *data) ;

void	psf_generate_format_desc (SF_PRIVATE *psf) ;

double	psf_calc_signal_max			(SF_PRIVATE *psf, int normalize) ;
int		psf_calc_max_all_channels	(SF_PRIVATE *psf, double *peaks, int normalize) ;

int		psf_get_signal_max			(SF_PRIVATE *psf, double *peak) ;
int		psf_get_max_all_channels	(SF_PRIVATE *psf, double *peaks) ;

/* Functions in strings.c. */

const char* psf_get_string (SF_PRIVATE *psf, int str_type) ;
int psf_set_string (SF_PRIVATE *psf, int str_type, const char *str) ;
int psf_store_string (SF_PRIVATE *psf, int str_type, const char *str) ;
int psf_location_string_count (const SF_PRIVATE * psf, int location) ;

/* Default seek function. Use for PCM and float encoded data. */
sf_count_t	psf_default_seek (SF_PRIVATE *psf, int mode, sf_count_t samples_from_start) ;

int macos_guess_file_type (SF_PRIVATE *psf, const char *filename) ;

/*------------------------------------------------------------------------------------
**	File I/O functions which will allow access to large files (> 2 Gig) on
**	some 32 bit OSes. Implementation in file_io.c.
*/

int psf_fopen (SF_PRIVATE *psf, const char *pathname, int flags) ;
int psf_set_stdio (SF_PRIVATE *psf, int mode) ;
int psf_file_valid (SF_PRIVATE *psf) ;
void psf_set_file (SF_PRIVATE *psf, int fd) ;
void psf_init_files (SF_PRIVATE *psf) ;
void psf_use_rsrc (SF_PRIVATE *psf, int on_off) ;

sf_count_t psf_fseek (SF_PRIVATE *psf, sf_count_t offset, int whence) ;
sf_count_t psf_fread (void *ptr, sf_count_t bytes, sf_count_t count, SF_PRIVATE *psf) ;
sf_count_t psf_fwrite (const void *ptr, sf_count_t bytes, sf_count_t count, SF_PRIVATE *psf) ;
sf_count_t psf_fgets (char *buffer, sf_count_t bufsize, SF_PRIVATE *psf) ;
sf_count_t psf_ftell (SF_PRIVATE *psf) ;
sf_count_t psf_get_filelen (SF_PRIVATE *psf) ;

void psf_fsync (SF_PRIVATE *psf) ;

int psf_is_pipe (SF_PRIVATE *psf) ;

int psf_ftruncate (SF_PRIVATE *psf, sf_count_t len) ;
int psf_fclose (SF_PRIVATE *psf) ;

/* Open and close the resource fork of a file. */
int psf_open_rsrc (SF_PRIVATE *psf, int mode) ;
int psf_close_rsrc (SF_PRIVATE *psf) ;

/*
void psf_fclearerr (SF_PRIVATE *psf) ;
int psf_ferror (SF_PRIVATE *psf) ;
*/

/*------------------------------------------------------------------------------------
** Functions for reading and writing different file formats.
*/

int		aiff_open	(SF_PRIVATE *psf) ;
int		au_open		(SF_PRIVATE *psf) ;
int		avr_open	(SF_PRIVATE *psf) ;
int		htk_open	(SF_PRIVATE *psf) ;
int		ircam_open	(SF_PRIVATE *psf) ;
int		mat4_open	(SF_PRIVATE *psf) ;
int		mat5_open	(SF_PRIVATE *psf) ;
int		nist_open	(SF_PRIVATE *psf) ;
int		paf_open	(SF_PRIVATE *psf) ;
int		pvf_open	(SF_PRIVATE *psf) ;
int		raw_open	(SF_PRIVATE *psf) ;
int		sd2_open	(SF_PRIVATE *psf) ;
int		sds_open	(SF_PRIVATE *psf) ;
int		svx_open	(SF_PRIVATE *psf) ;
int		voc_open	(SF_PRIVATE *psf) ;
int		w64_open	(SF_PRIVATE *psf) ;
int		wav_open	(SF_PRIVATE *psf) ;
int		xi_open		(SF_PRIVATE *psf) ;
int		flac_open	(SF_PRIVATE *psf) ;
int		caf_open	(SF_PRIVATE *psf) ;
int		mpc2k_open	(SF_PRIVATE *psf) ;
int		rf64_open	(SF_PRIVATE *psf) ;

/* In progress. Do not currently work. */

int		mpeg_open	(SF_PRIVATE *psf) ;
int		ogg_open	(SF_PRIVATE *psf) ;
int		rx2_open	(SF_PRIVATE *psf) ;
int		txw_open	(SF_PRIVATE *psf) ;
int		wve_open	(SF_PRIVATE *psf) ;
int		dwd_open	(SF_PRIVATE *psf) ;

int		macbinary3_open (SF_PRIVATE *psf) ;

/*------------------------------------------------------------------------------------
**	Init functions for a number of common data encodings.
*/

int		pcm_init		(SF_PRIVATE *psf) ;
int		ulaw_init		(SF_PRIVATE *psf) ;
int		alaw_init		(SF_PRIVATE *psf) ;
int		float32_init	(SF_PRIVATE *psf) ;
int		double64_init	(SF_PRIVATE *psf) ;
int		dwvw_init		(SF_PRIVATE *psf, int bitwidth) ;
int		gsm610_init		(SF_PRIVATE *psf) ;
int		vox_adpcm_init	(SF_PRIVATE *psf) ;
int		flac_init		(SF_PRIVATE *psf) ;
int		g72x_init 		(SF_PRIVATE * psf) ;

int 	dither_init		(SF_PRIVATE *psf, int mode) ;

int		wav_w64_ima_init (SF_PRIVATE *psf, int blockalign, int samplesperblock) ;
int		wav_w64_msadpcm_init (SF_PRIVATE *psf, int blockalign, int samplesperblock) ;

int		aiff_ima_init (SF_PRIVATE *psf, int blockalign, int samplesperblock) ;

int		interleave_init (SF_PRIVATE *psf) ;

/*------------------------------------------------------------------------------------
** Chunk logging functions.
*/

typedef struct
{	struct
	{	int chunk ;
		sf_count_t offset ;
		sf_count_t len ;
	} l [100] ;

	int count ;
} PRIV_CHUNK4 ;

void pchk4_store (PRIV_CHUNK4 * pchk, int marker, sf_count_t offset, sf_count_t len) ;
int pchk4_find (PRIV_CHUNK4 * pchk, int marker) ;

/*------------------------------------------------------------------------------------
** Other helper functions.
*/

void	*psf_memset (void *s, int c, sf_count_t n) ;

SF_INSTRUMENT * psf_instrument_alloc (void) ;

void	psf_sanitize_string (char * cptr, int len) ;

/* Generate the current date as a string. */
void	psf_get_date_str (char *str, int maxlen) ;

SF_BROADCAST_VAR* broadcast_var_alloc (size_t datasize) ;
int		broadcast_var_set (SF_PRIVATE *psf, const SF_BROADCAST_INFO * data, size_t datasize) ;
int		broadcast_var_get (SF_PRIVATE *psf, SF_BROADCAST_INFO * data, size_t datasize) ;


typedef struct
{	int channels ;
	int endianness ;
} AUDIO_DETECT ;

int audio_detect (SF_PRIVATE * psf, AUDIO_DETECT *ad, const unsigned char * data, int datalen) ;


/*------------------------------------------------------------------------------------
** Helper/debug functions.
*/

void	psf_hexdump (const void *ptr, int len) ;

const char * str_of_major_format (int format) ;
const char * str_of_minor_format (int format) ;
const char * str_of_open_mode (int mode) ;
const char * str_of_endianness (int end) ;

/*------------------------------------------------------------------------------------
** Here's how we fix systems which don't snprintf / vsnprintf.
** Systems without these functions should use the
*/

#if USE_WINDOWS_API
#define	LSF_SNPRINTF	_snprintf
#elif		(HAVE_SNPRINTF && ! FORCE_MISSING_SNPRINTF)
#define	LSF_SNPRINTF	snprintf
#else
int missing_snprintf (char *str, size_t n, char const *fmt, ...) ;
#define	LSF_SNPRINTF	missing_snprintf
#endif

#if USE_WINDOWS_API
#define	LSF_VSNPRINTF	_vsnprintf
#elif		(HAVE_VSNPRINTF && ! FORCE_MISSING_SNPRINTF)
#define	LSF_VSNPRINTF	vsnprintf
#else
int missing_vsnprintf (char *str, size_t n, const char *fmt, ...) ;
#define	LSF_VSNPRINTF	missing_vsnprintf
#endif

/*------------------------------------------------------------------------------------
** Extra commands for sf_command(). Not for public use yet.
*/

enum
{	SFC_TEST_AIFF_ADD_INST_CHUNK	= 0x2000,
	SFC_TEST_WAV_ADD_INFO_CHUNK		= 0x2010
} ;

/*
** Maybe, one day, make these functions or something like them, public.
**
** Buffer to buffer dithering. Pointer in and out are allowed to point
** to the same buffer for in-place dithering.
*/

#if 0
int sf_dither_short		(const SF_DITHER_INFO *dither, const short *in, short *out, int count) ;
int sf_dither_int		(const SF_DITHER_INFO *dither, const int *in, int *out, int count) ;
int sf_dither_float		(const SF_DITHER_INFO *dither, const float *in, float *out, int count) ;
int sf_dither_double	(const SF_DITHER_INFO *dither, const double *in, double *out, int count) ;
#endif

#endif /* SNDFILE_COMMON_H */

