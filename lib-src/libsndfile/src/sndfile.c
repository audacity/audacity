/*
** Copyright (C) 1999-2018 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	"sfconfig.h"

#include	<stdlib.h>
#include	<string.h>
#include	<ctype.h>
#include	<assert.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

#define		SNDFILE_MAGICK	0x1234C0DE

#ifdef __APPLE__
	/*
	**	Detect if a compile for a universal binary is being attempted and barf if it is.
	**	See the URL below for the rationale.
	*/
	#ifdef __BIG_ENDIAN__
		#if (CPU_IS_LITTLE_ENDIAN == 1)
			#error "Universal binary compile detected. See http://www.mega-nerd.com/libsndfile/FAQ.html#Q018"
		#endif
	#endif

	#ifdef __LITTLE_ENDIAN__
		#if (CPU_IS_BIG_ENDIAN == 1)
			#error "Universal binary compile detected. See http://www.mega-nerd.com/libsndfile/FAQ.html#Q018"
		#endif
	#endif
#endif


typedef struct
{	int 		error ;
	const char	*str ;
} ErrorStruct ;

static
ErrorStruct SndfileErrors [] =
{
	/* Public error values and their associated strings. */
	{	SF_ERR_NO_ERROR				, "No Error." },
	{	SF_ERR_UNRECOGNISED_FORMAT	, "Format not recognised." },
	{	SF_ERR_SYSTEM				, "System error." /* Often replaced. */ 	},
	{	SF_ERR_MALFORMED_FILE		, "Supported file format but file is malformed." },
	{	SF_ERR_UNSUPPORTED_ENCODING	, "Supported file format but unsupported encoding." },

	/* Private error values and their associated strings. */
	{	SFE_ZERO_MAJOR_FORMAT	, "Error : major format is 0." },
	{	SFE_ZERO_MINOR_FORMAT	, "Error : minor format is 0." },
	{	SFE_BAD_FILE			, "File does not exist or is not a regular file (possibly a pipe?)." },
	{	SFE_BAD_FILE_READ		, "File exists but no data could be read." },
	{	SFE_OPEN_FAILED			, "Could not open file." },
	{	SFE_BAD_SNDFILE_PTR		, "Not a valid SNDFILE* pointer." },
	{	SFE_BAD_SF_INFO_PTR		, "NULL SF_INFO pointer passed to libsndfile." },
	{	SFE_BAD_SF_INCOMPLETE	, "SF_PRIVATE struct incomplete and end of header parsing." },
	{	SFE_BAD_FILE_PTR		, "Bad FILE pointer." },
	{	SFE_BAD_INT_PTR			, "Internal error, Bad pointer." },
	{	SFE_BAD_STAT_SIZE		, "Error : software was misconfigured at compile time (sizeof statbuf.st_size)." },
	{	SFE_NO_TEMP_DIR			, "Error : Could not file temp dir." },

	{	SFE_MALLOC_FAILED		, "Internal malloc () failed." },
	{	SFE_UNIMPLEMENTED		, "File contains data in an unimplemented format." },
	{	SFE_BAD_READ_ALIGN		, "Attempt to read a non-integer number of channels." },
	{	SFE_BAD_WRITE_ALIGN 	, "Attempt to write a non-integer number of channels." },
	{	SFE_NOT_READMODE		, "Read attempted on file currently open for write." },
	{	SFE_NOT_WRITEMODE		, "Write attempted on file currently open for read." },
	{	SFE_BAD_MODE_RW			, "Error : This file format does not support read/write mode." },
	{	SFE_BAD_SF_INFO			, "Internal error : SF_INFO struct incomplete." },
	{	SFE_BAD_OFFSET			, "Error : supplied offset beyond end of file." },
	{	SFE_NO_EMBED_SUPPORT	, "Error : embedding not supported for this file format." },
	{	SFE_NO_EMBEDDED_RDWR	, "Error : cannot open embedded file read/write." },
	{	SFE_NO_PIPE_WRITE		, "Error : this file format does not support pipe write." },
	{	SFE_BAD_VIRTUAL_IO		, "Error : bad pointer on SF_VIRTUAL_IO struct." },
	{	SFE_BAD_BROADCAST_INFO_SIZE
								, "Error : bad coding_history_size in SF_BROADCAST_INFO struct." },
	{	SFE_BAD_BROADCAST_INFO_TOO_BIG
								, "Error : SF_BROADCAST_INFO struct too large." },
	{	SFE_BAD_CART_INFO_SIZE				, "Error: SF_CART_INFO struct too large." },
	{	SFE_BAD_CART_INFO_TOO_BIG			, "Error: bad tag_text_size in SF_CART_INFO struct." },
	{	SFE_INTERLEAVE_MODE		, "Attempt to write to file with non-interleaved data." },
	{	SFE_INTERLEAVE_SEEK		, "Bad karma in seek during interleave read operation." },
	{	SFE_INTERLEAVE_READ		, "Bad karma in read during interleave read operation." },

	{	SFE_INTERNAL			, "Unspecified internal error." },
	{	SFE_BAD_COMMAND_PARAM	, "Bad parameter passed to function sf_command." },
	{	SFE_BAD_ENDIAN			, "Bad endian-ness. Try default endian-ness" },
	{	SFE_CHANNEL_COUNT_ZERO	, "Channel count is zero." },
	{	SFE_CHANNEL_COUNT		, "Too many channels specified." },
	{	SFE_CHANNEL_COUNT_BAD	, "Bad channel count." },

	{	SFE_BAD_SEEK			, "Internal psf_fseek() failed." },
	{	SFE_NOT_SEEKABLE		, "Seek attempted on unseekable file type." },
	{	SFE_AMBIGUOUS_SEEK		, "Error : combination of file open mode and seek command is ambiguous." },
	{	SFE_WRONG_SEEK			, "Error : invalid seek parameters." },
	{	SFE_SEEK_FAILED			, "Error : parameters OK, but psf_seek() failed." },

	{	SFE_BAD_OPEN_MODE		, "Error : bad mode parameter for file open." },
	{	SFE_OPEN_PIPE_RDWR		, "Error : attempt to open a pipe in read/write mode." },
	{	SFE_RDWR_POSITION		, "Error on RDWR position (cryptic)." },
	{	SFE_RDWR_BAD_HEADER		, "Error : Cannot open file in read/write mode due to string data in header." },
	{	SFE_CMD_HAS_DATA		, "Error : Command fails because file already has audio data." },

	{	SFE_STR_NO_SUPPORT		, "Error : File type does not support string data." },
	{	SFE_STR_NOT_WRITE		, "Error : Trying to set a string when file is not in write mode." },
	{	SFE_STR_MAX_DATA		, "Error : Maximum string data storage reached." },
	{	SFE_STR_MAX_COUNT		, "Error : Maximum string data count reached." },
	{	SFE_STR_BAD_TYPE		, "Error : Bad string data type." },
	{	SFE_STR_NO_ADD_END		, "Error : file type does not support strings added at end of file." },
	{	SFE_STR_BAD_STRING		, "Error : bad string." },
	{	SFE_STR_WEIRD			, "Error : Weird string error." },

	{	SFE_WAV_NO_RIFF			, "Error in WAV file. No 'RIFF' chunk marker." },
	{	SFE_WAV_NO_WAVE			, "Error in WAV file. No 'WAVE' chunk marker." },
	{	SFE_WAV_NO_FMT			, "Error in WAV/W64/RF64 file. No 'fmt ' chunk marker." },
	{	SFE_WAV_BAD_FMT			, "Error in WAV/W64/RF64 file. Malformed 'fmt ' chunk." },
	{	SFE_WAV_FMT_SHORT		, "Error in WAV/W64/RF64 file. Short 'fmt ' chunk." },

	{	SFE_WAV_BAD_FACT		, "Error in WAV file. 'fact' chunk out of place." },
	{	SFE_WAV_BAD_PEAK		, "Error in WAV file. Bad 'PEAK' chunk." },
	{	SFE_WAV_PEAK_B4_FMT		, "Error in WAV file. 'PEAK' chunk found before 'fmt ' chunk." },

	{	SFE_WAV_BAD_FORMAT		, "Error in WAV file. Errors in 'fmt ' chunk." },
	{	SFE_WAV_BAD_BLOCKALIGN	, "Error in WAV file. Block alignment in 'fmt ' chunk is incorrect." },
	{	SFE_WAV_NO_DATA			, "Error in WAV file. No 'data' chunk marker." },
	{	SFE_WAV_BAD_LIST		, "Error in WAV file. Malformed LIST chunk." },
	{	SFE_WAV_UNKNOWN_CHUNK	, "Error in WAV file. File contains an unknown chunk marker." },
	{	SFE_WAV_WVPK_DATA		, "Error in WAV file. Data is in WAVPACK format." },

	{	SFE_WAV_ADPCM_NOT4BIT	, "Error in ADPCM WAV file. Invalid bit width." },
	{	SFE_WAV_ADPCM_CHANNELS	, "Error in ADPCM WAV file. Invalid number of channels." },
	{	SFE_WAV_ADPCM_SAMPLES	, "Error in ADPCM WAV file. Invalid number of samples per block." },
	{	SFE_WAV_GSM610_FORMAT	, "Error in GSM610 WAV file. Invalid format chunk." },
	{	SFE_WAV_NMS_FORMAT		, "Error in NMS ADPCM WAV file. Invalid format chunk." },

	{	SFE_AIFF_NO_FORM		, "Error in AIFF file, bad 'FORM' marker." },
	{	SFE_AIFF_AIFF_NO_FORM	, "Error in AIFF file, 'AIFF' marker without 'FORM'." },
	{	SFE_AIFF_COMM_NO_FORM	, "Error in AIFF file, 'COMM' marker without 'FORM'." },
	{	SFE_AIFF_SSND_NO_COMM	, "Error in AIFF file, 'SSND' marker without 'COMM'." },
	{	SFE_AIFF_UNKNOWN_CHUNK	, "Error in AIFF file, unknown chunk." },
	{	SFE_AIFF_COMM_CHUNK_SIZE, "Error in AIFF file, bad 'COMM' chunk size." },
	{	SFE_AIFF_BAD_COMM_CHUNK , "Error in AIFF file, bad 'COMM' chunk." },
	{	SFE_AIFF_PEAK_B4_COMM	, "Error in AIFF file. 'PEAK' chunk found before 'COMM' chunk." },
	{	SFE_AIFF_BAD_PEAK		, "Error in AIFF file. Bad 'PEAK' chunk." },
	{	SFE_AIFF_NO_SSND		, "Error in AIFF file, bad 'SSND' chunk." },
	{	SFE_AIFF_NO_DATA		, "Error in AIFF file, no sound data." },
	{	SFE_AIFF_RW_SSND_NOT_LAST, "Error in AIFF file, RDWR only possible if SSND chunk at end of file." },

	{	SFE_AU_UNKNOWN_FORMAT	, "Error in AU file, unknown format." },
	{	SFE_AU_NO_DOTSND		, "Error in AU file, missing '.snd' or 'dns.' marker." },
	{	SFE_AU_EMBED_BAD_LEN	, "Embedded AU file with unknown length." },

	{	SFE_RAW_READ_BAD_SPEC	, "Error while opening RAW file for read. Must specify format and channels.\n"
									"Possibly trying to open unsupported format." },
	{	SFE_RAW_BAD_BITWIDTH	, "Error. RAW file bitwidth must be a multiple of 8." },
	{	SFE_RAW_BAD_FORMAT		, "Error. Bad format field in SF_INFO struct when opening a RAW file for read." },

	{	SFE_PAF_NO_MARKER		, "Error in PAF file, no marker." },
	{	SFE_PAF_VERSION			, "Error in PAF file, bad version." },
	{	SFE_PAF_UNKNOWN_FORMAT	, "Error in PAF file, unknown format." },
	{	SFE_PAF_SHORT_HEADER	, "Error in PAF file. File shorter than minimal header." },
	{	SFE_PAF_BAD_CHANNELS	, "Error in PAF file. Bad channel count." },

	{	SFE_SVX_NO_FORM			, "Error in 8SVX / 16SV file, no 'FORM' marker." },
	{	SFE_SVX_NO_BODY			, "Error in 8SVX / 16SV file, no 'BODY' marker." },
	{	SFE_SVX_NO_DATA			, "Error in 8SVX / 16SV file, no sound data." },
	{	SFE_SVX_BAD_COMP		, "Error in 8SVX / 16SV file, unsupported compression format." },
	{	SFE_SVX_BAD_NAME_LENGTH	, "Error in 8SVX / 16SV file, NAME chunk too long." },

	{	SFE_NIST_BAD_HEADER		, "Error in NIST file, bad header." },
	{	SFE_NIST_CRLF_CONVERISON, "Error : NIST file damaged by Windows CR -> CRLF conversion process."	},
	{	SFE_NIST_BAD_ENCODING	, "Error in NIST file, unsupported compression format." },

	{	SFE_VOC_NO_CREATIVE		, "Error in VOC file, no 'Creative Voice File' marker." },
	{	SFE_VOC_BAD_FORMAT		, "Error in VOC file, bad format." },
	{	SFE_VOC_BAD_VERSION		, "Error in VOC file, bad version number." },
	{	SFE_VOC_BAD_MARKER		, "Error in VOC file, bad marker in file." },
	{	SFE_VOC_BAD_SECTIONS	, "Error in VOC file, incompatible VOC sections." },
	{	SFE_VOC_MULTI_SAMPLERATE, "Error in VOC file, more than one sample rate defined." },
	{	SFE_VOC_MULTI_SECTION	, "Unimplemented VOC file feature, file contains multiple sound sections." },
	{	SFE_VOC_MULTI_PARAM		, "Error in VOC file, file contains multiple bit or channel widths." },
	{	SFE_VOC_SECTION_COUNT	, "Error in VOC file, too many sections." },
	{	SFE_VOC_NO_PIPE			, "Error : not able to operate on VOC files over a pipe." },

	{	SFE_IRCAM_NO_MARKER		, "Error in IRCAM file, bad IRCAM marker." },
	{	SFE_IRCAM_BAD_CHANNELS	, "Error in IRCAM file, bad channel count." },
	{	SFE_IRCAM_UNKNOWN_FORMAT, "Error in IRCAM file, unknown encoding format." },

	{	SFE_W64_64_BIT			, "Error in W64 file, file contains 64 bit offset." },
	{	SFE_W64_NO_RIFF			, "Error in W64 file. No 'riff' chunk marker." },
	{	SFE_W64_NO_WAVE			, "Error in W64 file. No 'wave' chunk marker." },
	{	SFE_W64_NO_DATA			, "Error in W64 file. No 'data' chunk marker." },
	{	SFE_W64_ADPCM_NOT4BIT	, "Error in ADPCM W64 file. Invalid bit width." },
	{	SFE_W64_ADPCM_CHANNELS	, "Error in ADPCM W64 file. Invalid number of channels." },
	{	SFE_W64_GSM610_FORMAT	, "Error in GSM610 W64 file. Invalid format chunk." },

	{	SFE_MAT4_BAD_NAME		, "Error in MAT4 file. No variable name." },
	{	SFE_MAT4_NO_SAMPLERATE	, "Error in MAT4 file. No sample rate." },

	{	SFE_MAT5_BAD_ENDIAN		, "Error in MAT5 file. Not able to determine endian-ness." },
	{	SFE_MAT5_NO_BLOCK		, "Error in MAT5 file. Bad block structure." },
	{	SFE_MAT5_SAMPLE_RATE	, "Error in MAT5 file. Not able to determine sample rate." },

	{	SFE_PVF_NO_PVF1			, "Error in PVF file. No PVF1 marker." },
	{	SFE_PVF_BAD_HEADER		, "Error in PVF file. Bad header." },
	{	SFE_PVF_BAD_BITWIDTH	, "Error in PVF file. Bad bit width." },

	{	SFE_XI_BAD_HEADER		, "Error in XI file. Bad header." },
	{	SFE_XI_EXCESS_SAMPLES	, "Error in XI file. Excess samples in file." },
	{	SFE_XI_NO_PIPE			, "Error : not able to operate on XI files over a pipe." },

	{	SFE_HTK_NO_PIPE			, "Error : not able to operate on HTK files over a pipe." },

	{	SFE_SDS_NOT_SDS			, "Error : not an SDS file." },
	{	SFE_SDS_BAD_BIT_WIDTH	, "Error : bad bit width for SDS file." },

	{	SFE_SD2_FD_DISALLOWED	, "Error : cannot open SD2 file without a file name." },
	{	SFE_SD2_BAD_DATA_OFFSET	, "Error : bad data offset." },
	{	SFE_SD2_BAD_MAP_OFFSET	, "Error : bad map offset." },
	{	SFE_SD2_BAD_DATA_LENGTH	, "Error : bad data length." },
	{	SFE_SD2_BAD_MAP_LENGTH	, "Error : bad map length." },
	{	SFE_SD2_BAD_RSRC		, "Error : bad resource fork." },
	{	SFE_SD2_BAD_SAMPLE_SIZE	, "Error : bad sample size." },

	{	SFE_FLAC_BAD_HEADER		, "Error : bad flac header." },
	{	SFE_FLAC_NEW_DECODER	, "Error : problem while creating flac decoder." },
	{	SFE_FLAC_INIT_DECODER	, "Error : problem with initialization of the flac decoder." },
	{	SFE_FLAC_LOST_SYNC		, "Error : flac decoder lost sync." },
	{	SFE_FLAC_BAD_SAMPLE_RATE, "Error : flac does not support this sample rate." },
	{	SFE_FLAC_CHANNEL_COUNT_CHANGED, "Error : flac channel changed mid stream." },
	{	SFE_FLAC_UNKOWN_ERROR	, "Error : unknown error in flac decoder." },

	{	SFE_WVE_NOT_WVE			, "Error : not a WVE file." },
	{	SFE_WVE_NO_PIPE			, "Error : not able to operate on WVE files over a pipe." },

	{	SFE_DWVW_BAD_BITWIDTH	, "Error : Bad bit width for DWVW encoding. Must be 12, 16 or 24." },
	{	SFE_G72X_NOT_MONO		, "Error : G72x encoding does not support more than 1 channel." },
	{	SFE_NMS_ADPCM_NOT_MONO	, "Error : NMS ADPCM encoding does not support more than 1 channel." },

	{	SFE_VORBIS_ENCODER_BUG	, "Error : Sample rate chosen is known to trigger a Vorbis encoder bug on this CPU." },

	{	SFE_RF64_NOT_RF64		, "Error : Not an RF64 file." },
	{	SFE_RF64_PEAK_B4_FMT	, "Error in RF64 file. 'PEAK' chunk found before 'fmt ' chunk." },
	{	SFE_RF64_NO_DATA		, "Error in RF64 file. No 'data' chunk marker." },

	{	SFE_ALAC_FAIL_TMPFILE	, "Error : Failed to open tmp file for ALAC encoding." },

	{	SFE_BAD_CHUNK_PTR		, "Error : Bad SF_CHUNK_INFO pointer." },
	{	SFE_UNKNOWN_CHUNK		, "Error : Unknown chunk marker." },
	{	SFE_BAD_CHUNK_FORMAT	, "Error : Reading/writing chunks from this file format is not supported." },
	{	SFE_BAD_CHUNK_MARKER	, "Error : Bad chunk marker." },
	{	SFE_BAD_CHUNK_DATA_PTR	, "Error : Bad data pointer in SF_CHUNK_INFO struct." },
	{	SFE_FILENAME_TOO_LONG	, "Error : Supplied filename too long." },
	{	SFE_NEGATIVE_RW_LEN		, "Error : Length parameter passed to read/write is negative." },

	{	SFE_OPUS_BAD_SAMPLERATE	, "Error : Opus only supports sample rates of 8000, 12000, 16000, 24000 and 48000." },

	{	SFE_MAX_ERROR			, "Maximum error number." },
	{	SFE_MAX_ERROR + 1		, NULL }
} ;

/*------------------------------------------------------------------------------
*/

static int 	format_from_extension (SF_PRIVATE *psf) ;
static int	guess_file_type (SF_PRIVATE *psf) ;
static int	validate_sfinfo (SF_INFO *sfinfo) ;
static int	validate_psf (SF_PRIVATE *psf) ;
static void	save_header_info (SF_PRIVATE *psf) ;
static int	copy_filename (SF_PRIVATE *psf, const char *path) ;
static int	psf_close (SF_PRIVATE *psf) ;

static int	try_resource_fork (SF_PRIVATE * psf) ;

/*------------------------------------------------------------------------------
** Private (static) variables.
*/

int	sf_errno = 0 ;
static char	sf_parselog [SF_BUFFER_LEN] = { 0 } ;
static char	sf_syserr [SF_SYSERR_LEN] = { 0 } ;

/*------------------------------------------------------------------------------
*/

#define	VALIDATE_SNDFILE_AND_ASSIGN_PSF(a, b, c)	\
		{	if ((a) == NULL)						\
			{	sf_errno = SFE_BAD_SNDFILE_PTR ;	\
				return 0 ;							\
				} ;									\
			(b) = (SF_PRIVATE*) (a) ;				\
			if ((b)->virtual_io == SF_FALSE &&		\
				psf_file_valid (b) == 0)			\
			{	(b)->error = SFE_BAD_FILE_PTR ;		\
				return 0 ;							\
				} ;									\
			if ((b)->Magick != SNDFILE_MAGICK)		\
			{	(b)->error = SFE_BAD_SNDFILE_PTR ;	\
				return 0 ;							\
				} ;									\
			if (c) (b)->error = 0 ;					\
			}

/*------------------------------------------------------------------------------
**	Public functions.
*/

SNDFILE*
sf_open	(const char *path, int mode, SF_INFO *sfinfo)
{	SF_PRIVATE 	*psf ;

	/* Ultimate sanity check. */
	assert (sizeof (sf_count_t) == 8) ;

	if ((psf = psf_allocate ()) == NULL)
	{	sf_errno = SFE_MALLOC_FAILED ;
		return	NULL ;
		} ;

	psf_init_files (psf) ;

	psf_log_printf (psf, "File : %s\n", path) ;

	if (copy_filename (psf, path) != 0)
	{	sf_errno = psf->error ;
		return	NULL ;
		} ;

	psf->file.mode = mode ;
	if (strcmp (path, "-") == 0)
		psf->error = psf_set_stdio (psf) ;
	else
		psf->error = psf_fopen (psf) ;

	return psf_open_file (psf, sfinfo) ;
} /* sf_open */

SNDFILE*
sf_open_fd	(int fd, int mode, SF_INFO *sfinfo, int close_desc)
{	SF_PRIVATE 	*psf ;

	if ((SF_CONTAINER (sfinfo->format)) == SF_FORMAT_SD2)
	{	sf_errno = SFE_SD2_FD_DISALLOWED ;
		return	NULL ;
		} ;

	if ((psf = psf_allocate ()) == NULL)
	{	sf_errno = SFE_MALLOC_FAILED ;
		return	NULL ;
		} ;

	psf_init_files (psf) ;
	copy_filename (psf, "") ;

	psf->file.mode = mode ;
	psf_set_file (psf, fd) ;
	psf->is_pipe = psf_is_pipe (psf) ;
	psf->fileoffset = psf_ftell (psf) ;

	if (! close_desc)
		psf->file.do_not_close_descriptor = SF_TRUE ;

	return psf_open_file (psf, sfinfo) ;
} /* sf_open_fd */

SNDFILE*
sf_open_virtual	(SF_VIRTUAL_IO *sfvirtual, int mode, SF_INFO *sfinfo, void *user_data)
{	SF_PRIVATE 	*psf ;

	/* Make sure we have a valid set ot virtual pointers. */
	if (sfvirtual->get_filelen == NULL)
	{	sf_errno = SFE_BAD_VIRTUAL_IO ;
		snprintf (sf_parselog, sizeof (sf_parselog), "Bad vio_get_filelen in SF_VIRTUAL_IO struct.\n") ;
		return NULL ;
		} ;

	if ((sfvirtual->seek == NULL || sfvirtual->tell == NULL) && sfinfo->seekable)
	{	sf_errno = SFE_BAD_VIRTUAL_IO ;
		snprintf (sf_parselog, sizeof (sf_parselog), "Bad vio_seek / vio_tell in SF_VIRTUAL_IO struct.\n") ;
		return NULL ;
		} ;

	if ((mode == SFM_READ || mode == SFM_RDWR) && sfvirtual->read == NULL)
	{	sf_errno = SFE_BAD_VIRTUAL_IO ;
		snprintf (sf_parselog, sizeof (sf_parselog), "Bad vio_read in SF_VIRTUAL_IO struct.\n") ;
		return NULL ;
		} ;

	if ((mode == SFM_WRITE || mode == SFM_RDWR) && sfvirtual->write == NULL)
	{	sf_errno = SFE_BAD_VIRTUAL_IO ;
		snprintf (sf_parselog, sizeof (sf_parselog), "Bad vio_write in SF_VIRTUAL_IO struct.\n") ;
		return NULL ;
		} ;

	if ((psf = psf_allocate ()) == NULL)
	{	sf_errno = SFE_MALLOC_FAILED ;
		return	NULL ;
		} ;

	psf_init_files (psf) ;

	psf->virtual_io = SF_TRUE ;
	psf->vio = *sfvirtual ;
	psf->vio_user_data = user_data ;

	psf->file.mode = mode ;

	return psf_open_file (psf, sfinfo) ;
} /* sf_open_virtual */

int
sf_close	(SNDFILE *sndfile)
{	SF_PRIVATE	*psf ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	return psf_close (psf) ;
} /* sf_close */

void
sf_write_sync	(SNDFILE *sndfile)
{	SF_PRIVATE	*psf ;

	if ((psf = (SF_PRIVATE *) sndfile) == NULL)
		return ;

	psf_fsync (psf) ;

	return ;
} /* sf_write_sync */

/*==============================================================================
*/

const char*
sf_error_number	(int errnum)
{	static const char *bad_errnum =
		"No error defined for this error number. This is a bug in libsndfile." ;
	int	k ;

	if (errnum == SFE_MAX_ERROR)
		return SndfileErrors [0].str ;

	if (errnum < 0 || errnum > SFE_MAX_ERROR)
	{	/* This really shouldn't happen in release versions. */
		printf ("Not a valid error number (%d).\n", errnum) ;
		return bad_errnum ;
		} ;

	for (k = 0 ; SndfileErrors [k].str ; k++)
		if (errnum == SndfileErrors [k].error)
			return SndfileErrors [k].str ;

	return bad_errnum ;
} /* sf_error_number */

const char*
sf_strerror (SNDFILE *sndfile)
{	SF_PRIVATE 	*psf = NULL ;
	int errnum ;

	if (sndfile == NULL)
	{	errnum = sf_errno ;
		if (errnum == SFE_SYSTEM && sf_syserr [0])
			return sf_syserr ;
		}
	else
	{	psf = (SF_PRIVATE *) sndfile ;

		if (psf->Magick != SNDFILE_MAGICK)
			return	"sf_strerror : Bad magic number." ;

		errnum = psf->error ;

		if (errnum == SFE_SYSTEM && psf->syserr [0])
			return psf->syserr ;
		} ;

	return sf_error_number (errnum) ;
} /* sf_strerror */

/*------------------------------------------------------------------------------
*/

int
sf_error (SNDFILE *sndfile)
{	SF_PRIVATE	*psf ;

	if (sndfile == NULL)
		return sf_errno ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 0) ;

	if (psf->error)
		return psf->error ;

	return 0 ;
} /* sf_error */

/*------------------------------------------------------------------------------
*/

int
sf_perror (SNDFILE *sndfile)
{	SF_PRIVATE 	*psf ;
	int 		errnum ;

	if (sndfile == NULL)
	{	errnum = sf_errno ;
		}
	else
	{	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 0) ;
		errnum = psf->error ;
		} ;

	fprintf (stderr, "%s\n", sf_error_number (errnum)) ;
	return SFE_NO_ERROR ;
} /* sf_perror */


/*------------------------------------------------------------------------------
*/

int
sf_error_str (SNDFILE *sndfile, char *str, size_t maxlen)
{	SF_PRIVATE 	*psf ;
	int 		errnum ;

	if (str == NULL)
		return SFE_INTERNAL ;

	if (sndfile == NULL)
		errnum = sf_errno ;
	else
	{	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 0) ;
		errnum = psf->error ;
		} ;

	snprintf (str, maxlen, "%s", sf_error_number (errnum)) ;

	return SFE_NO_ERROR ;
} /* sf_error_str */

/*==============================================================================
*/

int
sf_format_check	(const SF_INFO *info)
{	int	subformat, endian ;

	subformat = SF_CODEC (info->format) ;
	endian = SF_ENDIAN (info->format) ;

	/* This is the place where each file format can check if the suppiled
	** SF_INFO struct is valid.
	** Return 0 on failure, 1 ons success.
	*/

	if (info->channels < 1 || info->channels > SF_MAX_CHANNELS)
		return 0 ;

	if (info->samplerate < 0)
		return 0 ;

	switch (SF_CONTAINER (info->format))
	{	case SF_FORMAT_WAV :
				/* WAV now allows both endian, RIFF or RIFX (little or big respectively) */
				if (subformat == SF_FORMAT_PCM_U8 || subformat == SF_FORMAT_PCM_16)
					return 1 ;
				if (subformat == SF_FORMAT_PCM_24 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				if ((subformat == SF_FORMAT_IMA_ADPCM || subformat == SF_FORMAT_MS_ADPCM) && info->channels <= 2)
					return 1 ;
				if (subformat == SF_FORMAT_GSM610 && info->channels == 1)
					return 1 ;
				if (subformat == SF_FORMAT_G721_32 && info->channels == 1)
					return 1 ;
				if (subformat == SF_FORMAT_ULAW || subformat == SF_FORMAT_ALAW)
					return 1 ;
				if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE)
					return 1 ;
				if ((subformat == SF_FORMAT_NMS_ADPCM_16 || subformat == SF_FORMAT_NMS_ADPCM_24 ||
							subformat == SF_FORMAT_NMS_ADPCM_32) && info->channels == 1)
					return 1 ;
				break ;

		case SF_FORMAT_WAVEX :
				if (endian == SF_ENDIAN_BIG || endian == SF_ENDIAN_CPU)
					return 0 ;
				if (subformat == SF_FORMAT_PCM_U8 || subformat == SF_FORMAT_PCM_16)
					return 1 ;
				if (subformat == SF_FORMAT_PCM_24 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				if (subformat == SF_FORMAT_ULAW || subformat == SF_FORMAT_ALAW)
					return 1 ;
				if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE)
					return 1 ;
				break ;

		case SF_FORMAT_AIFF :
				/* AIFF does allow both endian-nesses for PCM data.*/
				if (subformat == SF_FORMAT_PCM_16 || subformat == SF_FORMAT_PCM_24 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				/* For other encodings reject any endian-ness setting. */
				if (endian != 0)
					return 0 ;
				if (subformat == SF_FORMAT_PCM_U8 || subformat == SF_FORMAT_PCM_S8)
					return 1 ;
				if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE)
					return 1 ;
				if (subformat == SF_FORMAT_ULAW || subformat == SF_FORMAT_ALAW)
					return 1 ;
				if ((subformat == SF_FORMAT_DWVW_12 || subformat == SF_FORMAT_DWVW_16 ||
							subformat == SF_FORMAT_DWVW_24) && info-> channels == 1)
					return 1 ;
				if (subformat == SF_FORMAT_GSM610 && info->channels == 1)
					return 1 ;
				if (subformat == SF_FORMAT_IMA_ADPCM && (info->channels == 1 || info->channels == 2))
					return 1 ;
				break ;

		case SF_FORMAT_AU :
				if (subformat == SF_FORMAT_PCM_S8 || subformat == SF_FORMAT_PCM_16)
					return 1 ;
				if (subformat == SF_FORMAT_PCM_24 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				if (subformat == SF_FORMAT_ULAW || subformat == SF_FORMAT_ALAW)
					return 1 ;
				if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE)
					return 1 ;
				if (subformat == SF_FORMAT_G721_32 && info->channels == 1)
					return 1 ;
				if (subformat == SF_FORMAT_G723_24 && info->channels == 1)
					return 1 ;
				if (subformat == SF_FORMAT_G723_40 && info->channels == 1)
					return 1 ;
				break ;

		case SF_FORMAT_CAF :
				if (subformat == SF_FORMAT_PCM_S8 || subformat == SF_FORMAT_PCM_16)
					return 1 ;
				if (subformat == SF_FORMAT_PCM_24 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				if (subformat == SF_FORMAT_ULAW || subformat == SF_FORMAT_ALAW)
					return 1 ;
				if (subformat == SF_FORMAT_ALAC_16 || subformat == SF_FORMAT_ALAC_20)
					return 1 ;
				if (subformat == SF_FORMAT_ALAC_24 || subformat == SF_FORMAT_ALAC_32)
					return 1 ;
				if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE)
					return 1 ;
				break ;

		case SF_FORMAT_RAW :
				if (subformat == SF_FORMAT_PCM_U8 || subformat == SF_FORMAT_PCM_S8 || subformat == SF_FORMAT_PCM_16)
					return 1 ;
				if (subformat == SF_FORMAT_PCM_24 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE)
					return 1 ;
				if (subformat == SF_FORMAT_ALAW || subformat == SF_FORMAT_ULAW)
					return 1 ;
				if ((subformat == SF_FORMAT_DWVW_12 || subformat == SF_FORMAT_DWVW_16 ||
							subformat == SF_FORMAT_DWVW_24) && info-> channels == 1)
					return 1 ;
				if (subformat == SF_FORMAT_GSM610 && info->channels == 1)
					return 1 ;
				if (subformat == SF_FORMAT_VOX_ADPCM && info->channels == 1)
					return 1 ;
				if ((subformat == SF_FORMAT_NMS_ADPCM_16 || subformat == SF_FORMAT_NMS_ADPCM_24 ||
							subformat == SF_FORMAT_NMS_ADPCM_32) && info->channels == 1)
					return 1 ;
				break ;

		case SF_FORMAT_PAF :
				if (subformat == SF_FORMAT_PCM_S8 || subformat == SF_FORMAT_PCM_16 || subformat == SF_FORMAT_PCM_24)
					return 1 ;
				break ;

		case SF_FORMAT_SVX :
				/* SVX only supports writing mono SVX files. */
				if (info->channels > 1)
					return 0 ;
				/* Always big endian. */
				if (endian == SF_ENDIAN_LITTLE || endian == SF_ENDIAN_CPU)
					return 0 ;

				if (subformat == SF_FORMAT_PCM_S8 || subformat == SF_FORMAT_PCM_16)
					return 1 ;
				break ;

		case SF_FORMAT_NIST :
				if (subformat == SF_FORMAT_PCM_S8 || subformat == SF_FORMAT_PCM_16)
					return 1 ;
				if (subformat == SF_FORMAT_PCM_24 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				if (subformat == SF_FORMAT_ULAW || subformat == SF_FORMAT_ALAW)
					return 1 ;
				break ;

		case SF_FORMAT_IRCAM :
				if (info->channels > 256)
					return 0 ;
				if (subformat == SF_FORMAT_PCM_16 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				if (subformat == SF_FORMAT_ULAW || subformat == SF_FORMAT_ALAW || subformat == SF_FORMAT_FLOAT)
					return 1 ;
				break ;

		case SF_FORMAT_VOC :
				if (info->channels > 2)
					return 0 ;
				/* VOC is strictly little endian. */
				if (endian == SF_ENDIAN_BIG || endian == SF_ENDIAN_CPU)
					return 0 ;
				if (subformat == SF_FORMAT_PCM_U8 || subformat == SF_FORMAT_PCM_16)
					return 1 ;
				if (subformat == SF_FORMAT_ULAW || subformat == SF_FORMAT_ALAW)
					return 1 ;
				break ;

		case SF_FORMAT_W64 :
				/* W64 is strictly little endian. */
				if (endian == SF_ENDIAN_BIG || endian == SF_ENDIAN_CPU)
					return 0 ;
				if (subformat == SF_FORMAT_PCM_U8 || subformat == SF_FORMAT_PCM_16)
					return 1 ;
				if (subformat == SF_FORMAT_PCM_24 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				if ((subformat == SF_FORMAT_IMA_ADPCM || subformat == SF_FORMAT_MS_ADPCM) && info->channels <= 2)
					return 1 ;
				if (subformat == SF_FORMAT_GSM610 && info->channels == 1)
					return 1 ;
				if (subformat == SF_FORMAT_ULAW || subformat == SF_FORMAT_ALAW)
					return 1 ;
				if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE)
					return 1 ;
				break ;

		case SF_FORMAT_MAT4 :
				if (subformat == SF_FORMAT_PCM_16 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE)
					return 1 ;
				break ;

		case SF_FORMAT_MAT5 :
				if (subformat == SF_FORMAT_PCM_U8 || subformat == SF_FORMAT_PCM_16 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE)
					return 1 ;
				break ;

		case SF_FORMAT_PVF :
				if (subformat == SF_FORMAT_PCM_S8 || subformat == SF_FORMAT_PCM_16 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				break ;

		case SF_FORMAT_XI :
				if (info->channels != 1)
					return 0 ;
				if (subformat == SF_FORMAT_DPCM_8 || subformat == SF_FORMAT_DPCM_16)
					return 1 ;
				break ;

		case SF_FORMAT_HTK :
				if (info->channels != 1)
					return 0 ;
				/* HTK is strictly big endian. */
				if (endian == SF_ENDIAN_LITTLE || endian == SF_ENDIAN_CPU)
					return 0 ;
				if (subformat == SF_FORMAT_PCM_16)
					return 1 ;
				break ;

		case SF_FORMAT_SDS :
				if (info->channels != 1)
					return 0 ;
				/* SDS is strictly big endian. */
				if (endian == SF_ENDIAN_LITTLE || endian == SF_ENDIAN_CPU)
					return 0 ;
				if (subformat == SF_FORMAT_PCM_S8 || subformat == SF_FORMAT_PCM_16 || subformat == SF_FORMAT_PCM_24)
					return 1 ;
				break ;

		case SF_FORMAT_AVR :
				if (info->channels > 2)
					return 0 ;
				/* SDS is strictly big endian. */
				if (endian == SF_ENDIAN_LITTLE || endian == SF_ENDIAN_CPU)
					return 0 ;
				if (subformat == SF_FORMAT_PCM_U8 || subformat == SF_FORMAT_PCM_S8 || subformat == SF_FORMAT_PCM_16)
					return 1 ;
				break ;

		case SF_FORMAT_FLAC :
				/* FLAC can't do more than 8 channels. */
				if (info->channels > 8)
					return 0 ;
				if (endian != SF_ENDIAN_FILE)
					return 0 ;
				if (subformat == SF_FORMAT_PCM_S8 || subformat == SF_FORMAT_PCM_16 || subformat == SF_FORMAT_PCM_24)
					return 1 ;
				break ;

		case SF_FORMAT_SD2 :
				/* SD2 is strictly big endian. */
				if (endian == SF_ENDIAN_LITTLE || endian == SF_ENDIAN_CPU)
					return 0 ;
				if (subformat == SF_FORMAT_PCM_S8 || subformat == SF_FORMAT_PCM_16 || subformat == SF_FORMAT_PCM_24 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				break ;

		case SF_FORMAT_WVE :
				if (info->channels > 1)
					return 0 ;
				/* WVE is strictly big endian. */
				if (endian == SF_ENDIAN_BIG || endian == SF_ENDIAN_CPU)
					return 0 ;
				if (subformat == SF_FORMAT_ALAW)
					return 1 ;
				break ;

		case SF_FORMAT_OGG :
				if (endian != SF_ENDIAN_FILE)
					return 0 ;
				if (subformat == SF_FORMAT_VORBIS)
					return 1 ;
				if (subformat == SF_FORMAT_OPUS)
					return 1 ;
				break ;

		case SF_FORMAT_MPC2K :
				if (info->channels > 2)
					return 0 ;
				/* MPC2000 is strictly little endian. */
				if (endian == SF_ENDIAN_BIG || endian == SF_ENDIAN_CPU)
					return 0 ;
				if (subformat == SF_FORMAT_PCM_16)
					return 1 ;
				break ;

		case SF_FORMAT_RF64 :
				if (endian == SF_ENDIAN_BIG || endian == SF_ENDIAN_CPU)
					return 0 ;
				if (subformat == SF_FORMAT_PCM_U8 || subformat == SF_FORMAT_PCM_16)
					return 1 ;
				if (subformat == SF_FORMAT_PCM_24 || subformat == SF_FORMAT_PCM_32)
					return 1 ;
				if (subformat == SF_FORMAT_ULAW || subformat == SF_FORMAT_ALAW)
					return 1 ;
				if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE)
					return 1 ;
				break ;
		default : break ;
		} ;

	return 0 ;
} /* sf_format_check */

/*------------------------------------------------------------------------------
*/

const char *
sf_version_string (void)
{
#if	ENABLE_EXPERIMENTAL_CODE
	return PACKAGE_NAME "-" PACKAGE_VERSION "-exp" ;
#else
	return PACKAGE_NAME "-" PACKAGE_VERSION ;
#endif
}


/*------------------------------------------------------------------------------
*/

int
sf_command	(SNDFILE *sndfile, int command, void *data, int datasize)
{	SF_PRIVATE *psf = (SF_PRIVATE *) sndfile ;
	double quality ;
	int old_value ;

	/* This set of commands do not need the sndfile parameter. */
	switch (command)
	{	case SFC_GET_LIB_VERSION :
			if (data == NULL)
			{	if (psf)
					psf->error = SFE_BAD_COMMAND_PARAM ;
				return 0 ;
				} ;
			snprintf (data, datasize, "%s", sf_version_string ()) ;
			return strlen (data) ;

		case SFC_GET_SIMPLE_FORMAT_COUNT :
			if (data == NULL || datasize != SIGNED_SIZEOF (int))
				return (sf_errno = SFE_BAD_COMMAND_PARAM) ;
			*((int*) data) = psf_get_format_simple_count () ;
			return 0 ;

		case SFC_GET_SIMPLE_FORMAT :
			if (data == NULL || datasize != SIGNED_SIZEOF (SF_FORMAT_INFO))
				return (sf_errno = SFE_BAD_COMMAND_PARAM) ;
			return psf_get_format_simple (data) ;

		case SFC_GET_FORMAT_MAJOR_COUNT :
			if (data == NULL || datasize != SIGNED_SIZEOF (int))
				return (sf_errno = SFE_BAD_COMMAND_PARAM) ;
			*((int*) data) = psf_get_format_major_count () ;
			return 0 ;

		case SFC_GET_FORMAT_MAJOR :
			if (data == NULL || datasize != SIGNED_SIZEOF (SF_FORMAT_INFO))
				return (sf_errno = SFE_BAD_COMMAND_PARAM) ;
			return psf_get_format_major (data) ;

		case SFC_GET_FORMAT_SUBTYPE_COUNT :
			if (data == NULL || datasize != SIGNED_SIZEOF (int))
				return (sf_errno = SFE_BAD_COMMAND_PARAM) ;
			*((int*) data) = psf_get_format_subtype_count () ;
			return 0 ;

		case SFC_GET_FORMAT_SUBTYPE :
			if (data == NULL || datasize != SIGNED_SIZEOF (SF_FORMAT_INFO))
				return (sf_errno = SFE_BAD_COMMAND_PARAM) ;
			return psf_get_format_subtype (data) ;

		case SFC_GET_FORMAT_INFO :
			if (data == NULL || datasize != SIGNED_SIZEOF (SF_FORMAT_INFO))
				return (sf_errno = SFE_BAD_COMMAND_PARAM) ;
			return psf_get_format_info (data) ;
		} ;

	if (sndfile == NULL && command == SFC_GET_LOG_INFO)
	{	if (data == NULL)
			return (sf_errno = SFE_BAD_COMMAND_PARAM) ;
		snprintf (data, datasize, "%s", sf_parselog) ;
		return strlen (data) ;
		} ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	switch (command)
	{	case SFC_SET_NORM_FLOAT :
			old_value = psf->norm_float ;
			psf->norm_float = (datasize) ? SF_TRUE : SF_FALSE ;
			return old_value ;

		case SFC_GET_CURRENT_SF_INFO :
			if (data == NULL || datasize != SIGNED_SIZEOF (SF_INFO))
				return (sf_errno = SFE_BAD_COMMAND_PARAM) ;
			memcpy (data, &psf->sf, sizeof (SF_INFO)) ;
			break ;

		case SFC_SET_NORM_DOUBLE :
			old_value = psf->norm_double ;
			psf->norm_double = (datasize) ? SF_TRUE : SF_FALSE ;
			return old_value ;

		case SFC_GET_NORM_FLOAT :
			return psf->norm_float ;

		case SFC_GET_NORM_DOUBLE :
			return psf->norm_double ;

		case SFC_SET_SCALE_FLOAT_INT_READ :
			old_value = psf->float_int_mult ;

			psf->float_int_mult = (datasize != 0) ? SF_TRUE : SF_FALSE ;
			if (psf->float_int_mult && psf->float_max < 0.0)
				/* Scale to prevent wrap-around distortion. */
				psf->float_max = (32768.0 / 32767.0) * psf_calc_signal_max (psf, SF_FALSE) ;
			return old_value ;

		case SFC_SET_SCALE_INT_FLOAT_WRITE :
			old_value = psf->scale_int_float ;
			psf->scale_int_float = (datasize != 0) ? SF_TRUE : SF_FALSE ;
			return old_value ;

		case SFC_SET_ADD_PEAK_CHUNK :
			{	int format = SF_CONTAINER (psf->sf.format) ;

				/* Only WAV and AIFF support the PEAK chunk. */
				switch (format)
				{	case SF_FORMAT_AIFF :
					case SF_FORMAT_CAF :
					case SF_FORMAT_WAV :
					case SF_FORMAT_WAVEX :
					case SF_FORMAT_RF64 :
						break ;

					default :
						return SF_FALSE ;
					} ;

				format = SF_CODEC (psf->sf.format) ;

				/* Only files containg the following data types support the PEAK chunk. */
				if (format != SF_FORMAT_FLOAT && format != SF_FORMAT_DOUBLE)
					return SF_FALSE ;

				} ;
			/* Can only do this is in SFM_WRITE mode. */
			if (psf->file.mode != SFM_WRITE && psf->file.mode != SFM_RDWR)
				return SF_FALSE ;
			/* If data has already been written this must fail. */
			if (psf->have_written)
			{	psf->error = SFE_CMD_HAS_DATA ;
				return SF_FALSE ;
				} ;
			/* Everything seems OK, so set psf->has_peak and re-write header. */
			if (datasize == SF_FALSE && psf->peak_info != NULL)
			{	free (psf->peak_info) ;
				psf->peak_info = NULL ;
				}
			else if (psf->peak_info == NULL)
			{	psf->peak_info = peak_info_calloc (psf->sf.channels) ;
				if (psf->peak_info != NULL)
					psf->peak_info->peak_loc = SF_PEAK_START ;
				} ;

			if (psf->write_header)
				psf->write_header (psf, SF_TRUE) ;
			return datasize ;

		case SFC_SET_ADD_HEADER_PAD_CHUNK :
			return SF_FALSE ;

		case SFC_GET_LOG_INFO :
			if (data == NULL)
				return SFE_BAD_COMMAND_PARAM ;
			snprintf (data, datasize, "%s", psf->parselog.buf) ;
			return strlen (data) ;

		case SFC_CALC_SIGNAL_MAX :
			if (data == NULL || datasize != sizeof (double))
				return (psf->error = SFE_BAD_COMMAND_PARAM) ;
			*((double*) data) = psf_calc_signal_max (psf, SF_FALSE) ;
			break ;

		case SFC_CALC_NORM_SIGNAL_MAX :
			if (data == NULL || datasize != sizeof (double))
				return (psf->error = SFE_BAD_COMMAND_PARAM) ;
			*((double*) data) = psf_calc_signal_max (psf, SF_TRUE) ;
			break ;

		case SFC_CALC_MAX_ALL_CHANNELS :
			if (data == NULL || datasize != SIGNED_SIZEOF (double) * psf->sf.channels)
				return (psf->error = SFE_BAD_COMMAND_PARAM) ;
			return psf_calc_max_all_channels (psf, (double*) data, SF_FALSE) ;

		case SFC_CALC_NORM_MAX_ALL_CHANNELS :
			if (data == NULL || datasize != SIGNED_SIZEOF (double) * psf->sf.channels)
				return (psf->error = SFE_BAD_COMMAND_PARAM) ;
			return psf_calc_max_all_channels (psf, (double*) data, SF_TRUE) ;

		case SFC_GET_SIGNAL_MAX :
			if (data == NULL || datasize != sizeof (double))
			{	psf->error = SFE_BAD_COMMAND_PARAM ;
				return SF_FALSE ;
				} ;
			return psf_get_signal_max (psf, (double *) data) ;

		case SFC_GET_MAX_ALL_CHANNELS :
			if (data == NULL || datasize != SIGNED_SIZEOF (double) * psf->sf.channels)
			{	psf->error = SFE_BAD_COMMAND_PARAM ;
				return SF_FALSE ;
				} ;
			return psf_get_max_all_channels (psf, (double*) data) ;

		case SFC_UPDATE_HEADER_NOW :
			if (psf->write_header)
				psf->write_header (psf, SF_TRUE) ;
			break ;

		case SFC_SET_UPDATE_HEADER_AUTO :
			psf->auto_header = datasize ? SF_TRUE : SF_FALSE ;
			return psf->auto_header ;
			break ;

		case SFC_SET_ADD_DITHER_ON_WRITE :
		case SFC_SET_ADD_DITHER_ON_READ :
			/*
			** FIXME !
			** These are obsolete. Just return.
			** Remove some time after version 1.0.8.
			*/
			break ;

		case SFC_SET_DITHER_ON_WRITE :
			if (data == NULL || datasize != SIGNED_SIZEOF (SF_DITHER_INFO))
				return (psf->error = SFE_BAD_COMMAND_PARAM) ;
			memcpy (&psf->write_dither, data, sizeof (psf->write_dither)) ;
			if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
				dither_init (psf, SFM_WRITE) ;
			break ;

		case SFC_SET_DITHER_ON_READ :
			if (data == NULL || datasize != SIGNED_SIZEOF (SF_DITHER_INFO))
				return (psf->error = SFE_BAD_COMMAND_PARAM) ;
			memcpy (&psf->read_dither, data, sizeof (psf->read_dither)) ;
			if (psf->file.mode == SFM_READ || psf->file.mode == SFM_RDWR)
				dither_init (psf, SFM_READ) ;
			break ;

		case SFC_FILE_TRUNCATE :
			if (psf->file.mode != SFM_WRITE && psf->file.mode != SFM_RDWR)
				return SF_TRUE ;
			if (datasize != sizeof (sf_count_t))
				return SF_TRUE ;
			if (data == NULL || datasize != sizeof (sf_count_t))
			{	psf->error = SFE_BAD_COMMAND_PARAM ;
				return SF_FALSE ;
				}
			else
			{	sf_count_t position ;

				position = *((sf_count_t*) data) ;

				if (sf_seek (sndfile, position, SEEK_SET) != position)
					return SF_TRUE ;

				psf->sf.frames = position ;

				position = psf_fseek (psf, 0, SEEK_CUR) ;

				return psf_ftruncate (psf, position) ;
				} ;
			break ;

		case SFC_SET_RAW_START_OFFSET :
			if (data == NULL || datasize != sizeof (sf_count_t))
				return (psf->error = SFE_BAD_COMMAND_PARAM) ;

			if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_RAW)
				return (psf->error = SFE_BAD_COMMAND_PARAM) ;

			psf->dataoffset = *((sf_count_t*) data) ;
			sf_seek (sndfile, 0, SEEK_CUR) ;
			break ;

		case SFC_GET_EMBED_FILE_INFO :
			if (data == NULL || datasize != sizeof (SF_EMBED_FILE_INFO))
				return (psf->error = SFE_BAD_COMMAND_PARAM) ;

			((SF_EMBED_FILE_INFO*) data)->offset = psf->fileoffset ;
			((SF_EMBED_FILE_INFO*) data)->length = psf->filelength ;
			break ;

		/* Lite remove start */
		case SFC_TEST_IEEE_FLOAT_REPLACE :
			psf->ieee_replace = (datasize) ? SF_TRUE : SF_FALSE ;
			if ((SF_CODEC (psf->sf.format)) == SF_FORMAT_FLOAT)
				float32_init (psf) ;
			else if ((SF_CODEC (psf->sf.format)) == SF_FORMAT_DOUBLE)
				double64_init (psf) ;
			else
				return (psf->error = SFE_BAD_COMMAND_PARAM) ;
			break ;
		/* Lite remove end */

		case SFC_SET_CLIPPING :
			psf->add_clipping = (datasize) ? SF_TRUE : SF_FALSE ;
			return psf->add_clipping ;

		case SFC_GET_CLIPPING :
			return psf->add_clipping ;

		case SFC_GET_LOOP_INFO :
			if (datasize != sizeof (SF_LOOP_INFO) || data == NULL)
			{	psf->error = SFE_BAD_COMMAND_PARAM ;
				return SF_FALSE ;
				} ;
			if (psf->loop_info == NULL)
				return SF_FALSE ;
			memcpy (data, psf->loop_info, sizeof (SF_LOOP_INFO)) ;
			return SF_TRUE ;

		case SFC_SET_BROADCAST_INFO :
			{	int format = SF_CONTAINER (psf->sf.format) ;

				/* Only WAV and RF64 supports the BEXT (Broadcast) chunk. */
				if (format != SF_FORMAT_WAV && format != SF_FORMAT_WAVEX && format != SF_FORMAT_RF64)
					return SF_FALSE ;
				} ;

			/* Only makes sense in SFM_WRITE or SFM_RDWR mode. */
			if ((psf->file.mode != SFM_WRITE) && (psf->file.mode != SFM_RDWR))
				return SF_FALSE ;
			/* If data has already been written this must fail. */
			if (psf->broadcast_16k == NULL && psf->have_written)
			{	psf->error = SFE_CMD_HAS_DATA ;
				return SF_FALSE ;
				} ;

			if (NOT (broadcast_var_set (psf, data, datasize)))
				return SF_FALSE ;

			if (psf->write_header)
				psf->write_header (psf, SF_TRUE) ;
			return SF_TRUE ;

		case SFC_GET_BROADCAST_INFO :
			if (data == NULL)
			{	psf->error = SFE_BAD_COMMAND_PARAM ;
				return SF_FALSE ;
				} ;
			return broadcast_var_get (psf, data, datasize) ;

		case SFC_SET_CART_INFO :
			{	int format = SF_CONTAINER (psf->sf.format) ;
				/* Only WAV and RF64 support cart chunk format */
				if (format != SF_FORMAT_WAV && format != SF_FORMAT_RF64)
					return SF_FALSE ;
				} ;

			/* Only makes sense in SFM_WRITE or SFM_RDWR mode */
			if ((psf->file.mode != SFM_WRITE) && (psf->file.mode != SFM_RDWR))
				return SF_FALSE ;
			/* If data has already been written this must fail. */
			if (psf->cart_16k == NULL && psf->have_written)
			{	psf->error = SFE_CMD_HAS_DATA ;
				return SF_FALSE ;
				} ;
			if (NOT (cart_var_set (psf, data, datasize)))
				return SF_FALSE ;
			if (psf->write_header)
				psf->write_header (psf, SF_TRUE) ;
			return SF_TRUE ;

		case SFC_GET_CART_INFO :
			if (data == NULL)
			{	psf->error = SFE_BAD_COMMAND_PARAM ;
				return SF_FALSE ;
				} ;
			return cart_var_get (psf, data, datasize) ;

		case SFC_GET_CUE_COUNT :
			if (datasize != sizeof (uint32_t) || data == NULL)
			{	psf->error = SFE_BAD_COMMAND_PARAM ;
				return SF_FALSE ;
				} ;
			if (psf->cues != NULL)
			{	*((uint32_t *) data) = psf->cues->cue_count ;
				return SF_TRUE ;
				} ;
			return SF_FALSE ;

		case SFC_GET_CUE :
			if (datasize < (int) sizeof (uint32_t) || data == NULL)
			{	psf->error = SFE_BAD_COMMAND_PARAM ;
				return SF_FALSE ;
				} ;
			if (psf->cues == NULL)
				return SF_FALSE ;
			psf_get_cues (psf, data, datasize) ;
			return SF_TRUE ;

		case SFC_SET_CUE :
			if (psf->have_written)
			{	psf->error = SFE_CMD_HAS_DATA ;
				return SF_FALSE ;
				} ;
			if (datasize < (int) sizeof (uint32_t) || data == NULL)
			{	psf->error = SFE_BAD_COMMAND_PARAM ;
				return SF_FALSE ;
				} ;
			if (psf->cues == NULL && (psf->cues = psf_cues_dup (data, datasize)) == NULL)
			{	psf->error = SFE_MALLOC_FAILED ;
				return SF_FALSE ;
				} ;
			return SF_TRUE ;

		case SFC_GET_INSTRUMENT :
			if (datasize != sizeof (SF_INSTRUMENT) || data == NULL)
			{	psf->error = SFE_BAD_COMMAND_PARAM ;
				return SF_FALSE ;
				} ;
			if (psf->instrument == NULL)
				return SF_FALSE ;
			memcpy (data, psf->instrument, sizeof (SF_INSTRUMENT)) ;
			return SF_TRUE ;

		case SFC_SET_INSTRUMENT :
			/* If data has already been written this must fail. */
			if (psf->have_written)
			{	psf->error = SFE_CMD_HAS_DATA ;
				return SF_FALSE ;
				} ;
			if (datasize != sizeof (SF_INSTRUMENT) || data == NULL)
			{	psf->error = SFE_BAD_COMMAND_PARAM ;
				return SF_FALSE ;
				} ;

			if (psf->instrument == NULL && (psf->instrument = psf_instrument_alloc ()) == NULL)
			{	psf->error = SFE_MALLOC_FAILED ;
				return SF_FALSE ;
				} ;
			memcpy (psf->instrument, data, sizeof (SF_INSTRUMENT)) ;
			return SF_TRUE ;

		case SFC_RAW_DATA_NEEDS_ENDSWAP :
			return psf->data_endswap ;

		case SFC_GET_CHANNEL_MAP_INFO :
			if (psf->channel_map == NULL)
				return SF_FALSE ;

			if (data == NULL || datasize != SIGNED_SIZEOF (psf->channel_map [0]) * psf->sf.channels)
			{	psf->error = SFE_BAD_COMMAND_PARAM ;
				return SF_FALSE ;
				} ;

			memcpy (data, psf->channel_map, datasize) ;
			return SF_TRUE ;

		case SFC_SET_CHANNEL_MAP_INFO :
			if (psf->have_written)
			{	psf->error = SFE_CMD_HAS_DATA ;
				return SF_FALSE ;
				} ;
			if (data == NULL || datasize != SIGNED_SIZEOF (psf->channel_map [0]) * psf->sf.channels)
			{	psf->error = SFE_BAD_COMMAND_PARAM ;
				return SF_FALSE ;
				} ;

			{	int *iptr ;

				for (iptr = data ; iptr < (int*) data + psf->sf.channels ; iptr++)
				{	if (*iptr <= SF_CHANNEL_MAP_INVALID || *iptr >= SF_CHANNEL_MAP_MAX)
					{	psf->error = SFE_BAD_COMMAND_PARAM ;
						return SF_FALSE ;
						} ;
					} ;
				} ;

			free (psf->channel_map) ;
			if ((psf->channel_map = malloc (datasize)) == NULL)
			{	psf->error = SFE_MALLOC_FAILED ;
				return SF_FALSE ;
				} ;

			memcpy (psf->channel_map, data, datasize) ;

			/*
			**	Pass the command down to the container's command handler.
			**	Don't pass user data, use validated psf->channel_map data instead.
			*/
			if (psf->command)
				return psf->command (psf, command, NULL, 0) ;
			return SF_FALSE ;

		case SFC_SET_VBR_ENCODING_QUALITY :
			if (data == NULL || datasize != sizeof (double))
				return SF_FALSE ;

			quality = *((double *) data) ;
			quality = 1.0 - SF_MAX (0.0, SF_MIN (1.0, quality)) ;
			return sf_command (sndfile, SFC_SET_COMPRESSION_LEVEL, &quality, sizeof (quality)) ;


		default :
			/* Must be a file specific command. Pass it on. */
			if (psf->command)
				return psf->command (psf, command, data, datasize) ;

			psf_log_printf (psf, "*** sf_command : cmd = 0x%X\n", command) ;
			return (psf->error = SFE_BAD_COMMAND_PARAM) ;
		} ;

	return 0 ;
} /* sf_command */

/*------------------------------------------------------------------------------
*/

sf_count_t
sf_seek	(SNDFILE *sndfile, sf_count_t offset, int whence)
{	SF_PRIVATE 	*psf ;
	sf_count_t	seek_from_start = 0, retval ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (! psf->sf.seekable)
	{	psf->error = SFE_NOT_SEEKABLE ;
		return	PSF_SEEK_ERROR ;
		} ;

	/* If the whence parameter has a mode ORed in, check to see that
	** it makes sense.
	*/
	if (((whence & SFM_MASK) == SFM_WRITE && psf->file.mode == SFM_READ) ||
			((whence & SFM_MASK) == SFM_READ && psf->file.mode == SFM_WRITE))
	{	psf->error = SFE_WRONG_SEEK ;
		return PSF_SEEK_ERROR ;
		} ;

	/* Convert all SEEK_CUR and SEEK_END into seek_from_start to be
	** used with SEEK_SET.
	*/
	switch (whence)
	{	/* The SEEK_SET behaviour is independant of mode. */
		case SEEK_SET :
		case SEEK_SET | SFM_READ :
		case SEEK_SET | SFM_WRITE :
		case SEEK_SET | SFM_RDWR :
				seek_from_start = offset ;
				break ;

		/* The SEEK_CUR is a little more tricky. */
		case SEEK_CUR :
				if (offset == 0)
				{	if (psf->file.mode == SFM_READ)
						return psf->read_current ;
					if (psf->file.mode == SFM_WRITE)
						return psf->write_current ;
					} ;
				if (psf->file.mode == SFM_READ)
					seek_from_start = psf->read_current + offset ;
				else if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
					seek_from_start = psf->write_current + offset ;
				else
					psf->error = SFE_AMBIGUOUS_SEEK ;
				break ;

		case SEEK_CUR | SFM_READ :
				if (offset == 0)
					return psf->read_current ;
				seek_from_start = psf->read_current + offset ;
				break ;

		case SEEK_CUR | SFM_WRITE :
				if (offset == 0)
					return psf->write_current ;
				seek_from_start = psf->write_current + offset ;
				break ;

		/* The SEEK_END */
		case SEEK_END :
		case SEEK_END | SFM_READ :
		case SEEK_END | SFM_WRITE :
				seek_from_start = psf->sf.frames + offset ;
				break ;

		default :
				psf->error = SFE_BAD_SEEK ;
				break ;
		} ;

	if (psf->error)
		return PSF_SEEK_ERROR ;

	if (psf->file.mode == SFM_RDWR || psf->file.mode == SFM_WRITE)
	{	if (seek_from_start < 0)
		{	psf->error = SFE_BAD_SEEK ;
			return PSF_SEEK_ERROR ;
			} ;
		}
	else if (seek_from_start < 0 || seek_from_start > psf->sf.frames)
	{	psf->error = SFE_BAD_SEEK ;
		return PSF_SEEK_ERROR ;
		} ;

	if (psf->seek)
	{	int new_mode = (whence & SFM_MASK) ? (whence & SFM_MASK) : psf->file.mode ;

		retval = psf->seek (psf, new_mode, seek_from_start) ;

		switch (new_mode)
		{	case SFM_READ :
					psf->read_current = retval ;
					break ;
			case SFM_WRITE :
					psf->write_current = retval ;
					break ;
			case SFM_RDWR :
					psf->read_current = retval ;
					psf->write_current = retval ;
					new_mode = SFM_READ ;
					break ;
			} ;

		psf->last_op = new_mode ;

		return retval ;
		} ;

	psf->error = SFE_AMBIGUOUS_SEEK ;
	return PSF_SEEK_ERROR ;
} /* sf_seek */

/*------------------------------------------------------------------------------
*/

const char*
sf_get_string (SNDFILE *sndfile, int str_type)
{	SF_PRIVATE 	*psf ;

	if ((psf = (SF_PRIVATE*) sndfile) == NULL)
		return NULL ;
	if (psf->Magick != SNDFILE_MAGICK)
		return NULL ;

	return psf_get_string (psf, str_type) ;
} /* sf_get_string */

int
sf_set_string (SNDFILE *sndfile, int str_type, const char* str)
{	SF_PRIVATE 	*psf ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	return psf_set_string (psf, str_type, str) ;
} /* sf_get_string */

/*------------------------------------------------------------------------------
*/

int
sf_current_byterate (SNDFILE *sndfile)
{	SF_PRIVATE 	*psf ;

	if ((psf = (SF_PRIVATE*) sndfile) == NULL)
		return -1 ;
	if (psf->Magick != SNDFILE_MAGICK)
		return -1 ;

	/* This should cover all PCM and floating point formats. */
	if (psf->bytewidth)
		return psf->sf.samplerate * psf->sf.channels * psf->bytewidth ;

	if (psf->byterate)
		return psf->byterate (psf) ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_IMA_ADPCM :
		case SF_FORMAT_MS_ADPCM :
		case SF_FORMAT_VOX_ADPCM :
			return (psf->sf.samplerate * psf->sf.channels) / 2 ;

		case SF_FORMAT_GSM610 :
			return (psf->sf.samplerate * psf->sf.channels * 13000) / 8000 ;

		case SF_FORMAT_NMS_ADPCM_16:
			return psf->sf.samplerate / 4 + 10 ;

		case SF_FORMAT_NMS_ADPCM_24:
			return psf->sf.samplerate * 3 / 8 + 10 ;

		case SF_FORMAT_NMS_ADPCM_32:
			return psf->sf.samplerate / 2 + 10 ;

		case SF_FORMAT_G721_32 :	/* 32kbs G721 ADPCM encoding. */
			return (psf->sf.samplerate * psf->sf.channels) / 2 ;

		case SF_FORMAT_G723_24 :	/* 24kbs G723 ADPCM encoding. */
			return (psf->sf.samplerate * psf->sf.channels * 3) / 8 ;

		case SF_FORMAT_G723_40 :	/* 40kbs G723 ADPCM encoding. */
			return (psf->sf.samplerate * psf->sf.channels * 5) / 8 ;

		default :
			break ;
		} ;

	return -1 ;
} /* sf_current_byterate */

/*==============================================================================
*/

sf_count_t
sf_read_raw		(SNDFILE *sndfile, void *ptr, sf_count_t bytes)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count, extra ;
	int			bytewidth, blockwidth ;

	if (bytes == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	bytewidth = (psf->bytewidth > 0) ? psf->bytewidth : 1 ;
	blockwidth = (psf->blockwidth > 0) ? psf->blockwidth : 1 ;

	if (psf->file.mode == SFM_WRITE)
	{	psf->error = SFE_NOT_READMODE ;
		return	0 ;
		} ;

	if (bytes < 0 || psf->read_current >= psf->sf.frames)
	{	psf_memset (ptr, 0, bytes) ;
		return 0 ;
		} ;

	if (bytes % (psf->sf.channels * bytewidth))
	{	psf->error = SFE_BAD_READ_ALIGN ;
		return 0 ;
		} ;

	if (psf->last_op != SFM_READ)
		if (psf->seek (psf, SFM_READ, psf->read_current) < 0)
			return 0 ;

	count = psf_fread (ptr, 1, bytes, psf) ;

	if (psf->read_current + count / blockwidth <= psf->sf.frames)
		psf->read_current += count / blockwidth ;
	else
	{	count = (psf->sf.frames - psf->read_current) * blockwidth ;
		extra = bytes - count ;
		psf_memset (((char *) ptr) + count, 0, extra) ;
		psf->read_current = psf->sf.frames ;
		} ;

	psf->last_op = SFM_READ ;

	return count ;
} /* sf_read_raw */

/*------------------------------------------------------------------------------
*/

sf_count_t
sf_read_short	(SNDFILE *sndfile, short *ptr, sf_count_t len)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count, extra ;

	if (len == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (len <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_WRITE)
	{	psf->error = SFE_NOT_READMODE ;
		return 0 ;
		} ;

	if (len % psf->sf.channels)
	{	psf->error = SFE_BAD_READ_ALIGN ;
		return 0 ;
		} ;

	if (psf->read_current >= psf->sf.frames)
	{	psf_memset (ptr, 0, len * sizeof (short)) ;
		return 0 ; /* End of file. */
		} ;

	if (psf->read_short == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return	0 ;
		} ;

	if (psf->last_op != SFM_READ)
		if (psf->seek (psf, SFM_READ, psf->read_current) < 0)
			return 0 ;

	count = psf->read_short (psf, ptr, len) ;

	if (psf->read_current + count / psf->sf.channels <= psf->sf.frames)
		psf->read_current += count / psf->sf.channels ;
	else
	{	count = (psf->sf.frames - psf->read_current) * psf->sf.channels ;
		extra = len - count ;
		psf_memset (ptr + count, 0, extra * sizeof (short)) ;
		psf->read_current = psf->sf.frames ;
		} ;

	psf->last_op = SFM_READ ;

	return count ;
} /* sf_read_short */

sf_count_t
sf_readf_short		(SNDFILE *sndfile, short *ptr, sf_count_t frames)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count, extra ;

	if (frames == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (frames <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_WRITE)
	{	psf->error = SFE_NOT_READMODE ;
		return 0 ;
		} ;

	if (psf->read_current >= psf->sf.frames)
	{	psf_memset (ptr, 0, frames * psf->sf.channels * sizeof (short)) ;
		return 0 ; /* End of file. */
		} ;

	if (psf->read_short == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return 0 ;
		} ;

	if (psf->last_op != SFM_READ)
		if (psf->seek (psf, SFM_READ, psf->read_current) < 0)
			return 0 ;

	count = psf->read_short (psf, ptr, frames * psf->sf.channels) ;

	if (psf->read_current + count / psf->sf.channels <= psf->sf.frames)
		psf->read_current += count / psf->sf.channels ;
	else
	{	count = (psf->sf.frames - psf->read_current) * psf->sf.channels ;
		extra = frames * psf->sf.channels - count ;
		psf_memset (ptr + count, 0, extra * sizeof (short)) ;
		psf->read_current = psf->sf.frames ;
		} ;

	psf->last_op = SFM_READ ;

	return count / psf->sf.channels ;
} /* sf_readf_short */

/*------------------------------------------------------------------------------
*/

sf_count_t
sf_read_int		(SNDFILE *sndfile, int *ptr, sf_count_t len)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count, extra ;

	if (len == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (len <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_WRITE)
	{	psf->error = SFE_NOT_READMODE ;
		return 0 ;
		} ;

	if (len % psf->sf.channels)
	{	psf->error = SFE_BAD_READ_ALIGN ;
		return 0 ;
		} ;

	if (psf->read_current >= psf->sf.frames)
	{	psf_memset (ptr, 0, len * sizeof (int)) ;
		return 0 ;
		} ;

	if (psf->read_int == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return 0 ;
		} ;

	if (psf->last_op != SFM_READ)
		if (psf->seek (psf, SFM_READ, psf->read_current) < 0)
			return 0 ;

	count = psf->read_int (psf, ptr, len) ;

	if (psf->read_current + count / psf->sf.channels <= psf->sf.frames)
		psf->read_current += count / psf->sf.channels ;
	else
	{	count = (psf->sf.frames - psf->read_current) * psf->sf.channels ;
		extra = len - count ;
		psf_memset (ptr + count, 0, extra * sizeof (int)) ;
		psf->read_current = psf->sf.frames ;
		} ;

	psf->last_op = SFM_READ ;

	return count ;
} /* sf_read_int */

sf_count_t
sf_readf_int	(SNDFILE *sndfile, int *ptr, sf_count_t frames)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count, extra ;

	if (frames == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (frames <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_WRITE)
	{	psf->error = SFE_NOT_READMODE ;
		return 0 ;
		} ;

	if (psf->read_current >= psf->sf.frames)
	{	psf_memset (ptr, 0, frames * psf->sf.channels * sizeof (int)) ;
		return 0 ;
		} ;

	if (psf->read_int == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return	0 ;
		} ;

	if (psf->last_op != SFM_READ)
		if (psf->seek (psf, SFM_READ, psf->read_current) < 0)
			return 0 ;

	count = psf->read_int (psf, ptr, frames * psf->sf.channels) ;

	if (psf->read_current + count / psf->sf.channels <= psf->sf.frames)
		psf->read_current += count / psf->sf.channels ;
	else
	{	count = (psf->sf.frames - psf->read_current) * psf->sf.channels ;
		extra = frames * psf->sf.channels - count ;
		psf_memset (ptr + count, 0, extra * sizeof (int)) ;
		psf->read_current = psf->sf.frames ;
		} ;

	psf->last_op = SFM_READ ;

	return count / psf->sf.channels ;
} /* sf_readf_int */

/*------------------------------------------------------------------------------
*/

sf_count_t
sf_read_float	(SNDFILE *sndfile, float *ptr, sf_count_t len)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count, extra ;

	if (len == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (len <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_WRITE)
	{	psf->error = SFE_NOT_READMODE ;
		return 0 ;
		} ;

	if (len % psf->sf.channels)
	{	psf->error = SFE_BAD_READ_ALIGN ;
		return 0 ;
		} ;

	if (psf->read_current >= psf->sf.frames)
	{	psf_memset (ptr, 0, len * sizeof (float)) ;
		return 0 ;
		} ;

	if (psf->read_float == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return	0 ;
		} ;

	if (psf->last_op != SFM_READ)
		if (psf->seek (psf, SFM_READ, psf->read_current) < 0)
			return 0 ;

	count = psf->read_float (psf, ptr, len) ;

	if (psf->read_current + count / psf->sf.channels <= psf->sf.frames)
		psf->read_current += count / psf->sf.channels ;
	else
	{	count = (psf->sf.frames - psf->read_current) * psf->sf.channels ;
		extra = len - count ;
		psf_memset (ptr + count, 0, extra * sizeof (float)) ;
		psf->read_current = psf->sf.frames ;
		} ;

	psf->last_op = SFM_READ ;

	return count ;
} /* sf_read_float */

sf_count_t
sf_readf_float	(SNDFILE *sndfile, float *ptr, sf_count_t frames)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count, extra ;

	if (frames == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (frames <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_WRITE)
	{	psf->error = SFE_NOT_READMODE ;
		return 0 ;
		} ;

	if (psf->read_current >= psf->sf.frames)
	{	psf_memset (ptr, 0, frames * psf->sf.channels * sizeof (float)) ;
		return 0 ;
		} ;

	if (psf->read_float == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return	0 ;
		} ;

	if (psf->last_op != SFM_READ)
		if (psf->seek (psf, SFM_READ, psf->read_current) < 0)
			return 0 ;

	count = psf->read_float (psf, ptr, frames * psf->sf.channels) ;

	if (psf->read_current + count / psf->sf.channels <= psf->sf.frames)
		psf->read_current += count / psf->sf.channels ;
	else
	{	count = (psf->sf.frames - psf->read_current) * psf->sf.channels ;
		extra = frames * psf->sf.channels - count ;
		psf_memset (ptr + count, 0, extra * sizeof (float)) ;
		psf->read_current = psf->sf.frames ;
		} ;

	psf->last_op = SFM_READ ;

	return count / psf->sf.channels ;
} /* sf_readf_float */

/*------------------------------------------------------------------------------
*/

sf_count_t
sf_read_double	(SNDFILE *sndfile, double *ptr, sf_count_t len)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count, extra ;

	if (len == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (len <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_WRITE)
	{	psf->error = SFE_NOT_READMODE ;
		return 0 ;
		} ;

	if (len % psf->sf.channels)
	{	psf->error = SFE_BAD_READ_ALIGN ;
		return 0 ;
		} ;

	if (psf->read_current >= psf->sf.frames)
	{	psf_memset (ptr, 0, len * sizeof (double)) ;
		return 0 ;
		} ;

	if (psf->read_double == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return	0 ;
		} ;

	if (psf->last_op != SFM_READ)
		if (psf->seek (psf, SFM_READ, psf->read_current) < 0)
			return 0 ;

	count = psf->read_double (psf, ptr, len) ;

	if (psf->read_current + count / psf->sf.channels <= psf->sf.frames)
		psf->read_current += count / psf->sf.channels ;
	else
	{	count = (psf->sf.frames - psf->read_current) * psf->sf.channels ;
		extra = len - count ;
		psf_memset (ptr + count, 0, extra * sizeof (double)) ;
		psf->read_current = psf->sf.frames ;
		} ;

	psf->last_op = SFM_READ ;

	return count ;
} /* sf_read_double */

sf_count_t
sf_readf_double	(SNDFILE *sndfile, double *ptr, sf_count_t frames)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count, extra ;

	if (frames == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (frames <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_WRITE)
	{	psf->error = SFE_NOT_READMODE ;
		return 0 ;
		} ;

	if (psf->read_current >= psf->sf.frames)
	{	psf_memset (ptr, 0, frames * psf->sf.channels * sizeof (double)) ;
		return 0 ;
		} ;

	if (psf->read_double == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return	0 ;
		} ;

	if (psf->last_op != SFM_READ)
		if (psf->seek (psf, SFM_READ, psf->read_current) < 0)
			return 0 ;

	count = psf->read_double (psf, ptr, frames * psf->sf.channels) ;

	if (psf->read_current + count / psf->sf.channels <= psf->sf.frames)
		psf->read_current += count / psf->sf.channels ;
	else
	{	count = (psf->sf.frames - psf->read_current) * psf->sf.channels ;
		extra = frames * psf->sf.channels - count ;
		psf_memset (ptr + count, 0, extra * sizeof (double)) ;
		psf->read_current = psf->sf.frames ;
		} ;

	psf->last_op = SFM_READ ;

	return count / psf->sf.channels ;
} /* sf_readf_double */

/*------------------------------------------------------------------------------
*/

sf_count_t
sf_write_raw	(SNDFILE *sndfile, const void *ptr, sf_count_t len)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count ;
	int			bytewidth, blockwidth ;

	if (len == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (len <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	bytewidth = (psf->bytewidth > 0) ? psf->bytewidth : 1 ;
	blockwidth = (psf->blockwidth > 0) ? psf->blockwidth : 1 ;

	if (psf->file.mode == SFM_READ)
	{	psf->error = SFE_NOT_WRITEMODE ;
		return 0 ;
		} ;

	if (len % (psf->sf.channels * bytewidth))
	{	psf->error = SFE_BAD_WRITE_ALIGN ;
		return 0 ;
		} ;

	if (psf->last_op != SFM_WRITE)
		if (psf->seek (psf, SFM_WRITE, psf->write_current) < 0)
			return 0 ;

	if (psf->have_written == SF_FALSE && psf->write_header != NULL)
	{	if ((psf->error = psf->write_header (psf, SF_FALSE)))
			return 0 ;
		} ;
	psf->have_written = SF_TRUE ;

	count = psf_fwrite (ptr, 1, len, psf) ;

	psf->write_current += count / blockwidth ;

	psf->last_op = SFM_WRITE ;

	if (psf->write_current > psf->sf.frames)
	{	psf->sf.frames = psf->write_current ;
		psf->dataend = 0 ;
		} ;

	if (psf->auto_header && psf->write_header != NULL)
		psf->write_header (psf, SF_TRUE) ;

	return count ;
} /* sf_write_raw */

/*------------------------------------------------------------------------------
*/

sf_count_t
sf_write_short	(SNDFILE *sndfile, const short *ptr, sf_count_t len)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count ;

	if (len == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (len <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_READ)
	{	psf->error = SFE_NOT_WRITEMODE ;
		return 0 ;
		} ;

	if (len % psf->sf.channels)
	{	psf->error = SFE_BAD_WRITE_ALIGN ;
		return 0 ;
		} ;

	if (psf->write_short == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return 0 ;
		} ;

	if (psf->last_op != SFM_WRITE)
		if (psf->seek (psf, SFM_WRITE, psf->write_current) < 0)
			return 0 ;

	if (psf->have_written == SF_FALSE && psf->write_header != NULL)
	{	if ((psf->error = psf->write_header (psf, SF_FALSE)))
			return 0 ;
		} ;
	psf->have_written = SF_TRUE ;

	count = psf->write_short (psf, ptr, len) ;

	psf->write_current += count / psf->sf.channels ;

	psf->last_op = SFM_WRITE ;

	if (psf->write_current > psf->sf.frames)
	{	psf->sf.frames = psf->write_current ;
		psf->dataend = 0 ;
		} ;

	if (psf->auto_header && psf->write_header != NULL)
		psf->write_header (psf, SF_TRUE) ;

	return count ;
} /* sf_write_short */

sf_count_t
sf_writef_short	(SNDFILE *sndfile, const short *ptr, sf_count_t frames)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count ;

	if (frames == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (frames <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_READ)
	{	psf->error = SFE_NOT_WRITEMODE ;
		return 0 ;
		} ;

	if (psf->write_short == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return 0 ;
		} ;

	if (psf->last_op != SFM_WRITE)
		if (psf->seek (psf, SFM_WRITE, psf->write_current) < 0)
			return 0 ;

	if (psf->have_written == SF_FALSE && psf->write_header != NULL)
	{	if ((psf->error = psf->write_header (psf, SF_FALSE)))
			return 0 ;
		} ;
	psf->have_written = SF_TRUE ;

	count = psf->write_short (psf, ptr, frames * psf->sf.channels) ;

	psf->write_current += count / psf->sf.channels ;

	psf->last_op = SFM_WRITE ;

	if (psf->write_current > psf->sf.frames)
	{	psf->sf.frames = psf->write_current ;
		psf->dataend = 0 ;
		} ;

	if (psf->auto_header && psf->write_header != NULL)
		psf->write_header (psf, SF_TRUE) ;

	return count / psf->sf.channels ;
} /* sf_writef_short */

/*------------------------------------------------------------------------------
*/

sf_count_t
sf_write_int	(SNDFILE *sndfile, const int *ptr, sf_count_t len)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count ;

	if (len == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (len <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_READ)
	{	psf->error = SFE_NOT_WRITEMODE ;
		return 0 ;
		} ;

	if (len % psf->sf.channels)
	{	psf->error = SFE_BAD_WRITE_ALIGN ;
		return 0 ;
		} ;

	if (psf->write_int == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return 0 ;
		} ;

	if (psf->last_op != SFM_WRITE)
		if (psf->seek (psf, SFM_WRITE, psf->write_current) < 0)
			return 0 ;

	if (psf->have_written == SF_FALSE && psf->write_header != NULL)
	{	if ((psf->error = psf->write_header (psf, SF_FALSE)))
			return 0 ;
		} ;
	psf->have_written = SF_TRUE ;

	count = psf->write_int (psf, ptr, len) ;

	psf->write_current += count / psf->sf.channels ;

	psf->last_op = SFM_WRITE ;

	if (psf->write_current > psf->sf.frames)
	{	psf->sf.frames = psf->write_current ;
		psf->dataend = 0 ;
		} ;

	if (psf->auto_header && psf->write_header != NULL)
		psf->write_header (psf, SF_TRUE) ;

	return count ;
} /* sf_write_int */

sf_count_t
sf_writef_int	(SNDFILE *sndfile, const int *ptr, sf_count_t frames)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count ;

	if (frames == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (frames <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_READ)
	{	psf->error = SFE_NOT_WRITEMODE ;
		return 0 ;
		} ;

	if (psf->write_int == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return 0 ;
		} ;

	if (psf->last_op != SFM_WRITE)
		if (psf->seek (psf, SFM_WRITE, psf->write_current) < 0)
			return 0 ;

	if (psf->have_written == SF_FALSE && psf->write_header != NULL)
	{	if ((psf->error = psf->write_header (psf, SF_FALSE)))
			return 0 ;
		} ;
	psf->have_written = SF_TRUE ;

	count = psf->write_int (psf, ptr, frames * psf->sf.channels) ;

	psf->write_current += count / psf->sf.channels ;

	psf->last_op = SFM_WRITE ;

	if (psf->write_current > psf->sf.frames)
	{	psf->sf.frames = psf->write_current ;
		psf->dataend = 0 ;
		} ;

	if (psf->auto_header && psf->write_header != NULL)
		psf->write_header (psf, SF_TRUE) ;

	return count / psf->sf.channels ;
} /* sf_writef_int */

/*------------------------------------------------------------------------------
*/

sf_count_t
sf_write_float	(SNDFILE *sndfile, const float *ptr, sf_count_t len)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count ;

	if (len == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (len <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_READ)
	{	psf->error = SFE_NOT_WRITEMODE ;
		return 0 ;
		} ;

	if (len % psf->sf.channels)
	{	psf->error = SFE_BAD_WRITE_ALIGN ;
		return 0 ;
		} ;

	if (psf->write_float == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return 0 ;
		} ;

	if (psf->last_op != SFM_WRITE)
		if (psf->seek (psf, SFM_WRITE, psf->write_current) < 0)
			return 0 ;

	if (psf->have_written == SF_FALSE && psf->write_header != NULL)
	{	if ((psf->error = psf->write_header (psf, SF_FALSE)))
			return 0 ;
		} ;
	psf->have_written = SF_TRUE ;

	count = psf->write_float (psf, ptr, len) ;

	psf->write_current += count / psf->sf.channels ;

	psf->last_op = SFM_WRITE ;

	if (psf->write_current > psf->sf.frames)
	{	psf->sf.frames = psf->write_current ;
		psf->dataend = 0 ;
		} ;

	if (psf->auto_header && psf->write_header != NULL)
		psf->write_header (psf, SF_TRUE) ;

	return count ;
} /* sf_write_float */

sf_count_t
sf_writef_float	(SNDFILE *sndfile, const float *ptr, sf_count_t frames)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count ;

	if (frames == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (frames <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_READ)
	{	psf->error = SFE_NOT_WRITEMODE ;
		return 0 ;
		} ;

	if (psf->write_float == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return 0 ;
		} ;

	if (psf->last_op != SFM_WRITE)
		if (psf->seek (psf, SFM_WRITE, psf->write_current) < 0)
			return 0 ;

	if (psf->have_written == SF_FALSE && psf->write_header != NULL)
	{	if ((psf->error = psf->write_header (psf, SF_FALSE)))
			return 0 ;
		} ;
	psf->have_written = SF_TRUE ;

	count = psf->write_float (psf, ptr, frames * psf->sf.channels) ;

	psf->write_current += count / psf->sf.channels ;

	psf->last_op = SFM_WRITE ;

	if (psf->write_current > psf->sf.frames)
	{	psf->sf.frames = psf->write_current ;
		psf->dataend = 0 ;
		} ;

	if (psf->auto_header && psf->write_header != NULL)
		psf->write_header (psf, SF_TRUE) ;

	return count / psf->sf.channels ;
} /* sf_writef_float */

/*------------------------------------------------------------------------------
*/

sf_count_t
sf_write_double	(SNDFILE *sndfile, const double *ptr, sf_count_t len)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count ;

	if (len == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (len <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_READ)
	{	psf->error = SFE_NOT_WRITEMODE ;
		return 0 ;
		} ;

	if (len % psf->sf.channels)
	{	psf->error = SFE_BAD_WRITE_ALIGN ;
		return	0 ;
		} ;

	if (psf->write_double == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return 0 ;
		} ;

	if (psf->last_op != SFM_WRITE)
		if (psf->seek (psf, SFM_WRITE, psf->write_current) < 0)
			return 0 ;

	if (psf->have_written == SF_FALSE && psf->write_header != NULL)
	{	if ((psf->error = psf->write_header (psf, SF_FALSE)))
			return 0 ;
		} ;
	psf->have_written = SF_TRUE ;

	count = psf->write_double (psf, ptr, len) ;

	psf->write_current += count / psf->sf.channels ;

	psf->last_op = SFM_WRITE ;

	if (psf->write_current > psf->sf.frames)
	{	psf->sf.frames = psf->write_current ;
		psf->dataend = 0 ;
		} ;

	if (psf->auto_header && psf->write_header != NULL)
		psf->write_header (psf, SF_TRUE) ;

	return count ;
} /* sf_write_double */

sf_count_t
sf_writef_double	(SNDFILE *sndfile, const double *ptr, sf_count_t frames)
{	SF_PRIVATE 	*psf ;
	sf_count_t	count ;

	if (frames == 0)
		return 0 ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (frames <= 0)
	{	psf->error = SFE_NEGATIVE_RW_LEN ;
		return 0 ;
		} ;

	if (psf->file.mode == SFM_READ)
	{	psf->error = SFE_NOT_WRITEMODE ;
		return 0 ;
		} ;

	if (psf->write_double == NULL || psf->seek == NULL)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return 0 ;
		} ;

	if (psf->last_op != SFM_WRITE)
		if (psf->seek (psf, SFM_WRITE, psf->write_current) < 0)
			return 0 ;

	if (psf->have_written == SF_FALSE && psf->write_header != NULL)
	{	if ((psf->error = psf->write_header (psf, SF_FALSE)))
			return 0 ;
		} ;
	psf->have_written = SF_TRUE ;

	count = psf->write_double (psf, ptr, frames * psf->sf.channels) ;

	psf->write_current += count / psf->sf.channels ;

	psf->last_op = SFM_WRITE ;

	if (psf->write_current > psf->sf.frames)
	{	psf->sf.frames = psf->write_current ;
		psf->dataend = 0 ;
		} ;

	if (psf->auto_header && psf->write_header != NULL)
		psf->write_header (psf, SF_TRUE) ;

	return count / psf->sf.channels ;
} /* sf_writef_double */

/*=========================================================================
** Private functions.
*/

static int
try_resource_fork (SF_PRIVATE * psf)
{	int old_error = psf->error ;

	/* Set READ mode now, to see if resource fork exists. */
	psf->rsrc.mode = SFM_READ ;
	if (psf_open_rsrc (psf) != 0)
	{	psf->error = old_error ;
		return 0 ;
		} ;

	/* More checking here. */
	psf_log_printf (psf, "Resource fork : %s\n", psf->rsrc.path.c) ;

	return SF_FORMAT_SD2 ;
} /* try_resource_fork */

static int
format_from_extension (SF_PRIVATE *psf)
{	char *cptr ;
	char buffer [16] ;
	int format = 0 ;

	if ((cptr = strrchr (psf->file.name.c, '.')) == NULL)
		return 0 ;

	cptr ++ ;
	if (strlen (cptr) > sizeof (buffer) - 1)
		return 0 ;

	psf_strlcpy (buffer, sizeof (buffer), cptr) ;
	buffer [sizeof (buffer) - 1] = 0 ;

	/* Convert everything in the buffer to lower case. */
	cptr = buffer ;
	while (*cptr)
	{	*cptr = tolower (*cptr) ;
		cptr ++ ;
		} ;

	cptr = buffer ;

	if (strcmp (cptr, "au") == 0)
	{	psf->sf.channels = 1 ;
		psf->sf.samplerate = 8000 ;
		format = SF_FORMAT_RAW | SF_FORMAT_ULAW ;
		}
	else if (strcmp (cptr, "snd") == 0)
	{	psf->sf.channels = 1 ;
		psf->sf.samplerate = 8000 ;
		format = SF_FORMAT_RAW | SF_FORMAT_ULAW ;
		}

	else if (strcmp (cptr, "vox") == 0 || strcmp (cptr, "vox8") == 0)
	{	psf->sf.channels = 1 ;
		psf->sf.samplerate = 8000 ;
		format = SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM ;
		}
	else if (strcmp (cptr, "vox6") == 0)
	{	psf->sf.channels = 1 ;
		psf->sf.samplerate = 6000 ;
		format = SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM ;
		}
	else if (strcmp (cptr, "gsm") == 0)
	{	psf->sf.channels = 1 ;
		psf->sf.samplerate = 8000 ;
		format = SF_FORMAT_RAW | SF_FORMAT_GSM610 ;
		}

	/* For RAW files, make sure the dataoffset if set correctly. */
	if ((SF_CONTAINER (format)) == SF_FORMAT_RAW)
		psf->dataoffset = 0 ;

	return format ;
} /* format_from_extension */

static int
guess_file_type (SF_PRIVATE *psf)
{	uint32_t buffer [3], format ;

	if (psf_binheader_readf (psf, "b", &buffer, SIGNED_SIZEOF (buffer)) != SIGNED_SIZEOF (buffer))
	{	psf->error = SFE_BAD_FILE_READ ;
		return 0 ;
		} ;

	if ((buffer [0] == MAKE_MARKER ('R', 'I', 'F', 'F') || buffer [0] == MAKE_MARKER ('R', 'I', 'F', 'X'))
			&& buffer [2] == MAKE_MARKER ('W', 'A', 'V', 'E'))
		return SF_FORMAT_WAV ;

	if (buffer [0] == MAKE_MARKER ('F', 'O', 'R', 'M'))
	{	if (buffer [2] == MAKE_MARKER ('A', 'I', 'F', 'F') || buffer [2] == MAKE_MARKER ('A', 'I', 'F', 'C'))
			return SF_FORMAT_AIFF ;
		if (buffer [2] == MAKE_MARKER ('8', 'S', 'V', 'X') || buffer [2] == MAKE_MARKER ('1', '6', 'S', 'V'))
			return SF_FORMAT_SVX ;
		return 0 ;
		} ;

	if (buffer [0] == MAKE_MARKER ('.', 's', 'n', 'd') || buffer [0] == MAKE_MARKER ('d', 'n', 's', '.'))
		return SF_FORMAT_AU ;

	if ((buffer [0] == MAKE_MARKER ('f', 'a', 'p', ' ') || buffer [0] == MAKE_MARKER (' ', 'p', 'a', 'f')))
		return SF_FORMAT_PAF ;

	if (buffer [0] == MAKE_MARKER ('N', 'I', 'S', 'T'))
		return SF_FORMAT_NIST ;

	if (buffer [0] == MAKE_MARKER ('C', 'r', 'e', 'a') && buffer [1] == MAKE_MARKER ('t', 'i', 'v', 'e'))
		return SF_FORMAT_VOC ;

	if ((buffer [0] & MAKE_MARKER (0xFF, 0xFF, 0xF8, 0xFF)) == MAKE_MARKER (0x64, 0xA3, 0x00, 0x00) ||
		(buffer [0] & MAKE_MARKER (0xFF, 0xF8, 0xFF, 0xFF)) == MAKE_MARKER (0x00, 0x00, 0xA3, 0x64))
		return SF_FORMAT_IRCAM ;

	if (buffer [0] == MAKE_MARKER ('r', 'i', 'f', 'f'))
		return SF_FORMAT_W64 ;

	if (buffer [0] == MAKE_MARKER (0, 0, 0x03, 0xE8) && buffer [1] == MAKE_MARKER (0, 0, 0, 1) &&
								buffer [2] == MAKE_MARKER (0, 0, 0, 1))
		return SF_FORMAT_MAT4 ;

	if (buffer [0] == MAKE_MARKER (0, 0, 0, 0) && buffer [1] == MAKE_MARKER (1, 0, 0, 0) &&
								buffer [2] == MAKE_MARKER (1, 0, 0, 0))
		return SF_FORMAT_MAT4 ;

	if (buffer [0] == MAKE_MARKER ('M', 'A', 'T', 'L') && buffer [1] == MAKE_MARKER ('A', 'B', ' ', '5'))
		return SF_FORMAT_MAT5 ;

	if (buffer [0] == MAKE_MARKER ('P', 'V', 'F', '1'))
		return SF_FORMAT_PVF ;

	if (buffer [0] == MAKE_MARKER ('E', 'x', 't', 'e') && buffer [1] == MAKE_MARKER ('n', 'd', 'e', 'd') &&
								buffer [2] == MAKE_MARKER (' ', 'I', 'n', 's'))
		return SF_FORMAT_XI ;

	if (buffer [0] == MAKE_MARKER ('c', 'a', 'f', 'f') && buffer [2] == MAKE_MARKER ('d', 'e', 's', 'c'))
		return SF_FORMAT_CAF ;

	if (buffer [0] == MAKE_MARKER ('O', 'g', 'g', 'S'))
		return SF_FORMAT_OGG ;

	if (buffer [0] == MAKE_MARKER ('A', 'L', 'a', 'w') && buffer [1] == MAKE_MARKER ('S', 'o', 'u', 'n')
			&& buffer [2] == MAKE_MARKER ('d', 'F', 'i', 'l'))
		return SF_FORMAT_WVE ;

	if (buffer [0] == MAKE_MARKER ('D', 'i', 'a', 'm') && buffer [1] == MAKE_MARKER ('o', 'n', 'd', 'W')
			&& buffer [2] == MAKE_MARKER ('a', 'r', 'e', ' '))
		return SF_FORMAT_DWD ;

	if (buffer [0] == MAKE_MARKER ('L', 'M', '8', '9') || buffer [0] == MAKE_MARKER ('5', '3', 0, 0))
		return SF_FORMAT_TXW ;

	if ((buffer [0] & MAKE_MARKER (0xFF, 0xFF, 0x80, 0xFF)) == MAKE_MARKER (0xF0, 0x7E, 0, 0x01))
		return SF_FORMAT_SDS ;

	if ((buffer [0] & MAKE_MARKER (0xFF, 0xFF, 0, 0)) == MAKE_MARKER (1, 4, 0, 0))
		return SF_FORMAT_MPC2K ;

	if (buffer [0] == MAKE_MARKER ('C', 'A', 'T', ' ') && buffer [2] == MAKE_MARKER ('R', 'E', 'X', '2'))
		return SF_FORMAT_REX2 ;

	if (buffer [0] == MAKE_MARKER (0x30, 0x26, 0xB2, 0x75) && buffer [1] == MAKE_MARKER (0x8E, 0x66, 0xCF, 0x11))
		return 0 /*-SF_FORMAT_WMA-*/ ;

	/* HMM (Hidden Markov Model) Tool Kit. */
	if (buffer [2] == MAKE_MARKER (0, 2, 0, 0) && 2 * ((int64_t) BE2H_32 (buffer [0])) + 12 == psf->filelength)
		return SF_FORMAT_HTK ;

	if (buffer [0] == MAKE_MARKER ('f', 'L', 'a', 'C'))
		return SF_FORMAT_FLAC ;

	if (buffer [0] == MAKE_MARKER ('2', 'B', 'I', 'T'))
		return SF_FORMAT_AVR ;

	if (buffer [0] == MAKE_MARKER ('R', 'F', '6', '4') && buffer [2] == MAKE_MARKER ('W', 'A', 'V', 'E'))
		return SF_FORMAT_RF64 ;

	if (buffer [0] == MAKE_MARKER ('I', 'D', '3', 3))
	{	psf_log_printf (psf, "Found 'ID3' marker.\n") ;
		if (id3_skip (psf))
			return guess_file_type (psf) ;
		return 0 ;
		} ;

	/* Turtle Beach SMP 16-bit */
	if (buffer [0] == MAKE_MARKER ('S', 'O', 'U', 'N') && buffer [1] == MAKE_MARKER ('D', ' ', 'S', 'A'))
		return 0 ;

	/* Yamaha sampler format. */
	if (buffer [0] == MAKE_MARKER ('S', 'Y', '8', '0') || buffer [0] == MAKE_MARKER ('S', 'Y', '8', '5'))
		return 0 ;

	if (buffer [0] == MAKE_MARKER ('a', 'j', 'k', 'g'))
		return 0 /*-SF_FORMAT_SHN-*/ ;

	/* This must be the last one. */
	if (psf->filelength > 0 && (format = try_resource_fork (psf)) != 0)
		return format ;

	return 0 ;
} /* guess_file_type */


static int
validate_sfinfo (SF_INFO *sfinfo)
{	if (sfinfo->samplerate < 1)
		return 0 ;
	if (sfinfo->frames < 0)
		return 0 ;
	if (sfinfo->channels < 1)
		return 0 ;
	if ((SF_CONTAINER (sfinfo->format)) == 0)
		return 0 ;
	if ((SF_CODEC (sfinfo->format)) == 0)
		return 0 ;
	if (sfinfo->sections < 1)
		return 0 ;
	return 1 ;
} /* validate_sfinfo */

static int
validate_psf (SF_PRIVATE *psf)
{
	if (psf->datalength < 0)
	{	psf_log_printf (psf, "Invalid SF_PRIVATE field : datalength == %D.\n", psf->datalength) ;
		return 0 ;
		} ;
	if (psf->dataoffset < 0)
	{	psf_log_printf (psf, "Invalid SF_PRIVATE field : dataoffset == %D.\n", psf->dataoffset) ;
		return 0 ;
		} ;
	if (psf->blockwidth && psf->blockwidth != psf->sf.channels * psf->bytewidth)
	{	psf_log_printf (psf, "Invalid SF_PRIVATE field : channels * bytewidth == %d.\n",
								psf->sf.channels * psf->bytewidth) ;
		return 0 ;
		} ;
	return 1 ;
} /* validate_psf */

static void
save_header_info (SF_PRIVATE *psf)
{	snprintf (sf_parselog, sizeof (sf_parselog), "%s", psf->parselog.buf) ;
} /* save_header_info */

static int
copy_filename (SF_PRIVATE *psf, const char *path)
{	const char *ccptr ;
	char *cptr ;

	if (strlen (path) > 1 && strlen (path) - 1 >= sizeof (psf->file.path.c))
	{	psf->error = SFE_FILENAME_TOO_LONG ;
		return psf->error ;
		} ;

	snprintf (psf->file.path.c, sizeof (psf->file.path.c), "%s", path) ;
	if ((ccptr = strrchr (path, '/')) || (ccptr = strrchr (path, '\\')))
		ccptr ++ ;
	else
		ccptr = path ;

	snprintf (psf->file.name.c, sizeof (psf->file.name.c), "%s", ccptr) ;

	/* Now grab the directory. */
	snprintf (psf->file.dir.c, sizeof (psf->file.dir.c), "%s", path) ;
	if ((cptr = strrchr (psf->file.dir.c, '/')) || (cptr = strrchr (psf->file.dir.c, '\\')))
		cptr [1] = 0 ;
	else
		psf->file.dir.c [0] = 0 ;

	return 0 ;
} /* copy_filename */

/*==============================================================================
*/

static int
psf_close (SF_PRIVATE *psf)
{	uint32_t k ;
	int	error = 0 ;

	if (psf->codec_close)
	{	error = psf->codec_close (psf) ;
		/* To prevent it being called in psf->container_close(). */
		psf->codec_close = NULL ;
		} ;

	if (psf->container_close)
		error = psf->container_close (psf) ;

	error = psf_fclose (psf) ;
	psf_close_rsrc (psf) ;

	/* For an ISO C compliant implementation it is ok to free a NULL pointer. */
	free (psf->header.ptr) ;
	free (psf->container_data) ;
	free (psf->codec_data) ;
	free (psf->interleave) ;
	free (psf->dither) ;
	free (psf->peak_info) ;
	free (psf->broadcast_16k) ;
	free (psf->loop_info) ;
	free (psf->instrument) ;
	free (psf->cues) ;
	free (psf->channel_map) ;
	free (psf->format_desc) ;
	free (psf->strings.storage) ;

	if (psf->wchunks.chunks)
		for (k = 0 ; k < psf->wchunks.used ; k++)
			free (psf->wchunks.chunks [k].data) ;
	free (psf->rchunks.chunks) ;
	free (psf->wchunks.chunks) ;
	free (psf->iterator) ;
	free (psf->cart_16k) ;

	free (psf) ;

	return error ;
} /* psf_close */

SNDFILE *
psf_open_file (SF_PRIVATE *psf, SF_INFO *sfinfo)
{	int		error, format ;

	sf_errno = error = 0 ;
	sf_parselog [0] = 0 ;

	if (psf->error)
	{	error = psf->error ;
		goto error_exit ;
		} ;

	if (psf->file.mode != SFM_READ && psf->file.mode != SFM_WRITE && psf->file.mode != SFM_RDWR)
	{	error = SFE_BAD_OPEN_MODE ;
		goto error_exit ;
		} ;

	if (sfinfo == NULL)
	{	error = SFE_BAD_SF_INFO_PTR ;
		goto error_exit ;
		} ;

	if (psf->file.mode == SFM_READ)
	{	if ((SF_CONTAINER (sfinfo->format)) == SF_FORMAT_RAW)
		{	if (sf_format_check (sfinfo) == 0)
			{	error = SFE_RAW_BAD_FORMAT ;
				goto error_exit ;
				} ;
			}
		else
			memset (sfinfo, 0, sizeof (SF_INFO)) ;
		} ;

	memcpy (&psf->sf, sfinfo, sizeof (SF_INFO)) ;

	psf->Magick 		= SNDFILE_MAGICK ;
	psf->norm_float 	= SF_TRUE ;
	psf->norm_double	= SF_TRUE ;
	psf->dataoffset		= -1 ;
	psf->datalength		= -1 ;
	psf->read_current	= -1 ;
	psf->write_current	= -1 ;
	psf->auto_header 	= SF_FALSE ;
	psf->rwf_endian		= SF_ENDIAN_LITTLE ;
	psf->seek			= psf_default_seek ;
	psf->float_int_mult = 0 ;
	psf->float_max		= -1.0 ;

	/* An attempt at a per SF_PRIVATE unique id. */
	psf->unique_id		= psf_rand_int32 () ;

	psf->sf.sections = 1 ;

	psf->is_pipe = psf_is_pipe (psf) ;

	if (psf->is_pipe)
	{	psf->sf.seekable = SF_FALSE ;
		psf->filelength = SF_COUNT_MAX ;
		}
	else
	{	psf->sf.seekable = SF_TRUE ;

		/* File is open, so get the length. */
		psf->filelength = psf_get_filelen (psf) ;
		} ;

	if (psf->fileoffset > 0)
	{	switch (psf->file.mode)
		{	case SFM_READ :
				if (psf->filelength < 44)
				{	psf_log_printf (psf, "Short filelength: %D (fileoffset: %D)\n", psf->filelength, psf->fileoffset) ;
					error = SFE_BAD_OFFSET ;
					goto error_exit ;
					} ;
				break ;

			case SFM_WRITE :
				psf->fileoffset = 0 ;
				psf_fseek (psf, 0, SEEK_END) ;
				psf->fileoffset = psf_ftell (psf) ;
				break ;

			case SFM_RDWR :
				error = SFE_NO_EMBEDDED_RDWR ;
				goto error_exit ;
			} ;

		psf_log_printf (psf, "Embedded file offset : %D\n", psf->fileoffset) ;
		} ;

	if (psf->filelength == SF_COUNT_MAX)
		psf_log_printf (psf, "Length : unknown\n") ;
	else
		psf_log_printf (psf, "Length : %D\n", psf->filelength) ;

	if (psf->file.mode == SFM_WRITE || (psf->file.mode == SFM_RDWR && psf->filelength == 0))
	{	/* If the file is being opened for write or RDWR and the file is currently
		** empty, then the SF_INFO struct must contain valid data.
		*/
		if ((SF_CONTAINER (psf->sf.format)) == 0)
		{	error = SFE_ZERO_MAJOR_FORMAT ;
			goto error_exit ;
			} ;
		if ((SF_CODEC (psf->sf.format)) == 0)
		{	error = SFE_ZERO_MINOR_FORMAT ;
			goto error_exit ;
			} ;

		if (sf_format_check (&psf->sf) == 0)
		{	error = SFE_BAD_OPEN_FORMAT ;
			goto error_exit ;
			} ;
		}
	else if ((SF_CONTAINER (psf->sf.format)) != SF_FORMAT_RAW)
	{	/* If type RAW has not been specified then need to figure out file type. */
		psf->sf.format = guess_file_type (psf) ;

		if (psf->sf.format == 0)
			psf->sf.format = format_from_extension (psf) ;
		} ;

	/* Prevent unnecessary seeks */
	psf->last_op = psf->file.mode ;

	/* Set bytewidth if known. */
	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_PCM_S8 :
		case SF_FORMAT_PCM_U8 :
		case SF_FORMAT_ULAW :
		case SF_FORMAT_ALAW :
		case SF_FORMAT_DPCM_8 :
				psf->bytewidth = 1 ;
				break ;

		case SF_FORMAT_PCM_16 :
		case SF_FORMAT_DPCM_16 :
				psf->bytewidth = 2 ;
				break ;

		case SF_FORMAT_PCM_24 :
				psf->bytewidth = 3 ;
				break ;

		case SF_FORMAT_PCM_32 :
		case SF_FORMAT_FLOAT :
				psf->bytewidth = 4 ;
				break ;

		case SF_FORMAT_DOUBLE :
				psf->bytewidth = 8 ;
				break ;
		} ;

	/* Call the initialisation function for the relevant file type. */
	switch (SF_CONTAINER (psf->sf.format))
	{	case	SF_FORMAT_WAV :
		case	SF_FORMAT_WAVEX :
				error = wav_open (psf) ;
				break ;

		case	SF_FORMAT_AIFF :
				error = aiff_open (psf) ;
				break ;

		case	SF_FORMAT_AU :
				error = au_open (psf) ;
				break ;

		case	SF_FORMAT_RAW :
				error = raw_open (psf) ;
				break ;

		case	SF_FORMAT_W64 :
				error = w64_open (psf) ;
				break ;

		case	SF_FORMAT_RF64 :
				error = rf64_open (psf) ;
				break ;

		/* Lite remove start */
		case	SF_FORMAT_PAF :
				error = paf_open (psf) ;
				break ;

		case	SF_FORMAT_SVX :
				error = svx_open (psf) ;
				break ;

		case	SF_FORMAT_NIST :
				error = nist_open (psf) ;
				break ;

		case	SF_FORMAT_IRCAM :
				error = ircam_open (psf) ;
				break ;

		case	SF_FORMAT_VOC :
				error = voc_open (psf) ;
				break ;

		case	SF_FORMAT_SDS :
				error = sds_open (psf) ;
				break ;

		case	SF_FORMAT_OGG :
				error = ogg_open (psf) ;
				break ;

		case	SF_FORMAT_TXW :
				error = txw_open (psf) ;
				break ;

		case	SF_FORMAT_WVE :
				error = wve_open (psf) ;
				break ;

		case	SF_FORMAT_DWD :
				error = dwd_open (psf) ;
				break ;

		case	SF_FORMAT_MAT4 :
				error = mat4_open (psf) ;
				break ;

		case	SF_FORMAT_MAT5 :
				error = mat5_open (psf) ;
				break ;

		case	SF_FORMAT_PVF :
				error = pvf_open (psf) ;
				break ;

		case	SF_FORMAT_XI :
				error = xi_open (psf) ;
				break ;

		case	SF_FORMAT_HTK :
				error = htk_open (psf) ;
				break ;

		case	SF_FORMAT_SD2 :
				error = sd2_open (psf) ;
				break ;

		case	SF_FORMAT_REX2 :
				error = rx2_open (psf) ;
				break ;

		case	SF_FORMAT_AVR :
				error = avr_open (psf) ;
				break ;

		case	SF_FORMAT_FLAC :
				error = flac_open (psf) ;
				break ;

		case	SF_FORMAT_CAF :
				error = caf_open (psf) ;
				break ;

		case	SF_FORMAT_MPC2K :
				error = mpc2k_open (psf) ;
				break ;

		/* Lite remove end */

		default :
				error = SF_ERR_UNRECOGNISED_FORMAT ;
		} ;

	if (error)
		goto error_exit ;

	/* For now, check whether embedding is supported. */
	format = SF_CONTAINER (psf->sf.format) ;
	if (psf->fileoffset > 0)
	{	switch (format)
		{	case SF_FORMAT_WAV :
			case SF_FORMAT_WAVEX :
			case SF_FORMAT_AIFF :
			case SF_FORMAT_AU :
				/* Actual embedded files. */
				break ;

			case SF_FORMAT_FLAC :
				/* Flac with an ID3v2 header? */
				break ;

			default :
				error = SFE_NO_EMBED_SUPPORT ;
				goto error_exit ;
			} ;
		} ;

	if (psf->fileoffset > 0)
		psf_log_printf (psf, "Embedded file length : %D\n", psf->filelength) ;

	if (psf->file.mode == SFM_RDWR && sf_format_check (&psf->sf) == 0)
	{	error = SFE_BAD_MODE_RW ;
		goto error_exit ;
		} ;

	if (validate_sfinfo (&psf->sf) == 0)
	{	psf_log_SF_INFO (psf) ;
		save_header_info (psf) ;
		error = SFE_BAD_SF_INFO ;
		goto error_exit ;
		} ;

	if (validate_psf (psf) == 0)
	{	save_header_info (psf) ;
		error = SFE_INTERNAL ;
		goto error_exit ;
		} ;

	psf->read_current = 0 ;
	psf->write_current = 0 ;
	if (psf->file.mode == SFM_RDWR)
	{	psf->write_current = psf->sf.frames ;
		psf->have_written = psf->sf.frames > 0 ? SF_TRUE : SF_FALSE ;
		} ;

	memcpy (sfinfo, &psf->sf, sizeof (SF_INFO)) ;

	if (psf->file.mode == SFM_WRITE)
	{	/* Zero out these fields. */
		sfinfo->frames = 0 ;
		sfinfo->sections = 0 ;
		sfinfo->seekable = 0 ;
		} ;

	return (SNDFILE *) psf ;

error_exit :
	sf_errno = error ;

	if (error == SFE_SYSTEM)
		snprintf (sf_syserr, sizeof (sf_syserr), "%s", psf->syserr) ;
	snprintf (sf_parselog, sizeof (sf_parselog), "%s", psf->parselog.buf) ;

	switch (error)
	{	case SF_ERR_SYSTEM :
		case SF_ERR_UNSUPPORTED_ENCODING :
		case SFE_UNIMPLEMENTED :
			break ;

		case SFE_RAW_BAD_FORMAT :
			break ;

		default :
			if (psf->file.mode == SFM_READ)
			{	psf_log_printf (psf, "Parse error : %s\n", sf_error_number (error)) ;
				error = SF_ERR_MALFORMED_FILE ;
				} ;
		} ;

	psf_close (psf) ;
	return NULL ;
} /* psf_open_file */

/*==============================================================================
** Chunk getting and setting.
** This works for AIFF, CAF, RF64 and WAV.
** It doesn't work for W64 because W64 uses weird GUID style chunk markers.
*/

int
sf_set_chunk (SNDFILE * sndfile, const SF_CHUNK_INFO * chunk_info)
{	SF_PRIVATE 	*psf ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (chunk_info == NULL || chunk_info->data == NULL)
		return SFE_BAD_CHUNK_PTR ;

	if (psf->set_chunk)
		return psf->set_chunk (psf, chunk_info) ;

	return SFE_BAD_CHUNK_FORMAT ;
} /* sf_set_chunk */

SF_CHUNK_ITERATOR *
sf_get_chunk_iterator (SNDFILE * sndfile, const SF_CHUNK_INFO * chunk_info)
{	SF_PRIVATE 	*psf ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (chunk_info)
		return psf_get_chunk_iterator (psf, chunk_info->id) ;

	return psf_get_chunk_iterator (psf, NULL) ;
} /* sf_get_chunk_iterator */

SF_CHUNK_ITERATOR *
sf_next_chunk_iterator (SF_CHUNK_ITERATOR * iterator)
{	SF_PRIVATE 	*psf ;
	SNDFILE	*sndfile = iterator ? iterator->sndfile : NULL ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (psf->next_chunk_iterator)
		return psf->next_chunk_iterator (psf, iterator) ;

	return NULL ;
} /* sf_get_chunk_iterator_next */

int
sf_get_chunk_size (const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info)
{	SF_PRIVATE 	*psf ;
	SNDFILE	*sndfile = iterator ? iterator->sndfile : NULL ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (chunk_info == NULL)
		return SFE_BAD_CHUNK_PTR ;

	if (psf->get_chunk_size)
		return psf->get_chunk_size (psf, iterator, chunk_info) ;

	return SFE_BAD_CHUNK_FORMAT ;
} /* sf_get_chunk_size */

int
sf_get_chunk_data (const SF_CHUNK_ITERATOR * iterator, SF_CHUNK_INFO * chunk_info)
{	SF_PRIVATE	*psf ;
	SNDFILE	*sndfile = iterator ? iterator->sndfile : NULL ;

	VALIDATE_SNDFILE_AND_ASSIGN_PSF (sndfile, psf, 1) ;

	if (chunk_info == NULL || chunk_info->data == NULL)
		return SFE_BAD_CHUNK_PTR ;

	if (psf->get_chunk_data)
		return psf->get_chunk_data (psf, iterator, chunk_info) ;

	return SFE_BAD_CHUNK_FORMAT ;
} /* sf_get_chunk_data */
