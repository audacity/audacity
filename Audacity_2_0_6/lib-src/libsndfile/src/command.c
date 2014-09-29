/*
** Copyright (C) 2001-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	<stdio.h>
#include	<string.h>
#include	<math.h>

#include	"sndfile.h"
#include	"common.h"

static SF_FORMAT_INFO const simple_formats [] =
{
	{	SF_FORMAT_AIFF | SF_FORMAT_PCM_16,
		"AIFF (Apple/SGI 16 bit PCM)", "aiff"
		},

	{	SF_FORMAT_AIFF | SF_FORMAT_FLOAT,
		"AIFF (Apple/SGI 32 bit float)", "aifc"
		},

	{	SF_FORMAT_AIFF | SF_FORMAT_PCM_S8,
		"AIFF (Apple/SGI 8 bit PCM)", "aiff"
		},

	{	SF_FORMAT_AU | SF_FORMAT_PCM_16,
		"AU (Sun/Next 16 bit PCM)", "au"
		},

	{	SF_FORMAT_AU | SF_FORMAT_ULAW,
		"AU (Sun/Next 8-bit u-law)", "au"
		},

	{	SF_FORMAT_CAF | SF_FORMAT_PCM_16,
		"CAF (Apple 16 bit PCM)", "caf"
		},

#if HAVE_EXTERNAL_LIBS
	{	SF_FORMAT_FLAC | SF_FORMAT_PCM_16,
		"FLAC 16 bit", "flac"
		},
#endif

	{	SF_FORMAT_RAW | SF_FORMAT_VOX_ADPCM,
		"OKI Dialogic VOX ADPCM", "vox"
		},

#if HAVE_EXTERNAL_LIBS
	{	SF_FORMAT_OGG | SF_FORMAT_VORBIS,
		"Ogg Vorbis (Xiph Foundation)", "oga"
		},
#endif

	{	SF_FORMAT_WAV | SF_FORMAT_PCM_16,
		"WAV (Microsoft 16 bit PCM)", "wav"
		},

	{	SF_FORMAT_WAV | SF_FORMAT_FLOAT,
		"WAV (Microsoft 32 bit float)", "wav"
		},

	{	SF_FORMAT_WAV | SF_FORMAT_IMA_ADPCM,
		"WAV (Microsoft 4 bit IMA ADPCM)", "wav"
		},

	{	SF_FORMAT_WAV | SF_FORMAT_MS_ADPCM,
		"WAV (Microsoft 4 bit MS ADPCM)", "wav"
		},

	{	SF_FORMAT_WAV | SF_FORMAT_PCM_U8,
		"WAV (Microsoft 8 bit PCM)", "wav"
		},

} ; /* simple_formats */

int
psf_get_format_simple_count	(void)
{	return (sizeof (simple_formats) / sizeof (SF_FORMAT_INFO)) ;
} /* psf_get_format_simple_count */

int
psf_get_format_simple (SF_FORMAT_INFO *data)
{	int indx ;

	if (data->format < 0 || data->format >= (SIGNED_SIZEOF (simple_formats) / SIGNED_SIZEOF (SF_FORMAT_INFO)))
		return SFE_BAD_COMMAND_PARAM ;

	indx = data->format ;
	memcpy (data, &(simple_formats [indx]), SIGNED_SIZEOF (SF_FORMAT_INFO)) ;

	return 0 ;
} /* psf_get_format_simple */

/*============================================================================
** Major format info.
*/

static SF_FORMAT_INFO const major_formats [] =
{
	{	SF_FORMAT_AIFF,		"AIFF (Apple/SGI)",						"aiff" 	},
	{	SF_FORMAT_AU,		"AU (Sun/NeXT)", 						"au"	},
	{	SF_FORMAT_AVR,		"AVR (Audio Visual Research)",	 		"avr"	},
	{	SF_FORMAT_CAF,		"CAF (Apple Core Audio File)",	 		"caf"	},
#if HAVE_EXTERNAL_LIBS
	{	SF_FORMAT_FLAC,		"FLAC (FLAC Lossless Audio Codec)",	 	"flac"	},
#endif
	{	SF_FORMAT_HTK,		"HTK (HMM Tool Kit)",					"htk"	},
	{	SF_FORMAT_SVX,		"IFF (Amiga IFF/SVX8/SV16)",			"iff"	},
	{	SF_FORMAT_MAT4,		"MAT4 (GNU Octave 2.0 / Matlab 4.2)",	"mat"	},
	{	SF_FORMAT_MAT5,		"MAT5 (GNU Octave 2.1 / Matlab 5.0)",	"mat"	},
	{	SF_FORMAT_MPC2K,	"MPC (Akai MPC 2k)",					"mpc"	},
#if HAVE_EXTERNAL_LIBS
	{	SF_FORMAT_OGG,		"OGG (OGG Container format)",		 	"oga"	},
#endif
	{	SF_FORMAT_PAF,		"PAF (Ensoniq PARIS)", 					"paf"	},
	{	SF_FORMAT_PVF,		"PVF (Portable Voice Format)",			"pvf"	},
	{	SF_FORMAT_RAW,		"RAW (header-less)",				 	"raw"	},
	{	SF_FORMAT_RF64,		"RF64 (RIFF 64)",						"rf64"	},
	{	SF_FORMAT_SD2,		"SD2 (Sound Designer II)", 				"sd2"	},
	{	SF_FORMAT_SDS,		"SDS (Midi Sample Dump Standard)", 		"sds"	},
	{	SF_FORMAT_IRCAM,	"SF (Berkeley/IRCAM/CARL)",				"sf"	},
	{	SF_FORMAT_VOC,		"VOC (Creative Labs)",					"voc"	},
	{	SF_FORMAT_W64,		"W64 (SoundFoundry WAVE 64)",			"w64"	},
	{	SF_FORMAT_WAV,		"WAV (Microsoft)",						"wav"	},
	{	SF_FORMAT_NIST,		"WAV (NIST Sphere)",	 				"wav"	},
	{	SF_FORMAT_WAVEX,	"WAVEX (Microsoft)",					"wav"	},
	{	SF_FORMAT_WVE,		"WVE (Psion Series 3)",					"wve"	},
	{	SF_FORMAT_XI,		"XI (FastTracker 2)",					"xi"	},

} ; /* major_formats */

int
psf_get_format_major_count	(void)
{	return (sizeof (major_formats) / sizeof (SF_FORMAT_INFO)) ;
} /* psf_get_format_major_count */

int
psf_get_format_major (SF_FORMAT_INFO *data)
{	int indx ;

	if (data->format < 0 || data->format >= (SIGNED_SIZEOF (major_formats) / SIGNED_SIZEOF (SF_FORMAT_INFO)))
		return SFE_BAD_COMMAND_PARAM ;

	indx = data->format ;
	memcpy (data, &(major_formats [indx]), SIGNED_SIZEOF (SF_FORMAT_INFO)) ;

	return 0 ;
} /* psf_get_format_major */

/*============================================================================
** Subtype format info.
*/

static SF_FORMAT_INFO subtype_formats [] =
{
	{	SF_FORMAT_PCM_S8,		"Signed 8 bit PCM",		NULL 	},
	{	SF_FORMAT_PCM_16,		"Signed 16 bit PCM",	NULL 	},
	{	SF_FORMAT_PCM_24,		"Signed 24 bit PCM",	NULL 	},
	{	SF_FORMAT_PCM_32,		"Signed 32 bit PCM",	NULL 	},

	{	SF_FORMAT_PCM_U8,		"Unsigned 8 bit PCM",	NULL 	},

	{	SF_FORMAT_FLOAT,		"32 bit float",			NULL 	},
	{	SF_FORMAT_DOUBLE,		"64 bit float",			NULL 	},

	{	SF_FORMAT_ULAW,			"U-Law",				NULL 	},
	{	SF_FORMAT_ALAW,			"A-Law",				NULL 	},
	{	SF_FORMAT_IMA_ADPCM,	"IMA ADPCM",			NULL 	},
	{	SF_FORMAT_MS_ADPCM,		"Microsoft ADPCM",		NULL 	},

	{	SF_FORMAT_GSM610,		"GSM 6.10",				NULL 	},

	{	SF_FORMAT_G721_32,		"32kbs G721 ADPCM",		NULL 	},
	{	SF_FORMAT_G723_24,		"24kbs G723 ADPCM",		NULL 	},

	{	SF_FORMAT_DWVW_12,		"12 bit DWVW",			NULL 	},
	{	SF_FORMAT_DWVW_16,		"16 bit DWVW",			NULL 	},
	{	SF_FORMAT_DWVW_24,		"24 bit DWVW",			NULL 	},
	{	SF_FORMAT_VOX_ADPCM,	"VOX ADPCM",			"vox" 	},

	{	SF_FORMAT_DPCM_16,		"16 bit DPCM",			NULL 	},
	{	SF_FORMAT_DPCM_8,		"8 bit DPCM",			NULL 	},

#if HAVE_EXTERNAL_LIBS
	{	SF_FORMAT_VORBIS,		"Vorbis",				NULL 	},
#endif
} ; /* subtype_formats */

int
psf_get_format_subtype_count	(void)
{	return (sizeof (subtype_formats) / sizeof (SF_FORMAT_INFO)) ;
} /* psf_get_format_subtype_count */

int
psf_get_format_subtype (SF_FORMAT_INFO *data)
{	int indx ;

	if (data->format < 0 || data->format >= (SIGNED_SIZEOF (subtype_formats) / SIGNED_SIZEOF (SF_FORMAT_INFO)))
		return SFE_BAD_COMMAND_PARAM ;

	indx = data->format ;
	memcpy (data, &(subtype_formats [indx]), sizeof (SF_FORMAT_INFO)) ;

	return 0 ;
} /* psf_get_format_subtype */

/*==============================================================================
*/

int
psf_get_format_info (SF_FORMAT_INFO *data)
{	int k, format ;

	if (SF_CONTAINER (data->format))
	{	format = SF_CONTAINER (data->format) ;

		for (k = 0 ; k < (SIGNED_SIZEOF (major_formats) / SIGNED_SIZEOF (SF_FORMAT_INFO)) ; k++)
		{	if (format == major_formats [k].format)
			{	memcpy (data, &(major_formats [k]), sizeof (SF_FORMAT_INFO)) ;
				return 0 ;
				} ;
			} ;
		}
	else if (SF_CODEC (data->format))
	{	format = SF_CODEC (data->format) ;

		for (k = 0 ; k < (SIGNED_SIZEOF (subtype_formats) / SIGNED_SIZEOF (SF_FORMAT_INFO)) ; k++)
		{	if (format == subtype_formats [k].format)
			{	memcpy (data, &(subtype_formats [k]), sizeof (SF_FORMAT_INFO)) ;
				return 0 ;
				} ;
			} ;
		} ;

	memset (data, 0, sizeof (SF_FORMAT_INFO)) ;

	return SFE_BAD_COMMAND_PARAM ;
} /* psf_get_format_info */

/*==============================================================================
*/

double
psf_calc_signal_max (SF_PRIVATE *psf, int normalize)
{	sf_count_t	position ;
	double 		max_val, temp, *data ;
	int			k, len, readcount, save_state ;

	/* If the file is not seekable, there is nothing we can do. */
	if (! psf->sf.seekable)
	{	psf->error = SFE_NOT_SEEKABLE ;
		return	0.0 ;
		} ;

	if (! psf->read_double)
	{	psf->error = SFE_UNIMPLEMENTED ;
		return	0.0 ;
		} ;

	save_state = sf_command ((SNDFILE*) psf, SFC_GET_NORM_DOUBLE, NULL, 0) ;
	sf_command ((SNDFILE*) psf, SFC_SET_NORM_DOUBLE, NULL, normalize) ;

	/* Brute force. Read the whole file and find the biggest sample. */
	/* Get current position in file */
	position = sf_seek ((SNDFILE*) psf, 0, SEEK_CUR) ;
	/* Go to start of file. */
	sf_seek ((SNDFILE*) psf, 0, SEEK_SET) ;

	data = psf->u.dbuf ;
	len = ARRAY_LEN (psf->u.dbuf) ;

	for (readcount = 1, max_val = 0.0 ; readcount > 0 ; /* nothing */)
	{	readcount = sf_read_double ((SNDFILE*) psf, data, len) ;
		for (k = 0 ; k < readcount ; k++)
		{	temp = fabs (data [k]) ;
			max_val = temp > max_val ? temp : max_val ;
			} ;
		} ;

	/* Return to SNDFILE to original state. */
	sf_seek ((SNDFILE*) psf, position, SEEK_SET) ;
	sf_command ((SNDFILE*) psf, SFC_SET_NORM_DOUBLE, NULL, save_state) ;

	return	max_val ;
} /* psf_calc_signal_max */

int
psf_calc_max_all_channels (SF_PRIVATE *psf, double *peaks, int normalize)
{	sf_count_t	position ;
	double 		temp, *data ;
	int			k, len, readcount, save_state ;
	int			chan ;

	/* If the file is not seekable, there is nothing we can do. */
	if (! psf->sf.seekable)
		return (psf->error = SFE_NOT_SEEKABLE) ;

	if (! psf->read_double)
		return (psf->error = SFE_UNIMPLEMENTED) ;

	save_state = sf_command ((SNDFILE*) psf, SFC_GET_NORM_DOUBLE, NULL, 0) ;
	sf_command ((SNDFILE*) psf, SFC_SET_NORM_DOUBLE, NULL, normalize) ;

	memset (peaks, 0, sizeof (double) * psf->sf.channels) ;

	/* Brute force. Read the whole file and find the biggest sample for each channel. */
	position = sf_seek ((SNDFILE*) psf, 0, SEEK_CUR) ; /* Get current position in file */
	sf_seek ((SNDFILE*) psf, 0, SEEK_SET) ;			/* Go to start of file. */

	len = ARRAY_LEN (psf->u.dbuf) ;

	data = psf->u.dbuf ;

	chan = 0 ;
	readcount = len ;
	while (readcount > 0)
	{	readcount = sf_read_double ((SNDFILE*) psf, data, len) ;
		for (k = 0 ; k < readcount ; k++)
		{	temp = fabs (data [k]) ;
			peaks [chan] = temp > peaks [chan] ? temp : peaks [chan] ;
			chan = (chan + 1) % psf->sf.channels ;
			} ;
		} ;

	sf_seek ((SNDFILE*) psf, position, SEEK_SET) ;		/* Return to original position. */

	sf_command ((SNDFILE*) psf, SFC_SET_NORM_DOUBLE, NULL, save_state) ;

	return	0 ;
} /* psf_calc_max_all_channels */

int
psf_get_signal_max (SF_PRIVATE *psf, double *peak)
{	int k ;

	if (psf->peak_info == NULL)
		return SF_FALSE ;

	peak [0] = psf->peak_info->peaks [0].value ;

	for (k = 1 ; k < psf->sf.channels ; k++)
		peak [0] = SF_MAX (peak [0], psf->peak_info->peaks [k].value) ;

	return SF_TRUE ;
} /* psf_get_signal_max */

int
psf_get_max_all_channels (SF_PRIVATE *psf, double *peaks)
{	int k ;

	if (psf->peak_info == NULL)
		return SF_FALSE ;

	for (k = 0 ; k < psf->sf.channels ; k++)
		peaks [k] = psf->peak_info->peaks [k].value ;

	return SF_TRUE ;
} /* psf_get_max_all_channels */


