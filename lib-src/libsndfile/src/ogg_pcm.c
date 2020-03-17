/*
** Copyright (C) 2008-2016 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software ; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation ; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY ; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program ; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/


#include "sfconfig.h"

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <math.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#else
#include "sf_unistd.h"
#endif

#include "sndfile.h"
#include "sfendian.h"
#include "common.h"

#if (ENABLE_EXPERIMENTAL_CODE && HAVE_EXTERNAL_XIPH_LIBS)

#include <ogg/ogg.h>

#include "ogg.h"

typedef struct
{	int32_t serialno ;


	void * state ;
} OPCM_PRIVATE ;

static int	opcm_read_header (SF_PRIVATE * psf) ;
static int	opcm_close (SF_PRIVATE *psf) ;

int
ogg_pcm_open (SF_PRIVATE *psf)
{	OGG_PRIVATE* odata = psf->container_data ;
	OPCM_PRIVATE* opcm = calloc (1, sizeof (OPCM_PRIVATE)) ;
	int	error = 0 ;

	if (odata == NULL)
	{	psf_log_printf (psf, "%s : odata is NULL???\n", __func__) ;
		return SFE_INTERNAL ;
		} ;

	psf->codec_data = opcm ;
	if (opcm == NULL)
		return SFE_MALLOC_FAILED ;

	if (psf->file.mode == SFM_RDWR)
		return SFE_BAD_MODE_RW ;

	if (psf->file.mode == SFM_READ)
	{	/* Call this here so it only gets called once, so no memory is leaked. */
		ogg_sync_init (&odata->osync) ;

		if ((error = opcm_read_header (psf)))
			return error ;

#if 0
		psf->read_short		= opcm_read_s ;
		psf->read_int		= opcm_read_i ;
		psf->read_float		= opcm_read_f ;
		psf->read_double	= opcm_read_d ;
		psf->sf.frames		= opcm_length (psf) ;
#endif
		} ;

	psf->codec_close = opcm_close ;

	if (psf->file.mode == SFM_WRITE)
	{
#if 0
		/* Set the default opcm quality here. */
		vdata->quality = 0.4 ;

		psf->write_header	= opcm_write_header ;
		psf->write_short	= opcm_write_s ;
		psf->write_int		= opcm_write_i ;
		psf->write_float	= opcm_write_f ;
		psf->write_double	= opcm_write_d ;
#endif

		psf->sf.frames = SF_COUNT_MAX ; /* Unknown really */
		psf->strings.flags = SF_STR_ALLOW_START ;
		} ;

	psf->bytewidth = 1 ;
	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

#if 0
	psf->seek = opcm_seek ;
	psf->command = opcm_command ;
#endif

	/* FIXME, FIXME, FIXME : Hack these here for now and correct later. */
	psf->sf.format = SF_FORMAT_OGG | SF_FORMAT_SPEEX ;
	psf->sf.sections = 1 ;

	psf->datalength = 1 ;
	psf->dataoffset = 0 ;
	/* End FIXME. */

	return error ;
} /* ogg_pcm_open */

static int
opcm_read_header (SF_PRIVATE * UNUSED (psf))
{
	return 0 ;
} /* opcm_read_header */

static int
opcm_close (SF_PRIVATE * UNUSED (psf))
{


	return 0 ;
} /* opcm_close */



/*
encoded_speex_frames = (frames_per_packet * Packets)
                     = 1 * 272
                     = 272

audio_samples = encoded_speex_frames * frame_size
              = 272 * 640
              = 174080

duration = audio_samples / rate
         = 174080 / 44100
         = 3.947
*/

#else /* ENABLE_EXPERIMENTAL_CODE && HAVE_EXTERNAL_XIPH_LIBS */

int
ogg_pcm_open (SF_PRIVATE *psf)
{
	psf_log_printf (psf, "This version of libsndfile was compiled without Ogg/Speex support.\n") ;
	return SFE_UNIMPLEMENTED ;
} /* ogg_pcm_open */

#endif
