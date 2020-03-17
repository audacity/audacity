/*
** Copyright (C) 1999-2018 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#else
#include "sf_unistd.h"
#endif

#include <sndfile.h>

#include "common.h"

#if HAVE_ALSA_ASOUNDLIB_H
	#define ALSA_PCM_NEW_HW_PARAMS_API
	#define ALSA_PCM_NEW_SW_PARAMS_API
	#include <alsa/asoundlib.h>
	#include <sys/time.h>
#endif

#if defined (__ANDROID__)

#elif defined (__linux__) || defined (__FreeBSD_kernel__) || defined (__FreeBSD__)
	#include 	<fcntl.h>
	#include 	<sys/ioctl.h>
	#include 	<sys/soundcard.h>

#elif HAVE_SNDIO_H
	#include <sndio.h>

#elif (defined (sun) && defined (unix))
	#include <fcntl.h>
	#include <sys/ioctl.h>
	#include <sys/audioio.h>

#elif (OS_IS_WIN32 == 1)
	#include <windows.h>
	#include <mmsystem.h>

#endif

#define	SIGNED_SIZEOF(x)	((int) sizeof (x))
#define	BUFFER_LEN			(2048)

/*------------------------------------------------------------------------------
**	Linux/OSS functions for playing a sound.
*/

#if HAVE_ALSA_ASOUNDLIB_H

static snd_pcm_t * alsa_open (int channels, unsigned srate, int realtime) ;
static int alsa_write_float (snd_pcm_t *alsa_dev, float *data, int frames, int channels) ;

static void
alsa_play (int argc, char *argv [])
{	static float buffer [BUFFER_LEN] ;
	SNDFILE *sndfile ;
	SF_INFO sfinfo ;
	snd_pcm_t * alsa_dev ;
	int		k, readcount, subformat ;

	for (k = 1 ; k < argc ; k++)
	{	memset (&sfinfo, 0, sizeof (sfinfo)) ;

		printf ("Playing %s\n", argv [k]) ;
		if (! (sndfile = sf_open (argv [k], SFM_READ, &sfinfo)))
		{	puts (sf_strerror (NULL)) ;
			continue ;
			} ;

		if (sfinfo.channels < 1 || sfinfo.channels > 2)
		{	printf ("Error : channels = %d.\n", sfinfo.channels) ;
			continue ;
			} ;

		if ((alsa_dev = alsa_open (sfinfo.channels, (unsigned) sfinfo.samplerate, SF_FALSE)) == NULL)
			continue ;

		subformat = sfinfo.format & SF_FORMAT_SUBMASK ;

		if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE)
		{	double	scale ;
			int 	m ;

			sf_command (sndfile, SFC_CALC_SIGNAL_MAX, &scale, sizeof (scale)) ;
			if (scale > 1.0)
				scale = 1.0 / scale ;
			else
				scale = 1.0 ;

			while ((readcount = sf_read_float (sndfile, buffer, BUFFER_LEN)))
			{	for (m = 0 ; m < readcount ; m++)
					buffer [m] *= scale ;
				alsa_write_float (alsa_dev, buffer, BUFFER_LEN / sfinfo.channels, sfinfo.channels) ;
				} ;
			}
		else
		{	while ((readcount = sf_read_float (sndfile, buffer, BUFFER_LEN)))
				alsa_write_float (alsa_dev, buffer, BUFFER_LEN / sfinfo.channels, sfinfo.channels) ;
			} ;

		snd_pcm_drain (alsa_dev) ;
		snd_pcm_close (alsa_dev) ;

		sf_close (sndfile) ;
		} ;

	return ;
} /* alsa_play */

static snd_pcm_t *
alsa_open (int channels, unsigned samplerate, int realtime)
{	const char * device = "default" ;
	snd_pcm_t *alsa_dev = NULL ;
	snd_pcm_hw_params_t *hw_params ;
	snd_pcm_uframes_t buffer_size ;
	snd_pcm_uframes_t alsa_period_size, alsa_buffer_frames ;
	snd_pcm_sw_params_t *sw_params ;

	int err ;

	if (realtime)
	{	alsa_period_size = 256 ;
		alsa_buffer_frames = 3 * alsa_period_size ;
		}
	else
	{	alsa_period_size = 1024 ;
		alsa_buffer_frames = 4 * alsa_period_size ;
		} ;

	if ((err = snd_pcm_open (&alsa_dev, device, SND_PCM_STREAM_PLAYBACK, 0)) < 0)
	{	fprintf (stderr, "cannot open audio device \"%s\" (%s)\n", device, snd_strerror (err)) ;
		goto catch_error ;
		} ;

	snd_pcm_nonblock (alsa_dev, 0) ;

	if ((err = snd_pcm_hw_params_malloc (&hw_params)) < 0)
	{	fprintf (stderr, "cannot allocate hardware parameter structure (%s)\n", snd_strerror (err)) ;
		goto catch_error ;
		} ;

	if ((err = snd_pcm_hw_params_any (alsa_dev, hw_params)) < 0)
	{	fprintf (stderr, "cannot initialize hardware parameter structure (%s)\n", snd_strerror (err)) ;
		goto catch_error ;
		} ;

	if ((err = snd_pcm_hw_params_set_access (alsa_dev, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED)) < 0)
	{	fprintf (stderr, "cannot set access type (%s)\n", snd_strerror (err)) ;
		goto catch_error ;
		} ;

	if ((err = snd_pcm_hw_params_set_format (alsa_dev, hw_params, SND_PCM_FORMAT_FLOAT)) < 0)
	{	fprintf (stderr, "cannot set sample format (%s)\n", snd_strerror (err)) ;
		goto catch_error ;
		} ;

	if ((err = snd_pcm_hw_params_set_rate_near (alsa_dev, hw_params, &samplerate, 0)) < 0)
	{	fprintf (stderr, "cannot set sample rate (%s)\n", snd_strerror (err)) ;
		goto catch_error ;
		} ;

	if ((err = snd_pcm_hw_params_set_channels (alsa_dev, hw_params, channels)) < 0)
	{	fprintf (stderr, "cannot set channel count (%s)\n", snd_strerror (err)) ;
		goto catch_error ;
		} ;

	if ((err = snd_pcm_hw_params_set_buffer_size_near (alsa_dev, hw_params, &alsa_buffer_frames)) < 0)
	{	fprintf (stderr, "cannot set buffer size (%s)\n", snd_strerror (err)) ;
		goto catch_error ;
		} ;

	if ((err = snd_pcm_hw_params_set_period_size_near (alsa_dev, hw_params, &alsa_period_size, 0)) < 0)
	{	fprintf (stderr, "cannot set period size (%s)\n", snd_strerror (err)) ;
		goto catch_error ;
		} ;

	if ((err = snd_pcm_hw_params (alsa_dev, hw_params)) < 0)
	{	fprintf (stderr, "cannot set parameters (%s)\n", snd_strerror (err)) ;
		goto catch_error ;
		} ;

	/* extra check: if we have only one period, this code won't work */
	snd_pcm_hw_params_get_period_size (hw_params, &alsa_period_size, 0) ;
	snd_pcm_hw_params_get_buffer_size (hw_params, &buffer_size) ;
	if (alsa_period_size == buffer_size)
	{	fprintf (stderr, "Can't use period equal to buffer size (%lu == %lu)", alsa_period_size, buffer_size) ;
		goto catch_error ;
		} ;

	snd_pcm_hw_params_free (hw_params) ;

	if ((err = snd_pcm_sw_params_malloc (&sw_params)) != 0)
	{	fprintf (stderr, "%s: snd_pcm_sw_params_malloc: %s", __func__, snd_strerror (err)) ;
		goto catch_error ;
		} ;

	if ((err = snd_pcm_sw_params_current (alsa_dev, sw_params)) != 0)
	{	fprintf (stderr, "%s: snd_pcm_sw_params_current: %s", __func__, snd_strerror (err)) ;
		goto catch_error ;
		} ;

	/* note: set start threshold to delay start until the ring buffer is full */
	snd_pcm_sw_params_current (alsa_dev, sw_params) ;

	if ((err = snd_pcm_sw_params_set_start_threshold (alsa_dev, sw_params, buffer_size)) < 0)
	{	fprintf (stderr, "cannot set start threshold (%s)\n", snd_strerror (err)) ;
		goto catch_error ;
		} ;

	if ((err = snd_pcm_sw_params (alsa_dev, sw_params)) != 0)
	{	fprintf (stderr, "%s: snd_pcm_sw_params: %s", __func__, snd_strerror (err)) ;
		goto catch_error ;
		} ;

	snd_pcm_sw_params_free (sw_params) ;

	snd_pcm_reset (alsa_dev) ;

catch_error :

	if (err < 0 && alsa_dev != NULL)
	{	snd_pcm_close (alsa_dev) ;
		return NULL ;
		} ;

	return alsa_dev ;
} /* alsa_open */

static int
alsa_write_float (snd_pcm_t *alsa_dev, float *data, int frames, int channels)
{	static	int epipe_count = 0 ;

	int total = 0 ;
	int retval ;

	if (epipe_count > 0)
		epipe_count -- ;

	while (total < frames)
	{	retval = snd_pcm_writei (alsa_dev, data + total * channels, frames - total) ;

		if (retval >= 0)
		{	total += retval ;
			if (total == frames)
				return total ;

			continue ;
			} ;

		switch (retval)
		{	case -EAGAIN :
					puts ("alsa_write_float: EAGAIN") ;
					continue ;
					break ;

			case -EPIPE :
					if (epipe_count > 0)
					{	printf ("alsa_write_float: EPIPE %d\n", epipe_count) ;
						if (epipe_count > 140)
							return retval ;
						} ;
					epipe_count += 100 ;

#if 0
					if (0)
					{	snd_pcm_status_t *status ;

						snd_pcm_status_alloca (&status) ;
						if ((retval = snd_pcm_status (alsa_dev, status)) < 0)
							fprintf (stderr, "alsa_out: xrun. can't determine length\n") ;
						else if (snd_pcm_status_get_state (status) == SND_PCM_STATE_XRUN)
						{	struct timeval now, diff, tstamp ;

							gettimeofday (&now, 0) ;
							snd_pcm_status_get_trigger_tstamp (status, &tstamp) ;
							timersub (&now, &tstamp, &diff) ;

							fprintf (stderr, "alsa_write_float xrun: of at least %.3f msecs. resetting stream\n",
									diff.tv_sec * 1000 + diff.tv_usec / 1000.0) ;
							}
						else
							fprintf (stderr, "alsa_write_float: xrun. can't determine length\n") ;
						} ;
#endif

					snd_pcm_prepare (alsa_dev) ;
					break ;

			case -EBADFD :
					fprintf (stderr, "alsa_write_float: Bad PCM state.n") ;
					return 0 ;
					break ;

			case -ESTRPIPE :
					fprintf (stderr, "alsa_write_float: Suspend event.n") ;
					return 0 ;
					break ;

			case -EIO :
					puts ("alsa_write_float: EIO") ;
					return 0 ;

			default :
					fprintf (stderr, "alsa_write_float: retval = %d\n", retval) ;
					return 0 ;
					break ;
			} ; /* switch */
		} ; /* while */

	return total ;
} /* alsa_write_float */

#endif /* HAVE_ALSA_ASOUNDLIB_H */

/*------------------------------------------------------------------------------
**	Linux/OSS functions for playing a sound.
*/

#if !defined (__ANDROID__) && (defined (__linux__) || defined (__FreeBSD_kernel__) || defined (__FreeBSD__))

static	int	opensoundsys_open_device (int channels, int srate) ;

static int
opensoundsys_play (int argc, char *argv [])
{	static short buffer [BUFFER_LEN] ;
	SNDFILE *sndfile ;
	SF_INFO sfinfo ;
	int		k, audio_device, readcount, writecount, subformat ;

	for (k = 1 ; k < argc ; k++)
	{	memset (&sfinfo, 0, sizeof (sfinfo)) ;

		printf ("Playing %s\n", argv [k]) ;
		if (! (sndfile = sf_open (argv [k], SFM_READ, &sfinfo)))
		{	puts (sf_strerror (NULL)) ;
			continue ;
			} ;

		if (sfinfo.channels < 1 || sfinfo.channels > 2)
		{	printf ("Error : channels = %d.\n", sfinfo.channels) ;
			continue ;
			} ;

		audio_device = opensoundsys_open_device (sfinfo.channels, sfinfo.samplerate) ;

		subformat = sfinfo.format & SF_FORMAT_SUBMASK ;

		if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE)
		{	static float float_buffer [BUFFER_LEN] ;
			double	scale ;
			int 	m ;

			sf_command (sndfile, SFC_CALC_SIGNAL_MAX, &scale, sizeof (scale)) ;
			if (scale < 1e-10)
				scale = 1.0 ;
			else
				scale = 32700.0 / scale ;

			while ((readcount = sf_read_float (sndfile, float_buffer, BUFFER_LEN)))
			{	for (m = 0 ; m < readcount ; m++)
					buffer [m] = scale * float_buffer [m] ;
				writecount = write (audio_device, buffer, readcount * sizeof (short)) ;
				} ;
			}
		else
		{	while ((readcount = sf_read_short (sndfile, buffer, BUFFER_LEN)))
				writecount = write (audio_device, buffer, readcount * sizeof (short)) ;
			} ;

		if (ioctl (audio_device, SNDCTL_DSP_POST, 0) == -1)
			perror ("ioctl (SNDCTL_DSP_POST) ") ;

		if (ioctl (audio_device, SNDCTL_DSP_SYNC, 0) == -1)
			perror ("ioctl (SNDCTL_DSP_SYNC) ") ;

		close (audio_device) ;

		sf_close (sndfile) ;
		} ;

	return writecount ;
} /* opensoundsys_play */

static int
opensoundsys_open_device (int channels, int srate)
{	int fd, stereo, fmt ;

	if ((fd = open ("/dev/dsp", O_WRONLY, 0)) == -1 &&
		(fd = open ("/dev/sound/dsp", O_WRONLY, 0)) == -1)
	{	perror ("opensoundsys_open_device : open ") ;
		exit (1) ;
		} ;

	stereo = 0 ;
	if (ioctl (fd, SNDCTL_DSP_STEREO, &stereo) == -1)
	{ 	/* Fatal error */
		perror ("opensoundsys_open_device : stereo ") ;
		exit (1) ;
		} ;

	if (ioctl (fd, SNDCTL_DSP_RESET, 0))
	{	perror ("opensoundsys_open_device : reset ") ;
		exit (1) ;
		} ;

	fmt = CPU_IS_BIG_ENDIAN ? AFMT_S16_BE : AFMT_S16_LE ;
	if (ioctl (fd, SNDCTL_DSP_SETFMT, &fmt) != 0)
	{	perror ("opensoundsys_open_device : set format ") ;
		exit (1) ;
		} ;

	if (ioctl (fd, SNDCTL_DSP_CHANNELS, &channels) != 0)
	{	perror ("opensoundsys_open_device : channels ") ;
		exit (1) ;
		} ;

	if (ioctl (fd, SNDCTL_DSP_SPEED, &srate) != 0)
	{	perror ("opensoundsys_open_device : sample rate ") ;
		exit (1) ;
		} ;

	if (ioctl (fd, SNDCTL_DSP_SYNC, 0) != 0)
	{	perror ("opensoundsys_open_device : sync ") ;
		exit (1) ;
		} ;

	return 	fd ;
} /* opensoundsys_open_device */

#endif /* __linux__ */

/*------------------------------------------------------------------------------
**	Mac OS X functions for playing a sound.
*/

/* MacOSX 10.8 use a new Audio API. Someone needs to write some code for it. */

/*------------------------------------------------------------------------------
**	Win32 functions for playing a sound.
**
**	This API sucks. Its needlessly complicated and is *WAY* too loose with
**	passing pointers around in integers and using char* pointers to
**  point to data instead of short*. It plain sucks!
*/

#if (OS_IS_WIN32 == 1)

#define	WIN32_BUFFER_LEN	(1 << 15)

typedef struct
{	HWAVEOUT	hwave ;
	WAVEHDR		whdr [2] ;

	CRITICAL_SECTION	mutex ;		/* to control access to BuffersInUSe */
	HANDLE		Event ;			/* signal that a buffer is free */

	short		buffer [WIN32_BUFFER_LEN / sizeof (short)] ;
	int			current, bufferlen ;
	int			BuffersInUse ;

	SNDFILE 	*sndfile ;
	SF_INFO 	sfinfo ;

	sf_count_t	remaining ;
} Win32_Audio_Data ;


static void
win32_play_data (Win32_Audio_Data *audio_data)
{	int thisread, readcount ;

	/* fill a buffer if there is more data and we can read it sucessfully */
	readcount = (audio_data->remaining > audio_data->bufferlen) ? audio_data->bufferlen : (int) audio_data->remaining ;

	thisread = (int) sf_read_short (audio_data->sndfile, (short *) (audio_data->whdr [audio_data->current].lpData), readcount) ;

	audio_data->remaining -= thisread ;

	if (thisread > 0)
	{	/* Fix buffer length if this is only a partial block. */
		if (thisread < audio_data->bufferlen)
			audio_data->whdr [audio_data->current].dwBufferLength = thisread * sizeof (short) ;

		/* Queue the WAVEHDR */
		waveOutWrite (audio_data->hwave, (LPWAVEHDR) &(audio_data->whdr [audio_data->current]), sizeof (WAVEHDR)) ;

		/* count another buffer in use */
		EnterCriticalSection (&audio_data->mutex) ;
		audio_data->BuffersInUse ++ ;
		LeaveCriticalSection (&audio_data->mutex) ;

		/* use the other buffer next time */
		audio_data->current = (audio_data->current + 1) % 2 ;
		} ;

	return ;
} /* win32_play_data */

static void CALLBACK
win32_audio_out_callback (HWAVEOUT hwave, UINT msg, DWORD_PTR data, DWORD param1, DWORD param2)
{	Win32_Audio_Data	*audio_data ;

	/* Prevent compiler warnings. */
	(void) hwave ;
	(void) param1 ;
	(void) param2 ;

	if (data == 0)
		return ;

	/*
	** I consider this technique of passing a pointer via an integer as
	** fundamentally broken but thats the way microsoft has defined the
	** interface.
	*/
	audio_data = (Win32_Audio_Data*) data ;

	/* let main loop know a buffer is free */
	if (msg == MM_WOM_DONE)
	{	EnterCriticalSection (&audio_data->mutex) ;
		audio_data->BuffersInUse -- ;
		LeaveCriticalSection (&audio_data->mutex) ;
		SetEvent (audio_data->Event) ;
		} ;

	return ;
} /* win32_audio_out_callback */

static void
win32_play (int argc, char *argv [])
{	Win32_Audio_Data	audio_data ;

	WAVEFORMATEX wf ;
	int	k, error ;

	audio_data.sndfile = NULL ;
	audio_data.hwave = 0 ;

	for (k = 1 ; k < argc ; k++)
	{	printf ("Playing %s\n", argv [k]) ;

		if (! (audio_data.sndfile = sf_open (argv [k], SFM_READ, &(audio_data.sfinfo))))
		{	puts (sf_strerror (NULL)) ;
			continue ;
			} ;

		audio_data.remaining = audio_data.sfinfo.frames * audio_data.sfinfo.channels ;
		audio_data.current = 0 ;

		InitializeCriticalSection (&audio_data.mutex) ;
		audio_data.Event = CreateEvent (0, FALSE, FALSE, 0) ;

		wf.nChannels = audio_data.sfinfo.channels ;
		wf.wFormatTag = WAVE_FORMAT_PCM ;
		wf.cbSize = 0 ;
		wf.wBitsPerSample = 16 ;

		wf.nSamplesPerSec = audio_data.sfinfo.samplerate ;

		wf.nBlockAlign = audio_data.sfinfo.channels * sizeof (short) ;

		wf.nAvgBytesPerSec = wf.nBlockAlign * wf.nSamplesPerSec ;

		error = waveOutOpen (&(audio_data.hwave), WAVE_MAPPER, &wf, (DWORD_PTR) win32_audio_out_callback,
							(DWORD_PTR) &audio_data, CALLBACK_FUNCTION) ;
		if (error)
		{	puts ("waveOutOpen failed.") ;
			audio_data.hwave = 0 ;
			continue ;
			} ;

		audio_data.whdr [0].lpData = (char*) audio_data.buffer ;
		audio_data.whdr [1].lpData = ((char*) audio_data.buffer) + sizeof (audio_data.buffer) / 2 ;

		audio_data.whdr [0].dwBufferLength = sizeof (audio_data.buffer) / 2 ;
		audio_data.whdr [1].dwBufferLength = sizeof (audio_data.buffer) / 2 ;

		audio_data.whdr [0].dwFlags = 0 ;
		audio_data.whdr [1].dwFlags = 0 ;

		/* length of each audio buffer in samples */
		audio_data.bufferlen = sizeof (audio_data.buffer) / 2 / sizeof (short) ;

		/* Prepare the WAVEHDRs */
		if ((error = waveOutPrepareHeader (audio_data.hwave, &(audio_data.whdr [0]), sizeof (WAVEHDR))))
		{	printf ("waveOutPrepareHeader [0] failed : %08X\n", error) ;
			waveOutClose (audio_data.hwave) ;
			continue ;
			} ;

		if ((error = waveOutPrepareHeader (audio_data.hwave, &(audio_data.whdr [1]), sizeof (WAVEHDR))))
		{	printf ("waveOutPrepareHeader [1] failed : %08X\n", error) ;
			waveOutUnprepareHeader (audio_data.hwave, &(audio_data.whdr [0]), sizeof (WAVEHDR)) ;
			waveOutClose (audio_data.hwave) ;
			continue ;
			} ;

		/* Fill up both buffers with audio data */
		audio_data.BuffersInUse = 0 ;
		win32_play_data (&audio_data) ;
		win32_play_data (&audio_data) ;

		/* loop until both buffers are released */
		while (audio_data.BuffersInUse > 0)
		{
			/* wait for buffer to be released */
			WaitForSingleObject (audio_data.Event, INFINITE) ;

			/* refill the buffer if there is more data to play */
			win32_play_data (&audio_data) ;
			} ;

		waveOutUnprepareHeader (audio_data.hwave, &(audio_data.whdr [0]), sizeof (WAVEHDR)) ;
		waveOutUnprepareHeader (audio_data.hwave, &(audio_data.whdr [1]), sizeof (WAVEHDR)) ;

		waveOutClose (audio_data.hwave) ;
		audio_data.hwave = 0 ;

		DeleteCriticalSection (&audio_data.mutex) ;

		sf_close (audio_data.sndfile) ;
		} ;

} /* win32_play */

#endif /* Win32 */

/*------------------------------------------------------------------------------
**	OpenBSD's sndio.
*/

#if HAVE_SNDIO_H

static void
sndio_play (int argc, char *argv [])
{	struct sio_hdl	*hdl ;
	struct sio_par	par ;
	short buffer [BUFFER_LEN] ;
	SNDFILE	*sndfile ;
	SF_INFO	sfinfo ;
	int		k, readcount ;

	for (k = 1 ; k < argc ; k++)
	{	printf ("Playing %s\n", argv [k]) ;
		if (! (sndfile = sf_open (argv [k], SFM_READ, &sfinfo)))
		{	puts (sf_strerror (NULL)) ;
			continue ;
			} ;

		if (sfinfo.channels < 1 || sfinfo.channels > 2)
		{	printf ("Error : channels = %d.\n", sfinfo.channels) ;
			continue ;
			} ;

		if ((hdl = sio_open (NULL, SIO_PLAY, 0)) == NULL)
		{	fprintf (stderr, "open sndio device failed") ;
			return ;
			} ;

		sio_initpar (&par) ;
		par.rate = sfinfo.samplerate ;
		par.pchan = sfinfo.channels ;
		par.bits = 16 ;
		par.sig = 1 ;
		par.le = SIO_LE_NATIVE ;

		if (! sio_setpar (hdl, &par) || ! sio_getpar (hdl, &par))
		{	fprintf (stderr, "set sndio params failed") ;
			return ;
			} ;

		if (! sio_start (hdl))
		{	fprintf (stderr, "sndio start failed") ;
			return ;
			} ;

		while ((readcount = sf_read_short (sndfile, buffer, BUFFER_LEN)))
			sio_write (hdl, buffer, readcount * sizeof (short)) ;

		sio_close (hdl) ;
		} ;

	return ;
} /* sndio_play */

#endif /* sndio */

/*------------------------------------------------------------------------------
**	Solaris.
*/

#if (defined (sun) && defined (unix)) /* ie Solaris */

static void
solaris_play (int argc, char *argv [])
{	static short 	buffer [BUFFER_LEN] ;
	audio_info_t	audio_info ;
	SNDFILE			*sndfile ;
	SF_INFO			sfinfo ;
	unsigned long	delay_time ;
	long			k, start_count, output_count, write_count, read_count ;
	int				audio_fd, error, done ;

	for (k = 1 ; k < argc ; k++)
	{	printf ("Playing %s\n", argv [k]) ;
		if (! (sndfile = sf_open (argv [k], SFM_READ, &sfinfo)))
		{	puts (sf_strerror (NULL)) ;
			continue ;
			} ;

		if (sfinfo.channels < 1 || sfinfo.channels > 2)
		{	printf ("Error : channels = %d.\n", sfinfo.channels) ;
			continue ;
			} ;

		/* open the audio device - write only, non-blocking */
		if ((audio_fd = open ("/dev/audio", O_WRONLY | O_NONBLOCK)) < 0)
		{	perror ("open (/dev/audio) failed") ;
			return ;
			} ;

		/*	Retrive standard values. */
		AUDIO_INITINFO (&audio_info) ;

		audio_info.play.sample_rate = sfinfo.samplerate ;
		audio_info.play.channels = sfinfo.channels ;
		audio_info.play.precision = 16 ;
		audio_info.play.encoding = AUDIO_ENCODING_LINEAR ;
		audio_info.play.gain = AUDIO_MAX_GAIN ;
		audio_info.play.balance = AUDIO_MID_BALANCE ;

		if ((error = ioctl (audio_fd, AUDIO_SETINFO, &audio_info)))
		{	perror ("ioctl (AUDIO_SETINFO) failed") ;
			return ;
			} ;

		/* Delay time equal to 1/4 of a buffer in microseconds. */
		delay_time = (BUFFER_LEN * 1000000) / (audio_info.play.sample_rate * 4) ;

		done = 0 ;
		while (! done)
		{	read_count = sf_read_short (sndfile, buffer, BUFFER_LEN) ;
			if (read_count < BUFFER_LEN)
			{	memset (&(buffer [read_count]), 0, (BUFFER_LEN - read_count) * sizeof (short)) ;
				/* Tell the main application to terminate. */
				done = SF_TRUE ;
				} ;

			start_count = 0 ;
			output_count = BUFFER_LEN * sizeof (short) ;

			while (output_count > 0)
			{	/* write as much data as possible */
				write_count = write (audio_fd, &(buffer [start_count]), output_count) ;
				if (write_count > 0)
				{	output_count -= write_count ;
					start_count += write_count ;
					}
				else
				{	/*	Give the audio output time to catch up. */
					usleep (delay_time) ;
					} ;
				} ; /* while (outpur_count > 0) */
			} ; /* while (! done) */

		close (audio_fd) ;
		} ;

	return ;
} /* solaris_play */

#endif /* Solaris */

/*==============================================================================
**	Main function.
*/

int
main (int argc, char *argv [])
{
	if (argc < 2)
	{
		printf ("\nUsage : %s <input sound file>\n\n", program_name (argv [0])) ;
		printf ("Using %s.\n\n", sf_version_string ()) ;
#if (OS_IS_WIN32 == 1)
		printf ("This is a Unix style command line application which\n"
				"should be run in a MSDOS box or Command Shell window.\n\n") ;
		printf ("Sleeping for 5 seconds before exiting.\n\n") ;

		Sleep (5 * 1000) ;
#endif
		return 1 ;
		} ;

#if defined (__ANDROID__)
	puts ("*** Playing sound not yet supported on Android.") ;
	puts ("*** Please feel free to submit a patch.") ;
	return 1 ;
#elif defined (__linux__)
	#if HAVE_ALSA_ASOUNDLIB_H
		if (access ("/proc/asound/cards", R_OK) == 0)
			alsa_play (argc, argv) ;
		else
	#endif
		opensoundsys_play (argc, argv) ;
#elif defined (__FreeBSD_kernel__) || defined (__FreeBSD__)
	opensoundsys_play (argc, argv) ;
#elif HAVE_SNDIO_H
	sndio_play (argc, argv) ;
#elif (defined (sun) && defined (unix))
	solaris_play (argc, argv) ;
#elif (OS_IS_WIN32 == 1)
	win32_play (argc, argv) ;
#else
	puts ("*** Playing sound not supported on this platform.") ;
	puts ("*** Please feel free to submit a patch.") ;
	return 1 ;
#endif

	return 0 ;
} /* main */

