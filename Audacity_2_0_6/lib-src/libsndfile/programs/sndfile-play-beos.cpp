/*
** Copyright (C) 2001 Marcus Overhagen <marcus@overhagen.de>
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

#include	<stdio.h>

#include 	<Application.h>
#include 	<SoundPlayer.h>
#include 	<string.h>

#include	<sndfile.h>

#define	BUFFER_LEN		1024

/*------------------------------------------------------------------------------
**	BeOS functions for playing a sound.
*/

#if defined (__BEOS__)

struct shared_data
{
	BSoundPlayer *player;
	SNDFILE *sndfile;
	SF_INFO sfinfo;	
	sem_id finished;
};

static void 
buffer_callback(void *theCookie, void *buf, size_t size, const media_raw_audio_format &format) 
{
	shared_data *data = (shared_data *)theCookie;
	short *buffer = (short *)buf;
	int count = size / sizeof(short);
	int m, readcount;

	if (!data->player->HasData())
		return;

	readcount = sf_read_short(data->sndfile, buffer, count);
	if (readcount == 0) 
	{	data->player->SetHasData(false);		
		release_sem(data->finished);
		}
	if (readcount < count) 
	{	for (m = readcount ; m < count ; m++)
			buffer [m] = 0 ;
		}
	if (data->sfinfo.pcmbitwidth < 16) 
	{	for (m = 0 ; m < count ; m++)
			buffer [m] *= 256 ;
		}
}

static void
beos_play (int argc, char *argv [])
{
	shared_data data;
	status_t status;
	int	k;

	/* BSoundPlayer requires a BApplication object */
	BApplication app("application/x-vnd.MarcusOverhagen-sfplay");

	for (k = 1 ; k < argc ; k++)
	{	printf ("Playing %s\n", argv [k]) ;	
		if (! (data.sndfile = sf_open_read (argv [k], &data.sfinfo)))
		{	sf_perror (NULL) ;
			continue ;
			} ;
			
		if (data.sfinfo.channels < 1 || data.sfinfo.channels > 2)
		{	printf ("Error : channels = %d.\n", data.sfinfo.channels) ;
			sf_close (data.sndfile) ;
			continue ;
			} ;		

		data.finished = create_sem(0,"finished");			

		media_raw_audio_format format = 
		{ 	data.sfinfo.samplerate,
			data.sfinfo.channels,
			media_raw_audio_format::B_AUDIO_SHORT,
			B_HOST_IS_LENDIAN ? B_MEDIA_LITTLE_ENDIAN : B_MEDIA_BIG_ENDIAN,
			BUFFER_LEN * sizeof(short)
			};

		BSoundPlayer player(&format,"player",buffer_callback,NULL,&data);
		data.player = &player;
		
		if ((status = player.InitCheck()) != B_OK) 
		{
			printf ("Error : BSoundPlayer init failed, %s.\n", strerror(status)) ;
			delete_sem(data.finished);
			sf_close (data.sndfile) ;
			continue ;
			}

		player.SetVolume(1.0);
		player.Start();
		player.SetHasData(true);
		acquire_sem(data.finished);
		player.Stop();
		delete_sem(data.finished);
		
		sf_close (data.sndfile) ;

		} ;

} /* beos_play */

#endif

/*==============================================================================
**	Main function.
*/

int 
main (int argc, char *argv [])
{
	if (argc < 2)
	{	printf ("Usage : %s <input sound file>\n\n", argv [0]) ;
		return 1 ;
		} ;
	
	beos_play (argc, argv) ;

	return 0 ;
} /* main */
		
