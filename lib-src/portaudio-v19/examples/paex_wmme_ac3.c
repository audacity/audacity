/** @file paex_wmme_ac3.c
	@ingroup examples_src
	@brief Use WMME-specific interface to send raw AC3 data to a S/PDIF output.
	@author Ross Bencina <rossb@audiomulch.com>
*/
/*
 * $Id: $
 * Portable Audio I/O Library
 * Windows MME ac3 sound output test
 *
 * Copyright (c) 2009 Ross Bencina
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * The text above constitutes the entire PortAudio license; however, 
 * the PortAudio community also makes the following non-binding requests:
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version. It is also 
 * requested that these non-binding requests be included along with the 
 * license above.
 */

#include <stdio.h>
#include <math.h>

#include <windows.h>    /* required when using pa_win_wmme.h */
#include <mmsystem.h>   /* required when using pa_win_wmme.h */

#include "portaudio.h"
#include "pa_win_wmme.h"

#define NUM_SECONDS         (20)
#define SAMPLE_RATE         (48000)
#define FRAMES_PER_BUFFER   (64)

#ifndef M_PI
#define M_PI  (3.14159265)
#endif

#define TABLE_SIZE          (100)

#define CHANNEL_COUNT       (2)



typedef struct
{
    short *buffer;
    int bufferSampleCount;
    int playbackIndex;
}
paTestData;

/* This routine will be called by the PortAudio engine when audio is needed.
** It may called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/
static int patestCallback( const void *inputBuffer, void *outputBuffer,
                            unsigned long framesPerBuffer,
                            const PaStreamCallbackTimeInfo* timeInfo,
                            PaStreamCallbackFlags statusFlags,
                            void *userData )
{
    paTestData *data = (paTestData*)userData;
    short *out = (short*)outputBuffer;
    unsigned long i,j;

    (void) timeInfo; /* Prevent unused variable warnings. */
    (void) statusFlags;
    (void) inputBuffer;

    /* stream out contents of data->buffer looping at end */
    
    for( i=0; i<framesPerBuffer; i++ )
    {
		for( j = 0; j < CHANNEL_COUNT; ++j ){
            *out++ = data->buffer[ data->playbackIndex++ ];

            if( data->playbackIndex >= data->bufferSampleCount )
                data->playbackIndex = 0; /* loop at end of buffer */
		}
	}
    
    return paContinue;
}

/*******************************************************************/
int main(int argc, char* argv[])
{
    PaStreamParameters outputParameters;
    PaWinMmeStreamInfo wmmeStreamInfo;
    PaStream *stream;
    PaError err;
    paTestData data;
    int deviceIndex;
    FILE *fp;
    const char *fileName = "c:\\test_48k.ac3.spdif";
    data.buffer = NULL;

    printf("usage: patest_wmme_ac3 fileName [paDeviceIndex]\n");
    printf("**IMPORTANT*** The provided file must include the spdif preamble at the start of every AC-3 frame. Using a normal ac3 file won't work.\n");
    printf("PortAudio Test: output a raw spdif ac3 stream. SR = %d, BufSize = %d, Chans = %d\n", 
            SAMPLE_RATE, FRAMES_PER_BUFFER, CHANNEL_COUNT);

        
    if( argc >= 2 )
        fileName = argv[1];

    printf( "reading spdif ac3 raw stream file %s\n", fileName );

    fp = fopen( fileName, "rb" );
    if( !fp ){
        fprintf( stderr, "error opening spdif ac3 file.\n" );
        return -1;
    }
    /* get file size */
    fseek( fp, 0, SEEK_END );
    data.bufferSampleCount = ftell( fp ) / sizeof(short);
    fseek( fp, 0, SEEK_SET );

    /* allocate buffer, read the whole file into memory */
    data.buffer = (short*)malloc( data.bufferSampleCount * sizeof(short) );
    if( !data.buffer ){
        fprintf( stderr, "error allocating buffer.\n" );
        return -1;
    }

    fread( data.buffer, sizeof(short), data.bufferSampleCount, fp );
    fclose( fp );

    data.playbackIndex = 0;

    err = Pa_Initialize();
    if( err != paNoError ) goto error;

	deviceIndex = Pa_GetHostApiInfo( Pa_HostApiTypeIdToHostApiIndex( paMME ) )->defaultOutputDevice;
	if( argc >= 3 ){
		sscanf( argv[1], "%d", &deviceIndex );
	}

	printf( "using device id %d (%s)\n", deviceIndex, Pa_GetDeviceInfo(deviceIndex)->name );

    
    outputParameters.device = deviceIndex;
    outputParameters.channelCount = CHANNEL_COUNT;
    outputParameters.sampleFormat = paInt16; /* IMPORTANT must use paInt16 for WMME AC3 */
    outputParameters.suggestedLatency = Pa_GetDeviceInfo( outputParameters.device )->defaultLowOutputLatency;
    outputParameters.hostApiSpecificStreamInfo = NULL;

    wmmeStreamInfo.size = sizeof(PaWinMmeStreamInfo);
    wmmeStreamInfo.hostApiType = paMME; 
    wmmeStreamInfo.version = 1;
    wmmeStreamInfo.flags = paWinMmeWaveFormatDolbyAc3Spdif;
    outputParameters.hostApiSpecificStreamInfo = &wmmeStreamInfo;


	if( Pa_IsFormatSupported( 0, &outputParameters, SAMPLE_RATE ) == paFormatIsSupported  ){
		printf( "Pa_IsFormatSupported reports device will support %d channels.\n", CHANNEL_COUNT );
	}else{
		printf( "Pa_IsFormatSupported reports device will not support %d channels.\n", CHANNEL_COUNT );
	}

    err = Pa_OpenStream(
              &stream,
              NULL, /* no input */
              &outputParameters,
              SAMPLE_RATE,
              FRAMES_PER_BUFFER,
              0,
              patestCallback,
              &data );
    if( err != paNoError ) goto error;

    err = Pa_StartStream( stream );
    if( err != paNoError ) goto error;

    printf("Play for %d seconds.\n", NUM_SECONDS );
    Pa_Sleep( NUM_SECONDS * 1000 );

    err = Pa_StopStream( stream );
    if( err != paNoError ) goto error;

    err = Pa_CloseStream( stream );
    if( err != paNoError ) goto error;

    Pa_Terminate();
    free( data.buffer );
    printf("Test finished.\n");
    
    return err;
error:
    Pa_Terminate();
    free( data.buffer );

    fprintf( stderr, "An error occured while using the portaudio stream\n" );
    fprintf( stderr, "Error number: %d\n", err );
    fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
    return err;
}

