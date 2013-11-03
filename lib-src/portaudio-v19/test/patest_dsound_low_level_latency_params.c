/*
 * $Id: $
 * Portable Audio I/O Library
 * Windows DirectSound low level buffer parameters test
 *
 * Copyright (c) 2011 Ross Bencina
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

#include "portaudio.h"
#include "pa_win_ds.h"

#define NUM_SECONDS         (6)
#define SAMPLE_RATE         (44100)

#define DSOUND_FRAMES_PER_HOST_BUFFER  (256*2) //(440*10)

#define FRAMES_PER_BUFFER   256

#ifndef M_PI
#define M_PI  (3.14159265)
#endif

#define TABLE_SIZE          (2048)

#define CHANNEL_COUNT       (2)


typedef struct
{
    float sine[TABLE_SIZE];
	double phase;
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
    float *out = (float*)outputBuffer;
    unsigned long i,j;

    (void) timeInfo; /* Prevent unused variable warnings. */
    (void) statusFlags;
    (void) inputBuffer;
    
    for( i=0; i<framesPerBuffer; i++ )
    {
        float x = data->sine[(int)data->phase];
        data->phase += 20;
        if( data->phase >= TABLE_SIZE ){
			data->phase -= TABLE_SIZE;
		}

		for( j = 0; j < CHANNEL_COUNT; ++j ){
            *out++ = x;
		}
	}
    
    return paContinue;
}

/*******************************************************************/
int main(int argc, char* argv[])
{
    PaStreamParameters outputParameters;
    PaWinDirectSoundStreamInfo dsoundStreamInfo;
    PaStream *stream;
    PaError err;
    paTestData data;
    int i;
    int deviceIndex;

    printf("PortAudio Test: output a sine blip on each channel. SR = %d, BufSize = %d, Chans = %d\n", SAMPLE_RATE, FRAMES_PER_BUFFER, CHANNEL_COUNT);

    err = Pa_Initialize();
    if( err != paNoError ) goto error;

	deviceIndex = Pa_GetHostApiInfo( Pa_HostApiTypeIdToHostApiIndex( paDirectSound ) )->defaultOutputDevice;
	if( argc == 2 ){
		sscanf( argv[1], "%d", &deviceIndex );
	}

	printf( "using device id %d (%s)\n", deviceIndex, Pa_GetDeviceInfo(deviceIndex)->name );

    /* initialise sinusoidal wavetable */
    for( i=0; i<TABLE_SIZE; i++ )
    {
        data.sine[i] = (float) sin( ((double)i/(double)TABLE_SIZE) * M_PI * 2. );
    }

	data.phase = 0;

    outputParameters.device = deviceIndex;
    outputParameters.channelCount = CHANNEL_COUNT;
    outputParameters.sampleFormat = paFloat32; /* 32 bit floating point processing */
    outputParameters.suggestedLatency = 0; /*Pa_GetDeviceInfo( outputParameters.device )->defaultLowOutputLatency;*/
    outputParameters.hostApiSpecificStreamInfo = NULL;

    dsoundStreamInfo.size = sizeof(PaWinDirectSoundStreamInfo);
    dsoundStreamInfo.hostApiType = paDirectSound; 
    dsoundStreamInfo.version = 2;
    dsoundStreamInfo.flags = paWinDirectSoundUseLowLevelLatencyParameters;
    dsoundStreamInfo.framesPerBuffer = DSOUND_FRAMES_PER_HOST_BUFFER;
    outputParameters.hostApiSpecificStreamInfo = &dsoundStreamInfo;
   

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
              paClipOff,      /* we won't output out of range samples so don't bother clipping them */
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
    printf("Test finished.\n");
    
    return err;
error:
    Pa_Terminate();
    fprintf( stderr, "An error occured while using the portaudio stream\n" );
    fprintf( stderr, "Error number: %d\n", err );
    fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
    return err;
}

