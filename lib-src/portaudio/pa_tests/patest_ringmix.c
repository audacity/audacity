/* $Id: patest_ringmix.c,v 1.2 2003-03-02 08:01:42 dmazzoni Exp $ */

#include "stdio.h"
#include "portaudio.h"
/* This will be called asynchronously by the PortAudio engine. */
static int myCallback( void *inputBuffer, void *outputBuffer,
                       unsigned long framesPerBuffer, PaTimestamp outTime, void *userData )
{
    float *out = (float *) outputBuffer;
    float *in  = (float *) inputBuffer;
    float leftInput, rightInput;
    unsigned int i;
    if( inputBuffer == NULL ) return 0;
    /* Read input buffer, process data, and fill output buffer. */
    for( i=0; i<framesPerBuffer; i++ )
    {
        leftInput = *in++;      /* Get interleaved samples from input buffer. */
        rightInput = *in++;
        *out++ = leftInput * rightInput;            /* ring modulation */
        *out++ = 0.5f * (leftInput + rightInput);   /* mix */
    }
    return 0;
}
/* Open a PortAudioStream to input and output audio data. */
int main(void)
{
    PortAudioStream *stream;
    Pa_Initialize();
    Pa_OpenDefaultStream(
        &stream,
        2, 2,            /* stereo input and output */
        paFloat32,  44100.0,
        64,  0,          /* 64 frames per buffer, let PA determine numBuffers */
        myCallback, NULL );
    Pa_StartStream( stream );
    Pa_Sleep( 10000 );    /* Sleep for 10 seconds while processing. */
    Pa_StopStream( stream );
    Pa_CloseStream( stream );
    Pa_Terminate();
    return 0;
}
