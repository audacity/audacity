/*
 * $Id: pa_devs.c,v 1.2 2003-03-02 08:01:40 dmazzoni Exp $
 * pa_devs.c
 * List available devices.
 *
 * Author: Phil Burk  http://www.softsynth.com
 *
 * This program uses the PortAudio Portable Audio Library.
 * For more information see: http://www.portaudio.com
 * Copyright (c) 1999-2000 Ross Bencina and Phil Burk
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
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
#include <stdio.h>
#include <math.h>
#include "portaudio.h"

/*******************************************************************/
int main(void);
int main(void)
{
    int      i,j;
    int      numDevices;
    const    PaDeviceInfo *pdi;
    PaError  err;
    Pa_Initialize();
    numDevices = Pa_CountDevices();
    if( numDevices < 0 )
    {
        printf("ERROR: Pa_CountDevices returned 0x%x\n", numDevices );
        err = numDevices;
        goto error;
    }
    printf("Number of devices = %d\n", numDevices );
    for( i=0; i<numDevices; i++ )
    {
        pdi = Pa_GetDeviceInfo( i );
        printf("---------------------------------------------- #%d", i );
        if( i == Pa_GetDefaultInputDeviceID() ) printf(" DefaultInput");
        if( i == Pa_GetDefaultOutputDeviceID() ) printf(" DefaultOutput");
        printf("\nName         = %s\n", pdi->name );
        printf("Max Inputs   = %d", pdi->maxInputChannels  );
        printf(", Max Outputs = %d\n", pdi->maxOutputChannels  );
        if( pdi->numSampleRates == -1 )
        {
            printf("Sample Rate Range = %f to %f\n", pdi->sampleRates[0], pdi->sampleRates[1] );
        }
        else
        {
            printf("Sample Rates =");
            for( j=0; j<pdi->numSampleRates; j++ )
            {
                printf(" %8.2f,", pdi->sampleRates[j] );
            }
            printf("\n");
        }
        printf("Native Sample Formats = ");
        if( pdi->nativeSampleFormats & paInt8 )        printf("paInt8, ");
        if( pdi->nativeSampleFormats & paUInt8 )       printf("paUInt8, ");
        if( pdi->nativeSampleFormats & paInt16 )       printf("paInt16, ");
        if( pdi->nativeSampleFormats & paInt32 )       printf("paInt32, ");
        if( pdi->nativeSampleFormats & paFloat32 )     printf("paFloat32, ");
        if( pdi->nativeSampleFormats & paInt24 )       printf("paInt24, ");
        if( pdi->nativeSampleFormats & paPackedInt24 ) printf("paPackedInt24, ");
        printf("\n");
    }
    Pa_Terminate();

    printf("----------------------------------------------\n");
    return 0;
error:
    Pa_Terminate();
    fprintf( stderr, "An error occured while using the portaudio stream\n" );
    fprintf( stderr, "Error number: %d\n", err );
    fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
    return err;
}
