/** @file patest_converters.c
	@ingroup test_src
	@brief Tests the converter functions in pa_converters.c
	@author Ross Bencina <rossb@audiomulch.com>

    Link with pa_dither.c and pa_converters.c

    see http://www.portaudio.com/trac/wiki/V19ConvertersStatus for a discussion of this.
*/
/*
 * $Id: $
 *
 * This program uses the PortAudio Portable Audio Library.
 * For more information see: http://www.portaudio.com/
 * Copyright (c) 1999-2008 Ross Bencina and Phil Burk
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
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "portaudio.h"
#include "pa_converters.h"
#include "pa_dither.h"
#include "pa_types.h"
#include "pa_endianness.h"

#ifndef M_PI
#define M_PI  (3.14159265)
#endif

#define MAX_PER_CHANNEL_FRAME_COUNT     (2048)
#define MAX_CHANNEL_COUNT               (8)


#define SAMPLE_FORMAT_COUNT (6)

static PaSampleFormat sampleFormats_[ SAMPLE_FORMAT_COUNT ] = 
    { paFloat32, paInt32, paInt24, paInt16, paInt8, paUInt8 }; /* all standard PA sample formats */

static const char* sampleFormatNames_[SAMPLE_FORMAT_COUNT] = 
    { "paFloat32", "paInt32", "paInt24", "paInt16", "paInt8", "paUInt8" };


static const char* abbreviatedSampleFormatNames_[SAMPLE_FORMAT_COUNT] = 
    { "f32", "i32", "i24", "i16", " i8", "ui8" };


PaError My_Pa_GetSampleSize( PaSampleFormat format );

/*
    available flags are paClipOff and paDitherOff
    clipping is usually applied for float -> int conversions
    dither is usually applied for all downconversions (ie anything but 8bit->8bit conversions
*/

static int CanClip( PaSampleFormat sourceFormat, PaSampleFormat destinationFormat )
{
    if( sourceFormat == paFloat32 && destinationFormat != sourceFormat )
        return 1;
    else
        return 0;
}

static int CanDither( PaSampleFormat sourceFormat, PaSampleFormat destinationFormat )
{
    if( sourceFormat < destinationFormat && sourceFormat != paInt8 )
        return 1;
    else
        return 0;
}

static void GenerateOneCycleSineReference( double *out, int frameCount, int strideFrames )
{
    int i;
    for( i=0; i < frameCount; ++i ){
        *out = sin( ((double)i/(double)frameCount) * 2. * M_PI );
        out += strideFrames;
    }
}


static void GenerateOneCycleSine( PaSampleFormat format, void *buffer, int frameCount, int strideFrames )
{
    switch( format ){

        case paFloat32:
            {
                int i;
                float *out = (float*)buffer;
                for( i=0; i < frameCount; ++i ){
                    *out = (float).9 * sin( ((double)i/(double)frameCount) * 2. * M_PI );
                    out += strideFrames;
                }
            }
            break;
        case paInt32:
            {
                int i;
                PaInt32 *out = (PaInt32*)buffer;
                for( i=0; i < frameCount; ++i ){
                    *out = (PaInt32)(.9 * sin( ((double)i/(double)frameCount) * 2. * M_PI ) * 0x7FFFFFFF);
                    out += strideFrames;
                }
            }
            break;
        case paInt24:
            {
                int i;
                unsigned char *out = (unsigned char*)buffer;
                for( i=0; i < frameCount; ++i ){
                    signed long temp = (PaInt32)(.9 * sin( ((double)i/(double)frameCount) * 2. * M_PI ) * 0x7FFFFFFF);
                    
                    #if defined(PA_LITTLE_ENDIAN)
                            out[0] = (unsigned char)(temp >> 8) & 0xFF;
                            out[1] = (unsigned char)(temp >> 16) & 0xFF;
                            out[2] = (unsigned char)(temp >> 24) & 0xFF;
                    #elif defined(PA_BIG_ENDIAN)
                            out[0] = (unsigned char)(temp >> 24) & 0xFF;
                            out[1] = (unsigned char)(temp >> 16) & 0xFF;
                            out[2] = (unsigned char)(temp >> 8) & 0xFF;
                    #endif
                    out += 3;
                }
            }
            break;
        case paInt16:
            {
                int i;
                PaInt16 *out = (PaInt16*)buffer;
                for( i=0; i < frameCount; ++i ){
                    *out = (PaInt16)(.9 * sin( ((double)i/(double)frameCount) * 2. * M_PI ) * 0x7FFF );
                    out += strideFrames;
                }
            }
            break;
        case paInt8:
            {
                int i;
                signed char *out = (signed char*)buffer;
                for( i=0; i < frameCount; ++i ){
                    *out = (signed char)(.9 * sin( ((double)i/(double)frameCount) * 2. * M_PI ) * 0x7F );
                    out += strideFrames;
                }
            }
            break;
        case paUInt8:
            {
                int i;
                unsigned char *out = (unsigned char*)buffer;
                for( i=0; i < frameCount; ++i ){
                    *out = (unsigned char)( .5 * (1. + (.9 * sin( ((double)i/(double)frameCount) * 2. * M_PI ))) * 0xFF  );
                    out += strideFrames;
                }
            }
            break;
    }
}

int TestNonZeroPresent( void *buffer, int size )
{
    char *p = (char*)buffer;
    int i;

    for( i=0; i < size; ++i ){
    
        if( *p != 0 )
            return 1;
        ++p;
    }   

    return 0;
}

float MaximumAbsDifference( float* sourceBuffer, float* referenceBuffer, int count )
{
    float result = 0;
    float difference;
    while( count-- ){
        difference = fabs( *sourceBuffer++ - *referenceBuffer++ );
        if( difference > result )
            result = difference;
    }

    return result;
}  

int main( const char **argv, int argc )
{
    PaUtilTriangularDitherGenerator ditherState;
    PaUtilConverter *converter;
    void *destinationBuffer, *sourceBuffer;
    double *referenceBuffer;
    int sourceFormatIndex, destinationFormatIndex;
    PaSampleFormat sourceFormat, destinationFormat;
    PaStreamFlags flags;
    int passFailMatrix[SAMPLE_FORMAT_COUNT][SAMPLE_FORMAT_COUNT]; // [source][destination]
    float noiseAmplitudeMatrix[SAMPLE_FORMAT_COUNT][SAMPLE_FORMAT_COUNT]; // [source][destination]
    float amp;

#define FLAG_COMBINATION_COUNT (4)
    PaStreamFlags flagCombinations[FLAG_COMBINATION_COUNT] = { paNoFlag, paClipOff, paDitherOff, paClipOff | paDitherOff };
    const char *flagCombinationNames[FLAG_COMBINATION_COUNT] = { "paNoFlag", "paClipOff", "paDitherOff", "paClipOff | paDitherOff" };
    int flagCombinationIndex;

    PaUtil_InitializeTriangularDitherState( &ditherState );

    /* allocate more than enough space, we use sizeof(float) but we need to fit any 32 bit datum */

    destinationBuffer = (void*)malloc( MAX_PER_CHANNEL_FRAME_COUNT * MAX_CHANNEL_COUNT * sizeof(float) );
    sourceBuffer = (void*)malloc( MAX_PER_CHANNEL_FRAME_COUNT * MAX_CHANNEL_COUNT * sizeof(float) );
    referenceBuffer = (void*)malloc( MAX_PER_CHANNEL_FRAME_COUNT * MAX_CHANNEL_COUNT * sizeof(float) );


    /* the first round of tests simply iterates through the buffer combinations testing
        that putting something in gives something out */

    printf( "= Sine wave in, something out =\n" );

    printf( "\n" );

    GenerateOneCycleSine( paFloat32, referenceBuffer, MAX_PER_CHANNEL_FRAME_COUNT, 1 );

    for( flagCombinationIndex = 0; flagCombinationIndex < FLAG_COMBINATION_COUNT; ++flagCombinationIndex ){
        flags = flagCombinations[flagCombinationIndex];

        printf( "\n" );
        printf( "== flags = %s ==\n", flagCombinationNames[flagCombinationIndex] );

        for( sourceFormatIndex = 0; sourceFormatIndex < SAMPLE_FORMAT_COUNT; ++sourceFormatIndex ){
            for( destinationFormatIndex = 0; destinationFormatIndex < SAMPLE_FORMAT_COUNT; ++destinationFormatIndex ){
                sourceFormat = sampleFormats_[sourceFormatIndex];
                destinationFormat = sampleFormats_[destinationFormatIndex];
                //printf( "%s -> %s ", sampleFormatNames_[ sourceFormatIndex ], sampleFormatNames_[ destinationFormatIndex ] );

                converter = PaUtil_SelectConverter( sourceFormat, destinationFormat, flags );

                /* source is a sinewave */
                GenerateOneCycleSine( sourceFormat, sourceBuffer, MAX_PER_CHANNEL_FRAME_COUNT, 1 );

                /* zero destination */
                memset( destinationBuffer, 0, MAX_PER_CHANNEL_FRAME_COUNT * My_Pa_GetSampleSize( destinationFormat ) );

                (*converter)( destinationBuffer, 1, sourceBuffer, 1, MAX_PER_CHANNEL_FRAME_COUNT, &ditherState );

    /*
    Other ways we could test this would be:
        - pass a constant, check for a constant (wouldn't work with dither)
        - pass alternating +/-, check for the same...
    */
                if( TestNonZeroPresent( destinationBuffer, MAX_PER_CHANNEL_FRAME_COUNT * My_Pa_GetSampleSize( destinationFormat ) ) ){
                    //printf( "PASSED\n" );
                    passFailMatrix[sourceFormatIndex][destinationFormatIndex] = 1;
                }else{
                    //printf( "FAILED\n" );
                    passFailMatrix[sourceFormatIndex][destinationFormatIndex] = 0;
                }

                
                /* try to measure the noise floor (comparing output signal to a float32 sine wave) */

                if( passFailMatrix[sourceFormatIndex][destinationFormatIndex] ){

                    /* convert destination back to paFloat32 into source */
                    converter = PaUtil_SelectConverter( destinationFormat, paFloat32, paNoFlag );

                    memset( sourceBuffer, 0, MAX_PER_CHANNEL_FRAME_COUNT * My_Pa_GetSampleSize( paFloat32 ) );
                    (*converter)( sourceBuffer, 1, destinationBuffer, 1, MAX_PER_CHANNEL_FRAME_COUNT, &ditherState );

                    if( TestNonZeroPresent( sourceBuffer, MAX_PER_CHANNEL_FRAME_COUNT * My_Pa_GetSampleSize( paFloat32 ) ) ){
    
                        noiseAmplitudeMatrix[sourceFormatIndex][destinationFormatIndex] = MaximumAbsDifference( (float*)sourceBuffer, (float*)referenceBuffer, MAX_PER_CHANNEL_FRAME_COUNT );
                        
                    }else{
                        /* can't test noise floor because there is no conversion from dest format to float available */
                        noiseAmplitudeMatrix[sourceFormatIndex][destinationFormatIndex] = -1; // mark as failed
                    }
                }else{
                    noiseAmplitudeMatrix[sourceFormatIndex][destinationFormatIndex] = -1; // mark as failed
                }
            }
        }

        printf( "\n" );
        printf( "=== Output contains non-zero data ===\n" );
        printf( "Key: . - pass, X - fail\n" );
        printf( "{{{\n" ); // trac preformated text tag
        printf( "in|  out:    " );
        for( destinationFormatIndex = 0; destinationFormatIndex < SAMPLE_FORMAT_COUNT; ++destinationFormatIndex ){
            printf( "  %s   ", abbreviatedSampleFormatNames_[destinationFormatIndex] );
        }
        printf( "\n" );

        for( sourceFormatIndex = 0; sourceFormatIndex < SAMPLE_FORMAT_COUNT; ++sourceFormatIndex ){
            printf( "%s         ", abbreviatedSampleFormatNames_[sourceFormatIndex] );
            for( destinationFormatIndex = 0; destinationFormatIndex < SAMPLE_FORMAT_COUNT; ++destinationFormatIndex ){
                printf( "   %s   ", (passFailMatrix[sourceFormatIndex][destinationFormatIndex])? " ." : " X" );
            }
            printf( "\n" );
        }
        printf( "}}}\n" ); // trac preformated text tag

        printf( "\n" );
        printf( "=== Combined dynamic range (src->dest->float32) ===\n" );
        printf( "Key: Noise amplitude in dBfs, X - fail (either above failed or dest->float32 failed)\n" );
        printf( "{{{\n" ); // trac preformated text tag
        printf( "in|  out:    " );
        for( destinationFormatIndex = 0; destinationFormatIndex < SAMPLE_FORMAT_COUNT; ++destinationFormatIndex ){
            printf( "  %s   ", abbreviatedSampleFormatNames_[destinationFormatIndex] );
        }
        printf( "\n" );

        for( sourceFormatIndex = 0; sourceFormatIndex < SAMPLE_FORMAT_COUNT; ++sourceFormatIndex ){
            printf( " %s       ", abbreviatedSampleFormatNames_[sourceFormatIndex] );
            for( destinationFormatIndex = 0; destinationFormatIndex < SAMPLE_FORMAT_COUNT; ++destinationFormatIndex ){
                amp = noiseAmplitudeMatrix[sourceFormatIndex][destinationFormatIndex];
                if( amp < 0. )
                    printf( "    X   " );
                else
                    printf( " % 6.1f ", 20.*log10(amp) );
            }
            printf( "\n" );
        }
        printf( "}}}\n" ); // trac preformated text tag
    }


    free( destinationBuffer );
    free( sourceBuffer );
    free( referenceBuffer );
}

// copied here for now otherwise we need to include the world just for this function.
PaError My_Pa_GetSampleSize( PaSampleFormat format )
{
    int result;

    switch( format & ~paNonInterleaved )
    {

    case paUInt8:
    case paInt8:
        result = 1;
        break;

    case paInt16:
        result = 2;
        break;

    case paInt24:
        result = 3;
        break;

    case paFloat32:
    case paInt32:
        result = 4;
        break;

    default:
        result = paSampleFormatNotSupported;
        break;
    }

    return (PaError) result;
}
