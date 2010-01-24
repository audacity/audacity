/*
 *  pa_conversions.c
 *  portaudio
 *
 *  Created by Phil Burk on Mon Mar 18 2002.
 *
 */
#include <stdio.h>

#include "portaudio.h"
#include "pa_host.h"

#define CLIP( val, min, max )  { val = ((val) < (min)) ? min : (((val) < (max)) ? (max) : (val)); }

/*************************************************************************/
static void PaConvert_Float32_Int16(
    float *sourceBuffer, int sourceStride,
    short *targetBuffer, int targetStride,
    int numSamples )
{
	int i;
	for( i=0; i<numSamples; i++ )
	{
        short samp = (short) (*sourceBuffer * (32767.0f));
        *targetBuffer = samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}

/*************************************************************************/
static void PaConvert_Float32_Int16_Clip(
    float *sourceBuffer, int sourceStride,
    short *targetBuffer, int targetStride,
    int numSamples )
{
	int i;
	for( i=0; i<numSamples; i++ )
	{
        long samp = (long) (*sourceBuffer * (32767.0f));
        CLIP( samp, -0x8000, 0x7FFF );
        *targetBuffer = (short) samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}

/*************************************************************************/
static void PaConvert_Float32_Int16_ClipDither(
    float *sourceBuffer, int sourceStride,
    short *targetBuffer, int targetStride,
    int numSamples )
{
	int i;
	for( i=0; i<numSamples; i++ )
	{
    // use smaller scaler to prevent overflow when we add the dither
        float dither  = PaConvert_TriangularDither() * PA_DITHER_SCALE;
        float dithered = (*sourceBuffer * (32766.0f)) + dither;
        long samp = (long) dithered;
        CLIP( samp, -0x8000, 0x7FFF );
        *targetBuffer = (short) samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}

/*************************************************************************/
static void PaConvert_Float32_Int16_Dither(
    float *sourceBuffer, int sourceStride,
    short *targetBuffer, int targetStride,
    int numSamples )
{
	int i;
	for( i=0; i<numSamples; i++ )
	{
    // use smaller scaler to prevent overflow when we add the dither
        float dither  = PaConvert_TriangularDither() * PA_DITHER_SCALE;
        float dithered = (*sourceBuffer * (32766.0f)) + dither;
        *targetBuffer = (short) dithered;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}


/*************************************************************************/
static void PaConvert_Int16_Float32(
    short *sourceBuffer, int sourceStride,
    float *targetBuffer, int targetStride,
    int numSamples )
{
    int i;
	for( i=0; i<numSamples; i++ )
	{
        float samp = *sourceBuffer * (1.0f / 32768.0f);
        *targetBuffer = samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}

/*************************************************************************/
static void PaConvert_Float32_Int8(
    float *sourceBuffer, int sourceStride,
    char *targetBuffer, int targetStride,
    int numSamples )
{
	int i;
	for( i=0; i<numSamples; i++ )
	{
        char samp = (char) (*sourceBuffer * (127.0));
        *targetBuffer = samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}


/*************************************************************************/
static void PaConvert_Float32_Int8_Clip(
    float *sourceBuffer, int sourceStride,
    char *targetBuffer, int targetStride,
    int numSamples )
{
	int i;
	for( i=0; i<numSamples; i++ )
	{
        long samp = (long) (*sourceBuffer * 127.0f);
        CLIP( samp, -0x80, 0x7F );
        *targetBuffer = (char) samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}

/*************************************************************************/
static void PaConvert_Float32_Int8_ClipDither(
    float *sourceBuffer, int sourceStride,
    char *targetBuffer, int targetStride,
    int numSamples )
{
	int i;
	for( i=0; i<numSamples; i++ )
	{
    // use smaller scaler to prevent overflow when we add the dither
        float dither  = PaConvert_TriangularDither() * PA_DITHER_SCALE;
        float dithered = (*sourceBuffer * (126.0f)) + dither;
        long samp = (long) dithered;
        CLIP( samp, -0x80, 0x7F );
        *targetBuffer = (char) samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}

/*************************************************************************/
static void PaConvert_Float32_Int8_Dither(
    float *sourceBuffer, int sourceStride,
    char *targetBuffer, int targetStride,
    int numSamples )
{
	int i;
	for( i=0; i<numSamples; i++ )
	{
    // use smaller scaler to prevent overflow when we add the dither
        float dither  = PaConvert_TriangularDither() * PA_DITHER_SCALE;  //FIXME
        float dithered = (*sourceBuffer * (126.0f)) + dither;
        long samp = (long) dithered;
        *targetBuffer = (char) samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}

/*************************************************************************/
static void PaConvert_Int8_Float32(
    char *sourceBuffer, int sourceStride,
    float *targetBuffer, int targetStride,
    int numSamples )
{
    int i;
	for( i=0; i<numSamples; i++ )
	{
        float samp = *sourceBuffer * (1.0f / 128.0f);
        *targetBuffer = samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}

/*************************************************************************/
static void PaConvert_Float32_UInt8(
    float *sourceBuffer, int sourceStride,
    unsigned char *targetBuffer, int targetStride,
    int numSamples )
{
	int i;
	for( i=0; i<numSamples; i++ )
	{
        unsigned char samp = (unsigned char)(128 + (*sourceBuffer * (127.0)));
        *targetBuffer = samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}
    
/*************************************************************************/
static void PaConvert_UInt8_Float32(
    unsigned char *sourceBuffer, int sourceStride,
    float *targetBuffer, int targetStride,
    int numSamples )
{
    int i;
	for( i=0; i<numSamples; i++ )
	{
        float samp = (*sourceBuffer - 128) * (1.0f / 128.0f);
        *targetBuffer = samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}

/*************************************************************************/
static void PaConvert_Float32_Int32(
    float *sourceBuffer, int sourceStride,
    long *targetBuffer, int targetStride,
    int numSamples )
{
	int i;
	for( i=0; i<numSamples; i++ )
	{
        int samp = (int) (*sourceBuffer * 0x7FFFFFFF);
        *targetBuffer = samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}

/*************************************************************************/
static void PaConvert_Float32_Int32_Clip(
    float *sourceBuffer, int sourceStride,
    long *targetBuffer, int targetStride,
    int numSamples )
{
	int i;
	for( i=0; i<numSamples; i++ )
	{
        int samp;
        float fs = *sourceBuffer;
        CLIP( fs, -1.0, 0.999999 );
        samp = (int) (*sourceBuffer * 0x7FFFFFFF);
        *targetBuffer = samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}

/*************************************************************************/
static void PaConvert_Int32_Float32(
    long *sourceBuffer, int sourceStride,
    float *targetBuffer, int targetStride,
    int numSamples )
{
    int i;
	for( i=0; i<numSamples; i++ )
	{
        float samp = *sourceBuffer * (1.0f / 0x7FFFFFFF);
        *targetBuffer = samp;
        sourceBuffer += sourceStride;
        targetBuffer += targetStride;
    }
}

/*************************************************************************/
static PortAudioConverter *PaConvert_SelectProc( PaSampleFormat sourceFormat,
        PaSampleFormat targetFormat,  int ifClip, int ifDither )
{
    PortAudioConverter *proc = NULL;
    switch( sourceFormat )
    {
    case paUInt8:
        switch( targetFormat )
        {
        case paFloat32:
            proc = (PortAudioConverter *) PaConvert_UInt8_Float32;
            break;
        default:
            break;
        }
        break;
    case paInt8:
        switch( targetFormat )
        {
        case paFloat32:
            proc = (PortAudioConverter *) PaConvert_Int8_Float32;
            break;
        default:
            break;
        }
        break;
    case paInt16:
        switch( targetFormat )
        {
        case paFloat32:
            proc = (PortAudioConverter *) PaConvert_Int16_Float32;
            break;
        default:
            break;
        }
        break;

    case paInt32:
        switch( targetFormat )
        {
        case paFloat32:
            proc = (PortAudioConverter *) PaConvert_Int32_Float32;
            break;
        default:
            break;
        }
        break;
        
    case paFloat32:
        switch( targetFormat )
        {
        case paUInt8:
            proc = (PortAudioConverter *) PaConvert_Float32_UInt8;
            break;
        case paInt8:
            if( ifClip && ifDither ) proc = (PortAudioConverter *) PaConvert_Float32_Int8_ClipDither;
            else if( ifClip ) proc = (PortAudioConverter *) PaConvert_Float32_Int8_Clip;
            else if( ifDither ) proc = (PortAudioConverter *) PaConvert_Float32_Int8_Dither;
            else proc = (PortAudioConverter *) PaConvert_Float32_Int8;
            break;
        case paInt16:
            if( ifClip && ifDither ) proc = (PortAudioConverter *) PaConvert_Float32_Int16_ClipDither;
            else if( ifClip ) proc = (PortAudioConverter *) PaConvert_Float32_Int16_Clip;
            else if( ifDither ) proc = (PortAudioConverter *) PaConvert_Float32_Int16_Dither;
            else proc = (PortAudioConverter *) PaConvert_Float32_Int16;
            break;
        case paInt32:
            /* Don't bother dithering a 32 bit integer! */
            if( ifClip ) proc = (PortAudioConverter *) PaConvert_Float32_Int32_Clip;
            else proc = (PortAudioConverter *) PaConvert_Float32_Int32;
            break;
        default:
            break;
        }
        break;
    default:
        break;
    }
    return proc;
            
}

/*************************************************************************/
PaError PaConvert_SetupInput( internalPortAudioStream   *past,
    PaSampleFormat   nativeInputSampleFormat )
{    
    past->past_NativeInputSampleFormat = nativeInputSampleFormat;
    past->past_InputConversionSourceStride = 1;
    past->past_InputConversionTargetStride = 1;
    
    if( nativeInputSampleFormat != past->past_InputSampleFormat )
    {
        int ifDither = (past->past_Flags & paDitherOff) == 0;
        past->past_InputConversionProc = PaConvert_SelectProc( nativeInputSampleFormat,
             past->past_InputSampleFormat, 0, ifDither );
        if( past->past_InputConversionProc == NULL ) return paSampleFormatNotSupported;
    }
    else
    {
        past->past_InputConversionProc = NULL; /* no conversion necessary */
    }
    
    return paNoError;
}

/*************************************************************************/
PaError PaConvert_SetupOutput( internalPortAudioStream   *past,
    PaSampleFormat   nativeOutputSampleFormat )
{

    past->past_NativeOutputSampleFormat = nativeOutputSampleFormat;
    past->past_OutputConversionSourceStride = 1;
    past->past_OutputConversionTargetStride = 1;
    
    if( nativeOutputSampleFormat != past->past_OutputSampleFormat )
    {
        int ifDither = (past->past_Flags & paDitherOff) == 0;
        int ifClip = (past->past_Flags & paClipOff) == 0;

        past->past_OutputConversionProc = PaConvert_SelectProc( past->past_OutputSampleFormat,
            nativeOutputSampleFormat, ifClip, ifDither );
        if( past->past_OutputConversionProc == NULL ) return paSampleFormatNotSupported;
    }
    else
    {
        past->past_OutputConversionProc = NULL; /* no conversion necessary */
    }
    
    return paNoError;
}

/*************************************************************************
** Called by host code.
** Convert input from native format to user format,
** call user code,
** then convert output to native format.
** Returns result from user callback.
*/
long PaConvert_Process( internalPortAudioStream   *past,
                            void *nativeInputBuffer,
                            void *nativeOutputBuffer )
{
    int               userResult;
    void             *inputBuffer = NULL;
    void             *outputBuffer = NULL;

    /* Get native input data. */
    if( (past->past_NumInputChannels > 0) && (nativeInputBuffer != NULL) )
    {
        if( past->past_InputSampleFormat == past->past_NativeInputSampleFormat )
        {
        /*  Already in native format so just read directly from native buffer. */
            inputBuffer =  nativeInputBuffer;
        }
        else
        {
            inputBuffer = past->past_InputBuffer;
        /* Convert input data to user format. */
            (*past->past_InputConversionProc)(nativeInputBuffer, past->past_InputConversionSourceStride,
                inputBuffer, past->past_InputConversionTargetStride,
                past->past_FramesPerUserBuffer * past->past_NumInputChannels );
        }
    }

    /* Are we doing output? */
    if( (past->past_NumOutputChannels > 0) && (nativeOutputBuffer != NULL) )
    {
        outputBuffer = (past->past_OutputConversionProc == NULL) ?
                       nativeOutputBuffer : past->past_OutputBuffer;
    }
    /*
     AddTraceMessage("Pa_CallConvertInt16: inputBuffer = ", (int) inputBuffer );
     AddTraceMessage("Pa_CallConvertInt16: outputBuffer = ", (int) outputBuffer );
    */
    /* Call user callback routine. */
    userResult = past->past_Callback(
                     inputBuffer,
                     outputBuffer,
                     past->past_FramesPerUserBuffer,
                     past->past_FrameCount,
                     past->past_UserData );

    /* Advance frame counter for timestamp. */
    past->past_FrameCount += past->past_FramesPerUserBuffer; // FIXME - should this be in here?

    /* Convert to native format if necessary. */
    if( (past->past_OutputConversionProc != NULL ) && (outputBuffer != NULL) )
    {
        (*past->past_OutputConversionProc)( outputBuffer, past->past_OutputConversionSourceStride,
            nativeOutputBuffer, past->past_OutputConversionTargetStride,
            past->past_FramesPerUserBuffer * past->past_NumOutputChannels );
    }

    return userResult;
}
