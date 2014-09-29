
/*
 * PortAudio Portable Real-Time Audio Library
 * Latest Version at: http://www.portaudio.com
 *
 * Copyright (c) 1999-2010 Phil Burk and Ross Bencina
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
#include <memory.h>
#include <math.h>
#include <string.h>

#include "portaudio.h"

#include "qa_tools.h"

#include "paqa_tools.h"
#include "audio_analyzer.h"
#include "test_audio_analyzer.h"

/** Accumulate counts for how many tests pass or fail. */
int g_testsPassed = 0;
int g_testsFailed = 0;

#define MAX_NUM_GENERATORS                   (8)
#define MAX_NUM_RECORDINGS                   (8)
#define MAX_BACKGROUND_NOISE_RMS             (0.0004)
#define LOOPBACK_DETECTION_DURATION_SECONDS  (0.8)
#define DEFAULT_FRAMES_PER_BUFFER            (0)
#define PAQA_WAIT_STREAM_MSEC                (100)
#define PAQA_TEST_DURATION                   (1.2)

// Use two separate streams instead of one full duplex stream.
#define PAQA_FLAG_TWO_STREAMS       (1<<0)
// Use bloching read/write for loopback.
#define PAQA_FLAG_USE_BLOCKING_IO   (1<<1)

const char * s_FlagOnNames[] =
{
	"Two Streams (Half Duplex)",
	"Blocking Read/Write"
};

const char * s_FlagOffNames[] =
{
	"One Stream (Full Duplex)",
	"Callback"
};


/** Parameters that describe a single test run. */
typedef struct TestParameters_s
{
	PaStreamParameters inputParameters;
	PaStreamParameters outputParameters;
	double             sampleRate;
	int                samplesPerFrame;
	int                framesPerBuffer;
	int                maxFrames;
	double             baseFrequency;
	double             amplitude;
    PaStreamFlags      streamFlags;  // paClipOff, etc
	int                flags;        // PAQA_FLAG_TWO_STREAMS, PAQA_FLAG_USE_BLOCKING_IO
} TestParameters;

typedef struct LoopbackContext_s
{
	// Generate a unique signal on each channel.
	PaQaSineGenerator  generators[MAX_NUM_GENERATORS];
	// Record each channel individually.
	PaQaRecording      recordings[MAX_NUM_RECORDINGS];
	
	// Reported by the stream after it's opened
	PaTime             streamInfoInputLatency;
	PaTime             streamInfoOutputLatency;

	// Measured at runtime.
	volatile int       callbackCount; // incremented for each callback
	volatile int       inputBufferCount; // incremented if input buffer not NULL
	int                inputUnderflowCount;
	int                inputOverflowCount;
	
	volatile int       outputBufferCount; // incremented if output buffer not NULL
	int                outputOverflowCount;
	int                outputUnderflowCount;
	
    // Measure whether input or output is lagging behind.
    volatile int       minInputOutputDelta;
    volatile int       maxInputOutputDelta;
    
	int                minFramesPerBuffer;
	int                maxFramesPerBuffer;
	int                primingCount;
	TestParameters    *test;
	volatile int       done;
} LoopbackContext;

typedef struct UserOptions_s
{
	int           sampleRate;
	int           framesPerBuffer;
	int           inputLatency;
	int           outputLatency;
	int           saveBadWaves;
	int           verbose;
	int           waveFileCount;
	const char   *waveFilePath;
	PaDeviceIndex inputDevice;
	PaDeviceIndex outputDevice;
} UserOptions;

#define BIG_BUFFER_SIZE  (sizeof(float) * 2 * 2 * 1024)
static unsigned char g_ReadWriteBuffer[BIG_BUFFER_SIZE];

#define MAX_CONVERSION_SAMPLES   (2 * 32 * 1024)
#define CONVERSION_BUFFER_SIZE  (sizeof(float) * 2 * MAX_CONVERSION_SAMPLES)
static unsigned char g_ConversionBuffer[CONVERSION_BUFFER_SIZE];

/*******************************************************************/
static int RecordAndPlaySinesCallback( const void *inputBuffer, void *outputBuffer,
						unsigned long framesPerBuffer,
						const PaStreamCallbackTimeInfo* timeInfo,
						PaStreamCallbackFlags statusFlags,
						void *userData )
{
	int i;
	LoopbackContext *loopbackContext = (LoopbackContext *) userData;
	
	
	loopbackContext->callbackCount += 1;
	if( statusFlags & paInputUnderflow ) loopbackContext->inputUnderflowCount += 1;
	if( statusFlags & paInputOverflow ) loopbackContext->inputOverflowCount += 1;
	if( statusFlags & paOutputUnderflow ) loopbackContext->outputUnderflowCount += 1;
	if( statusFlags & paOutputOverflow ) loopbackContext->outputOverflowCount += 1;
	if( statusFlags & paPrimingOutput ) loopbackContext->primingCount += 1;
	if( framesPerBuffer > loopbackContext->maxFramesPerBuffer )
	{
		loopbackContext->maxFramesPerBuffer = framesPerBuffer;
	}
	if( framesPerBuffer < loopbackContext->minFramesPerBuffer )
	{
		loopbackContext->minFramesPerBuffer = framesPerBuffer;
	}
	
    /* This may get called with NULL inputBuffer during initial setup.
	 * We may also use the same callback with output only streams.
	 */
	if( inputBuffer != NULL)
	{
		int channelsPerFrame = loopbackContext->test->inputParameters.channelCount;
		float *in = (float *)inputBuffer;
		PaSampleFormat inFormat = loopbackContext->test->inputParameters.sampleFormat;
		
		loopbackContext->inputBufferCount += 1;
		
		if( inFormat != paFloat32 )
		{
			int samplesToConvert = framesPerBuffer * channelsPerFrame;
			in = (float *) g_ConversionBuffer;
			if( samplesToConvert > MAX_CONVERSION_SAMPLES )
			{
				// Hack to prevent buffer overflow.
				// @todo Loop with small buffer instead of failing.
				printf("Format conversion buffer too small!\n");
				return paComplete;
			}
			PaQa_ConvertToFloat( inputBuffer, samplesToConvert, inFormat, (float *) g_ConversionBuffer );
		}
		
		// Read each channel from the buffer.
		for( i=0; i<channelsPerFrame; i++ )
		{			
			loopbackContext->done |= PaQa_WriteRecording( &loopbackContext->recordings[i],
										in + i, 
										framesPerBuffer,
										channelsPerFrame );
		}
	}
	
	if( outputBuffer != NULL )
	{
		int channelsPerFrame = loopbackContext->test->outputParameters.channelCount;
		float *out = (float *)outputBuffer;
		PaSampleFormat outFormat = loopbackContext->test->outputParameters.sampleFormat;
		
		loopbackContext->outputBufferCount += 1;
		
		if( outFormat != paFloat32 )
		{
			// If we need to convert then mix to the g_ConversionBuffer and then convert into the PA outputBuffer.
			out = (float *) g_ConversionBuffer;
		}
			
		PaQa_EraseBuffer( out, framesPerBuffer, channelsPerFrame );
		for( i=0; i<channelsPerFrame; i++ )
		{
			PaQa_MixSine( &loopbackContext->generators[i],
						 out + i,
						 framesPerBuffer,
						 channelsPerFrame );
		}
		
		if( outFormat != paFloat32 )
		{
			int samplesToConvert = framesPerBuffer * channelsPerFrame;
			if( samplesToConvert > MAX_CONVERSION_SAMPLES )
			{
				printf("Format conversion buffer too small!\n");
				return paComplete;
			}			
			PaQa_ConvertFromFloat( out, framesPerBuffer * channelsPerFrame, outFormat, outputBuffer );
		}
		
	}
    
    // Measure whether the input or output are lagging behind.
    // Don't measure lag at end.
    if( !loopbackContext->done )
    {        
        int inputOutputDelta = loopbackContext->inputBufferCount - loopbackContext->outputBufferCount;
        if( loopbackContext->maxInputOutputDelta < inputOutputDelta )
        {
            loopbackContext->maxInputOutputDelta = inputOutputDelta;
        }
        if( loopbackContext->minInputOutputDelta > inputOutputDelta )
        {
            loopbackContext->minInputOutputDelta = inputOutputDelta;
        }
    }
    
	return loopbackContext->done ? paComplete : paContinue;
}

static void CopyStreamInfoToLoopbackContext( LoopbackContext *loopbackContext, PaStream *inputStream, PaStream *outputStream )
{
	const PaStreamInfo *inputStreamInfo = Pa_GetStreamInfo( inputStream );
	const PaStreamInfo *outputStreamInfo = Pa_GetStreamInfo( outputStream );

	loopbackContext->streamInfoInputLatency = inputStreamInfo ? inputStreamInfo->inputLatency : -1;
	loopbackContext->streamInfoOutputLatency = outputStreamInfo ? outputStreamInfo->outputLatency : -1;
}

/*******************************************************************/
/** 
 * Open a full duplex audio stream.
 * Generate sine waves on the output channels and record the input channels.
 * Then close the stream.
 * @return 0 if OK or negative error.
 */
int PaQa_RunLoopbackFullDuplex( LoopbackContext *loopbackContext )
{
	PaStream *stream = NULL;
	PaError err = 0;
	TestParameters *test = loopbackContext->test;
	loopbackContext->done = 0;
	// Use one full duplex stream.
	err = Pa_OpenStream(
					&stream,
					&test->inputParameters,
					&test->outputParameters,
					test->sampleRate,
					test->framesPerBuffer,
					paClipOff, /* we won't output out of range samples so don't bother clipping them */
					RecordAndPlaySinesCallback,
					loopbackContext );
	if( err != paNoError ) goto error;
	
	CopyStreamInfoToLoopbackContext( loopbackContext, stream, stream );

	err = Pa_StartStream( stream );
	if( err != paNoError ) goto error;
		
	// Wait for stream to finish.
	while( loopbackContext->done == 0 )
	{
		Pa_Sleep(PAQA_WAIT_STREAM_MSEC);
	}
	
	err = Pa_StopStream( stream );
	if( err != paNoError ) goto error;

	err = Pa_CloseStream( stream );
	if( err != paNoError ) goto error;
		
	return 0;
	
error:
	return err;	
}

/*******************************************************************/
/** 
 * Open two audio streams, one for input and one for output.
 * Generate sine waves on the output channels and record the input channels.
 * Then close the stream.
 * @return 0 if OK or paTimedOut.
 */

int PaQa_WaitForStream( LoopbackContext *loopbackContext )
{
	int timeoutMSec = 1000 * PAQA_TEST_DURATION * 2;
	
	// Wait for stream to finish or timeout.
	while( (loopbackContext->done == 0) && (timeoutMSec > 0) )
	{
		Pa_Sleep(PAQA_WAIT_STREAM_MSEC);
		timeoutMSec -= PAQA_WAIT_STREAM_MSEC;
	}
	
	if( loopbackContext->done == 0 )
	{
		printf("ERROR - stream completion timed out!");
		return paTimedOut;
	}
	return 0;
}

/*******************************************************************/
/** 
 * Open two audio streams, one for input and one for output.
 * Generate sine waves on the output channels and record the input channels.
 * Then close the stream.
 * @return 0 if OK or negative error.
 */
int PaQa_RunLoopbackHalfDuplex( LoopbackContext *loopbackContext )
{
	PaStream *inStream = NULL;
	PaStream *outStream = NULL;
	PaError err = 0;
	int timedOut = 0;
	TestParameters *test = loopbackContext->test;
	loopbackContext->done = 0;
	
	// Use two half duplex streams.
	err = Pa_OpenStream(
						&inStream,
						&test->inputParameters,
						NULL,
						test->sampleRate,
						test->framesPerBuffer,
						test->streamFlags,
						RecordAndPlaySinesCallback,
						loopbackContext );
	if( err != paNoError ) goto error;
	err = Pa_OpenStream(
						&outStream,
						NULL,
						&test->outputParameters,
						test->sampleRate,
						test->framesPerBuffer,
						test->streamFlags,
						RecordAndPlaySinesCallback,
						loopbackContext );
	if( err != paNoError ) goto error;

	CopyStreamInfoToLoopbackContext( loopbackContext, inStream, outStream );

	err = Pa_StartStream( inStream );
	if( err != paNoError ) goto error;
	
	// Start output later so we catch the beginning of the waveform.
	err = Pa_StartStream( outStream );
	if( err != paNoError ) goto error;
	
	timedOut = PaQa_WaitForStream( loopbackContext );
	
	err = Pa_StopStream( inStream );
	if( err != paNoError ) goto error;
		
	err = Pa_StopStream( outStream );
	if( err != paNoError ) goto error;
	
	err = Pa_CloseStream( inStream );
	if( err != paNoError ) goto error;
	
	err = Pa_CloseStream( outStream );
	if( err != paNoError ) goto error;
	
	return timedOut;
	
error:
	return err;	
}


/*******************************************************************/
/** 
 * Open one audio streams, just for input.
 * Record background level.
 * Then close the stream.
 * @return 0 if OK or negative error.
 */
int PaQa_RunInputOnly( LoopbackContext *loopbackContext )
{
	PaStream *inStream = NULL;
	PaError err = 0;
	int timedOut = 0;
	TestParameters *test = loopbackContext->test;
	loopbackContext->done = 0;
	
	// Just open an input stream.
	err = Pa_OpenStream(
						&inStream,
						&test->inputParameters,
						NULL,
						test->sampleRate,
						test->framesPerBuffer,
						paClipOff, /* We won't output out of range samples so don't bother clipping them. */
						RecordAndPlaySinesCallback,
						loopbackContext );
	if( err != paNoError ) goto error;

	err = Pa_StartStream( inStream );
	if( err != paNoError ) goto error;
	
	timedOut = PaQa_WaitForStream( loopbackContext );
	
	err = Pa_StopStream( inStream );
	if( err != paNoError ) goto error;
	
	err = Pa_CloseStream( inStream );
	if( err != paNoError ) goto error;
	
	return timedOut;
	
error:
	return err;	
}

/*******************************************************************/
static int RecordAndPlayBlockingIO( PaStream *inStream,
									  PaStream *outStream,
									  LoopbackContext *loopbackContext
									  )
{	
	int i;
	float *in = (float *)g_ReadWriteBuffer;
	float *out = (float *)g_ReadWriteBuffer;
	PaError err;
	int done = 0;
	long available;
	const long maxPerBuffer = 64;
	TestParameters *test = loopbackContext->test;
	long framesPerBuffer = test->framesPerBuffer;
	if( framesPerBuffer <= 0 )
	{
		framesPerBuffer = maxPerBuffer; // bigger values might run past end of recording
	}
	
	// Read in audio.
	err = Pa_ReadStream( inStream, in, framesPerBuffer );
	// Ignore an overflow on the first read.
	//if( !((loopbackContext->callbackCount == 0) && (err == paInputOverflowed)) )
	if( err != paInputOverflowed )
	{
		QA_ASSERT_EQUALS( "Pa_ReadStream failed", paNoError, err );
	}
	else
	{
		loopbackContext->inputOverflowCount += 1;
	}

	
	// Save in a recording.
	for( i=0; i<loopbackContext->test->inputParameters.channelCount; i++ )
	{
		done |= PaQa_WriteRecording( &loopbackContext->recordings[i],
		         in + i,
		         framesPerBuffer,
		         loopbackContext->test->inputParameters.channelCount );
	}
	
	// Synthesize audio.
	available = Pa_GetStreamWriteAvailable( outStream );
	if( available > (2*framesPerBuffer) ) available = (2*framesPerBuffer);
	PaQa_EraseBuffer( out, available, loopbackContext->test->outputParameters.channelCount );
	for( i=0; i<loopbackContext->test->outputParameters.channelCount; i++ )
	{
		PaQa_MixSine( &loopbackContext->generators[i],
		          out + i,
		          available,
		          loopbackContext->test->outputParameters.channelCount );
	}
	
	// Write out audio.
	err = Pa_WriteStream( outStream, out, available );
	// Ignore an underflow on the first write.
	//if( !((loopbackContext->callbackCount == 0) && (err == paOutputUnderflowed)) )
	if( err != paOutputUnderflowed )
	{
		QA_ASSERT_EQUALS( "Pa_WriteStream failed", paNoError, err );
	}
	else
	{
		loopbackContext->outputUnderflowCount += 1;
	}
	
		
	loopbackContext->callbackCount += 1;
	
	return done;
error:
	return err;
}


/*******************************************************************/
/** 
 * Open two audio streams with non-blocking IO.
 * Generate sine waves on the output channels and record the input channels.
 * Then close the stream.
 * @return 0 if OK or negative error.
 */
int PaQa_RunLoopbackHalfDuplexBlockingIO( LoopbackContext *loopbackContext )
{
	PaStream *inStream = NULL;
	PaStream *outStream = NULL;
	PaError err = 0;
	TestParameters *test = loopbackContext->test;
	
	// Use two half duplex streams.
	err = Pa_OpenStream(
						&inStream,
						&test->inputParameters,
						NULL,
						test->sampleRate,
						test->framesPerBuffer,
						paClipOff, /* we won't output out of range samples so don't bother clipping them */
						NULL, // causes non-blocking IO
						NULL );
	if( err != paNoError ) goto error1;
	err = Pa_OpenStream(
						&outStream,
						NULL,
						&test->outputParameters,
						test->sampleRate,
						test->framesPerBuffer,
						paClipOff, /* we won't output out of range samples so don't bother clipping them */
						NULL, // causes non-blocking IO
						NULL );
	if( err != paNoError ) goto error2;
	
	CopyStreamInfoToLoopbackContext( loopbackContext, inStream, outStream );

	err = Pa_StartStream( outStream );
	if( err != paNoError ) goto error3;
	
	err = Pa_StartStream( inStream );
	if( err != paNoError ) goto error3;
	
	while( err == 0 )
	{
		err = RecordAndPlayBlockingIO( inStream, outStream, loopbackContext );
		if( err < 0 ) goto error3;
	}
	
	err = Pa_StopStream( inStream );
	if( err != paNoError ) goto error3;
	
	err = Pa_StopStream( outStream );
	if( err != paNoError ) goto error3;
	
	err = Pa_CloseStream( outStream );
	if( err != paNoError ) goto error2;
	
	err = Pa_CloseStream( inStream );
	if( err != paNoError ) goto error1;
	
	
	return 0;
	
error3:
	Pa_CloseStream( outStream );
error2:
	Pa_CloseStream( inStream );
error1:
	return err;	
}


/*******************************************************************/
/** 
 * Open one audio stream with non-blocking IO.
 * Generate sine waves on the output channels and record the input channels.
 * Then close the stream.
 * @return 0 if OK or negative error.
 */
int PaQa_RunLoopbackFullDuplexBlockingIO( LoopbackContext *loopbackContext )
{
	PaStream *stream = NULL;
	PaError err = 0;
	TestParameters *test = loopbackContext->test;
	
	// Use one full duplex stream.
	err = Pa_OpenStream(
						&stream,
						&test->inputParameters,
						&test->outputParameters,
						test->sampleRate,
						test->framesPerBuffer,
						paClipOff, /* we won't output out of range samples so don't bother clipping them */
						NULL, // causes non-blocking IO
						NULL );
	if( err != paNoError ) goto error1;
		
	CopyStreamInfoToLoopbackContext( loopbackContext, stream, stream );

	err = Pa_StartStream( stream );
	if( err != paNoError ) goto error2;
	
	while( err == 0 )
	{
		err = RecordAndPlayBlockingIO( stream, stream, loopbackContext );
		if( err < 0 ) goto error2;
	}
	
	err = Pa_StopStream( stream );
	if( err != paNoError ) goto error2;
	
	
	err = Pa_CloseStream( stream );
	if( err != paNoError ) goto error1;
	
	
	return 0;
	
error2:
	Pa_CloseStream( stream );
error1:
	return err;	
}


/*******************************************************************/
/** 
 * Run some kind of loopback test.
 * @return 0 if OK or negative error.
 */
int PaQa_RunLoopback( LoopbackContext *loopbackContext )
{
	PaError err = 0;
	TestParameters *test = loopbackContext->test;
	
	
	if( test->flags & PAQA_FLAG_TWO_STREAMS )
	{
		if( test->flags & PAQA_FLAG_USE_BLOCKING_IO )
		{
			err = PaQa_RunLoopbackHalfDuplexBlockingIO( loopbackContext );
		}
		else
		{
			err = PaQa_RunLoopbackHalfDuplex( loopbackContext );
		}
	}
	else
	{
		if( test->flags & PAQA_FLAG_USE_BLOCKING_IO )
		{
			err = PaQa_RunLoopbackFullDuplexBlockingIO( loopbackContext );
		}
		else
		{
			err = PaQa_RunLoopbackFullDuplex( loopbackContext );
		}
	}
	
	if( err != paNoError )
	{
		printf("PortAudio error = %s\n", Pa_GetErrorText( err ) );
	}
	return err;	
}

/*******************************************************************/
static int PaQa_SaveTestResultToWaveFile( UserOptions *userOptions, PaQaRecording *recording )
{
	if( userOptions->saveBadWaves )
	{
		char filename[256];
#ifdef WIN32
        _snprintf( filename, sizeof(filename), "%s\\paloopback_%d.wav", userOptions->waveFilePath, userOptions->waveFileCount++ );
#else
		snprintf( filename, sizeof(filename), "%s/paloopback_%d.wav", userOptions->waveFilePath, userOptions->waveFileCount++ );
#endif   
		printf( "\"%s\", ", filename );
		return PaQa_SaveRecordingToWaveFile( recording, filename );
	}
	return 0;
}

/*******************************************************************/
static int PaQa_SetupLoopbackContext( LoopbackContext *loopbackContextPtr, TestParameters *testParams )
{
	int i;
	// Setup loopback context.
	memset( loopbackContextPtr, 0, sizeof(LoopbackContext) );	
	loopbackContextPtr->test = testParams;
	for( i=0; i<testParams->samplesPerFrame; i++ )
	{
		int err = PaQa_InitializeRecording( &loopbackContextPtr->recordings[i], testParams->maxFrames, testParams->sampleRate );
		QA_ASSERT_EQUALS( "PaQa_InitializeRecording failed", paNoError, err );
	}
	for( i=0; i<testParams->samplesPerFrame; i++ )
	{
		PaQa_SetupSineGenerator( &loopbackContextPtr->generators[i], PaQa_GetNthFrequency( testParams->baseFrequency, i ),
								testParams->amplitude, testParams->sampleRate );
	}
	loopbackContextPtr->minFramesPerBuffer = 0x0FFFFFFF;
	return 0;
error:
	return -1;
}

/*******************************************************************/
static void PaQa_TeardownLoopbackContext( LoopbackContext *loopbackContextPtr )
{
	int i;
	if( loopbackContextPtr->test != NULL )
	{
		for( i=0; i<loopbackContextPtr->test->samplesPerFrame; i++ )
		{
			PaQa_TerminateRecording( &loopbackContextPtr->recordings[i] );
		}
	}
}

/*******************************************************************/
static void PaQa_PrintShortErrorReport( PaQaAnalysisResult *analysisResultPtr, int channel )
{
	printf("channel %d ", channel);
	if( analysisResultPtr->popPosition > 0 )
	{
		printf("POP %0.3f at %d, ", (double)analysisResultPtr->popAmplitude, (int)analysisResultPtr->popPosition );	
	}
	else
	{
		if( analysisResultPtr->addedFramesPosition > 0 )
		{
			printf("ADD %d at %d ", (int)analysisResultPtr->numAddedFrames, (int)analysisResultPtr->addedFramesPosition );	
		}
		
		if( analysisResultPtr->droppedFramesPosition > 0 )
		{
			printf("DROP %d at %d ", (int)analysisResultPtr->numDroppedFrames, (int)analysisResultPtr->droppedFramesPosition );	
		}
	}
}

/*******************************************************************/
static void PaQa_PrintFullErrorReport( PaQaAnalysisResult *analysisResultPtr, int channel )
{
	printf("\n=== Loopback Analysis ===================\n");
	printf("             channel: %d\n", channel );
	printf("             latency: %10.3f\n", analysisResultPtr->latency );
	printf("      amplitudeRatio: %10.3f\n", (double)analysisResultPtr->amplitudeRatio );	
	printf("         popPosition: %10.3f\n", (double)analysisResultPtr->popPosition );	
	printf("        popAmplitude: %10.3f\n", (double)analysisResultPtr->popAmplitude );
	printf("    num added frames: %10.3f\n", analysisResultPtr->numAddedFrames );
	printf("     added frames at: %10.3f\n", analysisResultPtr->addedFramesPosition );
	printf("  num dropped frames: %10.3f\n", analysisResultPtr->numDroppedFrames );
	printf("   dropped frames at: %10.3f\n", analysisResultPtr->droppedFramesPosition );
}

/*******************************************************************/
/** 
 * Test loopback connection using the given parameters.
 * @return number of channels with glitches, or negative error.
 */
static int PaQa_SingleLoopBackTest( UserOptions *userOptions, TestParameters *testParams )
{
	int i;
	LoopbackContext loopbackContext;
	PaError err = paNoError;
	PaQaTestTone testTone;
	PaQaAnalysisResult analysisResult;
	int numBadChannels = 0;
	
	printf("| %5d | %6d | ", ((int)(testParams->sampleRate+0.5)), testParams->framesPerBuffer );
	fflush(stdout);
	
	testTone.samplesPerFrame = testParams->samplesPerFrame;
	testTone.sampleRate = testParams->sampleRate;
	testTone.amplitude = testParams->amplitude;
	testTone.startDelay = 0;
	
	err = PaQa_SetupLoopbackContext( &loopbackContext, testParams );
	if( err ) return err;
	
	err = PaQa_RunLoopback( &loopbackContext );
	QA_ASSERT_TRUE("loopback did not run", (loopbackContext.callbackCount > 1) );

	printf( "%7.2f %7.2f %7.2f | ",
		   loopbackContext.streamInfoInputLatency * 1000.0,
		   loopbackContext.streamInfoOutputLatency * 1000.0,
		   (loopbackContext.streamInfoInputLatency + loopbackContext.streamInfoOutputLatency) * 1000.0
		   );

	printf( "%4d/%4d/%4d, %4d/%4d/%4d | ",
		   loopbackContext.inputOverflowCount,
		   loopbackContext.inputUnderflowCount,
		   loopbackContext.inputBufferCount,
		   loopbackContext.outputOverflowCount,
		   loopbackContext.outputUnderflowCount,
		   loopbackContext.outputBufferCount
		   );
	
	// Analyse recording to detect glitches.
	for( i=0; i<testParams->samplesPerFrame; i++ )
	{
		double freq = PaQa_GetNthFrequency( testParams->baseFrequency, i );
		testTone.frequency = freq;
		
		PaQa_AnalyseRecording(  &loopbackContext.recordings[i], &testTone, &analysisResult );
		
		if( i==0 )
		{
            double latencyMSec;

			printf( "%4d-%4d | ",
				   loopbackContext.minFramesPerBuffer,
				   loopbackContext.maxFramesPerBuffer
				   );
			
			latencyMSec = 1000.0 * analysisResult.latency / testParams->sampleRate;
			printf("%7.2f | ", latencyMSec );
						
		}
		
		if( analysisResult.valid )
		{
			int badChannel = ( (analysisResult.popPosition > 0)
					   || (analysisResult.addedFramesPosition > 0)
					   || (analysisResult.droppedFramesPosition > 0) );
			
			if( badChannel )
			{	
				if( userOptions->verbose )
				{
					PaQa_PrintFullErrorReport( &analysisResult, i );
				}
				else
				{
					PaQa_PrintShortErrorReport( &analysisResult, i );
				}
				PaQa_SaveTestResultToWaveFile( userOptions, &loopbackContext.recordings[i] );
			}
			numBadChannels += badChannel;
		}
		else
		{
			printf( "[%d] No or low signal, ampRatio = %f", i, analysisResult.amplitudeRatio );
			numBadChannels += 1;
		}

	}
	if( numBadChannels == 0 )
	{
		printf( "OK" );
	}
    
	printf( "\n" );
    
				
	PaQa_TeardownLoopbackContext( &loopbackContext );
	if( numBadChannels > 0 )
	{
		g_testsFailed += 1;
	}
	return numBadChannels;	
	
error:
	PaQa_TeardownLoopbackContext( &loopbackContext );
	printf( "\n" );
	g_testsFailed += 1;
	return err;	
}

/*******************************************************************/
static void PaQa_SetDefaultTestParameters( TestParameters *testParamsPtr, PaDeviceIndex inputDevice, PaDeviceIndex outputDevice )
{
	memset( testParamsPtr, 0, sizeof(TestParameters) );
	
	testParamsPtr->samplesPerFrame = 2;
	testParamsPtr->amplitude = 0.5;
	testParamsPtr->sampleRate = 44100;
	testParamsPtr->maxFrames = (int) (PAQA_TEST_DURATION * testParamsPtr->sampleRate);
	testParamsPtr->framesPerBuffer = DEFAULT_FRAMES_PER_BUFFER;
	testParamsPtr->baseFrequency = 200.0;
	testParamsPtr->flags = PAQA_FLAG_TWO_STREAMS;
    testParamsPtr->streamFlags = paClipOff; /* we won't output out of range samples so don't bother clipping them */
	
	testParamsPtr->inputParameters.device = inputDevice;
	testParamsPtr->inputParameters.sampleFormat = paFloat32;
	testParamsPtr->inputParameters.channelCount = testParamsPtr->samplesPerFrame;
	testParamsPtr->inputParameters.suggestedLatency = Pa_GetDeviceInfo( inputDevice )->defaultLowInputLatency;
	//testParamsPtr->inputParameters.suggestedLatency = Pa_GetDeviceInfo( inputDevice )->defaultHighInputLatency;
	
	testParamsPtr->outputParameters.device = outputDevice;
	testParamsPtr->outputParameters.sampleFormat = paFloat32;
	testParamsPtr->outputParameters.channelCount = testParamsPtr->samplesPerFrame;
	testParamsPtr->outputParameters.suggestedLatency = Pa_GetDeviceInfo( outputDevice )->defaultLowOutputLatency;
	//testParamsPtr->outputParameters.suggestedLatency = Pa_GetDeviceInfo( outputDevice )->defaultHighOutputLatency;
}

/*******************************************************************/
static void PaQa_OverrideTestParameters( TestParameters *testParamsPtr,  UserOptions *userOptions )
{
	// Check to see if a specific value was requested.
	if( userOptions->sampleRate >= 0 )
	{
		testParamsPtr->sampleRate = userOptions->sampleRate;
		testParamsPtr->maxFrames = (int) (PAQA_TEST_DURATION * testParamsPtr->sampleRate);
	}
	if( userOptions->framesPerBuffer >= 0 )
	{
		testParamsPtr->framesPerBuffer = userOptions->framesPerBuffer;
	}
	if( userOptions->inputLatency >= 0 )
	{
		testParamsPtr->inputParameters.suggestedLatency = userOptions->inputLatency * 0.001;
	}
	if( userOptions->outputLatency >= 0 )
	{
		testParamsPtr->outputParameters.suggestedLatency = userOptions->outputLatency * 0.001;
	}
	printf( "   Running with suggested latency (msec): input = %5.2f, out = %5.2f\n",
		(testParamsPtr->inputParameters.suggestedLatency * 1000.0),
		(testParamsPtr->outputParameters.suggestedLatency * 1000.0) );
}

/*******************************************************************/
/** 
 * Run a series of tests on this loopback connection.
 * @return number of bad channel results
 */
static int PaQa_AnalyzeLoopbackConnection( UserOptions *userOptions, PaDeviceIndex inputDevice, PaDeviceIndex outputDevice )
{
	int iFlags;
	int iRate;
	int iSize;
	int iFormat;
	int savedValue;
	TestParameters testParams;
	const PaDeviceInfo *inputDeviceInfo = Pa_GetDeviceInfo( inputDevice );	
	const PaDeviceInfo *outputDeviceInfo = Pa_GetDeviceInfo( outputDevice );		
    int totalBadChannels = 0;

	// test half duplex first because it is more likely to work.
	int flagSettings[] = { PAQA_FLAG_TWO_STREAMS, 0 };
	int numFlagSettings = (sizeof(flagSettings)/sizeof(int));

	double sampleRates[] = { 8000.0, 11025.0, 16000.0, 22050.0, 32000.0, 44100.0, 48000.0, 96000.0 };
	int numRates = (sizeof(sampleRates)/sizeof(double));
	
	// framesPerBuffer==0 means PA decides on the buffer size.
	int framesPerBuffers[] = { 0, 16, 32, 40, 64, 100, 128, 256, 512, 1024 };
	int numBufferSizes = (sizeof(framesPerBuffers)/sizeof(int));
	
	PaSampleFormat sampleFormats[] = { paFloat32, paUInt8, paInt8, paInt16, paInt32 };
	const char *sampleFormatNames[] = { "paFloat32", "paUInt8", "paInt8", "paInt16", "paInt32" };
	int numSampleFormats = (sizeof(sampleFormats)/sizeof(PaSampleFormat));
	
    printf( "=============== Analysing Loopback %d to %d =====================\n", outputDevice, inputDevice  );
	printf( "   Devices: %s => %s\n", outputDeviceInfo->name, inputDeviceInfo->name);
	
	PaQa_SetDefaultTestParameters( &testParams, inputDevice, outputDevice );
	
	PaQa_OverrideTestParameters( &testParams, userOptions );
	
	// Loop though combinations of audio parameters.
	for( iFlags=0; iFlags<numFlagSettings; iFlags++ )
	{
		int numRuns = 0;

		testParams.flags = flagSettings[iFlags];
		printf( "\n************ Mode = %s ************\n",
			   (( testParams.flags & 1 ) ? s_FlagOnNames[0] : s_FlagOffNames[0]) );

		printf("|-   requested  -|-  stream info latency  -|- measured ------------------------------\n");
		printf("|-sRate-|-fr/buf-|- in    - out   - total -|- over/under/calls for in, out -|- frm/buf -|-latency-|- channel results -\n");

		// Loop though various sample rates.
		if( userOptions->sampleRate < 0 )
		{
			savedValue = testParams.sampleRate;
			for( iRate=0; iRate<numRates; iRate++ )
			{
                int numBadChannels;

				// SAMPLE RATE
				testParams.sampleRate = sampleRates[iRate];
				testParams.maxFrames = (int) (PAQA_TEST_DURATION * testParams.sampleRate);
				
				numBadChannels = PaQa_SingleLoopBackTest( userOptions, &testParams );
				totalBadChannels += numBadChannels;
			}
			testParams.sampleRate = savedValue;
			testParams.maxFrames = (int) (PAQA_TEST_DURATION * testParams.sampleRate);
			printf( "\n" );
			numRuns += 1;
		}
		
		// Loop through various buffer sizes.
		if( userOptions->framesPerBuffer < 0 )
		{
			savedValue = testParams.framesPerBuffer;
			for( iSize=0; iSize<numBufferSizes; iSize++ )
			{	
                int numBadChannels;

				// BUFFER SIZE
				testParams.framesPerBuffer = framesPerBuffers[iSize];
				
				numBadChannels = PaQa_SingleLoopBackTest( userOptions, &testParams );
				totalBadChannels += numBadChannels;			
			}
			testParams.framesPerBuffer = savedValue;
			printf( "\n" );		
			numRuns += 1;
		}
		// Run one with single parameters in case we did not do a series.
		if( numRuns == 0 )
		{
			int numBadChannels = PaQa_SingleLoopBackTest( userOptions, &testParams );
			totalBadChannels += numBadChannels;						
		}
	}
			
	printf("\nTest Sample Formats using Half Duplex IO -----\n" );
    
	PaQa_SetDefaultTestParameters( &testParams, inputDevice, outputDevice );
	testParams.flags = PAQA_FLAG_TWO_STREAMS;	
    for( iFlags= 0; iFlags<4; iFlags++ )
    {
        // Cycle through combinations of flags.
        testParams.streamFlags = 0;
        if( iFlags & 1 ) testParams.streamFlags |= paClipOff;
        if( iFlags & 2 ) testParams.streamFlags |= paDitherOff;
        
        for( iFormat=0; iFormat<numSampleFormats; iFormat++ )
        {	
            int numBadChannels;
            PaSampleFormat format = sampleFormats[ iFormat ];
            testParams.inputParameters.sampleFormat = format;
            testParams.outputParameters.sampleFormat = format;
            printf("Sample format = %d = %s, PaStreamFlags = 0x%02X\n", (int) format, sampleFormatNames[iFormat], (unsigned int) testParams.streamFlags );
            numBadChannels = PaQa_SingleLoopBackTest( userOptions, &testParams );
            totalBadChannels += numBadChannels;			
        }
    }
	printf( "\n" );
	printf( "****************************************\n");
	
	return totalBadChannels;
}

/*******************************************************************/
int PaQa_CheckForClippedLoopback( LoopbackContext *loopbackContextPtr )
{
	int clipped = 0;
	TestParameters *testParamsPtr = loopbackContextPtr->test;
	
	// Start in the middle assuming past latency.
	int startFrame = testParamsPtr->maxFrames/2;
	int numFrames = testParamsPtr->maxFrames/2;
	
	// Check to see if the signal is clipped.
	double amplitudeLeft = PaQa_MeasureSineAmplitudeBySlope( &loopbackContextPtr->recordings[0],
															testParamsPtr->baseFrequency, testParamsPtr->sampleRate,
															startFrame, numFrames );
	double gainLeft = amplitudeLeft / testParamsPtr->amplitude;
	double amplitudeRight = PaQa_MeasureSineAmplitudeBySlope( &loopbackContextPtr->recordings[1],
															 testParamsPtr->baseFrequency, testParamsPtr->sampleRate,
															 startFrame, numFrames );
	double gainRight = amplitudeLeft / testParamsPtr->amplitude;
	printf("   Loop gain: left = %f, right = %f\n", gainLeft, gainRight );

	if( (amplitudeLeft > 1.0 ) || (amplitudeRight > 1.0) )
	{
		printf("ERROR - loop gain is too high. Should be around than 1.0. Please lower output level and/or input gain.\n" );
		clipped = 1;
	}
	return clipped;
}

/*******************************************************************/
int PaQa_MeasureBackgroundNoise( LoopbackContext *loopbackContextPtr, double *rmsPtr )
{
	int result = 0;
	*rmsPtr = 0.0;
	// Rewind so we can record some input.
	loopbackContextPtr->recordings[0].numFrames = 0;
	loopbackContextPtr->recordings[1].numFrames = 0;
	result = PaQa_RunInputOnly( loopbackContextPtr );
	if( result == 0 )
	{
		double leftRMS = PaQa_MeasureRootMeanSquare( loopbackContextPtr->recordings[0].buffer,
													loopbackContextPtr->recordings[0].numFrames );
		double rightRMS = PaQa_MeasureRootMeanSquare( loopbackContextPtr->recordings[1].buffer,
													 loopbackContextPtr->recordings[1].numFrames );
		*rmsPtr = (leftRMS + rightRMS) / 2.0;
	}
	return result;
}

/*******************************************************************/
/** 
 * Output a sine wave then try to detect it on input.
 *
 * @return 1 if loopback connected, 0 if not, or negative error.
 */
int PaQa_CheckForLoopBack( UserOptions *userOptions, PaDeviceIndex inputDevice, PaDeviceIndex outputDevice )
{
	TestParameters testParams;
	LoopbackContext loopbackContext;
    const PaDeviceInfo *inputDeviceInfo;	
    const PaDeviceInfo *outputDeviceInfo;		
	PaError err = paNoError;
	double minAmplitude;
	int loopbackIsConnected;
    int startFrame, numFrames;
    double magLeft, magRight;

	inputDeviceInfo = Pa_GetDeviceInfo( inputDevice );
	if( inputDeviceInfo == NULL )
	{
		printf("ERROR - Pa_GetDeviceInfo for input returned NULL.\n");
		return paInvalidDevice;
	}
	if( inputDeviceInfo->maxInputChannels < 2 )
	{
		return 0;
	}
	
	outputDeviceInfo = Pa_GetDeviceInfo( outputDevice );
	if( outputDeviceInfo == NULL )
	{
		printf("ERROR - Pa_GetDeviceInfo for output returned NULL.\n");
		return paInvalidDevice;
	}
	if( outputDeviceInfo->maxOutputChannels < 2 )
	{
		return 0;
	}
	
	printf( "Look for loopback cable between \"%s\" => \"%s\"\n", outputDeviceInfo->name, inputDeviceInfo->name);
	
	printf( "   Default suggested input latency (msec): low = %5.2f, high = %5.2f\n",
		(inputDeviceInfo->defaultLowInputLatency * 1000.0),
		(inputDeviceInfo->defaultHighInputLatency * 1000.0) );
	printf( "   Default suggested output latency (msec): low = %5.2f, high = %5.2f\n",
		(outputDeviceInfo->defaultLowOutputLatency * 1000.0),
		(outputDeviceInfo->defaultHighOutputLatency * 1000.0) );
		
	PaQa_SetDefaultTestParameters( &testParams, inputDevice, outputDevice );
	
	PaQa_OverrideTestParameters( &testParams, userOptions );
	
	testParams.maxFrames = (int) (LOOPBACK_DETECTION_DURATION_SECONDS * testParams.sampleRate);	
	minAmplitude = testParams.amplitude / 4.0;
	
	// Check to see if the selected formats are supported.
	if( Pa_IsFormatSupported( &testParams.inputParameters, NULL, testParams.sampleRate ) != paFormatIsSupported )
	{
		printf( "Input not supported for this format!\n" );
		return 0;
	}
	if( Pa_IsFormatSupported( NULL, &testParams.outputParameters, testParams.sampleRate ) != paFormatIsSupported )
	{
		printf( "Output not supported for this format!\n" );
		return 0;
	}
	
	PaQa_SetupLoopbackContext( &loopbackContext, &testParams );
			
	if( inputDevice == outputDevice )
	{
		// Use full duplex if checking for loopback on one device.
		testParams.flags &= ~PAQA_FLAG_TWO_STREAMS;
	}
	else
	{
		// Use half duplex if checking for loopback on two different device.
		testParams.flags = PAQA_FLAG_TWO_STREAMS;
	}
	err = PaQa_RunLoopback( &loopbackContext );
	QA_ASSERT_TRUE("loopback detection callback did not run", (loopbackContext.callbackCount > 1) );
	
	// Analyse recording to see if we captured the output.
	// Start in the middle assuming past latency.
	startFrame = testParams.maxFrames/2;
	numFrames = testParams.maxFrames/2;
	magLeft = PaQa_CorrelateSine( &loopbackContext.recordings[0],
									loopbackContext.generators[0].frequency,
									testParams.sampleRate,
									startFrame, numFrames, NULL );
	magRight = PaQa_CorrelateSine( &loopbackContext.recordings[1],
									loopbackContext.generators[1].frequency,
									testParams.sampleRate,
									startFrame, numFrames, NULL );
	printf("   Amplitudes: left = %f, right = %f\n", magLeft, magRight );
	
	// Check for backwards cable.
    loopbackIsConnected = ((magLeft > minAmplitude) && (magRight > minAmplitude));

	if( !loopbackIsConnected )
	{
		double magLeftReverse = PaQa_CorrelateSine( &loopbackContext.recordings[0],
												   loopbackContext.generators[1].frequency,
												   testParams.sampleRate,
												   startFrame, numFrames, NULL );
		
		double magRightReverse = PaQa_CorrelateSine( &loopbackContext.recordings[1], 
													loopbackContext.generators[0].frequency,
													testParams.sampleRate,
													startFrame, numFrames, NULL );
		
		if ((magLeftReverse > minAmplitude) && (magRightReverse>minAmplitude))
		{
			printf("ERROR - You seem to have the left and right channels swapped on the loopback cable!\n");
		}
	}
	else
	{
		double rms = 0.0;
		if( PaQa_CheckForClippedLoopback( &loopbackContext ) )
		{
			// Clipped so don't use this loopback.
			loopbackIsConnected = 0;
		}
		
		err = PaQa_MeasureBackgroundNoise( &loopbackContext, &rms );
		printf("   Background noise = %f\n", rms );
		if( err )
		{
			printf("ERROR - Could not measure background noise on this input!\n");
			loopbackIsConnected = 0;
		}
		else if( rms > MAX_BACKGROUND_NOISE_RMS )
		{			
			printf("ERROR - There is too much background noise on this input!\n");
			loopbackIsConnected = 0;
		}
	}
	
	PaQa_TeardownLoopbackContext( &loopbackContext );
	return loopbackIsConnected;	
	
error:
	PaQa_TeardownLoopbackContext( &loopbackContext );
	return err;	
}

/*******************************************************************/
/**
 * If there is a loopback connection then run the analysis.
 */
static int CheckLoopbackAndScan( UserOptions *userOptions,
								PaDeviceIndex iIn, PaDeviceIndex iOut )
{
	int loopbackConnected = PaQa_CheckForLoopBack( userOptions, iIn, iOut );
	if( loopbackConnected > 0 )
	{
		PaQa_AnalyzeLoopbackConnection( userOptions, iIn, iOut );
		return 1;
	}
	return 0;
}
								
/*******************************************************************/
/**
 * Scan every combination of output to input device.
 * If a loopback is found the analyse the combination.
 * The scan can be overriden using the -i and -o command line options.
 */
static int ScanForLoopback(UserOptions *userOptions)
{
	PaDeviceIndex iIn,iOut;
	int  numLoopbacks = 0;
    int  numDevices;
    numDevices = Pa_GetDeviceCount();    
		
	// If both devices are specified then just use that combination.
	if ((userOptions->inputDevice >= 0) && (userOptions->outputDevice >= 0))
	{
		numLoopbacks += CheckLoopbackAndScan( userOptions, userOptions->inputDevice, userOptions->outputDevice );
	}
	else if (userOptions->inputDevice >= 0)
	{
		// Just scan for output.
		for( iOut=0; iOut<numDevices; iOut++ )
		{					
			numLoopbacks += CheckLoopbackAndScan( userOptions, userOptions->inputDevice, iOut );
		}
	}
	else if (userOptions->outputDevice >= 0)
	{
		// Just scan for input.
		for( iIn=0; iIn<numDevices; iIn++ )
		{					
			numLoopbacks += CheckLoopbackAndScan( userOptions, iIn, userOptions->outputDevice );
		}
	}
	else
	{	
		// Scan both.
		for( iOut=0; iOut<numDevices; iOut++ )
		{
			for( iIn=0; iIn<numDevices; iIn++ )
			{				
				numLoopbacks += CheckLoopbackAndScan( userOptions, iIn, iOut );
			}
		}
	}
	QA_ASSERT_TRUE( "No good loopback cable found.", (numLoopbacks > 0) );
	return numLoopbacks;
	
error:
	return -1;
}

/*==========================================================================================*/
int TestSampleFormatConversion( void )
{
	int i;
	const float floatInput[] = { 1.0, 0.5, -0.5, -1.0 };
	
	const char charInput[] = { 127, 64, -64, -128 };
	const unsigned char ucharInput[] = { 255, 128+64, 64, 0 };
	const short shortInput[] = { 32767, 32768/2, -32768/2, -32768 };
	const int intInput[] = { 2147483647, 2147483647/2, -1073741824 /*-2147483648/2 doesn't work in msvc*/, -2147483648 };
	
	float floatOutput[4];
	short shortOutput[4];
	int intOutput[4];	
	unsigned char ucharOutput[4];
	char charOutput[4];
	
	QA_ASSERT_EQUALS("int must be 32-bit", 4, (int) sizeof(int) );
	QA_ASSERT_EQUALS("short must be 16-bit", 2, (int) sizeof(short) );
	
	// from Float ======
	PaQa_ConvertFromFloat( floatInput, 4, paUInt8, ucharOutput );
	for( i=0; i<4; i++ )
	{
		QA_ASSERT_CLOSE( "paFloat32 -> paUInt8 -> error", ucharInput[i], ucharOutput[i], 1 );
	}
	
	PaQa_ConvertFromFloat( floatInput, 4, paInt8, charOutput );
	for( i=0; i<4; i++ )
	{
		QA_ASSERT_CLOSE( "paFloat32 -> paInt8 -> error", charInput[i], charOutput[i], 1 );
	}
	
	PaQa_ConvertFromFloat( floatInput, 4, paInt16, shortOutput );
	for( i=0; i<4; i++ )
	{
		QA_ASSERT_CLOSE( "paFloat32 -> paInt16 error", shortInput[i], shortOutput[i], 1 );
	}
		
	PaQa_ConvertFromFloat( floatInput, 4, paInt32, intOutput );
	for( i=0; i<4; i++ )
	{
		QA_ASSERT_CLOSE( "paFloat32 -> paInt32 error", intInput[i], intOutput[i], 0x00010000 );
	}
	
	
	// to Float ======
	memset( floatOutput, 0, sizeof(floatOutput) );
	PaQa_ConvertToFloat( ucharInput, 4, paUInt8, floatOutput );
	for( i=0; i<4; i++ )
	{
		QA_ASSERT_CLOSE( "paUInt8 -> paFloat32 error", floatInput[i], floatOutput[i], 0.01 );
	}
	
	memset( floatOutput, 0, sizeof(floatOutput) );
	PaQa_ConvertToFloat( charInput, 4, paInt8, floatOutput );
	for( i=0; i<4; i++ )
	{
		QA_ASSERT_CLOSE( "paInt8 -> paFloat32 error", floatInput[i], floatOutput[i], 0.01 );
	}
	
	memset( floatOutput, 0, sizeof(floatOutput) );
	PaQa_ConvertToFloat( shortInput, 4, paInt16, floatOutput );
	for( i=0; i<4; i++ )
	{
		QA_ASSERT_CLOSE( "paInt16 -> paFloat32 error", floatInput[i], floatOutput[i], 0.001 );
	}
	
	memset( floatOutput, 0, sizeof(floatOutput) );
	PaQa_ConvertToFloat( intInput, 4, paInt32, floatOutput );
	for( i=0; i<4; i++ )
	{
		QA_ASSERT_CLOSE( "paInt32 -> paFloat32 error", floatInput[i], floatOutput[i], 0.00001 );
	}
	
	return 0;
	
error:
	return -1;
}


/*******************************************************************/
void usage( const char *name )
{
	printf("%s [-i# -o# -l# -r# -s# -m -w -dDir]\n", name);
	printf("  -i# - Input device ID. Will scan for loopback cable if not specified.\n");
	printf("  -o# - Output device ID. Will scan for loopback if not specified.\n");
	printf("  -l# - Latency for both input and output in milliseconds.\n");
	printf("  --inputLatency # Input latency in milliseconds.\n");	
	printf("  --outputLatency # Output latency in milliseconds.\n");
	printf("  -r# - Sample Rate in Hz.  Will use multiple common rates if not specified.\n");
	printf("  -s# - Size of callback buffer in frames, framesPerBuffer. Will use common values if not specified.\n");
	printf("  -w  - Save bad recordings in a WAV file.\n");
	printf("  -dDir - Path for Directory for WAV files. Default is current directory.\n");
	printf("  -m  - Just test the DSP Math code and not the audio devices.\n");
	printf("  -v  - Verbose reports.\n");
}

/*******************************************************************/
int main( int argc, char **argv )
{
    int i;
	UserOptions userOptions;
	int result = 0;
	int justMath = 0;
    char *executableName = argv[0];

	printf("PortAudio LoopBack Test built " __DATE__ " at " __TIME__ "\n");

	if( argc > 1 ){
		printf("running with arguments:");
		for(i=1; i < argc; ++i )
			printf(" %s", argv[i] );
		printf("\n");
	}else{
		printf("running with no arguments\n");
	}
	
	memset(&userOptions, 0, sizeof(userOptions));
	userOptions.inputDevice = paNoDevice;
	userOptions.outputDevice = paNoDevice;
	userOptions.sampleRate = -1;
	userOptions.framesPerBuffer = -1;
	userOptions.inputLatency = -1;
	userOptions.outputLatency = -1;
	userOptions.waveFilePath = ".";
	
	// Process arguments. Skip name of executable.
	i = 1;
	while( i<argc )
	{
		char *arg = argv[i];
		if( arg[0] == '-' )
		{
			switch(arg[1])
			{
				case 'i':
					userOptions.inputDevice = atoi(&arg[2]);
					break;
				case 'o':
					userOptions.outputDevice = atoi(&arg[2]);
					break;
				case 'l':
					userOptions.inputLatency = userOptions.outputLatency = atoi(&arg[2]);
					break;
				case 'r':
					userOptions.sampleRate = atoi(&arg[2]);
					break;
				case 's':
					userOptions.framesPerBuffer = atoi(&arg[2]);
					break;
					
				case 'm':
					printf("Option -m set so just testing math and not the audio devices.\n");
					justMath = 1;
					break;
					
				case 'w':
					userOptions.saveBadWaves = 1;
					break;
				case 'd':
					userOptions.waveFilePath = &arg[2];
					break;
					
				case 'v':
					userOptions.verbose = 1;
					break;
					
				case 'h':
					usage( executableName );
					exit(0);
					break;
					
				case '-':
				{
					if( strcmp( &arg[2], "inputLatency" ) == 0 )
					{
						i += 1;
						userOptions.inputLatency = atoi(argv[i]);
					}
					else if( strcmp( &arg[2], "outputLatency" ) == 0 )
					{
						i += 1;
						userOptions.outputLatency = atoi(argv[i]);					
					}
					else
					{
						printf("Illegal option: %s\n", arg);
						usage( executableName );
						exit(1);
					}

				}
					break;
					
					
				default:
					printf("Illegal option: %s\n", arg);
					usage( executableName );
					exit(1);
					break;
			}
		}
		else
		{
			printf("Illegal argument: %s\n", arg);
			usage( executableName );
			exit(1);

		}
		i += 1;
	}
		
	result = PaQa_TestAnalyzer();
	
	// Test sample format conversion tool.
	result = TestSampleFormatConversion();
	
	if( (result == 0) && (justMath == 0) )
	{
		Pa_Initialize();
		printf( "PortAudio version number = %d\nPortAudio version text = '%s'\n",
			   Pa_GetVersion(), Pa_GetVersionText() );
		printf( "=============== PortAudio Devices ========================\n" );
		PaQa_ListAudioDevices();
        if( Pa_GetDeviceCount() == 0 )
            printf( "no devices found.\n" );
        
		printf( "=============== Detect Loopback ==========================\n" );
		ScanForLoopback(&userOptions);
 
		Pa_Terminate();
	}

	if (g_testsFailed == 0)
	{
		printf("PortAudio QA SUCCEEDED! %d tests passed, %d tests failed\n", g_testsPassed, g_testsFailed );
		return 0;

	}
	else
	{
		printf("PortAudio QA FAILED! %d tests passed, %d tests failed\n", g_testsPassed, g_testsFailed );
		return 1;
	}	
}
