
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
#include <math.h>
#include "qa_tools.h"
#include "audio_analyzer.h"
#include "test_audio_analyzer.h"
#include "write_wav.h"
#include "biquad_filter.h"

#define FRAMES_PER_BLOCK  (64)
#define PRINT_REPORTS  0

#define TEST_SAVED_WAVE  (0)

/*==========================================================================================*/
/**
 * Detect a single tone.
 */
static int TestSingleMonoTone( void )
{
	int result = 0;
	PaQaSineGenerator generator;
	PaQaRecording     recording;
	float buffer[FRAMES_PER_BLOCK];
	double sampleRate = 44100.0;
	int maxFrames = ((int)sampleRate) * 1;
	int samplesPerFrame = 1;
	int stride = 1;
    int done = 0;

	double freq = 234.5;
	double amp = 0.5;
	
    double mag1, mag2;

	// Setup a sine oscillator.
	PaQa_SetupSineGenerator( &generator, freq, amp, sampleRate );
	
	result = PaQa_InitializeRecording( &recording, maxFrames, (int) sampleRate );
	QA_ASSERT_EQUALS( "PaQa_InitializeRecording failed", 0, result );

	done = 0;
	while (!done)
	{
		PaQa_EraseBuffer( buffer, FRAMES_PER_BLOCK, samplesPerFrame );
		PaQa_MixSine( &generator, buffer, FRAMES_PER_BLOCK, stride );
		done = PaQa_WriteRecording( &recording, buffer, FRAMES_PER_BLOCK, samplesPerFrame );
	}
	
	mag1 = PaQa_CorrelateSine( &recording, freq, sampleRate, 0, recording.numFrames, NULL );
	QA_ASSERT_CLOSE( "exact frequency match", amp, mag1, 0.01 );
	
	mag2 = PaQa_CorrelateSine( &recording, freq * 1.23, sampleRate, 0, recording.numFrames, NULL );
	QA_ASSERT_CLOSE( "wrong frequency", 0.0, mag2, 0.01 );
	
	PaQa_TerminateRecording( &recording );
	return 0;
		
error:
	PaQa_TerminateRecording( &recording);	
	return 1;
	
}

/*==========================================================================================*/
/**
 * Mix multiple tones and then detect them.
 */

static int TestMixedMonoTones( void )
{
	int i;
	int result = 0;
#define NUM_TONES (5)
	PaQaSineGenerator generators[NUM_TONES];
	PaQaRecording     recording;
	float buffer[FRAMES_PER_BLOCK];
	double sampleRate = 44100.0;
	int maxFrames = ((int)sampleRate) * 1;
	int samplesPerFrame = 1;
	
	double baseFreq = 234.5;
	double amp = 0.1;
	
    double mag2;

    int stride = samplesPerFrame;
	int done = 0;

	// Setup a sine oscillator.
	for( i=0; i<NUM_TONES; i++ )
	{
		PaQa_SetupSineGenerator( &generators[i], PaQa_GetNthFrequency( baseFreq, i ), amp, sampleRate );
	}
	
	result = PaQa_InitializeRecording( &recording, maxFrames, (int) sampleRate );
	QA_ASSERT_EQUALS( "PaQa_InitializeRecording failed", 0, result );
	
	done = 0;
	while (!done)
	{
		PaQa_EraseBuffer( buffer, FRAMES_PER_BLOCK, samplesPerFrame );
		for( i=0; i<NUM_TONES; i++ )
		{
			PaQa_MixSine( &generators[i], buffer, FRAMES_PER_BLOCK, stride );
		}
		done = PaQa_WriteRecording( &recording, buffer, FRAMES_PER_BLOCK, samplesPerFrame );
	}
	
	for( i=0; i<NUM_TONES; i++ )
	{
		double mag = PaQa_CorrelateSine( &recording, PaQa_GetNthFrequency( baseFreq, i), sampleRate, 0, recording.numFrames, NULL );
		QA_ASSERT_CLOSE( "exact frequency match", amp, mag, 0.01 );
	}
			
	mag2 = PaQa_CorrelateSine( &recording, baseFreq * 0.87, sampleRate, 0, recording.numFrames, NULL );
	QA_ASSERT_CLOSE( "wrong frequency", 0.0, mag2, 0.01 );
	
	PaQa_TerminateRecording( &recording );
	return 0;
	
error:
	PaQa_TerminateRecording( &recording);	
	return 1;
	
}


/*==========================================================================================*/
/**
 * Generate a recording with added or dropped frames.
 */

static void MakeRecordingWithAddedFrames( PaQaRecording *recording, PaQaTestTone *testTone, int glitchPosition, int framesToAdd )
{
	PaQaSineGenerator generator;
#define BUFFER_SIZE 512
	float buffer[BUFFER_SIZE];
	
    int frameCounter = testTone->startDelay;
	
	int stride = 1;
	// Record some initial silence.
	int done = PaQa_WriteSilence( recording, testTone->startDelay );
	
    // Setup a sine oscillator.
	PaQa_SetupSineGenerator( &generator, testTone->frequency, testTone->amplitude, testTone->sampleRate );
	
	while (!done)
	{
		int framesThisLoop = BUFFER_SIZE;
		
		if( frameCounter == glitchPosition )
		{
			if( framesToAdd > 0 )
			{
				// Record some frozen data without advancing the sine generator.
				done = PaQa_RecordFreeze( recording, framesToAdd );
				frameCounter += framesToAdd;
			}
			else if( framesToAdd < 0 )
			{
				// Advance sine generator a few frames.
				PaQa_MixSine( &generator, buffer, 0 - framesToAdd, stride );
			}

		}
		else if( (frameCounter < glitchPosition) && ((frameCounter + framesThisLoop) > glitchPosition) )
		{
			// Go right up to the glitchPosition.
			framesThisLoop = glitchPosition - frameCounter;
		}
		
		if( framesThisLoop > 0 )
		{
			PaQa_EraseBuffer( buffer, framesThisLoop, testTone->samplesPerFrame );
			PaQa_MixSine( &generator, buffer, framesThisLoop, stride );
			done = PaQa_WriteRecording( recording, buffer, framesThisLoop, testTone->samplesPerFrame );
		}
		frameCounter += framesThisLoop;
	}
}


/*==========================================================================================*/
/**
 * Generate a clean recording.
 */

static void MakeCleanRecording( PaQaRecording *recording, PaQaTestTone *testTone )
{
	PaQaSineGenerator generator;
#define BUFFER_SIZE 512
	float buffer[BUFFER_SIZE];
	
	int stride = 1;
	// Record some initial silence.
	int done = PaQa_WriteSilence( recording, testTone->startDelay );
	
	// Setup a sine oscillator.
	PaQa_SetupSineGenerator( &generator, testTone->frequency, testTone->amplitude, testTone->sampleRate );
	
	// Generate recording with good phase.
	while (!done)
	{
		PaQa_EraseBuffer( buffer, BUFFER_SIZE, testTone->samplesPerFrame );
		PaQa_MixSine( &generator, buffer, BUFFER_SIZE, stride );
		done = PaQa_WriteRecording( recording, buffer, BUFFER_SIZE, testTone->samplesPerFrame );
	}
}

/*==========================================================================================*/
/**
 * Generate a recording with pop.
 */

static void MakeRecordingWithPop( PaQaRecording *recording, PaQaTestTone *testTone, int popPosition, int popWidth, double popAmplitude )
{
	int i;
	
	MakeCleanRecording( recording, testTone );
	
	// Apply glitch to good recording.
	if( (popPosition + popWidth) >= recording->numFrames )
	{
		popWidth = (recording->numFrames - popPosition) - 1;
	}
	
	for( i=0; i<popWidth; i++ )
	{
		float good = recording->buffer[i+popPosition];
		float bad = (good > 0.0) ? (good - popAmplitude) : (good + popAmplitude);
		recording->buffer[i+popPosition] = bad;
	}
}

/*==========================================================================================*/
/**
 * Detect one phase error in a recording.
 */
static int TestDetectSinglePhaseError( double sampleRate, int cycleSize, int latencyFrames, int glitchPosition, int framesAdded )
{
	int result = 0;
	PaQaRecording     recording;	
	PaQaTestTone testTone;
	PaQaAnalysisResult analysisResult = { 0.0 };
	int framesDropped = 0;
    int maxFrames = ((int)sampleRate) * 2;

	testTone.samplesPerFrame = 1;
	testTone.sampleRate = sampleRate;
	testTone.frequency = sampleRate / cycleSize;
	testTone.amplitude = 0.5;
	testTone.startDelay = latencyFrames;
	
	result = PaQa_InitializeRecording( &recording, maxFrames, (int) sampleRate );
	QA_ASSERT_EQUALS( "PaQa_InitializeRecording failed", 0, result );
	
	MakeRecordingWithAddedFrames( &recording, &testTone, glitchPosition, framesAdded );
	
	PaQa_AnalyseRecording( &recording, &testTone, &analysisResult );
	
	if( framesAdded < 0 )
	{
		framesDropped = -framesAdded;
		framesAdded = 0;
	}
	
#if PRINT_REPORTS
	printf("\n=== Dropped Frame Analysis ===================\n");
	printf("                        expected      actual\n");
	printf("             latency: %10.3f  %10.3f\n", (double)latencyFrames, analysisResult.latency );
	printf("    num added frames: %10.3f  %10.3f\n", (double)framesAdded, analysisResult.numAddedFrames );
	printf("     added frames at: %10.3f  %10.3f\n", (double)glitchPosition, analysisResult.addedFramesPosition );
	printf("  num dropped frames: %10.3f  %10.3f\n", (double)framesDropped, analysisResult.numDroppedFrames );
	printf("   dropped frames at: %10.3f  %10.3f\n", (double)glitchPosition, analysisResult.droppedFramesPosition );
#endif
	
	QA_ASSERT_CLOSE( "PaQa_AnalyseRecording latency", latencyFrames, analysisResult.latency, 0.5 );
	QA_ASSERT_CLOSE( "PaQa_AnalyseRecording framesAdded", framesAdded, analysisResult.numAddedFrames, 1.0 );
	QA_ASSERT_CLOSE( "PaQa_AnalyseRecording framesDropped", framesDropped, analysisResult.numDroppedFrames, 1.0 );
//	QA_ASSERT_CLOSE( "PaQa_AnalyseRecording glitchPosition", glitchPosition, analysisResult.glitchPosition, cycleSize );

	PaQa_TerminateRecording( &recording );
	return 0;
	
error:
	PaQa_TerminateRecording( &recording);	
	return 1;
}	

/*==========================================================================================*/
/**
 * Test various dropped sample scenarios.
 */
static int TestDetectPhaseErrors( void )
{
	int result;
	
	result = TestDetectSinglePhaseError( 44100, 200, 477, -1, 0 );
	if( result < 0 ) return result;
/*
	result = TestDetectSinglePhaseError( 44100, 200, 77, -1, 0 );
	if( result < 0 ) return result;
	
	result = TestDetectSinglePhaseError( 44100, 200, 83, 3712, 9 );
	if( result < 0 ) return result;
	
	result = TestDetectSinglePhaseError( 44100, 280, 83, 3712, 27 );
	if( result < 0 ) return result;
	
	result = TestDetectSinglePhaseError( 44100, 200, 234, 3712, -9 );
	if( result < 0 ) return result;
	
	result = TestDetectSinglePhaseError( 44100, 200, 2091, 8923, -2 );
	if( result < 0 ) return result;
	
	result = TestDetectSinglePhaseError( 44100, 120, 1782, 5772, -18 );
	if( result < 0 ) return result;
	
	// Note that if the frequency is too high then it is hard to detect single dropped frames.
	result = TestDetectSinglePhaseError( 44100, 200, 500, 4251, -1 );
	if( result < 0 ) return result;
*/
	return 0;
}

/*==========================================================================================*/
/**
 * Detect one pop in a recording.
 */
static int TestDetectSinglePop( double sampleRate, int cycleSize, int latencyFrames, int popPosition, int popWidth, double popAmplitude )
{
	int result = 0;
	PaQaRecording     recording;	
	PaQaTestTone testTone;
	PaQaAnalysisResult analysisResult = { 0.0 };
	int maxFrames = ((int)sampleRate) * 2;

	testTone.samplesPerFrame = 1;
	testTone.sampleRate = sampleRate;
	testTone.frequency = sampleRate / cycleSize;
	testTone.amplitude = 0.5;
	testTone.startDelay = latencyFrames;
	
	result = PaQa_InitializeRecording( &recording, maxFrames, (int) sampleRate );
	QA_ASSERT_EQUALS( "PaQa_InitializeRecording failed", 0, result );
	
	MakeRecordingWithPop( &recording, &testTone, popPosition, popWidth, popAmplitude );
		
	PaQa_AnalyseRecording( &recording, &testTone, &analysisResult );
	
#if PRINT_REPORTS
	printf("\n=== Pop Analysis ===================\n");
	printf("                        expected      actual\n");
	printf("             latency: %10.3f  %10.3f\n", (double)latencyFrames, analysisResult.latency );
	printf("         popPosition: %10.3f  %10.3f\n", (double)popPosition, analysisResult.popPosition );	
	printf("        popAmplitude: %10.3f  %10.3f\n", popAmplitude, analysisResult.popAmplitude );	
	printf("           cycleSize: %6d\n", cycleSize );	
	printf("    num added frames: %10.3f\n", analysisResult.numAddedFrames );
	printf("     added frames at: %10.3f\n", analysisResult.addedFramesPosition );
	printf("  num dropped frames: %10.3f\n", analysisResult.numDroppedFrames );
	printf("   dropped frames at: %10.3f\n", analysisResult.droppedFramesPosition );
#endif
	
	QA_ASSERT_CLOSE( "PaQa_AnalyseRecording latency", latencyFrames, analysisResult.latency, 0.5 );
	QA_ASSERT_CLOSE( "PaQa_AnalyseRecording popPosition", popPosition, analysisResult.popPosition, 10 );
	if( popWidth > 0 )
	{
		QA_ASSERT_CLOSE( "PaQa_AnalyseRecording popAmplitude", popAmplitude, analysisResult.popAmplitude, 0.1 * popAmplitude  );
	}
	
	PaQa_TerminateRecording( &recording );
	return 0;
	
error:
	PaQa_SaveRecordingToWaveFile( &recording, "bad_recording.wav" );
	PaQa_TerminateRecording( &recording);	
	return 1;
}	

/*==========================================================================================*/
/**
 * Analyse recording with a DC offset.
 */
static int TestSingleInitialSpike( double sampleRate, int stepPosition, int cycleSize, int latencyFrames, double stepAmplitude )
{
	int i;
	int result = 0;
	// Account for highpass filter offset.
	int expectedLatency = latencyFrames + 1;
	PaQaRecording     recording;	
	
	PaQaRecording     hipassOutput = { 0 };
	BiquadFilter      hipassFilter;
	
	PaQaTestTone testTone;
	PaQaAnalysisResult analysisResult = { 0.0 };
	int maxFrames = ((int)sampleRate) * 2;
	
	testTone.samplesPerFrame = 1;
	testTone.sampleRate = sampleRate;
	testTone.frequency = sampleRate / cycleSize;
	testTone.amplitude = -0.5;
	testTone.startDelay = latencyFrames;
	
	result = PaQa_InitializeRecording( &recording, maxFrames, (int) sampleRate );
	QA_ASSERT_EQUALS( "PaQa_InitializeRecording failed", 0, result );
	
	result = PaQa_InitializeRecording( &hipassOutput, maxFrames, (int) sampleRate );
	QA_ASSERT_EQUALS( "PaQa_InitializeRecording failed", 0, result );
	
	MakeCleanRecording( &recording, &testTone );
	
	// Apply DC step.
	for( i=stepPosition; i<recording.numFrames; i++ )
	{
		recording.buffer[i] += stepAmplitude;
	}
	
	// Use high pass as a DC blocker!
	BiquadFilter_SetupHighPass( &hipassFilter, 10.0 / sampleRate, 0.5 );
	PaQa_FilterRecording( &recording, &hipassOutput, &hipassFilter );
	
	testTone.amplitude = 0.5;
	PaQa_AnalyseRecording( &hipassOutput, &testTone, &analysisResult );
	
#if PRINT_REPORTS
	printf("\n=== InitialSpike Analysis ===================\n");
	printf("                        expected      actual\n");
	printf("             latency: %10.3f  %10.3f\n", (double)expectedLatency, analysisResult.latency );
	printf("         popPosition: %10.3f\n", analysisResult.popPosition );	
	printf("        popAmplitude: %10.3f\n", analysisResult.popAmplitude );	
	printf("      amplitudeRatio: %10.3f\n", analysisResult.amplitudeRatio );	
	printf("           cycleSize: %6d\n", cycleSize );	
	printf("    num added frames: %10.3f\n", analysisResult.numAddedFrames );
	printf("     added frames at: %10.3f\n", analysisResult.addedFramesPosition );
	printf("  num dropped frames: %10.3f\n", analysisResult.numDroppedFrames );
	printf("   dropped frames at: %10.3f\n", analysisResult.droppedFramesPosition );
#endif
	
	QA_ASSERT_CLOSE( "PaQa_AnalyseRecording latency", expectedLatency, analysisResult.latency, 4.0 );
	QA_ASSERT_EQUALS( "PaQa_AnalyseRecording no pop from step", -1, (int) analysisResult.popPosition );	
	PaQa_TerminateRecording( &recording );
	PaQa_TerminateRecording( &hipassOutput );
	return 0;
	
error:
	PaQa_SaveRecordingToWaveFile( &recording, "bad_step_original.wav" );
	PaQa_SaveRecordingToWaveFile( &hipassOutput, "bad_step_hipass.wav" );
	PaQa_TerminateRecording( &recording);	
	PaQa_TerminateRecording( &hipassOutput );
	return 1;
}	

/*==========================================================================================*/
/**
 * Test various dropped sample scenarios.
 */
static int TestDetectPops( void )
{
	int result;
	
	// No pop.
	result = TestDetectSinglePop( 44100, 200, 477, -1, 0, 0.0 );
	if( result < 0 ) return result;
	
	// short pop
	result = TestDetectSinglePop( 44100, 300, 810, 3987, 1, 0.5 );
	if( result < 0 ) return result;
	
	// medium long pop
	result = TestDetectSinglePop( 44100, 300, 810, 9876, 5, 0.5 );
	if( result < 0 ) return result;
	
	// short tiny pop
	result = TestDetectSinglePop( 44100, 250, 810, 5672, 1, 0.05 );
	if( result < 0 ) return result;
	
	
	return 0;
}

/*==========================================================================================*/
/**
 * Test analysis when there is a DC offset step before the sine signal.
 */
static int TestInitialSpike( void )
{
	int result;
	
//( double sampleRate, int stepPosition, int cycleSize, int latencyFrames, double stepAmplitude )
	// No spike.
	result = TestSingleInitialSpike( 44100, 32, 100, 537, 0.0 );
	if( result < 0 ) return result;
	
	// Small spike.
	result = TestSingleInitialSpike( 44100, 32, 100, 537, 0.1 );
	if( result < 0 ) return result;
	
	// short pop like Ross's error.
	result = TestSingleInitialSpike( 8000, 32, 42, 2000, 0.1 );
	if( result < 0 ) return result;
	
	// Medium spike.
	result = TestSingleInitialSpike( 44100, 40, 190, 3000, 0.5 );
	if( result < 0 ) return result;
	
	// Spike near sine.
	//result = TestSingleInitialSpike( 44100, 2900, 140, 3000, 0.1 );
	if( result < 0 ) return result;
	
	
	return 0;
}


#if TEST_SAVED_WAVE
/*==========================================================================================*/
/**
 * Simple test that writes a sawtooth waveform to a file.
 */
static int TestSavedWave()
{
    int i,j;
    WAV_Writer writer;
    int result = 0;
#define NUM_SAMPLES  (200)
    short data[NUM_SAMPLES];
    short saw = 0;
    	
	
    result =  Audio_WAV_OpenWriter( &writer, "test_sawtooth.wav", 44100, 1 );
    if( result < 0 ) goto error;
	
    for( i=0; i<15; i++ )
    {
		for( j=0; j<NUM_SAMPLES; j++ )
		{
			data[j] = saw;
			saw += 293;
		}
        result =  Audio_WAV_WriteShorts( &writer, data, NUM_SAMPLES );
        if( result < 0 ) goto error;
    }
	
    result =  Audio_WAV_CloseWriter( &writer );
    if( result < 0 ) goto error;
	
	
    return 0;
	
error:
    printf("ERROR: result = %d\n", result );
    return result;
}
#endif /* TEST_SAVED_WAVE */

/*==========================================================================================*/
/**
 * Easy way to generate a sine tone recording.
 */
void PaQa_FillWithSine( PaQaRecording *recording, double sampleRate, double freq, double amp )
{
	PaQaSineGenerator generator;
	float buffer[FRAMES_PER_BLOCK];
	int samplesPerFrame = 1;
	int stride = 1;
	int done = 0;

	// Setup a sine oscillator.
	PaQa_SetupSineGenerator( &generator, freq, amp, sampleRate );
		
	done = 0;
	while (!done)
	{
		PaQa_EraseBuffer( buffer, FRAMES_PER_BLOCK, samplesPerFrame );
		PaQa_MixSine( &generator, buffer, FRAMES_PER_BLOCK, stride );
		done = PaQa_WriteRecording( recording, buffer, FRAMES_PER_BLOCK, samplesPerFrame );
	}
	
}

/*==========================================================================================*/
/**
 * Generate a tone then knock it out using a filter.
 * Also check using filter slightly off tune to see if some energy gets through.
 */
static int TestNotchFilter( void )
{
	int result = 0;
	PaQaRecording     original = { 0 };
	PaQaRecording     filtered = { 0 };
	BiquadFilter      notchFilter;
	double sampleRate = 44100.0;
	int maxFrames = ((int)sampleRate) * 1;
	
	double freq = 234.5;
	double amp = 0.5;
	
    double mag1, mag2, mag3;

	result = PaQa_InitializeRecording( &original, maxFrames, (int) sampleRate );
	QA_ASSERT_EQUALS( "PaQa_InitializeRecording failed", 0, result );
	
	PaQa_FillWithSine( &original, sampleRate, freq, amp );
	
	//result = PaQa_SaveRecordingToWaveFile( &original, "original.wav" );
	//QA_ASSERT_EQUALS( "PaQa_SaveRecordingToWaveFile failed", 0, result );	

    mag1 = PaQa_CorrelateSine( &original, freq, sampleRate, 0, original.numFrames, NULL );
	QA_ASSERT_CLOSE( "exact frequency match", amp, mag1, 0.01 );
		
	// Filter with exact frequency.
	result = PaQa_InitializeRecording( &filtered, maxFrames, (int) sampleRate );
	QA_ASSERT_EQUALS( "PaQa_InitializeRecording failed", 0, result );
	
	BiquadFilter_SetupNotch( &notchFilter, freq / sampleRate, 0.5 );
	PaQa_FilterRecording( &original, &filtered, &notchFilter );
	result = PaQa_SaveRecordingToWaveFile( &filtered, "filtered1.wav" );
	QA_ASSERT_EQUALS( "PaQa_SaveRecordingToWaveFile failed", 0, result );
	
	mag2 = PaQa_CorrelateSine( &filtered, freq, sampleRate, 0, filtered.numFrames, NULL );
	QA_ASSERT_CLOSE( "should eliminate tone", 0.0, mag2, 0.01 );
	
	// Filter with mismatched frequency.
	BiquadFilter_SetupNotch( &notchFilter, 1.07 * freq / sampleRate, 2.0 );
	PaQa_FilterRecording( &original, &filtered, &notchFilter );
	
	//result = PaQa_SaveRecordingToWaveFile( &filtered, "badfiltered.wav" );
	//QA_ASSERT_EQUALS( "PaQa_SaveRecordingToWaveFile failed", 0, result );
	
	mag3 = PaQa_CorrelateSine( &filtered, freq, sampleRate, 0, filtered.numFrames, NULL );
	QA_ASSERT_CLOSE( "should eliminate tone", amp*0.26, mag3, 0.01 );

	
	PaQa_TerminateRecording( &original );
	PaQa_TerminateRecording( &filtered );
	return 0;
	
error:
	PaQa_TerminateRecording( &original);
	PaQa_TerminateRecording( &filtered );	
	return 1;
	
}

/*==========================================================================================*/
/**
 */ 
int PaQa_TestAnalyzer( void )
{
	int result;
	
#if TEST_SAVED_WAVE
	// Write a simple wave file.
	if ((result = TestSavedWave()) != 0) return result;
#endif /* TEST_SAVED_WAVE */
	
	// Generate single tone and verify presence.
	if ((result = TestSingleMonoTone()) != 0) return result;

	// Generate prime series of tones and verify presence.
	if ((result = TestMixedMonoTones()) != 0) return result;
	
	// Detect dropped or added samples in a sine wave recording.
	if ((result = TestDetectPhaseErrors()) != 0) return result;
	
	// Test to see if notch filter can knock out the test tone.
	if ((result = TestNotchFilter()) != 0) return result;
	
	// Detect pops that get back in phase.
	if ((result = TestDetectPops()) != 0) return result;
	
	// Test to see if the latency detector can be tricked like it was on Ross' Windows machine.
	if ((result = TestInitialSpike()) != 0) return result;
	

	return 0;
}
