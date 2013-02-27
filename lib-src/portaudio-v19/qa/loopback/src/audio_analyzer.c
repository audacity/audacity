
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
#include <string.h>
#include <assert.h>
#include <math.h>
#include "qa_tools.h"
#include "audio_analyzer.h"
#include "write_wav.h"

#define PAQA_POP_THRESHOLD  (0.04)

/*==========================================================================================*/
double PaQa_GetNthFrequency( double baseFrequency, int index )
{
	// Use 13 tone equal tempered scale because it does not generate harmonic ratios.
	return baseFrequency * pow( 2.0, index / 13.0 );
}

/*==========================================================================================*/
void PaQa_EraseBuffer( float *buffer, int numFrames, int samplesPerFrame )
{
	int i;
	int numSamples = numFrames * samplesPerFrame;
	for( i=0; i<numSamples; i++ )
	{
		*buffer++ = 0.0;
	}
}

/*==========================================================================================*/
void PaQa_SetupSineGenerator( PaQaSineGenerator *generator, double frequency, double amplitude, double frameRate )
{
	generator->phase = 0.0;
	generator->amplitude = amplitude;
	generator->frequency = frequency;
	generator->phaseIncrement = 2.0 * frequency * MATH_PI / frameRate;
}

/*==========================================================================================*/
void PaQa_MixSine( PaQaSineGenerator *generator, float *buffer, int numSamples, int stride )
{
	int i;
	for( i=0; i<numSamples; i++ )
	{
		float value = sinf( (float) generator->phase ) * generator->amplitude;
		*buffer += value; // Mix with existing value.
		buffer += stride;
		// Advance phase and wrap around.
		generator->phase += generator->phaseIncrement;
		if (generator->phase > MATH_TWO_PI)
		{
			generator->phase -= MATH_TWO_PI;
		}
	}
}

/*==========================================================================================*/
void PaQa_GenerateCrackDISABLED( float *buffer, int numSamples, int stride )
{
	int i;
	int offset = numSamples/2;
	for( i=0; i<numSamples; i++ )
	{		
		float phase = (MATH_TWO_PI * 0.5 * (i - offset)) / numSamples;
		float cosp = cosf( phase );
		float cos2 = cosp * cosp;
		// invert second half of signal
		float value = (i < offset) ? cos2 : (0-cos2);
		*buffer = value;
		buffer += stride;
	}
}


/*==========================================================================================*/
int PaQa_InitializeRecording( PaQaRecording *recording, int maxFrames, int frameRate )
{
	int numBytes = maxFrames * sizeof(float);
	recording->buffer = (float*)malloc(numBytes);
	QA_ASSERT_TRUE( "Allocate recording buffer.", (recording->buffer != NULL) );
	recording->maxFrames = maxFrames;	recording->sampleRate = frameRate;
	recording->numFrames = 0;
	return 0;
error:
	return 1;
}

/*==========================================================================================*/
void PaQa_TerminateRecording( PaQaRecording *recording )
{	
	if (recording->buffer != NULL)
	{
		free( recording->buffer );
		recording->buffer = NULL;
	}
	recording->maxFrames = 0;
}

/*==========================================================================================*/
int PaQa_WriteRecording( PaQaRecording *recording, float *buffer, int numFrames, int stride )
{
	int i;
	int framesToWrite;
    float *data = &recording->buffer[recording->numFrames];

    framesToWrite = numFrames;
	if ((framesToWrite + recording->numFrames) > recording->maxFrames)
	{
		framesToWrite = recording->maxFrames - recording->numFrames;
	}
	
	for( i=0; i<framesToWrite; i++ )
	{
		*data++ = *buffer;
		buffer += stride;
	}
	recording->numFrames += framesToWrite;
	return (recording->numFrames >= recording->maxFrames);
}

/*==========================================================================================*/
int PaQa_WriteSilence( PaQaRecording *recording, int numFrames )
{
	int i;
	int framesToRecord;
    float *data = &recording->buffer[recording->numFrames];

    framesToRecord = numFrames;
	if ((framesToRecord + recording->numFrames) > recording->maxFrames)
	{
		framesToRecord = recording->maxFrames - recording->numFrames;
	}
	
	for( i=0; i<framesToRecord; i++ )
	{
		*data++ = 0.0f;
	}
	recording->numFrames += framesToRecord;
	return (recording->numFrames >= recording->maxFrames);
}

/*==========================================================================================*/
int PaQa_RecordFreeze( PaQaRecording *recording, int numFrames )
{
	int i;
	int framesToRecord;
    float *data = &recording->buffer[recording->numFrames];

    framesToRecord = numFrames;
	if ((framesToRecord + recording->numFrames) > recording->maxFrames)
	{
		framesToRecord = recording->maxFrames - recording->numFrames;
	}
	
	for( i=0; i<framesToRecord; i++ )
	{
		// Copy old value forward as if the signal had frozen.
		data[i] = data[i-1];
	}
	recording->numFrames += framesToRecord;
	return (recording->numFrames >= recording->maxFrames);
}

/*==========================================================================================*/
/**
 * Write recording to WAV file.
 */
int PaQa_SaveRecordingToWaveFile( PaQaRecording *recording, const char *filename )
{
    WAV_Writer writer;
    int result = 0;
#define NUM_SAMPLES  (200)
    short data[NUM_SAMPLES];
	const int samplesPerFrame = 1;
    int numLeft = recording->numFrames;
	float *buffer = &recording->buffer[0];

    result =  Audio_WAV_OpenWriter( &writer, filename, recording->sampleRate, samplesPerFrame );
    if( result < 0 ) goto error;
	
	while( numLeft > 0 )
    {
		int i;
        int numToSave = (numLeft > NUM_SAMPLES) ? NUM_SAMPLES : numLeft;
		// Convert double samples to shorts.
		for( i=0; i<numToSave; i++ )
		{
			double fval = *buffer++;
			// Convert float to int and clip to short range.
			int ival = fval * 32768.0;
			if( ival > 32767 ) ival = 32767;
			else if( ival < -32768 ) ival = -32768;
			data[i] = ival;
		}
		result =  Audio_WAV_WriteShorts( &writer, data, numToSave );
        if( result < 0 ) goto error;
		numLeft -= numToSave;
    }
	
    result =  Audio_WAV_CloseWriter( &writer );
    if( result < 0 ) goto error;
	
    return 0;
	
error:
    printf("ERROR: result = %d\n", result );
    return result;
#undef NUM_SAMPLES
}

/*==========================================================================================*/

double PaQa_MeasureCrossingSlope( float *buffer, int numFrames )
{
	int i;
	double slopeTotal = 0.0;
	int slopeCount = 0;
	float previous;
	double averageSlope = 0.0;
	
	previous = buffer[0];
	for( i=1; i<numFrames; i++ )
	{
		float current = buffer[i];
		if( (current > 0.0) && (previous < 0.0) )
		{
			double delta = current - previous;
			slopeTotal += delta;
			slopeCount += 1;
		}
		previous = current;
	}
	if( slopeCount > 0 )
	{
		averageSlope = slopeTotal / slopeCount;
	}
	return averageSlope;
}

/*==========================================================================================*/
/*
 * We can't just measure the peaks cuz they may be clipped.
 * But the zero crossing should be intact.
 * The measured slope of a sine wave at zero should be:
 *
 *   slope = sin( 2PI * frequency / sampleRate )
 *
 */
double PaQa_MeasureSineAmplitudeBySlope( PaQaRecording *recording,
						  double frequency, double frameRate,
						int startFrame, int numFrames )
{
	float *buffer = &recording->buffer[startFrame];
	double measuredSlope = PaQa_MeasureCrossingSlope( buffer, numFrames );
	double unitySlope = sin( MATH_TWO_PI * frequency / frameRate );
	double estimatedAmplitude = measuredSlope / unitySlope;
	return estimatedAmplitude;
}

/*==========================================================================================*/
double PaQa_CorrelateSine( PaQaRecording *recording, double frequency, double frameRate,
						  int startFrame, int numFrames, double *phasePtr )
{
	double magnitude = 0.0;
    int numLeft = numFrames;
	double phase = 0.0;
	double phaseIncrement = 2.0 * MATH_PI * frequency / frameRate;
	double sinAccumulator = 0.0;
	double cosAccumulator = 0.0;
	float *data = &recording->buffer[startFrame];

    QA_ASSERT_TRUE( "startFrame out of bounds", (startFrame < recording->numFrames) );
	QA_ASSERT_TRUE( "numFrames out of bounds", ((startFrame+numFrames) <= recording->numFrames) );

	while( numLeft > 0 )
	{			
		double sample = (double) *data++;
		sinAccumulator += sample * sin( phase );
		cosAccumulator += sample * cos( phase );
		phase += phaseIncrement;
		if (phase > MATH_TWO_PI)
		{
			phase -= MATH_TWO_PI;
		}
		numLeft -= 1;
	}
	sinAccumulator = sinAccumulator / numFrames; 
	cosAccumulator = cosAccumulator / numFrames;
	// TODO Why do I have to multiply by 2.0? Need it to make result come out right.
	magnitude = 2.0 * sqrt( (sinAccumulator * sinAccumulator) + (cosAccumulator * cosAccumulator ));
	if( phasePtr != NULL )
	{
		double phase = atan2( cosAccumulator, sinAccumulator );
		*phasePtr = phase;
	}
	return magnitude;
error:
	return -1.0;
}

/*==========================================================================================*/
void PaQa_FilterRecording( PaQaRecording *input, PaQaRecording *output, BiquadFilter *filter )
{
	int numToFilter = (input->numFrames > output->maxFrames) ? output->maxFrames : input->numFrames;
	BiquadFilter_Filter( filter, &input->buffer[0], &output->buffer[0], numToFilter );
	output->numFrames = numToFilter;
}

/*==========================================================================================*/
/** Scan until we get a correlation of a single that goes over the tolerance level,
 * peaks then drops to half the peak.
 * Look for inverse correlation as well.
 */
double PaQa_FindFirstMatch( PaQaRecording *recording, float *buffer, int numFrames, double threshold  )
{
	int ic,is;
	// How many buffers will fit in the recording?
	int maxCorrelations = recording->numFrames - numFrames;
	double maxSum = 0.0;
	int peakIndex = -1;
	double inverseMaxSum = 0.0;
	int inversePeakIndex = -1;
	double location = -1.0;

    QA_ASSERT_TRUE( "numFrames out of bounds", (numFrames < recording->numFrames) );

	for( ic=0; ic<maxCorrelations; ic++ )
	{
		int pastPeak;
		int inversePastPeak;
		
		double sum = 0.0;
		// Correlate buffer against the recording.
		float *recorded = &recording->buffer[ ic ];
		for( is=0; is<numFrames; is++ )
		{
			float s1 = buffer[is];
			float s2 = *recorded++;
			sum += s1 * s2;
		}
		if( (sum > maxSum) )
		{
			maxSum = sum;
			peakIndex = ic;
		}
		if( ((-sum) > inverseMaxSum) )
		{
			inverseMaxSum = -sum;
			inversePeakIndex = ic;
		}
		pastPeak = (maxSum > threshold) && (sum < 0.5*maxSum);
		inversePastPeak = (inverseMaxSum > threshold) && ((-sum) < 0.5*inverseMaxSum);
		//printf("PaQa_FindFirstMatch: ic = %4d, sum = %8f, maxSum = %8f, inverseMaxSum = %8f\n", ic, sum, maxSum, inverseMaxSum );														  
		if( pastPeak && inversePastPeak )
		{
			if( maxSum > inverseMaxSum )
			{
				location = peakIndex;
			}
			else
			{
				location = inversePeakIndex;
			}
			break;
		}
		
	}
	//printf("PaQa_FindFirstMatch: location = %4d\n", (int)location );
	return location;
error:
	return -1.0;
}

/*==========================================================================================*/
// Measure the area under the curve by summing absolute value of each value.
double PaQa_MeasureArea( float *buffer, int numFrames, int stride  )
{
	int is;
	double area = 0.0;
	for( is=0; is<numFrames; is++ )
	{
		area += fabs( *buffer );
		buffer += stride;
	}
	return area;
}

/*==========================================================================================*/
// Measure the area under the curve by summing absolute value of each value.
double PaQa_MeasureRootMeanSquare( float *buffer, int numFrames )
{
	int is;
	double area = 0.0;
	double root;
	for( is=0; is<numFrames; is++ )
	{
		float value = *buffer++;
		area += value * value;
	}
	root = sqrt( area );
	return root / numFrames;
}


/*==========================================================================================*/
// Compare the amplitudes of these two signals.
// Return ratio of recorded signal over buffer signal.

double PaQa_CompareAmplitudes( PaQaRecording *recording, int startAt, float *buffer, int numFrames )
{
	QA_ASSERT_TRUE( "startAt+numFrames out of bounds", ((startAt+numFrames) < recording->numFrames) );

    {
	    double recordedArea = PaQa_MeasureArea( &recording->buffer[startAt], numFrames, 1 );
	    double bufferArea = PaQa_MeasureArea( buffer, numFrames, 1 );
	    if( bufferArea == 0.0 ) return 100000000.0;
	    return recordedArea / bufferArea;
    }
error:
	return -1.0;
}


/*==========================================================================================*/
double PaQa_ComputePhaseDifference( double phase1, double phase2 )
{
	double delta = phase1 - phase2;
	while( delta > MATH_PI )
	{
		delta -= MATH_TWO_PI;
	}
	while( delta < -MATH_PI )
	{
		delta += MATH_TWO_PI;
	}
	return delta;
}

/*==========================================================================================*/
int PaQa_MeasureLatency( PaQaRecording *recording, PaQaTestTone *testTone, PaQaAnalysisResult *analysisResult )
{
	double threshold;
	PaQaSineGenerator generator;
#define MAX_BUFFER_SIZE 2048
	float buffer[MAX_BUFFER_SIZE];
	double period = testTone->sampleRate / testTone->frequency;
	int cycleSize = (int) (period + 0.5);
	//printf("PaQa_AnalyseRecording: frequency = %8f, frameRate = %8f, period = %8f, cycleSize = %8d\n", 
	//	   testTone->frequency, testTone->sampleRate, period, cycleSize );
	analysisResult->latency = -1;
	analysisResult->valid = (0);
	
	// Set up generator to find matching first cycle.
	QA_ASSERT_TRUE( "cycleSize out of bounds", (cycleSize < MAX_BUFFER_SIZE) );
	PaQa_SetupSineGenerator( &generator, testTone->frequency, testTone->amplitude, testTone->sampleRate );
	PaQa_EraseBuffer( buffer, cycleSize, testTone->samplesPerFrame );
	PaQa_MixSine( &generator, buffer, cycleSize, testTone->samplesPerFrame );

	threshold = cycleSize * 0.02;
	analysisResult->latency = PaQa_FindFirstMatch( recording, buffer, cycleSize, threshold );	
	QA_ASSERT_TRUE( "Could not find the start of the signal.", (analysisResult->latency >= 0) );
	analysisResult->amplitudeRatio = PaQa_CompareAmplitudes( recording, analysisResult->latency, buffer, cycleSize );
	return 0;
error:
	return -1;
}

/*==========================================================================================*/
// Apply cosine squared window.
void PaQa_FadeInRecording( PaQaRecording *recording, int startFrame, int count )
{
	int is;
	double phase = 0.5 * MATH_PI;
	// Advance a quarter wave
	double phaseIncrement = 0.25 * 2.0 * MATH_PI / count;
	
    assert( startFrame >= 0 );
	assert( count > 0 );
    
    /* Zero out initial part of the recording. */
	for( is=0; is<startFrame; is++ )
	{
        recording->buffer[ is ] = 0.0f;
    }
    /* Fade in where signal begins. */
    for( is=0; is<count; is++ )
    {
		double c = cos( phase );
		double w = c * c;
		float x = recording->buffer[ is + startFrame ];
		float y = x * w;
		//printf("FADE %d : w=%f, x=%f, y=%f\n", is, w, x, y );
		recording->buffer[ is + startFrame ] = y;

        phase += phaseIncrement;
	}
}


/*==========================================================================================*/
/** Apply notch filter and high pass filter then detect remaining energy.
 */
int PaQa_DetectPop( PaQaRecording *recording, PaQaTestTone *testTone, PaQaAnalysisResult *analysisResult )
{	
    int result = 0;
	int i;
    double maxAmplitude;
	int maxPosition;

	PaQaRecording     notchOutput = { 0 };
	BiquadFilter      notchFilter;
	
	PaQaRecording     hipassOutput = { 0 };
	BiquadFilter      hipassFilter;
	
	int frameRate = (int) recording->sampleRate;
	
	analysisResult->popPosition = -1;
	analysisResult->popAmplitude = 0.0;
	
	result = PaQa_InitializeRecording( &notchOutput, recording->numFrames, frameRate );
	QA_ASSERT_EQUALS( "PaQa_InitializeRecording failed", 0, result );
	
	result = PaQa_InitializeRecording( &hipassOutput, recording->numFrames, frameRate );
	QA_ASSERT_EQUALS( "PaQa_InitializeRecording failed", 0, result );
	
	// Use notch filter to remove test tone.
	BiquadFilter_SetupNotch( &notchFilter, testTone->frequency / frameRate, 0.5 );
	PaQa_FilterRecording( recording, &notchOutput, &notchFilter );
	//result = PaQa_SaveRecordingToWaveFile( &notchOutput, "notch_output.wav" );
	//QA_ASSERT_EQUALS( "PaQa_SaveRecordingToWaveFile failed", 0, result );
	
	// Apply fade-in window.
	PaQa_FadeInRecording( &notchOutput, (int) analysisResult->latency, 500 );
	
	// Use high pass to accentuate the edges of a pop. At higher frequency!
	BiquadFilter_SetupHighPass( &hipassFilter, 2.0 * testTone->frequency / frameRate, 0.5 );
	PaQa_FilterRecording( &notchOutput, &hipassOutput, &hipassFilter );
	//result = PaQa_SaveRecordingToWaveFile( &hipassOutput, "hipass_output.wav" );
	//QA_ASSERT_EQUALS( "PaQa_SaveRecordingToWaveFile failed", 0, result );
	
	// Scan remaining signal looking for peak.
	maxAmplitude = 0.0;
	maxPosition = -1;
	for( i=(int) analysisResult->latency; i<hipassOutput.numFrames; i++ )
	{
		float x = hipassOutput.buffer[i];
		float mag = fabs( x );
		if( mag > maxAmplitude )
		{
			maxAmplitude = mag;
			maxPosition = i;
		}
	}
	
	if( maxAmplitude > PAQA_POP_THRESHOLD )
	{
		analysisResult->popPosition = maxPosition;
		analysisResult->popAmplitude = maxAmplitude;
	}
	
	PaQa_TerminateRecording( &notchOutput );
	PaQa_TerminateRecording( &hipassOutput );
	return 0;
	
error:
	PaQa_TerminateRecording( &notchOutput );
	PaQa_TerminateRecording( &hipassOutput );
	return -1;
}

/*==========================================================================================*/
int PaQa_DetectPhaseError( PaQaRecording *recording, PaQaTestTone *testTone, PaQaAnalysisResult *analysisResult )
{		
	int i;
	double period = testTone->sampleRate / testTone->frequency;
	int cycleSize = (int) (period + 0.5);
	
	double maxAddedFrames = 0.0;
	double maxDroppedFrames = 0.0;
	
	double previousPhase = 0.0;
	double previousFrameError = 0;
	int loopCount = 0;
	int skip = cycleSize;
	int windowSize = cycleSize;

    // Scan recording starting with first cycle, looking for phase errors.
	analysisResult->numDroppedFrames = 0.0;
	analysisResult->numAddedFrames = 0.0;
	analysisResult->droppedFramesPosition = -1.0;
	analysisResult->addedFramesPosition = -1.0;
	
	for( i=analysisResult->latency; i<(recording->numFrames - windowSize); i += skip )
	{
		double expectedPhase = previousPhase + (skip * MATH_TWO_PI / period);
		double expectedPhaseIncrement = PaQa_ComputePhaseDifference( expectedPhase, previousPhase );

		double phase = 666.0;
		double mag = PaQa_CorrelateSine( recording, testTone->frequency, testTone->sampleRate, i, windowSize, &phase );
		if( (loopCount > 1) && (mag > 0.0) )
		{
			double phaseDelta = PaQa_ComputePhaseDifference( phase, previousPhase );
			double phaseError = PaQa_ComputePhaseDifference( phaseDelta, expectedPhaseIncrement );
			// Convert phaseError to equivalent number of frames.
			double frameError = period * phaseError / MATH_TWO_PI;
			double consecutiveFrameError = frameError + previousFrameError;
//			if( fabs(frameError) > 0.01 )
//			{
//				printf("FFFFFFFFFFFFF frameError = %f, at %d\n", frameError, i );
//			}
			if( consecutiveFrameError > 0.8 )
			{
				double droppedFrames = consecutiveFrameError;
				if (droppedFrames > (maxDroppedFrames * 1.001))
				{
					analysisResult->numDroppedFrames = droppedFrames;
					analysisResult->droppedFramesPosition = i + (windowSize/2);
					maxDroppedFrames = droppedFrames;
				}
			}
			else if( consecutiveFrameError < -0.8 )
			{
				double addedFrames = 0 - consecutiveFrameError;
				if (addedFrames > (maxAddedFrames * 1.001))
				{
					analysisResult->numAddedFrames = addedFrames;
					analysisResult->addedFramesPosition = i + (windowSize/2);
					maxAddedFrames = addedFrames;
				}
			}
			previousFrameError = frameError;
			

			//if( i<8000 )
			//{
			//	printf("%d: phase = %8f, expected = %8f, delta = %8f, frameError = %8f\n", i, phase, expectedPhaseIncrement, phaseDelta, frameError );
			//}
		}
		previousPhase = phase;
		loopCount += 1;
	}
	return 0;
}

/*==========================================================================================*/
int PaQa_AnalyseRecording( PaQaRecording *recording, PaQaTestTone *testTone, PaQaAnalysisResult *analysisResult )
{
    int result = 0;

	memset( analysisResult, 0, sizeof(PaQaAnalysisResult) );
	result = PaQa_MeasureLatency( recording, testTone, analysisResult );
    QA_ASSERT_EQUALS( "latency measurement", 0, result );
	
	if( (analysisResult->latency >= 0) && (analysisResult->amplitudeRatio > 0.1) )
	{
		analysisResult->valid = (1);
		
		result = PaQa_DetectPop( recording, testTone, analysisResult );
		QA_ASSERT_EQUALS( "detect pop", 0, result );

		result = PaQa_DetectPhaseError( recording, testTone, analysisResult );
		QA_ASSERT_EQUALS( "detect phase error", 0, result );
	}
	return 0;
error:
	return -1;
}

