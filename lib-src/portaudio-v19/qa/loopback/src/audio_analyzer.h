
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

#ifndef _AUDIO_ANALYZER_H
#define _AUDIO_ANALYZER_H

#include "biquad_filter.h"

#define MATH_PI  (3.141592653589793238462643)
#define MATH_TWO_PI  (2.0 * MATH_PI)

typedef struct PaQaSineGenerator_s
{
	double    phase;
	double    phaseIncrement;
	double    frequency;
	double    amplitude;
} PaQaSineGenerator;

/** Container for a monophonic audio sample in memory. */ 
typedef struct PaQaRecording_s
{
	/** Maximum number of frames that can fit in the allocated buffer. */
	int       maxFrames;
	float    *buffer;
	/** Actual number of valid frames in the buffer. */
	int       numFrames;
	int       sampleRate;
} PaQaRecording;

typedef struct PaQaTestTone_s
{
	int       samplesPerFrame;
	int       startDelay;
	double    sampleRate;
	double    frequency;
	double    amplitude;
} PaQaTestTone;

typedef struct PaQaAnalysisResult_s
{
	int       valid;
	/** Latency in samples from output to input. */
	double    latency;
	double    amplitudeRatio;
	double    popAmplitude;
	double    popPosition;
	double    numDroppedFrames;
	double    droppedFramesPosition;
	double    numAddedFrames;
	double    addedFramesPosition;
} PaQaAnalysisResult;


/*================================================================*/
/*================= General DSP Tools ============================*/
/*================================================================*/
/**
 * Calculate Nth frequency of a series for use in testing multiple channels.
 * Series should avoid harmonic overlap between channels.
 */
double PaQa_GetNthFrequency( double baseFrequency, int index );

void PaQa_EraseBuffer( float *buffer, int numFrames, int samplesPerFrame );

void PaQa_MixSine( PaQaSineGenerator *generator, float *buffer, int numSamples, int stride );

void PaQa_WriteSine( float *buffer, int numSamples, int stride,
					double frequency, double amplitude );

/**
 * Generate a signal with a sharp edge in the middle that can be recognized despite some phase shift.
 */
void PaQa_GenerateCrack( float *buffer, int numSamples, int stride );

double PaQa_ComputePhaseDifference( double phase1, double phase2 );

/**
 * Measure the area under the curve by summing absolute value of each value.
 */
double PaQa_MeasureArea( float *buffer, int numFrames, int stride  );

/**
 * Measure slope of the positive zero crossings.
 */
double PaQa_MeasureCrossingSlope( float *buffer, int numFrames );


/**
 * Prepare an oscillator that can generate a sine tone for testing.
 */
void PaQa_SetupSineGenerator( PaQaSineGenerator *generator, double frequency, double amplitude, double frameRate );

/*================================================================*/
/*================= Recordings ===================================*/
/*================================================================*/
/**
 * Allocate memory for containg a mono audio signal. Set up recording for writing.
 */
 int PaQa_InitializeRecording( PaQaRecording *recording, int maxSamples, int sampleRate );
 
/**
* Free memory allocated by PaQa_InitializeRecording.
 */
 void PaQa_TerminateRecording( PaQaRecording *recording );
 
/**
 * Apply a biquad filter to the audio from the input recording and write it to the output recording.
 */
void PaQa_FilterRecording( PaQaRecording *input, PaQaRecording *output, BiquadFilter *filter );


int PaQa_SaveRecordingToWaveFile( PaQaRecording *recording, const char *filename );
 
/**
 * @param stride is the spacing of samples to skip in the input buffer. To use every samples pass 1. To use every other sample pass 2.
 */
int PaQa_WriteRecording( PaQaRecording *recording, float *buffer, int numSamples, int stride );
 
/** Write zeros into a recording. */
int PaQa_WriteSilence( PaQaRecording *recording, int numSamples );
 
int PaQa_RecordFreeze( PaQaRecording *recording, int numSamples );

double PaQa_CorrelateSine( PaQaRecording *recording, double frequency, double frameRate,
						  int startFrame, int numSamples, double *phasePtr );

double PaQa_FindFirstMatch( PaQaRecording *recording, float *buffer, int numSamples, double tolerance  );

/** 
 * Estimate the original amplitude of a clipped sine wave by measuring
 * its average slope at the zero crossings.
 */
double PaQa_MeasureSineAmplitudeBySlope( PaQaRecording *recording,
										double frequency, double frameRate,
										int startFrame, int numFrames );

double PaQa_MeasureRootMeanSquare( float *buffer, int numFrames );

/**
 * Compare the amplitudes of these two signals.
 * Return ratio of recorded signal over buffer signal.
 */
double PaQa_CompareAmplitudes( PaQaRecording *recording, int startAt, float *buffer, int numSamples );

/**
 * Analyse a recording of a sine wave.
 * Measure latency and look for dropped frames, etc.
 */
int PaQa_AnalyseRecording( PaQaRecording *recording, PaQaTestTone *testTone, PaQaAnalysisResult *analysisResult );

#endif /* _AUDIO_ANALYZER_H */
