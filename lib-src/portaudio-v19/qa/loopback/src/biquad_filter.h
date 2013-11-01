#ifndef _BIQUADFILTER_H
#define _BIQUADFILTER_H


/**
 *	Unit_BiquadFilter implements a second order IIR filter. 
 *
 * @author (C) 2002 Phil Burk, SoftSynth.com, All Rights Reserved
 */

#define BIQUAD_MIN_RATIO     (0.000001)
#define BIQUAD_MIN_Q         (0.00001)

typedef struct BiquadFilter_s
{
    double      xn1;    // storage for delayed signals
	double      xn2;
	double      yn1;
	double      yn2;

	double      a0;    // coefficients
	double      a1;
	double      a2;

	double      b1;
	double      b2;

	double      cos_omega;
	double      sin_omega;
	double      alpha;
} BiquadFilter;

void BiquadFilter_SetupHighPass( BiquadFilter *filter, double ratio, double Q );
void BiquadFilter_SetupNotch( BiquadFilter *filter, double ratio, double Q );

void BiquadFilter_Filter( BiquadFilter *filter, float *inputs, float *outputs, int numSamples );

#endif
