#include <math.h>
#include <string.h>

#include "biquad_filter.h"

/**
 *	Unit_BiquadFilter implements a second order IIR filter. 
	Here is the equation that we use for this filter:
     y(n) = a0*x(n) + a1*x(n-1)  + a2*x(n-2) - b1*y(n-1)  - b2*y(n-2)
 *
 * @author (C) 2002 Phil Burk, SoftSynth.com, All Rights Reserved
 */

#define FILTER_PI  (3.141592653589793238462643)
/***********************************************************
** Calculate coefficients common to many parametric biquad filters.
*/
static void BiquadFilter_CalculateCommon( BiquadFilter *filter, double ratio, double Q )
{
	double omega;
	
	memset( filter, 0, sizeof(BiquadFilter) );

/* Don't let frequency get too close to Nyquist or filter will blow up. */
	if( ratio >= 0.499 ) ratio = 0.499; 
	omega = 2.0 * (double)FILTER_PI * ratio;

	filter->cos_omega = (double) cos( omega );
	filter->sin_omega = (double) sin( omega );
	filter->alpha = filter->sin_omega / (2.0 * Q);
}

/*********************************************************************************
 ** Calculate coefficients for Highpass filter.
 */
void BiquadFilter_SetupHighPass( BiquadFilter *filter, double ratio, double Q )
{
	double    scalar, opc;
	
	if( ratio  < BIQUAD_MIN_RATIO )  ratio  = BIQUAD_MIN_RATIO;
	if( Q < BIQUAD_MIN_Q ) Q = BIQUAD_MIN_Q;
	
	BiquadFilter_CalculateCommon( filter, ratio, Q );
	
	scalar = 1.0 / (1.0 + filter->alpha);
	opc = (1.0 + filter->cos_omega);
	
	filter->a0 = opc * 0.5 * scalar;
	filter->a1 =  - opc * scalar;
    filter->a2 = filter->a0;
	filter->b1 = -2.0 * filter->cos_omega * scalar;
	filter->b2 = (1.0 - filter->alpha) * scalar;
}


/*********************************************************************************
 ** Calculate coefficients for Notch filter.
 */
void BiquadFilter_SetupNotch( BiquadFilter *filter, double ratio, double Q )
{
	double    scalar, opc;
	
	if( ratio  < BIQUAD_MIN_RATIO )  ratio  = BIQUAD_MIN_RATIO;
	if( Q < BIQUAD_MIN_Q ) Q = BIQUAD_MIN_Q;
	
	BiquadFilter_CalculateCommon( filter, ratio, Q );
	
	scalar = 1.0 / (1.0 + filter->alpha);
	opc = (1.0 + filter->cos_omega);
	
	filter->a0 = scalar;
	filter->a1 =  -2.0 * filter->cos_omega * scalar;
    filter->a2 = filter->a0;
	filter->b1 = filter->a1;
	filter->b2 = (1.0 - filter->alpha) * scalar;
}

/*****************************************************************
** Perform core IIR filter calculation without permutation.
*/
void BiquadFilter_Filter( BiquadFilter *filter, float *inputs, float *outputs, int numSamples )
{
	int i;
    double xn, yn;
	// Pull values from structure to speed up the calculation.
	double a0 = filter->a0;
	double a1 = filter->a1;
	double a2 = filter->a2;
	double b1 = filter->b1;
	double b2 = filter->b2;
	double xn1 = filter->xn1;
	double xn2 = filter->xn2;
	double yn1 = filter->yn1;
	double yn2 = filter->yn2;

	for( i=0; i<numSamples; i++)
	{
		// Generate outputs by filtering inputs.
		xn = inputs[i];
		yn = (a0 * xn) + (a1 * xn1) + (a2 * xn2) - (b1 * yn1) - (b2 * yn2);
		outputs[i] = yn;

		// Delay input and output values.
        xn2 = xn1;
        xn1 = xn;
        yn2 = yn1;
        yn1 = yn;
		
		if( (i & 7) == 0 )
		{
			// Apply a small bipolar impulse to filter to prevent arithmetic underflow.
			// Underflows can cause the FPU to interrupt the CPU.
			yn1 += (double) 1.0E-26;
			yn2 -= (double) 1.0E-26;
		}
	}
	
	filter->xn1 = xn1;
	filter->xn2 = xn2;
	filter->yn1 = yn1;
	filter->yn2 = yn2;		
}