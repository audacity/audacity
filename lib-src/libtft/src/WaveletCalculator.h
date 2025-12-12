/**
Time Frequency Calculator Library
Copyright (C) 2025  Klaus Gram-Hansen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#include "TimeFrequencyCalculator.h"
#include "DyadicFilter.h"
#include "ConfinedGaussianWaveletVoice.h"

class WaveletCalculator : public ITimeFrequencyCalculator
{
public:
   /**
    Setup a new wavelet time-frequemcy transform.
     
     LOWER FREQUENCY
     ================
     Assuming nominal sampling frequncy (1.0), the lower frequency covered by the analysis will be 0.5/2^nOctaves
    
     FREQUENCY RESOLUTION
     ====================
      Frequency Resolution (dF) will be defined as 2*RMS bandwidth of analyzing wavelet. Frequency resolution relative to actual frequency, Q = F/dF, will be found such that Q = 1/(2^(1/2nBands)-2^-(1/2nBands))
    
         Typical choices are
      nBands   Q
        1         1.4 (Octave filter)
        3         4.3    (3rd Octave filter )
        6         8.7 (6th Octave filter, good default for acoustics)
       12        17.3 (12th Octave filter)

     TEMPORAL RESOLUTION
     ===================
      Temporal Resolution (dT) will be defined as 2*RMS duration of analyzing wavelet. Due to the application of close-to Gaussian wavelet, the Heisenberg equality holds with good approximation:
        dF * dT = 1/PI
        dT = 1/PI/dF = Q/(PI*F)  [ie meaning Q/PI oscillations within temporal resolution]
    
     FRAME BOUNDS (Discretization)
     =============
         Parseval's theorem must also hold true fro a time-frequency representation of a signal. This can only be achieved as an approximation. Looking at the discretization of the constatnt Q transform, we define overlap in time and frequency domain via dT and dF, respectively. The maximum ripple contributed by the trasform (max enery lost is
               at 0% overlap : -2.2dB
               at 50% overlap in time AND frequency : 0.5 dB
    
            0% overlap in time and frequency (dF spacing in frequency, dT spacing in time)
            ==========================================================
               Nb values in 1st octave, first band : 1/dT per second. --> Approximately nBands / (Q/(PI * 0.375)) --> nBands/Q
               Nb values in all octaves 2*nBands/Q -->3 per second at nominal sampling frequency (approximately according to aove table - just a guideline)
            
            50% overlap in time and frequency (dF/2 spacing in frequency, dT/2 spacing in time)
            ==========================================================
                Nb values in 1st octave, first band : 2/dT per second. --> Approximately (2*nBands) *2 / (Q/(PI * 0.375)) --> 4*nBands/Q
                Nb values in all octaves 8*nBands/Q -->12 per second at nominal sampling frequency (approximately according to aove table - just a guideline)

    */
   
    /**
     Construct a wavelet calculator that will divide frequency axis into nOctaves having each nBands
      Lower frequency analyzed being equal to Fs/2/2^nOctaves
       Example Fs = 44100Hz, 10 octaves --> 22Hz
     @param nOctaves is the lowest range of frequencies as specifed above. Note that this should not be chosen higher than necessary since this implies larger time domain support and thus more buffering of adjoining samples (or zeropadding)
     @param overlapPercentage defines the precision. choosing 0.5 (50% overlap) is a good starting point. For significantly more speed, a value of 0 could prove more relevant. This also consumes less memory
     */
   WaveletCalculator( unsigned int nOctaves,
                      double fmax,
                      float Q,
                      float overlapPercentage);
    ~WaveletCalculator();
   
   /**
    do a time frequency transform calculating the internal representation in the time/frequency plane
    @param pSamples points to the data being investigated,
    @param nSamples is the number of samples to be investigated
    @param nValidSamplesBefore is the number of samples valid BEFORE the sample pointed to by pSamples, is pSamples-1, pSamples-2 ... pSamples-nValidSamplesBefore also points to valid samples
    @param nValidSamplesAfter is the number of samples valid starting from the samples pointed to by pSamples + nSamples, ie pSamples+nSamples, pSamples+nSamples+1, ... , psamples+nSamples+nValidSamplesAfter-1 must point to valid samples
        |-- nValidSamplesBefore --|-- nSamples --|-- nValidSamplesAfter --|
                           ^
                           |_ pSamples
    
    @return the number of values stored internally by the transformation.
    
    More precisely pSamples points to the point in time at which the continuous time/frequency transform will start. In order to achieve this, information before must exists (nValidSamplesBefore).
    The continuous time/frequency transform must be calculated until just before the point in time corresponding to pSamples + nSamples
    In case nValidSamplesBefore is lower than first of getRequiredPaddingSamples() or nValidSamplesAfter is lower than second of getRequiredPaddingSamples(), then zero padding will be applied
    */
   virtual unsigned int  doTransform(const TF_DATA_TYPE* pSamples, unsigned int nSamples, unsigned int nValidSamplesBefore, unsigned int nValidSamplesAfter) override;
   
   /**
    Reset any internal state to that of a newly created object. This may be a time-saver as opposed to free/allocate af new object
    */
   virtual void  prepare(unsigned int nSamples, unsigned int resolution, unsigned int & nPre, unsigned int & nPost) override;
   
   /** Member functions for obtaining discrete values of the underlying transform. This discretization happens in the continuous time/frequency plane and a number of schemes can apply. Such methods must be defined or implied during creation of specific instances implementing this interface
    1) Some method for interpolation between transform values. In its simples form: Choose closest neighbour. More advanced could be using 2D splines
    2) Some scheme of normalisation. We here assume that the instantaneous level is returned (numerical value). For STFT or constant Q, this can be seen as instantaneous RMS level of a given frequency component.
    3) Some method for aggregation of densely calculated transforms must exist. In its simplest form this could imply choosing level of closest neighbour. In order to avoid missing peaks, a scheme of choosing highest value in neighbourhood could be i,lied. Such schemes must be defined and documented by implementing classes
    
    The below methods provide ways to extract values at equidistant time stamps on either linearly or arbitrarily spaced frequence intervals
    
    @param sampleOffset is the offset from pSamples (time zero) in a previous call to doTransform() or doNextTransform(). This offset allows stitching adjoining transforms together nicely
    @param stepTime is the equidistant step between frequency slices
    @param nTimeSteps is the number of timesteps for which to return slices
    
    @param freqOffset is a value in the range 0..0.5 and specifies the first frequency returned in slice
    @param freqStep is a linear interval between frequencies
    @param nFreqSteps is the number of frequency steps. The inequality freqOffset + freqStep*(nFreqStep-1) < 0.5 must be fulfilled
    
    @param frequencies is a vector of arbitrary frequencies, each of which must be in range 0..0.5
    
    @param out is an array with space for nOut values
    @param nOut must fulfill inequality nTimeSteps*nFreqSteps <= nOut
    The inequality    sampleOffset + stepTime*nTimeSteps < nSamples
    must always be fulfilled. Otherwise, 0 will be returned
    
    @return Number of points calculated, ie nTimeSteps * nFreqSteps */
   virtual int extractFrequencySlices(const std::vector<double> & timestamps,
                                      const std::vector<double> & frequencies,
                                      TF_DATA_TYPE *  out,
                                      int    nOut,
                                      bool transpose
                                      ) const override;
   
   

private:
   DyadicFilter dyadicFilter;
   vector<WaveletBaseClass *> waveletVoices;
   unsigned int nSamples;
   
   /**
    Tell caller about the amount of padding required (left,right) when making calls to doTransform.
    This is essential information to the caller when seeking to avoid artificial transients at the edges of signal
    being investigated
    */
   virtual void getRequiredPaddingSamples(unsigned int &nPre, unsigned int & nPost) const;
};


