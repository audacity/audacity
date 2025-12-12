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
#include "RealFFTf.h"
#include <vector>
using namespace std;

class StftCalculator : public ITimeFrequencyCalculator
{
public:
   /**
    Setup a Short-time Fourier Transform time-frequemcy transform having constant bandwidth and time duration
    
     TIME and FREQUENCY RESOLUTION
     ============================
    Governed directly by the window function passed in constructor. Not described here. Likewise, no responsibiity taken for snugness of frame constituted by transform: This is left to the caller
      Frequency Resolution (dF) will be defined as 2*RMS bandwidth of analyzing window.
    
    */
   
    /**
     Construct a STFT calculator from a window function and a given transform length
     @param windowLength is the number of samples in window function. Must be a power of two. Center of symmetry is assumed at windowLength/2
     @param transformLength is the length of FT applied. Must be a power of two. Must be larger than or equal to windowLength. Maximum 32768 points
     Zeropadding will be applied if transformLength > windowLength
     @param pWindow points to an array of window coefficients (length windowLength(
     */
   StftCalculator(unsigned int windowLength,
                  unsigned int transformLength,
                  const TF_DATA_TYPE * pWindow);
    ~StftCalculator();
   
   /**
    do a time frequency transform calculating the internal representation in the time/frequency plane
    @param pSamples points to the data being investigated,
    @param nSamples is the number of samples to be investigated
    @param nValidSamplesBefore is the number of samples valid BEFORE the sample pointed to by pSamples, is pSamples-1, pSamples-2 ... pSamples-nValidSamplesBefore also points to valid samples
    @param nValidSamplesAfter is the number of samples valid starting from the samples pointed to by pSamples + nSamples, ie pSamples+nSamples, pSamples+nSamples+1, ... , psamples+nSamples+nValidSamplesAfter-1 must point to valid samples
        |-- nValidSamplesBefore --|-- nSamples --|-- nValidSamplesAfter --|
                           ^
                           |_ pSamples
    
    @return the number of values stored internally by the transformation (will be zero since we store none in current implementation)
    
    More precisely pSamples points to the point in time at which the continuous time/frequency transform will start. In order to achieve this, information before must exists (nValidSamplesBefore).
    The continuous time/frequency transform must be calculated until just before the point in time corresponding to pSamples + nSamples
    In case nValidSamplesBefore is lower than nPre (as indicated by call to prepare()) or nValidSamplesAfter is lower than nPre (as indicated by call to prepare()), then zero padding will be applied
    
    An FFT calculation will be configured for the given transformLength (twiddle factors and bit reverse table)
    */
   virtual unsigned int  doTransform(const TF_DATA_TYPE* pSamples, unsigned int nSamples, unsigned int nValidSamplesBefore, unsigned int nValidSamplesAfter) override;
   
   /**
    Prepare a call to "doTransform" (optional) in order to learn about required context samples
    @param pSamples points to the data being investigated,
    @param nSamples is the number of samples to be investigated
    @param nPre will on return hold the context required before beginning of samples
    @param nPost will on return hold the context required after end of samples
    */
   virtual void  prepare(unsigned int nSamples, unsigned int resolution, unsigned int & nPre, unsigned int & nPost) override;
   
   /** Member functions for obtaining discrete values of the underlying transform. This discretization happens in the continuous time/frequency plane and a number of schemes can apply. Such methods must be defined or implied during creation of specific instances implementing this interface
    1) Some method for interpolation between transform values. In its simples form: Choose closest neighbour. More advanced could be using 2D splines
    2) Some scheme of normalisation. We here assume that the instantaneous level is returned (numerical value). For STFT or constant Q, this can be seen as instantaneous Power level of a given frequency component.
    3) Some method for aggregation of densely calculated transforms must exist. In its simplest form this could imply choosing level of closest neighbour. In order to avoid missing peaks, a scheme of choosing highest value in neighbourhood could be i,lied. Such schemes must be defined and documented by implementing classes
    
    The below methods provide ways to extract values at equidistant time stamps on either linearly or arbitrarily spaced frequence intervals (see also overloaded versions from base class)
    
    @param timestamps is a vector of arbitrary timestamps, each of which must be in range 0..nSamples-1
    @param frequencies is a vector of arbitrary frequencies, each of which must be in range 0..0.5
    
    @param out is an array with space for nOut values
    @param nOut must fulfill inequality nTimeSteps*nFreqSteps <= nOut
    
    @return Number of points calculated, ie nTimeSteps * nFreqSteps */
   virtual int extractFrequencySlices(const std::vector<double> & timestamps,
                                      const std::vector<double> & frequencies,
                                      TF_DATA_TYPE *  out,
                                      int    nOut,
                                      bool transpose
                                      ) const override;
   
   

private:
   unsigned int nWindowLength;               ///< Length of window function used
   unsigned int nTransformLength;            ///< FFT transform length applied
   unsigned int nSamples;                    ///< Length of signal being investigated
   vector<TF_DATA_TYPE>  vWindow;            ///< Holds coefficients of window function properly normalized
   vector<TF_DATA_TYPE> vTransformBuffer;    ///< Buffer used to hold signal being investigated
   unique_ptr<FFTParam> hFFT;                ///< Pointer to FFT related structures
};


