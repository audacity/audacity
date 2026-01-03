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

#ifndef _TIME_FREQ_CALC_H
#define _TIME_FREQ_CALC_H
#include <vector>
#include <utility>
#include "version.h"

/**
 A Time/frequency transform is in this context understood as a transform that operatates on a 1D digital signal: It calculates a representation in the time/frequency plane of the signal thus blowing it up from a 1D signal into a 2D signal (values as a function of time --> values as a function of time and frequency)
 A Time/frequency transform can be either purely digital if having orthonormal bases in the t/f plane or may result in a continuous transform with values existing for any point in the time/frequency plane. In the latter case, a transform inevitably involves a loss of information since not all values can be calculated - however a sufficiently dense representation can still be very useful
 
 The current interface does not immediately provide any quality insights related to represetation of information for an analyzed signal. Such insight is a specific property of the implementing sub classes
 
 In its current version, the interface is quite rudimentary in providing support for these steps in performing time/frequency anasyis:
 
 Construct --> (Transform --> Extract)*
 
 Since the construction can be time-consuming by itself, it can be advantageous to re-use an instantiated object
 
 Construct
 =======
 The interface provides no methods for construcing  instances: The logic is entirely left to implementing sub-classes.
 By design, it is not expected that a constructor should require information about the period of time, that an analysis will be done
 However, one member function must be provided that gives information to the caller about the required time-support necessary for providing proper time/frequency analysis in a time interval.
 This member, prepare(), is not set to have const attribute since it must allow for setting up buffers for calculations and results. Still, the member is not mandatory to call during construction
 
 Transform
 =======
 A single method, doTransform(),  provides support for transformation. It works by passing in a time sequence following which the class is expected to calculate a representation in the time/frequency domain
 
 Extract
 =====
 Currently, two member functions fall in this category, namely overloaded versions of extractFrequencySlices(). They provide for extracting the RMS level of the time/frequency transform on either a rectangular grid in the t/f plane or for arbitrary frequencies
  These functions will pick the closest calculated point in the plane. A lot of variations and combinations could be imagined
 1) Provide magnitude/phase information
 2) Provide levels converted to decibels
 3) Provided values interpolated linearly or higher-order
 
 Possible Extensions
 ===============
 It could be interesting to extend with methods for modifying values in the time/frequency plane and next transform back, Ie the sequency Construct->Transform->Extract->Modify->TransformBack
 
 Known subclasses
 =============
 WaveletCalculator : Implements time/frequency transform using  Gaussian wavelets
 */
class ITimeFrequencyCalculator
{
public:
   /**
    Adapt internal state to a specific length of analysis. This may be a time-saver as opposed to free/allocate af new object because it need only be called once as long as number of samples being anlysed will not change
    Tell caller about the amount of padding required (left,right) prior to making calls to doTransform.
    This is essential information to the caller when seeking to avoid artificial transients at the edges of signal
    being investigated.
    
     @param nSamples specifies the number of samples that need investigation
     @return a pair of integers specifying padding required before (first) and after (second) a signal
    */
   virtual void  prepare(unsigned int nSamples, unsigned int resolution, unsigned int & nPre, unsigned int & nPost) = 0;
   
   /**
    do a time frequency transform calculating the internal representation in the time/frequency plane.
    The internal representation is valid in the time range 0..nSamples-1 and in the frequency range ranging from lowest frequency supported up to 0.5 (nominal frequency)
    In order to provide this range, an adjoining neighbour hood of samples can be made available to the transform. In case this neighbourhood is not made avaliable or is insufficient, zeropadding of the provided samples will be in effect
    The relevant neighbour hood can be interrogated from the time/Frequency calculater once instantiated via the return of the method prepare().
    @param pSamples points to the data being investigated,
    @param nSamples is the number of samples to be investigated
    @param nValidSamplesBefore is the number of samples valid BEFORE the sample pointed to by pSamples, ie pSamples-1, pSamples-2 ... pSamples-nValidSamplesBefore also points to valid samples
    @param nValidSamplesAfter is the number of samples valid starting from the samples pointed to by pSamples + nSamples, ie pSamples+nSamples, pSamples+nSamples+1, ... , psamples+nSamples+nValidSamplesAfter-1 must point to valid samples
        |-- nValidSamplesBefore --|-- nSamples --|-- nValidSamplesAfter --|
                           ^
                           |_ pSamples
    
    @return the number of values stored internally by the transformation.
    
    More precisely pSamples points to the point in time at which the continuous time/frequency transform will start. In order to achieve this, information before must exists (nValidSamplesBefore).
    The continuous time/frequency transform must be calculated until just before the point in time corresponding to pSamples + nSamples-1
    In case nValidSamplesBefore is lower than first of prepare() or nValidSamplesAfter is lower than second of prepare(), then zero padding will be applied
    */
   virtual unsigned int  doTransform(const TF_DATA_TYPE* pSamples, unsigned int nSamples, unsigned int nValidSamplesBefore, unsigned int nValidSamplesAfter) = 0;
   
   /** Member functions for obtaining discrete values of the underlying transform. This discretization happens in the continuous time/frequency plane and a number of schemes can apply. Such methods must be defined or implied during creation of specific instances implementing this interface
    1) Some method for interpolation between transform values. In its simples form: Choose closest neighbour. More advanced could be using 2D splines
    2) Some scheme of normalisation. We here assume that the instantaneous level is returned (numerical value). For STFT or constant Q, this can be seen as instantaneous RMS level of a given frequency component.
    3) Some method for aggregation of densely calculated transforms must exist. In its simplest form this could imply choosing level of closest neighbour. In order to avoid missing peaks, a scheme of choosing highest value in neighbourhood could be implied. Such schemes must be defined and documented by implementing classes
    
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
                                      ) const = 0;
   virtual int extractFrequencySlices(double sampleOffset,
                                      double stepTime,
                                      int    nTimeSteps,
                                      const std::vector<double> & frequencies,
                                      TF_DATA_TYPE *  out,
                                      int    nOut,
                                      bool transpose
                                      ) const
   {
      // Pack a vector of timestamps
      std::vector<double> timestamps;
      for (int i = 0; i < nTimeSteps; i++)
      {
         timestamps.push_back(sampleOffset + i * stepTime);
      }
      return extractFrequencySlices(timestamps, frequencies, out, nOut, transpose);
   }
   virtual int extractFrequencySlices(const std::vector<double> & timestamps,
                                      double freqOffset,
                                      double freqStep,
                                      int    nFreqSteps,
                                      TF_DATA_TYPE *  out,
                                      int    nOut,
                                      bool transpose
                                      ) const
   {
      // Pack a vector of frequencies
      std::vector<double> frequencies;
      for (int i = 0; i < nFreqSteps; i++)
      {
         frequencies.push_back(freqOffset + i * freqStep);
      }
      return extractFrequencySlices(timestamps, frequencies, out, nOut, transpose);
   }
   virtual int extractFrequencySlices(double sampleOffset,
                                      double stepTime,
                                      int    nTimeSteps,
                                      double freqOffset,
                                      double freqStep,
                                      int    nFreqSteps,
                                      TF_DATA_TYPE *  out,
                                      int    nOut,
                                      bool transpose
                                      ) const
   {
      // Pack a vector of frequencies
      std::vector<double> frequencies;
      for (int i = 0; i < nFreqSteps; i++)
      {
         frequencies.push_back(freqOffset + i * freqStep);
      }
      return extractFrequencySlices(sampleOffset, stepTime, nTimeSteps, frequencies, out, nOut, transpose);
   }

   /**
    Virtual destructor does nothing - but must be defined
    */
   virtual     ~ITimeFrequencyCalculator() {}
};
#endif
