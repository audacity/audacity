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
#include <iostream>
#include <assert.h>
#include "WaveletCalculator.h"
#include "ConfinedGaussianWaveletVoice.h"

WaveletCalculator::WaveletCalculator(unsigned int nOctaves,
                                     double fmax,
                                     float Q,
                                     float overlapPercentage) : dyadicFilter(nOctaves), nSamples(0)
{
   std::cerr << __FILE__ << ", Construct" << std::endl;
   assert(nOctaves > 0);
   assert(fmax > 0 && fmax <= 0.5);
   assert(Q > 0);
   assert(overlapPercentage < 100); // negative is ok, though unusual
   
   // Calculate increment
   float increment = (Q - 0.5) / (Q + 0.5 - overlapPercentage/100) ;
   assert(increment < 1); //Avoid recursion
   
   float fCenter = fmax;  // Place first filter as high as possible
   int nVoices = (int)(nOctaves * log(2) /( -log(increment)));
   for (int i = nVoices; i--; fCenter *= increment)
   {
      cout << "Making wavelet at fc " << fCenter << endl;
      waveletVoices.push_back(new ConfinedGaussianWaveletVoice(fCenter, Q, overlapPercentage, &dyadicFilter));
      waveletVoices.back()->dump();
      
   }
   cout << "Total of " << waveletVoices.size() << " wavelets in " << nOctaves << " Octaves. Q is " << Q << endl;
}

WaveletCalculator::~WaveletCalculator()
{
   while (!waveletVoices.empty())
   {
      delete waveletVoices.back();
      waveletVoices.pop_back();
   }
   std::cerr << __FILE__ << ", Destruct" << std::endl;
}

/**
 Tell caller about the amount of padding required (left,right) when making calls to doTransform.
 This is essential information to the caller when seeking to avoid artificial transients at the edges of signal
 being investigated
 */
 void WaveletCalculator::getRequiredPaddingSamples(unsigned int &nPre, unsigned int & nPost) const
 {
    unsigned int pre(0);
    unsigned int post(0);
    nPre = 0;
    nPost = 0;
    for (auto iter = waveletVoices.begin(); iter != waveletVoices.end(); iter++)
    {
       (*iter)->getRequiredPaddingSamples(pre, post);
       nPre = max(pre, nPre);
       nPost = max(post, nPost);
    }
 }
    

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
 unsigned int  WaveletCalculator::doTransform(const TF_DATA_TYPE* pSamples, const unsigned int nSamples, const unsigned int nValidSamplesBefore, const unsigned int nValidSamplesAfter)
 {
    unsigned int pre, post;
    prepare(nSamples, 0, pre, post); // Here we prepare without limiting resolution. If prepare was already called with limiting resolution, such resolution will be preserved
    // Feed data into our dyadic filter
    dyadicFilter.filterSamples(pSamples, nSamples, nValidSamplesBefore, nValidSamplesAfter);
    
    // And then ask all voices to co-operate
    unsigned int rval = 0;
    for (auto iter = waveletVoices.begin(); iter != waveletVoices.end(); iter++)
    {
       rval += (*iter)->transform();
    }
    return rval;
 }
   
 /**
  Reset any internal state to that of a newly created object. This may be a time-saver as opposed to free/allocate af new object
 */
void  WaveletCalculator::prepare(unsigned int n_samples, unsigned int resolution, unsigned int & nPre, unsigned int & nPost)
 {
    nSamples = n_samples;
    for (auto iter = waveletVoices.begin(); iter != waveletVoices.end(); iter++)
    {
       (*iter)->allocateResult(nSamples, resolution);
    }
    
    // Then do allocation of dyadic filter since we now do know requirements
    getRequiredPaddingSamples(nPre, nPost);
    dyadicFilter.doAllocation(nSamples, nPre, nPost);
 }

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

 int WaveletCalculator::extractFrequencySlices(const std::vector<double> & timestamps,
                                               const std::vector<double> & frequencies,
                                               TF_DATA_TYPE *  out,
                                               int    nOut,
                                               bool transpose
                                               ) const
 {
    // Do some consistency checks
    assert(nSamples);
    assert(timestamps.size() * frequencies.size() <= nOut);
    
    // Iterate frequencies
    TF_DATA_TYPE * pOut = out;
    size_t transposeOffset = 0;
    size_t outIncrement = 1;
    for (std::vector<double>::const_iterator iterFreq = frequencies.begin(); iterFreq != frequencies.end(); iterFreq++)
    {
       if (transpose)
       {
          // Restart a new slice
          pOut = out + transposeOffset++;
          outIncrement = frequencies.size();
       }
       // Consider some out-of-range frequencies
       assert(*iterFreq <= 0.5);
       if (*iterFreq <= 0.0) {
          for (int i = 0; i < timestamps.size(); i++)
          {
             *pOut = 0;
             pOut += outIncrement;
          }
          continue;
       }

       // Locate the voice closest to the asked-for frequency
       const WaveletBaseClass * pVoice = NULL;
       double bestLogDist = 999;
       double bwBest, freqBest;
       
       for (auto iter = waveletVoices.cbegin(); iter != waveletVoices.cend(); iter++)
       {
          double freq;
          double bw;
          double dist;
          (*iter)->getFrequency(freq, bw);
          dist = abs(log(freq / *iterFreq));
          if (dist < bestLogDist)
          {
             freqBest = freq;
             bwBest = bw;
             bestLogDist = dist;
             pVoice = *iter;
          }
       }
       assert(pVoice);
       
       // Maybe we are not within range
       if (abs(*iterFreq - freqBest) > 0.5* bwBest) {
          for (int i = 0; i < timestamps.size(); i++)
          {
             *pOut = 0;
             pOut += outIncrement;
          }
          continue;
       }
       
       // Finally, iterate all timestamps
       for (std::vector<double>::const_iterator iterTime = timestamps.begin(); iterTime != timestamps.end(); iterTime++)
       {
          assert(*iterTime <= nSamples - 1);
          TF_DATA_TYPE val = pVoice->get(*iterTime);
          *pOut = val;
          pOut += outIncrement;
       }

    }
    return (int)(timestamps.size() * frequencies.size());
 }

/**
 Calculate half-length approximate confined gaussian window, ref https://en.wikipedia.org/wiki/Window_function#Confined_Gaussian_window
  This window is a good-enough estimate for confined gaussian window which has optimum time-frequency location for a fixed-length window
 */
/**
     Calculate half-length wavelet (complex conjugate symmetri) at relative frequency
      Result will have dimension halfLen+1 upon return
      Approximate temporal resolution is dT =  (2 * halfLen + 1) * sigma
      Approximate frequency resolution is dF = 1/4/pi/dT
      Approximate 70dB frequency resolution depends on choice of sigma. For sigma = 0.1, we get approximately 6.5 * dF
 
      Relative bandwidth of generated wavelet is 2*dF / relF which we solve for halfLen vs Q factor
      Q^-1 = 2*dF/relF = 1/2/pi/dT/relF  = 1 / (2 * pi * (2 * halfLen + 1) * sigma * relF)
 
       2 * halflen + 1 = 1 / (2 * pi * sigma * relF * Q^-1)
         halfLen = (1 / (2 * pi * sigma * relF * Q^-1) - 1) / 2
 
      Example
         Q^-1 = 0.1156 (1/6 octave filter, 1/Q = sqrt(2**1/6) - sqrt(2**-1/6)
         sigma = 0.1
         relF = 0.25
   
       ==> halfLen = 27.04
 
      Function will return true as long as upper -70dB limit of generated wavelet is below limiting frequency range. Note that return arrays resultRe/Im are populated in any case
       Wavelet upper frequency is estimated like upperF = relF + 6.5*dF = relF + 6.5 / 4 / pi / dT  = 0.52 / (2 * halfLen + 1) / sigma
      For above example we get
       upperF = 0.25 + 0.52 /0.1 /55 = 0.34
 
 *
 */
