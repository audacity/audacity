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

//
//  DyadicFilter.hpp
//  tfq
//
//  Created by Klaus Gram-Hansen on 12/11/2025.
//

#ifndef DyadicFilter_h
#define DyadicFilter_h

#include <utility>
#include <vector>
#include "version.h"

using namespace std;

class DyadicFilter
{
public:
   DyadicFilter(const unsigned int nOctaves);
   ~DyadicFilter();
   
   static unsigned int  getExtraSamples(unsigned int nOctaves);

   /**
    Find lowest octave that still contains frequencies up to fmax
    */
   unsigned int findOctave(const float fmax) const;
   void filterSamples(const TF_DATA_TYPE * pSamples, const unsigned int n_samples, unsigned int n_samples_before, unsigned int n_samples_after);
   /**
    @param nSamples will be filled with number of samples in range from last "insertSamples" operation for the requested octave
    */
   const pair<TF_DATA_TYPE *, unsigned int> getSamples(const unsigned int octave, const int fromSample) const;
   void doAllocation(unsigned int nSamples, unsigned int nSamplesBefore, unsigned int nSamplesAfter);
private:
   vector<TF_DATA_TYPE *> vBufferBegin;      // Point to start of allocated buffer
   vector<TF_DATA_TYPE *> vBufferTimeZero;   // Point to location of time zero in allocated buffer
   vector<unsigned int> vBufferLengths;            // Point to total length of allocated buffer (starting from BufferBegin)
   void verify();

   /*

   Lowpass FIR filter designed with
   http://t-filter.appspot.com

   sampling frequency: 1000 Hz

   * 0 Hz - 195 Hz
     gain = 1
     desired ripple = 0.1 dB
     actual ripple = 0.07025740605239647 dB

   * 305 Hz - 500 Hz
     gain = 0
     desired attenuation = -100 dB
     actual attenuation = -100.41931587015137 dB

   */

   static constexpr unsigned int cstFilterTaps = 39;
   static constexpr float cstFreqLimit = 0.2;
   static const long cstMagic;
   static const TF_DATA_TYPE filter_taps[cstFilterTaps];
};
#endif /* DyadicFilter_h */
