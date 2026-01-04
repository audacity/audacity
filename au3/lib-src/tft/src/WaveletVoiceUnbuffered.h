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
//  WaveletVoice.h
//  test
//
//  Created by Klaus Gram-Hansen on 17/11/2025.
//

#ifndef WaveletVoiceUnbuffered_h
#define WaveletVoiceUnbuffered_h

#include "DyadicFilter.h"
#include <vector>

class WaveletVoiceUnbuffered
{
public:
   virtual void dump() const;
   virtual ~WaveletVoiceUnbuffered();
   virtual void getRequiredPaddingSamples(unsigned int & pre, unsigned int & post) const;
   virtual void allocateResult(unsigned int nSamples, unsigned int resolution);
   virtual int transform();
   virtual TF_DATA_TYPE get(double timestamp) const;
   virtual void getFrequency(double & freq, double & bw) const = 0;
   bool containsFrequency(double f) { return f >= fLow && f < fHigh;}
   void executeSequence(int freqStride, int timeStride, TF_DATA_TYPE * out, std::vector<double>::const_iterator timeIterBegin, std::vector<double>::const_iterator timeIterEnd, bool transpose);

protected:
   WaveletVoiceUnbuffered(const float overlapPercentage,
                const DyadicFilter * dFilter,
                double fCenter,
                double flow,
                double fhigh);
   virtual pair<unsigned int, unsigned int>  calculateResultLenAndStep(unsigned int _resolution) const;

   
   const DyadicFilter * dyadicFilter;
   unsigned int octave;
   TF_DATA_TYPE * waveletRe;
   TF_DATA_TYPE * waveletIm;
   unsigned int waveletHalfLength;
   unsigned int resultLen;
   const float overlap;
   float duration;
   unsigned int transformStep;
   unsigned int resultStep;
   unsigned int transformLength;
   double frequency;
   double fLow;
   double fHigh;
   mutable struct
   {
      const void * key;
      TF_DATA_TYPE value;
      void invalidate(){ key = NULL;};
      void set(const void * k, TF_DATA_TYPE val)
      {
         key = k;
         value = val;
      } const
      bool lookup(void * k, TF_DATA_TYPE & rval) const
      {
         if (k != key) return false;
         rval = value;
         return true;
      }
   } valueCache;
};

#endif // !WaveletVoiceUnbuffered_h
