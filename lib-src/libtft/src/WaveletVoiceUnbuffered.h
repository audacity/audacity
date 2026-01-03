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

class WaveletVoiceUnbuffered
{
public:
   virtual void dump();
   virtual ~WaveletVoiceUnbuffered();
   virtual void getRequiredPaddingSamples(unsigned int & pre, unsigned int & post) const;
   virtual void allocateResult(unsigned int nSamples, unsigned int resolution);
   virtual int transform();
   virtual TF_DATA_TYPE get(double timestamp) const;
   virtual void getFrequency(double & freq, double & bw) const = 0;

protected:
   WaveletVoiceUnbuffered(const float overlapPercentage,
                const DyadicFilter * dFilter,
                const double fCenter);
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
};

#endif // !WaveletVoiceUnbuffered_h
