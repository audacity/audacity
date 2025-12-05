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
//  WaveletVoiceUnbuffered.cpp
//  test
//
//  Created by Klaus Gram-Hansen on 17/11/2025.
//

#include "WaveletVoiceUnbuffered.h"
#include <iostream>
#include <assert.h>

WaveletVoiceUnbuffered::WaveletVoiceUnbuffered(const float overlapPercentage,
                           const DyadicFilter * dFilter,
                           const double fCenter) :
   dyadicFilter(dFilter),
   octave(0),
   resultLen(0),
   overlap(overlapPercentage),
   waveletHalfLength(0),
   duration(0),
   transformLength(0),
   resultStep(0),
   frequency(fCenter)
{}

void WaveletVoiceUnbuffered::dump()
{
}

WaveletVoiceUnbuffered::~WaveletVoiceUnbuffered()
{
   if(waveletHalfLength)
   {
      delete[]waveletRe;
      delete[]waveletIm;
      waveletHalfLength=0;
   }
}
   
void WaveletVoiceUnbuffered::getRequiredPaddingSamples(unsigned int & pre, unsigned int & post) const
{
   // We need WaveletHalfLen samples from the LP filter before and WaveletHalfLen+1 after
   // These numbers must be multiplied by 1<<octave. Additionally, we must add requirement from LP filter
   assert(waveletHalfLength); // Must be initialised by derived constructor
   assert(transformLength);
   assert(resultLen);
   assert(resultStep > 0);
   pre = (waveletHalfLength << octave) + dyadicFilter->getExtraSamples(octave);
   post = pre + resultLen * resultStep - transformLength;
}

pair<unsigned int, unsigned int> WaveletVoiceUnbuffered::calculateResultLenAndStep(unsigned int _resolution) const
{
   assert(transformLength);
   unsigned int resultStep = std::max(transformStep, (_resolution / 2) >> octave ) << octave; // Be more conservative internally with resolution
   unsigned int rval = 1 + (transformLength - 1) / resultStep;
   int remaining = (transformLength - 1) - (rval -1) * resultStep;
   while (2 * remaining >= (long) resultStep)
   {
      remaining -= resultStep;
      assert(2 * remaining < (long) resultStep);
      rval++;
   }
   return pair<unsigned int, unsigned int>(rval, resultStep);
}

void WaveletVoiceUnbuffered::allocateResult(unsigned int nSamples, unsigned int _resolution)
{
   transformLength = nSamples;
   tie(resultLen, resultStep) = calculateResultLenAndStep(_resolution);
}
   
int WaveletVoiceUnbuffered::transform()
{
   return 0; // We do not do anything when unbuffered
}
   
TF_DATA_TYPE WaveletVoiceUnbuffered::get(double timestamp) const
{
   // During getting, we do the calculations
   assert(timestamp >= 0 && timestamp < transformLength);
   assert(resultLen);
   assert(resultStep);
   
   // Simply step through data until result is full!
   pair<TF_DATA_TYPE *, unsigned int> rval = dyadicFilter->getSamples(octave, (unsigned int)(timestamp + 0.5) -(waveletHalfLength << octave));
   if (rval.second == 0)
   {
      return 0;
   }

   TF_DATA_TYPE * ptS = rval.first + waveletHalfLength;
   TF_DATA_TYPE * ptR = ptS + 1;
   TF_DATA_TYPE * ptL = ptS - 1;
   TF_DATA_TYPE * ptWRe = waveletRe + 1;
   TF_DATA_TYPE * ptWIm = waveletIm + 1;
   TF_DATA_TYPE sumRe = *waveletRe * *ptS;
   TF_DATA_TYPE sumIm = 0;
   for (int j = waveletHalfLength; j--;)
   {
      sumRe += (*ptL   + *ptR  ) * *ptWRe++;
      sumIm += (*ptL-- - *ptR++) * *ptWIm++;
   }
   assert(ptL - rval.first >= -1);
   assert(ptR - rval.first <= rval.second);
   return sumRe * sumRe + sumIm * sumIm;
}
   
