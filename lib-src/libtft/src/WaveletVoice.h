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

#ifndef WaveletVoice_h
#define WaveletVoice_h

#include "DyadicFilter.h"
#include "WaveletVoiceUnbuffered.h"

class WaveletVoice : public WaveletVoiceUnbuffered
{
public:
   virtual void dump() override;
   virtual ~WaveletVoice();
   virtual void allocateResult(unsigned int nSamples, unsigned int resolution) override;
   int transform() override;
   TF_DATA_TYPE get(double timestamp) const override;

protected:
   WaveletVoice(const float overlapPercentage,
                const DyadicFilter * dFilter,
                const double fCenter);
   TF_DATA_TYPE * resultRe;
   TF_DATA_TYPE * resultIm;
};

#endif /* WaveletVoice_h */
