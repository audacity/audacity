/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeAndPitchRealSource.cpp

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "TimeAndPitchInterface.h"

#include <cassert>

class TimeAndPitchRealSource final : public TimeAndPitchSource
{
public:
   TimeAndPitchRealSource(const std::vector<std::vector<float>>& input)
       : mInput(input)
   {
   }

   size_t Pull(
      float* const* buffer, size_t numChannels,
      size_t samplesPerChannel) override
   {
      assert(numChannels == mInput.size());
      const auto numFrames = mInput[0].size();
      const auto remainingSamples =
         numFrames > mNumPulledFrames ? numFrames - mNumPulledFrames : 0u;
      const size_t framesToRead = std::min(
         remainingSamples,
         static_cast<decltype(remainingSamples)>(samplesPerChannel));
      const auto numZerosToPad = samplesPerChannel - framesToRead;
      for (auto i = 0u; i < numChannels; ++i)
      {
         const auto in = mInput[i].data() + mNumPulledFrames;
         std::copy(in, in + framesToRead, buffer[i]);
         std::fill(
            buffer[i] + framesToRead, buffer[i] + framesToRead + numZerosToPad,
            0.f);
      }
      mNumPulledFrames += framesToRead;
      return samplesPerChannel - numZerosToPad;
   }

   bool Empty() const override
   {
      return mNumPulledFrames >= mInput[0].size();
   }

private:
   const std::vector<std::vector<float>>& mInput;
   unsigned long long mNumPulledFrames = 0u;
};
