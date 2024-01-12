/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DecimatingMirAudioReader.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "DecimatingMirAudioReader.h"

#include <cmath>
#include <vector>

namespace MIR
{
DecimatingMirAudioReader::DecimatingMirAudioReader(const MirAudioReader& reader)
    : mReader { reader }
    , mDecimationFactor {
       // The highest sample rate less than or equal to 16kHz that is a
       // divisor of the input sample rate.
       static_cast<int>(std::ceil(reader.GetSampleRate() / 24000.))
    }
{
}

double DecimatingMirAudioReader::GetSampleRate() const
{
   return 1. * mReader.GetSampleRate() / mDecimationFactor;
}

long long DecimatingMirAudioReader::GetNumSamples() const
{
   // Return the floor
   return mReader.GetNumSamples() / mDecimationFactor;
}

void DecimatingMirAudioReader::ReadFloats(
   float* decimated, long long decimatedStart, size_t numDecimatedFrames) const
{
   const auto numFrames = numDecimatedFrames * mDecimationFactor;
   if (mBuffer.size() < numFrames)
      mBuffer.resize(numFrames);
   const auto start = decimatedStart * mDecimationFactor;
   mReader.ReadFloats(mBuffer.data(), start, numFrames);
   for (auto i = 0; i < numDecimatedFrames; ++i)
      decimated[i] = mBuffer[i * mDecimationFactor];
}
} // namespace MIR
