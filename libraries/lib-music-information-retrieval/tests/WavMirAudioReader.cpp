/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavMirAudioReader.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "WavMirAudioReader.h"

#include <cassert>
#include <exception>
#include <stdexcept>

namespace MIR
{
WavMirAudioReader::WavMirAudioReader(
   const std::string& filename, std::optional<double> timeLimit)
{
   WavFileIO::Info info;
   std::vector<std::vector<float>> samples;
   if (!WavFileIO::Read(filename, samples, info))
      throw std::runtime_error("Failed to read WAV file");

   const_cast<double&>(mSampleRate) = info.sampleRate;
   const auto limit = timeLimit.has_value() ?
                         static_cast<long long>(*timeLimit * info.sampleRate) :
                         std::numeric_limits<long long>::max();
   auto& mutableSamples = const_cast<std::vector<float>&>(mSamples);
   const auto numFrames = std::min<long long>(info.numFrames, limit);
   mutableSamples.resize(numFrames);
   if (info.numChannels == 2)
      for (size_t i = 0; i < numFrames; ++i)
         mutableSamples[i] = (samples[0][i] + samples[1][i]) / 2.f;
   else
      std::copy(
         samples[0].begin(), samples[0].begin() + numFrames,
         mutableSamples.begin());
}

int WavMirAudioReader::GetSampleRate() const
{
   return mSampleRate;
}

long long WavMirAudioReader::GetNumSamples() const
{
   return mSamples.size();
}

void WavMirAudioReader::ReadFloats(
   float* buffer, long long start, size_t numFrames) const
{
   assert(start >= 0);
   assert(start + numFrames <= mSamples.size());
   std::copy(
      mSamples.begin() + start, mSamples.begin() + start + numFrames, buffer);
}
} // namespace MIR
