/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavMirAudioReader.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "MirAudioReader.h"

#include <WavFileIO.h>

namespace MIR
{
class WavMirAudioReader : public MirAudioReader
{
public:
   WavMirAudioReader(
      const std::string& filename, std::optional<double> timeLimit = {});

   int GetSampleRate() const override;
   long long GetNumSamples() const override;
   void
   ReadFloats(float* buffer, long long start, size_t numFrames) const override;

private:
   const std::vector<float> mSamples;
   const double mSampleRate = 0.;
};
} // namespace MIR
