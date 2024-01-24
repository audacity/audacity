/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MirAudioReader.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <cstddef>

namespace MIR
{
class MirAudioReader
{
public:
   virtual double GetSampleRate() const = 0;
   virtual long long GetNumSamples() const = 0;
   virtual void
   ReadFloats(float* buffer, long long where, size_t numFrames) const = 0;
   double GetDuration() const;
   virtual ~MirAudioReader() = default;
};
} // namespace MIR
