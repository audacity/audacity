/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MirAudioReaders.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "MirAudioReader.h"

namespace MIR
{

class EmptyMirAudioReader : public MirAudioReader
{
   double GetSampleRate() const override
   {
      return 0;
   }
   long long GetNumSamples() const override
   {
      return 0;
   }
   void
   ReadFloats(float* buffer, long long start, size_t numFrames) const override
   {
   }
   std::unique_ptr<MirAudioReader> Clone() const override
   {
      return std::make_unique<EmptyMirAudioReader>(*this);
   }
};

class SquareWaveMirAudioReader : public MirAudioReader
{
public:
   const int period = 8;

   double GetSampleRate() const override
   {
      return 10;
   }
   long long GetNumSamples() const override
   {
      return period * 10;
   }
   void
   ReadFloats(float* buffer, long long where, size_t numFrames) const override
   {
      for (size_t i = 0; i < numFrames; ++i)
         buffer[i] = (where + i) % period < period / 2 ? 1.f : -1.f;
   }
   std::unique_ptr<MirAudioReader> Clone() const override
   {
      return std::make_unique<SquareWaveMirAudioReader>(*this);
   }
};

} // namespace MIR
