/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipMirAudioReader.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioSegmentSampleView.h"
#include "MirAudioReader.h"

#include <array>
#include <optional>

class ClipInterface;

class ClipMirAudioReader : public MIR::MirAudioReader
{
public:
   ClipMirAudioReader(const ClipInterface& clip);

   double GetSampleRate() const override;
   long long GetNumSamples() const override;

   /*!
    * @pre `where >= 0`
    * @pre `where + numFrames <= GetNumSamples()`
    */
   void
   ReadFloats(float* buffer, long long where, size_t numFrames) const override;

private:
   void AddChannel(
      size_t iChannel, float* buffer, sampleCount start, size_t len) const;

   const ClipInterface& mClip;
   mutable std::array<AudioSegmentSampleView, 2> mCache;
};
