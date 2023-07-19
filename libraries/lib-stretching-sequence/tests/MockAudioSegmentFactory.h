/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MockAudioSegmentFactory.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioSegment.h"
#include "AudioSegmentFactoryInterface.h"

class NiceAudioSegment final : public AudioSegment
{
public:
   size_t GetFloats(std::vector<float*>&, size_t numSamples) override
   {
      return numSamples;
   }

   size_t GetWidth() const override
   {
      return 1u;
   }

   bool Empty() const override
   {
      return false;
   }
};

class MockAudioSegmentFactory : public AudioSegmentFactoryInterface
{
public:
   std::vector<std::shared_ptr<AudioSegment>>
   CreateAudioSegmentSequence(double, PlaybackDirection) const override
   {
      ++const_cast<size_t&>(callCount);
      return { std::make_shared<NiceAudioSegment>() };
   }

   size_t callCount = 0u;
};
