/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  NumericConverterTests.cpp

  Dmitry Vedenko

**********************************************************************/

#include <iostream>
#include <numeric>
#include <catch2/catch.hpp>

#include "MockedAudio.h"
#include "MockedPrefs.h"
#include "MockSampleBlock.h"
#include "Project.h"
#include "SampleBlock.h"
#include "WaveClip.h"

namespace
{
   class MockSampleBlockFactory final : public SampleBlockFactory
   {
      SampleBlockIDs GetActiveBlockIDs() override
      {
         std::vector<long long> ids(blockIdCount);
         std::iota(ids.begin(), ids.end(), 1LL);
         return { ids.begin(), ids.end() };
      }

      SampleBlockPtr DoCreate(
         constSamplePtr src, size_t numsamples, sampleFormat srcformat) override
      {
         return std::make_shared<MockSampleBlock>(
            blockIdCount++, src, numsamples, srcformat);
      }

      SampleBlockPtr
      DoCreateSilent(size_t numsamples, sampleFormat srcformat) override
      {
         std::vector<char> silence(numsamples * SAMPLE_SIZE(srcformat));
         return std::make_shared<MockSampleBlock>(
            blockIdCount++, silence.data(), numsamples, srcformat);
      }

      SampleBlockPtr
      DoCreateFromXML(sampleFormat srcformat, const AttributesList& attrs) override
      {
         return nullptr;
      }

      long long blockIdCount = 0;
   };

   constexpr double rate = 192000;

   bool eq(double t, double sample)
   {
      auto d = std::abs(t - sample / rate);
      return d < 0.5 / rate;
   }
}

bool TestClipTrimLengthInvariant(WaveClip& clip)
{
   auto offset = .0;
   auto trim = .0;
   const auto targetOffset = 0.5 / clip.GetRate();
   const auto targetTrim = 1.0 / clip.GetRate();
   const auto eps = 0.000001 / clip.GetRate();

   const auto duration = clip.GetPlayDuration();
   const auto durationEps = duration * std::numeric_limits<double>::epsilon();

   do 
   {
      // Clips could have arbitrary offset,
      // length of the clip should not depend on position
      clip.SetSequenceStartTime(offset);
      if(std::abs(clip.GetPlayDuration() - duration) > durationEps)
         return false;

      // Trim reduces the length of the clip.
      // Clip length should stay multiple of the sample length,
      // and not exceed the length of a trimmed sequence
      
      while (targetTrim - trim > eps)
      {
         trim = (targetTrim + trim) * 0.5;
         clip.SetTrimRight(trim);
         clip.SetTrimLeft(targetTrim - trim);
         if(std::abs(clip.GetPlayDuration() - (duration - targetTrim)) > durationEps)
            return false;
      }

      clip.SetTrimLeft(targetTrim);
      clip.SetTrimRight(.0);
      if(std::abs(clip.GetPlayDuration() - (duration - targetTrim)) > durationEps)
         return false;

      clip.SetTrimLeft(.0);
      clip.SetTrimRight(targetTrim);
      if(std::abs(clip.GetPlayDuration() - (duration - targetTrim)) > durationEps)
         return false;

      clip.SetTrimLeft(.0);
      clip.SetTrimRight(.0);
      trim = .0;

      offset = (targetOffset + offset) * 0.5;
   } while (targetOffset - offset > eps);

   return true;
}


TEST_CASE("ParsedNumericConverterFormatter", "")
{
   MockedPrefs mockedPrefs;
   MockedAudio mockedAudio;

   auto sampleBlockFactory = std::make_shared<MockSampleBlockFactory>();

   SECTION("Test clip length invariant")
   {
      auto stretchFactors =
      {
         1.0, // no stretching applied
         3.0 / 10.0, // stretched sample length is more than twice shorter compared to unstretched sample
         25.0 / 10.0  // stretched sample length is more than twice longer compared to unstretched sample
      };

      for(auto factor : stretchFactors)
      {
         const auto clip = std::make_unique<WaveClip>(1, sampleBlockFactory, floatSample, rate, 0);
         clip->InsertSilence(0, 10.0 / rate);
         clip->Stretch(clip->GetPlayDuration() / factor);

         REQUIRE(TestClipTrimLengthInvariant(*clip));
         //TODO: check sequence sample mapping
      }
   }

   SECTION("Sample grid alignment")
   {
      auto clip = std::make_unique<WaveClip>(1, sampleBlockFactory, floatSample, rate, 0);

      clip->InsertSilence(0, 10.0 / rate);
      REQUIRE(eq(clip->GetPlayStartTime(), 0));
      REQUIRE(eq(clip->GetPlayEndTime(), 10));
      REQUIRE(eq(clip->GetPlayDuration(), 10));

      clip->SetPlayStartTime(0.25 / rate);
      REQUIRE(eq(clip->GetPlayStartTime(), 0));
      REQUIRE(eq(clip->GetPlayEndTime(), 10));
      REQUIRE(eq(clip->GetPlayDuration(), 10));

      clip->SetPlayStartTime(0.5 / rate);
      REQUIRE(eq(clip->GetPlayStartTime(), 1));
      REQUIRE(eq(clip->GetPlayEndTime(), 11));
      REQUIRE(eq(clip->GetPlayDuration(), 10));

      clip->SetTrimLeft(1.0 / rate);
      REQUIRE(eq(clip->GetPlayStartTime(), 2));
      REQUIRE(eq(clip->GetPlayEndTime(), 11));
      REQUIRE(eq(clip->GetPlayDuration(), 9));

      clip->SetTrimRight(1.0 / rate);
      REQUIRE(eq(clip->GetPlayStartTime(), 2));
      REQUIRE(eq(clip->GetPlayEndTime(), 10));
      REQUIRE(eq(clip->GetPlayDuration(), 8));
   }
}
