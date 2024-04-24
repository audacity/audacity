/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressorProcessor.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <array>
#include <memory>
#include <vector>

namespace DanielRudrich
{
class GainReductionComputer;
class LookAheadGainReduction;
} // namespace DanielRudrich

struct LimiterSettings
{
   static constexpr double thresholdDbDefault = -10;
   static constexpr double kneeDbDefault = 0;
   static constexpr double lookaheadMsDefault = 0;
   static constexpr double releaseMsDefault = 150;
   static constexpr double makeUpDbDefault = 0;

   double thresholdDb { thresholdDbDefault };
   double kneeDb { kneeDbDefault };
   double lookaheadMs { lookaheadMsDefault };
   double releaseMs { releaseMsDefault };
   double makeUpDb { makeUpDbDefault };
};

struct CompressorSettings : LimiterSettings
{
   static constexpr double attackMsDefault = 30;
   static constexpr double ratioDefault = 10;
   static constexpr double infRatio = std::numeric_limits<double>::infinity();

   double attackMs { attackMsDefault };
   double ratio { ratioDefault };

   CompressorSettings() = default;
   CompressorSettings(const LimiterSettings& settings)
       : LimiterSettings(settings)
   {
      attackMs = 0;
      ratio = infRatio;
   }
};

constexpr bool
operator==(const CompressorSettings& lhs, const CompressorSettings& rhs)
{
   return lhs.thresholdDb == rhs.thresholdDb && lhs.kneeDb == rhs.kneeDb &&
          lhs.lookaheadMs == rhs.lookaheadMs && lhs.attackMs == rhs.attackMs &&
          lhs.releaseMs == rhs.releaseMs && lhs.ratio == rhs.ratio &&
          lhs.makeUpDb == rhs.makeUpDb;
}

constexpr bool
operator!=(const CompressorSettings& lhs, const CompressorSettings& rhs)
{
   return !(lhs == rhs);
}

class EFFECTS_API CompressorProcessor
{
public:
   CompressorProcessor(const CompressorSettings& settings = {});
   CompressorProcessor(const CompressorProcessor& other) = delete;
   ~CompressorProcessor();

   void ApplySettingsIfNeeded(const CompressorSettings& settings);
   void Init(int sampleRate, int numChannels, int blockSize);
   void Reinit();
   const CompressorSettings& GetSettings() const;
   void
   Process(const float* const* inBlock, float* const* outBlock, int blockLen);

private:
   void UpdateEnvelope(const float* const* inBlock, int blockLen);
   void CopyWithDelay(const float* const* inBlock, int blockLen);
   void ApplyEnvelope(float* const* outBlock, int blockLen);
   bool Initialized() const;

   static constexpr auto maxBlockSize = 512;

   const std::unique_ptr<DanielRudrich::GainReductionComputer>
      mGainReductionComputer;
   const std::unique_ptr<DanielRudrich::LookAheadGainReduction>
      mLookAheadGainReduction;
   CompressorSettings mSettings;
   int mSampleRate = 0;
   int mNumChannels = 0;
   int mBlockSize = 0;
   std::array<float, maxBlockSize> mEnvelope;
   std::vector<std::vector<float>>
      mDelayedInput; // Can't conveniently use an array here, because neither
                     // delay time nor sample rate are known at compile time.
                     // Re-allocation during playback is only done if the user
                     // changes the look-ahead settings, in which case glitches
                     // are hardly avoidable anyway.
};
