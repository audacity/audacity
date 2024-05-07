// TODO header
#pragma once

#include <array>
#include <memory>
#include <vector>

enum DynamicRangeProcessorType
{
   CompressorDynamicRangeProcessor,
   LimiterDynamicRangeProcessor,
   NumDynamicRangeProcessorTypes
};

static constexpr auto limiterRatio = 1000.;

struct DynamicRangeProcessorOutputSample
{
   int frameCounter = 0;
   float targetCompressionDb = 0;
   float actualCompressionDb = 0;
};

static constexpr std::array<double, NumDynamicRangeProcessorTypes>
   inCompressionThreshDbDefaults { -10., -5. };
static constexpr std::array<double, NumDynamicRangeProcessorTypes>
   outCompressionThreshDbDefaults { -10., -1. };
static constexpr std::array<double, NumDynamicRangeProcessorTypes>
   kneeWidthDbDefaults { 0., 0. };
static constexpr std::array<double, NumDynamicRangeProcessorTypes>
   compressionRatioDefaults { 10., limiterRatio };
static constexpr std::array<double, NumDynamicRangeProcessorTypes>
   lookaheadMsDefaults { 0., 0. };
static constexpr std::array<double, NumDynamicRangeProcessorTypes>
   attackMsDefaults { 30., 0. };
static constexpr std::array<double, NumDynamicRangeProcessorTypes>
   releaseMsDefaults { 150., 20. };

struct CompressorSettings
{
   double inCompressionThreshDb {
      inCompressionThreshDbDefaults[CompressorDynamicRangeProcessor]
   };
   double outCompressionThreshDb {
      outCompressionThreshDbDefaults[CompressorDynamicRangeProcessor]
   };
   double kneeWidthDb { kneeWidthDbDefaults[CompressorDynamicRangeProcessor] };
   double compressionRatio {
      compressionRatioDefaults[CompressorDynamicRangeProcessor]
   };
   double lookaheadMs { lookaheadMsDefaults[CompressorDynamicRangeProcessor] };
   double attackMs { attackMsDefaults[CompressorDynamicRangeProcessor] };
   double releaseMs { releaseMsDefaults[CompressorDynamicRangeProcessor] };
};

struct LimiterSettings : CompressorSettings
{
   LimiterSettings()
       : CompressorSettings {
          inCompressionThreshDbDefaults[LimiterDynamicRangeProcessor],
          outCompressionThreshDbDefaults[LimiterDynamicRangeProcessor],
          kneeWidthDbDefaults[LimiterDynamicRangeProcessor],
          compressionRatioDefaults[LimiterDynamicRangeProcessor],
          lookaheadMsDefaults[LimiterDynamicRangeProcessor],
          attackMsDefaults[LimiterDynamicRangeProcessor],
          releaseMsDefaults[LimiterDynamicRangeProcessor]
       }
   {
   }
};

constexpr bool
operator==(const CompressorSettings& lhs, const CompressorSettings& rhs)
{
   return lhs.inCompressionThreshDb == rhs.inCompressionThreshDb &&
          lhs.outCompressionThreshDb == rhs.outCompressionThreshDb &&
          lhs.kneeWidthDb == rhs.kneeWidthDb &&
          lhs.compressionRatio == rhs.compressionRatio &&
          lhs.lookaheadMs == rhs.lookaheadMs && lhs.attackMs == rhs.attackMs &&
          lhs.releaseMs == rhs.releaseMs;
}

constexpr bool
operator!=(const CompressorSettings& lhs, const CompressorSettings& rhs)
{
   return !(lhs == rhs);
}
