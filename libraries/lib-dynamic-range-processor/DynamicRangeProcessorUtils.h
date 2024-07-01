/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorUtils.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "DynamicRangeProcessorTypes.h"
#include "Internat.h"
#include "TranslatableString.h"

namespace DynamicRangeProcessorUtils
{
template <typename Struct> struct Preset
{
   const TranslatableString name;
   Struct preset;
};

using CompressorPreset = Preset<CompressorSettings>;
using LimiterPreset = Preset<LimiterSettings>;

DYNAMIC_RANGE_PROCESSOR_API
std::vector<CompressorPreset> GetCompressorPresets();
DYNAMIC_RANGE_PROCESSOR_API
std::vector<LimiterPreset> GetLimiterPresets();

namespace Detail
{
// Public for testing, shouldn't be needed in production code.

struct SerializedPreset
{
   const TranslatableString name;
   const std::string settings;
};

const std::vector<SerializedPreset> serializedCompressorPresets {
   { XO("Fat VO"),
     "Compressor:attackMs=\"86.9\" compressionRatio=\"1.7\" kneeWidthDb=\"5\" lookaheadMs=\"1\" makeupGainDb=\"2.5\" releaseMs=\"15.2\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-32\"" },
   { XO("Female Power VO"),
     "Compressor:attackMs=\"2.8\" compressionRatio=\"1.5\" kneeWidthDb=\"19.6\" lookaheadMs=\"46.2\" makeupGainDb=\"3\" releaseMs=\"356.3\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-16.8\"" },
   { XO("Voice Memos Balancer VO"),
     "Compressor:attackMs=\"6.5\" compressionRatio=\"10.1\" kneeWidthDb=\"5.8\" lookaheadMs=\"1\" makeupGainDb=\"4.5\" releaseMs=\"3.6\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-22.3\"" },
   { XO("Great Impact SFX"),
     "Compressor:attackMs=\"172\" compressionRatio=\"24.6\" kneeWidthDb=\"5\" lookaheadMs=\"0.6\" makeupGainDb=\"8.3\" releaseMs=\"562.6\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-49.3\"" },
   { XO("Great Body SFX"),
     "Compressor:attackMs=\"74.6\" compressionRatio=\"2.4\" kneeWidthDb=\"0.3\" lookaheadMs=\"29.3\" makeupGainDb=\"8.6\" releaseMs=\"204.8\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-32.8\"" },
   { XO("Great Tail SFX"),
     "Compressor:attackMs=\"1.4\" compressionRatio=\"2.4\" kneeWidthDb=\"0.3\" lookaheadMs=\"0\" makeupGainDb=\"23.9\" releaseMs=\"199.6\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-55.4\"" },
   { XO("Deep Dive Master"),
     "Compressor:attackMs=\"52.2\" compressionRatio=\"1.2\" kneeWidthDb=\"1\" lookaheadMs=\"33.2\" makeupGainDb=\"1.6\" releaseMs=\"12.2\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"1\" thresholdDb=\"-23.5\"" },
   { XO("Beefy Master"),
     "Compressor:attackMs=\"49.6\" compressionRatio=\"1.2\" kneeWidthDb=\"4.9\" lookaheadMs=\"100.4\" makeupGainDb=\"2.5\" releaseMs=\"17.9\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-16.8\"" },
   { XO("Make It Right Master"),
     "Compressor:attackMs=\"1\" compressionRatio=\"1.4\" kneeWidthDb=\"1\" lookaheadMs=\"10\" makeupGainDb=\"1.6\" releaseMs=\"1\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-6.5\"" },
};

const std::vector<SerializedPreset> serializedLimiterPresets {
   { XO("Master Limiter"),
     "Limiter:kneeWidthDb=\"0.1\" lookaheadMs=\"0.1\" makeupTargetDb=\"-0.1\" releaseMs=\"0.1\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-0.1\"" },
   { XO("SFX Limiter"),
     "Limiter:kneeWidthDb=\"1\" lookaheadMs=\"1\" makeupTargetDb=\"-1\" releaseMs=\"1\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-3\"" }
};
} // namespace Detail
} // namespace DynamicRangeProcessorUtils
