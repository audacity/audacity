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
   // clang-format off
   // Example:
   // { XO("Fat VO"),
   //   "Compressor:attackMs=\"86.9\" compressionRatio=\"1.7\" kneeWidthDb=\"5\" lookaheadMs=\"1\" makeupGainDb=\"2.5\" releaseMs=\"15.2\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-32\"" },
   // clang-format on
};

const std::vector<SerializedPreset> serializedLimiterPresets {
   // clang-format off
   // Example:
   // { XO("Master Limiter"),
   //   "Limiter:kneeWidthDb=\"0.1\" lookaheadMs=\"0.1\" makeupTargetDb=\"-0.1\" releaseMs=\"0.1\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-0.1\"" },
   // clang-format on
};
} // namespace Detail
} // namespace DynamicRangeProcessorUtils
