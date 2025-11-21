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

namespace DynamicRangeProcessorUtils {
template<typename Struct> struct Preset
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

namespace Detail {
// Public for testing, shouldn't be needed in production code.

struct SerializedPreset
{
    const TranslatableString name;
    const std::string settings;
};

const std::vector<SerializedPreset> serializedCompressorPresets {
//general
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Modern"),
      "Compressor:attackMs=\"0.2\" compressionRatio=\"4\" kneeWidthDb=\"18\" lookaheadMs=\"1\" makeupGainDb=\"0\" releaseMs=\"210\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-14\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Glue Compressor"),
      "Compressor:attackMs=\"20\" compressionRatio=\"1.2\" kneeWidthDb=\"12\" lookaheadMs=\"1\" makeupGainDb=\"2.5\" releaseMs=\"1000\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-22\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Gentle"),
      "Compressor:attackMs=\"1\" compressionRatio=\"1.5\" kneeWidthDb=\"6\" lookaheadMs=\"1\" makeupGainDb=\"0\" releaseMs=\"100\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-18\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Beat Booster"),
      "Compressor:attackMs=\"14\" compressionRatio=\"4\" kneeWidthDb=\"1\" lookaheadMs=\"1\" makeupGainDb=\"3\" releaseMs=\"9\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-18\"" },
//master
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Deep Dive Master"),
      "Compressor:attackMs=\"52.2\" compressionRatio=\"1.2\" kneeWidthDb=\"1\" lookaheadMs=\"33.2\" makeupGainDb=\"1.6\" releaseMs=\"12.2\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"1\" thresholdDb=\"-23.5\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Beefy Master"),
      "Compressor:attackMs=\"49.6\" compressionRatio=\"1.2\" kneeWidthDb=\"4.9\" lookaheadMs=\"100.4\" makeupGainDb=\"2.5\" releaseMs=\"17.9\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-16.8\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Make It Right Master"),
      "Compressor:attackMs=\"1\" compressionRatio=\"1.4\" kneeWidthDb=\"1\" lookaheadMs=\"10\" makeupGainDb=\"1.6\" releaseMs=\"1\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-6.5\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Brick Wall Master"),
      "Compressor:attackMs=\"0\" compressionRatio=\"100\" kneeWidthDb=\"2\" lookaheadMs=\"1\" makeupGainDb=\"3\" releaseMs=\"2\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-10\"" },
//Vocal
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Lead Vocals"),
      "Compressor:attackMs=\"1\" compressionRatio=\"5.2\" kneeWidthDb=\"5.5\" lookaheadMs=\"1\" makeupGainDb=\"0\" releaseMs=\"60\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-14\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Fat Vocals"),
      "Compressor:attackMs=\"86.9\" compressionRatio=\"1.7\" kneeWidthDb=\"5\" lookaheadMs=\"1\" makeupGainDb=\"2.5\" releaseMs=\"15.2\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-32\"" },
    /*i18n-hint: This is the name of an effect preset. */
    { XO("Power Vocals"),
      "Compressor:attackMs=\"2.8\" compressionRatio=\"1.5\" kneeWidthDb=\"19.6\" lookaheadMs=\"46.2\" makeupGainDb=\"3\" releaseMs=\"356.3\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-16.8\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Vocal Control"),
      "Compressor:attackMs=\"0\" compressionRatio=\"3\" kneeWidthDb=\"23.5\" lookaheadMs=\"1\" makeupGainDb=\"4.5\" releaseMs=\"196\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-15\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Vocal Touch-Up"),
      "Compressor:attackMs=\"2\" compressionRatio=\"1.5\" kneeWidthDb=\"30\" lookaheadMs=\"0\" makeupGainDb=\"3.6\" releaseMs=\"450\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"1\" thresholdDb=\"-22\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Voice Memos Balancer"),
      "Compressor:attackMs=\"6.5\" compressionRatio=\"10.1\" kneeWidthDb=\"5.8\" lookaheadMs=\"1\" makeupGainDb=\"4.5\" releaseMs=\"3.6\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-22.3\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Podcast/Radio"),
      "Compressor:attackMs=\"15\" compressionRatio=\"3\" kneeWidthDb=\"24\" lookaheadMs=\"1\" makeupGainDb=\"1\" releaseMs=\"40\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-15\"" },
//instruments
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Piano"),
      "Compressor:attackMs=\"0.2\" compressionRatio=\"2\" kneeWidthDb=\"18\" lookaheadMs=\"1\" makeupGainDb=\"1\" releaseMs=\"150\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-16\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Acoustic Guitar"),
      "Compressor:attackMs=\"15\" compressionRatio=\"2.5\" kneeWidthDb=\"8\" lookaheadMs=\"1\" makeupGainDb=\"1.5\" releaseMs=\"225\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-15\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Bass Guitar"),
      "Compressor:attackMs=\"1\" compressionRatio=\"3\" kneeWidthDb=\"2\" lookaheadMs=\"40\" makeupGainDb=\"0\" releaseMs=\"50\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-13\"" },
    /*i18n-hint: This is the name of an effect preset. Strings is violins, etc*/
    { XO("Strings"),
      "Compressor:attackMs=\"30\" compressionRatio=\"1.8\" kneeWidthDb=\"14.3\" lookaheadMs=\"1\" makeupGainDb=\"2.5\" releaseMs=\"400\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-15\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Kick Drums"),
      "Compressor:attackMs=\"30\" compressionRatio=\"4\" kneeWidthDb=\"0.5\" lookaheadMs=\"1\" makeupGainDb=\"2\" releaseMs=\"120\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-14\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Drums Control"),
      "Compressor:attackMs=\"2\" compressionRatio=\"2\" kneeWidthDb=\"29\" lookaheadMs=\"1\" makeupGainDb=\"1\" releaseMs=\"40\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-12\"" },
//SFX
    /*i18n-hint: This is the name of an effect preset. SFX means sound effects.*/
    { XO("Climax Impulser SFX"),
      "Compressor:attackMs=\"172\" compressionRatio=\"23.4\" kneeWidthDb=\"27.4\" lookaheadMs=\"0\" makeupGainDb=\"0\" releaseMs=\"813.4\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-55.1\"" },
    /*i18n-hint: This is the name of an effect preset. SFX means sound effects.*/
    { XO("Engine Breathing SFX"),
      "Compressor:attackMs=\"190.2\" compressionRatio=\"4.7\" kneeWidthDb=\"3.5\" lookaheadMs=\"2.3\" makeupGainDb=\"0\" releaseMs=\"0.2\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-37.7\"" },
    /*i18n-hint: This is the name of an effect preset. SFX means sound effects.*/
    { XO("Great Impact SFX"),
      "Compressor:attackMs=\"172\" compressionRatio=\"24.6\" kneeWidthDb=\"5\" lookaheadMs=\"0.6\" makeupGainDb=\"8.3\" releaseMs=\"562.6\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-49.3\"" },
    /*i18n-hint: This is the name of an effect preset. SFX means sound effects.*/
    { XO("Great Body SFX"),
      "Compressor:attackMs=\"74.6\" compressionRatio=\"2.4\" kneeWidthDb=\"0.3\" lookaheadMs=\"29.3\" makeupGainDb=\"8.6\" releaseMs=\"204.8\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-32.8\"" },
    /*i18n-hint: This is the name of an effect preset. SFX means sound effects.*/
    { XO("Great Tail SFX"),
      "Compressor:attackMs=\"1.4\" compressionRatio=\"2.4\" kneeWidthDb=\"0.3\" lookaheadMs=\"0\" makeupGainDb=\"23.9\" releaseMs=\"199.6\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-55.4\"" },
    /*i18n-hint: This is the name of an effect preset. SFX means sound effects.*/
    { XO("Smack Explosion SFX"),
      "Compressor:attackMs=\"155.5\" compressionRatio=\"5.9\" kneeWidthDb=\"24.4\" lookaheadMs=\"1.3\" makeupGainDb=\"7.1\" releaseMs=\"1.7\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-32.5\"" }
};

const std::vector<SerializedPreset> serializedLimiterPresets {
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Master Limiter"),
      "Limiter:kneeWidthDb=\"0.1\" lookaheadMs=\"0.1\" makeupTargetDb=\"-0.1\" releaseMs=\"0.1\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-0.1\"" },
    /*i18n-hint: This is the name of an effect preset. SFX means sound effects.*/
    { XO("SFX Limiter"),
      "Limiter:kneeWidthDb=\"1\" lookaheadMs=\"1\" makeupTargetDb=\"-1\" releaseMs=\"1\" showActual=\"1\" showInput=\"0\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-3\"" },
    /*i18n-hint: This is the name of an effect preset. VO means Voiceover.*/
    { XO("VO Limiter"),
      "Limiter:kneeWidthDb=\"0\" lookaheadMs=\"0.1\" makeupTargetDb=\"-1\" releaseMs=\"10.1\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-4\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Modern"),
      "Limiter:kneeWidthDb=\"0\" lookaheadMs=\"1\" makeupTargetDb=\"-1.5\" releaseMs=\"5\" showActual=\"0\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-1.5\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Modern Punch"),
      "Limiter:kneeWidthDb=\"0\" lookaheadMs=\"1\" makeupTargetDb=\"-1.5\" releaseMs=\"5\" showActual=\"0\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-1.5\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Modern Punch 2"),
      "Limiter:kneeWidthDb=\"1\" lookaheadMs=\"1\" makeupTargetDb=\"-1.5\" releaseMs=\"2\" showActual=\"1\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-1\"" },
    /*i18n-hint: This is the name of an effect preset.*/
    { XO("Play it Loud"),
      "Limiter:kneeWidthDb=\"0\" lookaheadMs=\"1\" makeupTargetDb=\"-2.2\" releaseMs=\"5\" showActual=\"0\" showInput=\"1\" showOutput=\"1\" showTarget=\"0\" thresholdDb=\"-8.5\"" },
};
} // namespace Detail
} // namespace DynamicRangeProcessorUtils
