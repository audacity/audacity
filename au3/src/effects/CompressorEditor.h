/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressorEditor.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "DynamicRangeProcessorEditor.h"
#include "SettingsVisitor.h"

class CompressorInstance;

using CompressorParameter = EffectParameter<CompressorSettings, double, double>;

class CompressorEditor final : public DynamicRangeProcessorEditor
{
public:
    CompressorEditor(
        wxWindow* parent, CompressorInstance& instance, bool isRealtime, const EffectUIServices& services, EffectSettingsAccess& access,
        CompressorSettings settings);

    static constexpr CompressorParameter thresholdDb {
        &CompressorSettings::thresholdDb,
        L"thresholdDb",
        compressorThresholdDbDefault,
        -60 / dbStep,
        0,
        1 / dbStep
    };

    static constexpr CompressorParameter makeupGainDb {
        &CompressorSettings::makeupGainDb,
        L"makeupGainDb",
        compressorMakeupGainDbDefault,
        -30 / dbStep,
        30 / dbStep,
        1 / dbStep
    };

    static constexpr CompressorParameter kneeWidthDb {
        &CompressorSettings::kneeWidthDb,
        L"kneeWidthDb",
        compressorKneeWidthDbDefault,
        0,
        30 / dbStep,
        1 / dbStep
    };

    static constexpr CompressorParameter compressionRatio {
        &CompressorSettings::compressionRatio,
        L"compressionRatio",
        compressorCompressionRatioDefault,
        1,
        100,
        1
    };

    static constexpr CompressorParameter lookaheadMs {
        &CompressorSettings::lookaheadMs, L"lookaheadMs",
        compressorLookaheadMsDefault,     0,
        compressorMaxLookaheadMs,         1
    };

    static constexpr CompressorParameter attackMs {
        &CompressorSettings::attackMs,
        L"attackMs",
        compressorAttackMsDefault,
        0,
        200,
        1
    };

    static constexpr CompressorParameter releaseMs {
        &CompressorSettings::releaseMs,
        L"releaseMs",
        compressorReleaseMsDefault,
        0,
        1000,
        1
    };

    static constexpr CompressorParameter showInput {
        &CompressorSettings::showInput, L"showInput", showInputDefault, 0, 1, 1
    };
    static constexpr CompressorParameter showOutput {
        &CompressorSettings::showOutput, L"showOutput", showOutputDefault, 0, 1, 1
    };
    static constexpr CompressorParameter showActual {
        &CompressorSettings::showActual, L"showActual", showActualDefault, 0, 1, 1
    };
    static constexpr CompressorParameter showTarget {
        &CompressorSettings::showTarget, L"showTarget", showTargetDefault, 0, 1, 1
    };

private:
    const CompressorSettings* GetCompressorSettings() const override
    {
        return &mSettings;
    }

    CompressorSettings mSettings;
};
